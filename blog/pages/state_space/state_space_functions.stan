//////////////////////////////////
// Matrix Helper Functions
/////////////////////////////////

// Design Matrix
  // Z is the only matrix that will vary with time
array[] row_vector gen_z_arr(int n, int include_slope, matrix x_mat, int n_seasons){
  int season_cont = n_seasons > 2 ? n_seasons - 1 : 0;
  int z_size = include_slope + cols(x_mat) + season_cont + 1;
  array[n] row_vector[z_size] z_arr;
  
  row_vector[1 + season_cont + include_slope] left_pad;
  
  // Left pad with trend, slope, and seasons
  if (n_seasons > 2){
    left_pad = append_col(
      1,
      append_col(
        rep_row_vector(0, include_slope),
        append_col(
          1,
          rep_row_vector(0, n_seasons - 2)
        )
      )
    );
  }
  else {
    left_pad = append_col(1, rep_row_vector(0, include_slope));
  }
  
  // Add in covariate data if applicable
  if (cols(x_mat) > 0){
    for (i in 1:n){
      z_arr[i] = append_col(left_pad, x_mat[i]);
    }
  }
  else{
    for (i in 1:n){
      z_arr[i] = left_pad;
    }
  }
  return z_arr;
}

// State Transition Matrix
matrix gen_T_mat(int include_slope, int n_covar, int n_seasons){
  int season_cont = n_seasons > 2 ? n_seasons - 1 : 0;
  int t_size = include_slope + n_covar + season_cont + 1;
  
  matrix[t_size, t_size] T_mat = rep_matrix(0, t_size, t_size);
  T_mat[1,1] = 1;
  
  if (include_slope == 1){
    T_mat[1,2] = 1;
    T_mat[2,2] = 1;
  }
  // Add seasons
  if (n_seasons > 2){
    int start_ind = 2 + include_slope;
    int end_ind = n_seasons - 2 + start_ind;
    // Add -1s for next season transition
    T_mat[start_ind, start_ind:end_ind] = rep_row_vector(-1, n_seasons - 1);
    
    for (i in start_ind:(start_ind + n_seasons - 3)){
      T_mat[i + 1, i] = 1;
    }
  }
  // Add coef transition
  if (n_covar > 0){
    int start_ind = t_size - n_covar + 1;
    for (i in start_ind:t_size){
      T_mat[i,i] = 1;
    }
  }
  return T_mat;
}

// State Covariance Selection Matrix
  // Assume that trend, slope, and season are allowed to vary with time 
  // Not coefficients
matrix gen_R_mat(int include_slope, int n_covar, int n_seasons){
  int season_cont = n_seasons > 2 ? n_seasons - 1 : 0;
  int col_size = 1 + include_slope + (n_seasons > 2 ? 1 : 0);
  int row_size = 1 + include_slope + n_covar + season_cont;
  
  matrix [row_size, col_size] R_mat = rep_matrix(0, row_size, col_size);
  R_mat[1,1] = 1;
  
  if (include_slope == 1)
    R_mat[2,2] = 1;
  
  if (n_seasons > 2)
    R_mat[2 + include_slope, include_slope + 2] = 1;
  
  return R_mat;
}

//////////////////////////////////
// Kalman Filtering and Smoothing
/////////////////////////////////
vector kalman_filter_smooth(int n, vector y, vector a_1, vector P_1_diag, real noise_var, 
                            vector var_vec, matrix T_mat, matrix R_mat, 
                            array[] row_vector Z_arr){
  matrix[size(P_1_diag), size(P_1_diag)] P_1 = diag_matrix(P_1_diag);
  
  // Forward pass (filtering)
  vector[n] F;
  vector[n] v;
  array[n+1] vector[num_elements(a_1)] a;
  a[1] = a_1;
  
  array[n+1] matrix[rows(P_1), cols(P_1)] P;
  P[1] = P_1;
  
  array[n] vector[num_elements(a_1)] K;
  array[n] matrix[rows(T_mat), cols(T_mat)] L;
  
  matrix[num_elements(var_vec), num_elements(var_vec)] Q_mat = diag_matrix(var_vec);
  

  for (i in 1:n){
    v[i] = y[i] - dot_product(Z_arr[i], a[i]);
    F[i] = Z_arr[i] * P[i] * Z_arr[i]' + noise_var;
    K[i] = T_mat * P[i] * Z_arr[i]' * (1 / F[i]);
    
    L[i] = T_mat - K[i] * Z_arr[i];
    
    P[i + 1] = T_mat * P[i] * L[i]' + R_mat * Q_mat * R_mat';
    a[i + 1] = (T_mat * a[i]) + (K[i] * v[i]);

  }
  // Backward Pass (smoothing)
    // r goes from 0 to n, not 1 to n + 1 (like a or alpha)
  array[n + 1] vector[num_elements(a_1)] r = rep_array(rep_vector(0,num_elements(a_1)), n + 1);
    // Only need to first n
  array[n] vector[num_elements(a_1)] alpha_smooth;

  for (d in 1:n){
    // Make the loop go from n to 1, not 1 to n
    int i = n - d + 1;
    
    // We are actually writing to r_{n-1} for the first iteration, not r_n
    // Again, this is becauase r is defined from 0 to n, so r[n] actually grabs r_{n-1}
    r[i] = Z_arr[i]' * (1/F[i]) * v[i] + L[i]' * r[i + 1];
    alpha_smooth[i] = a[i] + P[i] * r[i];
  }

  // Pack relevant matricies into single array
  vector[num_elements(alpha_smooth) + num_elements(F) + num_elements(v)] rag_arr;

    // Alpha Smooth
  array[2] int alpha_dim = dims(alpha_smooth);
  int cnt = 1;

  for (i in 1:alpha_dim[1]){
    for (j in 1:alpha_dim[2]){
      rag_arr[cnt] = alpha_smooth[i,j];
      cnt += 1;
    }
  }

    // F vector
  for (i in 1:num_elements(F)){
    rag_arr[cnt] = F[i];
    cnt += 1;
  }

    // v Vector
  for (i in 1:num_elements(v)){
    rag_arr[cnt] = v[i];
    cnt += 1;
  }
  return rag_arr;
}

array[] vector get_alpha_smooth(vector kalman_object, array[] int s, int n, int a_size){
  // Alpha is stored in the first segment
  array[n] vector[a_size] alpha_smooth;
  vector[s[1]] alpha_store =  segment(kalman_object, 1, s[1]);
  
  int cnt = 1;
  for (i in 1:(n)){
    for (j in 1:a_size){
      alpha_smooth[i,j] = alpha_store[cnt];
      cnt += 1;
    }
  }
  return alpha_smooth;
}

vector get_F(vector kalman_object, array[] int s){
  // F is stored in the second segment
  return segment(kalman_object, 1 + s[1], s[2]);
}

vector get_v(vector kalman_object, array[] int s){
  return segment(kalman_object, 1 + sum(s[1:2]), s[3]);
}

real state_space_lpdf(vector kalman_object, array[] int s, int n){
  vector[n] F = get_F(kalman_object, s);
  vector[n] v = get_v(kalman_object, s);

  real ll = 0;
  for (i in 1:n){
    ll += log(F[i]) + v[i]^2 * (1 / F[i]);
  }
  return -0.5 * ll;
}

//////////////////////////////////
// Simulation
/////////////////////////////////

// Evolve series from a given alpha and matricies
  // Note, we are starting with a given alpha, not a
vector evolve_series_rng(vector starting_alpha, int n, real noise_var, vector var_vec, matrix T_mat, matrix R_mat, 
                     array[] row_vector Z_arr){
  int a_size = num_elements(starting_alpha);
  matrix[num_elements(var_vec), num_elements(var_vec)] Q_mat = diag_matrix(var_vec);
  
  vector[n] y_plus;
  array[n + 1] vector[a_size] alpha_plus;
  alpha_plus[1] = starting_alpha;
  
  for (i in 1:n){
    y_plus[i] = dot_product(Z_arr[i], alpha_plus[i]) + normal_rng(0, noise_var);
    alpha_plus[i + 1] = T_mat * alpha_plus[i] + R_mat * multi_normal_rng(rep_vector(0, cols(Q_mat)), Q_mat);
  }
  
  int cnt = 1;
  // pack alpha and y into vector
  vector[n * a_size + n] rag_arr;
  
    // pack alpha first
  for (i in 1:n){
    for (j in 1:a_size){
      rag_arr[cnt] = alpha_plus[i,j];
      cnt += 1;
    }
  }
    // pack y
  for (i in 1:n){
    rag_arr[cnt] = y_plus[i];
    cnt += 1;
  }
  
  return rag_arr;
}

// Get alpha plus from evolve object
array[] vector get_alpha_from_evolve(vector evolve_object, array[] int s_evolve, int n, int a_size){
  // Alpha is stored in the first segment
  array[n] vector [a_size] alpha;
  vector[s_evolve[1]] alpha_store = segment(evolve_object, 1, s_evolve[1]);
  
  int cnt = 1;
  for (i in 1:n){
    for (j in 1:a_size){
      alpha[i,j] = alpha_store[cnt];
    }
  }
  return alpha;
}

// Get y plus from evolve object
vector get_y_from_evolve(vector evolve_object, array[] int s_evolve){
  // y is stored in the second segment
  return segment(evolve_object, 1 + s_evolve[1], s_evolve[2]);
}

// Simulate state
array[] vector simulate_state_rng(vector kalman_object, int n, array[] int s, array[] int s_evolve, vector y, vector a_1, 
                      vector P_1_diag, real noise_var, vector var_vec, matrix T_mat, matrix R_mat, 
                      array[] row_vector Z_arr){
  // Set up constants
  int a_size = num_elements(a_1);
  matrix[a_size, a_size] P_1 = diag_matrix(P_1_diag);
  
  // Get alpha smooth for actually observed data
  array[n] vector[a_size] alpha_smooth = get_alpha_smooth(kalman_object, s, n, a_size);
  
  // Create plus system
  vector[a_size] starting_alpha = multi_normal_rng(rep_vector(0, a_size), P_1);
  vector[num_elements(alpha_smooth) + n] plus_system = evolve_series_rng(starting_alpha, n, noise_var, var_vec, T_mat,
                                                                         R_mat, Z_arr);
                                                                     
  vector[n] y_plus_system = get_y_from_evolve(plus_system, s_evolve);
  array[n] vector[a_size] alpha_plus_system = get_alpha_from_evolve(plus_system, s_evolve, n, a_size);
  
  // Smooth plus system
  vector[n * a_size + (2 * n)] plus_system_smooth = kalman_filter_smooth(n, y_plus_system, a_1, P_1_diag, noise_var,
                                                    var_vec, T_mat, R_mat, Z_arr);
  
  array[n] vector[a_size] alpha_plus_system_smooth = get_alpha_smooth(plus_system_smooth, s, n, a_size);
  
  // Get final sim vector
    // alpha_sim = alpha_plus - alpha_plus_smooth + alpha_smooth
  array[n] vector[a_size] alpha_smooth_sim;
  for (i in 1:n)
    alpha_smooth_sim[i] = alpha_plus_system[i] - alpha_plus_system_smooth[i] + alpha_smooth[i];
  
  return alpha_smooth_sim;
}

array[] vector extract_trends(array[] vector alpha_sim, int n, array[] row_vector Z_arr, int include_slope, int n_covar, 
                              int n_seasons){
  int season_cont = n_seasons > 2 ? n_seasons - 1 : 0;
  int has_season =  n_seasons > 2 ? 1 : 0;
  int has_regress = n_covar > 0 ? 1 : 0;
  
  array[1 + has_season + has_regress] vector[n] trend_comp_arr;
  for (i in 1:n){
    trend_comp_arr[1,i] = alpha_sim[i,1];
    if (has_season == 1)
      trend_comp_arr[2,i] = alpha_sim[i,2 + include_slope];
      
    if (has_regress == 1){
      vector[n_covar] reg_vec = Z_arr[i]' .* alpha_sim[i];
      trend_comp_arr[2 + has_season, i] = sum(reg_vec[(1 + include_slope + season_cont + 1):num_elements(Z_arr[i])]);
    }
  }
  return trend_comp_arr;
}

