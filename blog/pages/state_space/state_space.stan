functions {
  #include state_space_functions.stan
}

data {
  int<lower=1> N;
  vector[N] y;
  int<lower=0, upper=1> has_slope;
  int<lower=0> n_covar;
  int<lower=0> n_seasons;
  matrix[N,n_covar] X_mat;
}
transformed data{
  int season_cont = n_seasons > 2 ? n_seasons - 1 : 0;
  int a_size = has_slope + n_covar + season_cont + 1;
  int has_season =  n_seasons > 2 ? 1 : 0;
  int has_regress = n_covar > 0 ? 1 : 0;

  array[N] row_vector[a_size] Z_arr = gen_z_arr(N, has_slope, X_mat, n_seasons);
  matrix[a_size, a_size] T_mat = gen_T_mat(has_slope, n_covar, n_seasons);
  matrix[a_size, 1 + has_slope + has_season] R_mat = gen_R_mat(has_slope, n_covar, n_seasons);
  
  array[3] int s = {a_size*(N), N, N};
  array[2] int s_evolve = {a_size*(N), N};

  vector[N] mu_placeholder = rep_vector(0,N);
  // vector[a_size] P_1_diag = rep_vector(1,a_size);
}

parameters{
  vector<lower=0>[cols(R_mat)] var_vec;
  vector<lower=0>[a_size] P_1_diag;
  real<lower=0> noise_var;
}

transformed parameters{
  vector[a_size * N + (2 * N)] k_obj = kalman_filter_smooth(N, y, rep_vector(0, a_size), P_1_diag,
                                                            noise_var, var_vec, T_mat, R_mat, Z_arr);
  vector[N] F = sqrt(get_F(k_obj, s));
  vector[N] v = get_v(k_obj, s);
}

model {
  var_vec ~ exponential(1);
  P_1_diag ~ exponential(1);
  noise_var ~ exponential(1);

  v ~ normal(mu_placeholder, F);
}

generated quantities{
  array[N] vector[a_size] alpha_sim = simulate_state_rng(k_obj, N, s, s_evolve, y, rep_vector(0,a_size), P_1_diag, noise_var,
                                               var_vec, T_mat, R_mat, Z_arr);
  array[1 + has_season + has_regress] vector[N] trend_comp = extract_trends(alpha_sim, N, Z_arr, has_slope, n_covar, n_seasons);

  // // Working through example
  // matrix[a_size, a_size] P_1 = diag_matrix(P_1_diag);
  // vector[a_size] starting_alpha = multi_normal_rng(rep_vector(0, a_size), P_1);
  // vector[a_size * N + N] plus_system = evolve_series_rng(starting_alpha, N, noise_var, var_vec, T_mat,
  //                                                                       R_mat, Z_arr);

  // vector[N] y_plus_system = get_y_from_evolve(plus_system, s_evolve);
  // array[N] vector[a_size] alpha_plus_system = get_alpha_from_evolve(plus_system, s_evolve, N, a_size);
  
  // // Smooth plus system
  // vector[N * a_size + (2 * N)] plus_system_smooth = kalman_filter_smooth(N, y_plus_system, rep_vector(0, a_size), P_1_diag, noise_var,
  //                                                   var_vec, T_mat, R_mat, Z_arr);
  // array[N] vector[a_size] alpha_plus_system_smooth = get_alpha_smooth(plus_system_smooth, s, N, a_size);

}

