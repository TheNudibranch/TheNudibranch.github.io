functions {
  #include state_space_functions.stan
}

data {
  int<lower=1> N; // Number of 
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
  matrix[a_size, a_size - n_covar] R_mat = gen_R_mat(has_slope, n_covar, n_seasons);
  
  array[3] int s = {a_size*(N), N, N};
  array[2] int s_evolve = {a_size*(N), N};
}

parameters{
  // vector[a_size] a_1;
  vector<lower=0>[cols(R_mat)] var_vec;
  vector<lower=0>[a_size] P_1_diag;
  real<lower=0> noise_var;
}

  transformed parameters{
  vector[a_size * N + (2 * N)] k_obj = kalman_filter_smooth(N, y, rep_vector(0, a_size), P_1_diag,
                                                            noise_var, var_vec, T_mat, R_mat, Z_arr);
}

model {
  // a_1 ~ normal(0,5);
  var_vec ~ exponential(1);
  P_1_diag ~ exponential(1);
  noise_var ~ exponential(1);

  k_obj ~ state_space(s, N);
}

generated quantities{
  array[N] vector[a_size] alpha_sim = simulate_state_rng(k_obj, N, s, s_evolve, y, rep_vector(0,a_size), P_1_diag, noise_var,
                                               var_vec, T_mat, R_mat, Z_arr);
  array[1 + has_season + has_regress] vector[N] trend_comp = extract_trends(alpha_sim, N, Z_arr, has_slope, n_covar, n_seasons);
  
}

