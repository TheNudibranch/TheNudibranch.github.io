functions {
  array[,] gen_z_arr(int n, int include_slope, matrix[,] x_mat, int n_seasons){
    int season_cont = n_seasons > 2 ? n_seasons - 2 : 0;
    int z_size = include_slope + cols(x_mat) + season_cont + 1;
    array[n] row_vector[z_size] z_arr;
    for (i in 1:n){
      z_arr[i,] = append_col(1, append_col(rep_row_vector(1, include_slope), rep_row_vector(1, season_cont)))
    }
  }
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
  print(size(X_mat));
}
// parameters{
//   real xx;
// }
// model {
//   xx ~ std_normal();
// }

