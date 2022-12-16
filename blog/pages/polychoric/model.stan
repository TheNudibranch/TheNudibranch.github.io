functions{
    real trunc_norm(array[] int y_data, array[] vector cut_points, matrix chol_mat, array[] real u, int D_y, int yMin, int yMax){
        real j_log_collection;
        j_log_collection = 0;
        real prev;
        vector[D_y] z;
        prev = 0;
        for (d in 1:D_y) {
            if (y_data[d] == yMax){
                real bound_lower;
                real t;
                bound_lower = Phi((cut_points[d, yMax - yMin] - prev) / (chol_mat[d,d]));
                t = bound_lower + (1 - bound_lower) * u[d];
                z[d] = inv_Phi(t);
                j_log_collection += log1m(bound_lower);
            }
            else if (y_data[d] == yMin){
                real bound_upper;
                real t;
                bound_upper = Phi((cut_points[d,1] - prev) / (chol_mat[d,d]));
                t = bound_upper*u[d];
                z[d] = inv_Phi(t);
                j_log_collection += log(bound_upper);
            }
            else {
                real bound_lower;
                real bound_upper;
                real t;
                bound_lower = Phi((cut_points[d,y_data[d] - 1] - prev) / (chol_mat[d,d]));
                bound_upper = Phi((cut_points[d,y_data[d]] - prev) / (chol_mat[d,d]));
                t = bound_lower + (bound_upper - bound_lower)*u[d];
                z[d] = inv_Phi(t);
                j_log_collection += log(bound_upper - bound_lower);
            }
            if (d < D_y){
                prev = chol_mat[d + 1, 1:d] * head(z,d);
            }
        }
    return(j_log_collection);
    }

    real induced_dirichlet_lpdf(vector c, vector alpha, real gamma){
        int K = num_elements(c) + 1;
        vector[K - 1] cuml = Phi(c - gamma);
        vector[K] p;
        matrix[K,K] J = rep_matrix(0,K,K);

        p[1] = cuml[1];
        for (k in 2:(K-1)){
            p[k] = cuml[k] - cuml[k-1];
        }
        p[K] = 1 - cuml[K-1];

        for (k in 1:K) J[k,1] = 1;

        for (k in 2:K){
            real rho = exp(std_normal_lpdf(c[k-1] - gamma));
            J[k,k] = -rho;
            J[k - 1, k] = rho;
        }
        return dirichlet_lpdf(p | alpha) + log_determinant(J);
    }
}

data {
  int<lower=1> D;
  int<lower=0> N;
  array[N, D] int<lower=0, upper=10> y;
  int<lower=0> y_min;
  int y_max;
//   array[D] matrix[D,D] L_Omega;
}
transformed data {
   int n_cut = y_max - y_min;
}
parameters {
  cholesky_factor_corr[D] L_Omega;
  array[N,D] real<lower=0, upper=1> u;
  array[D] ordered[n_cut] c_points;
}
model {
    L_Omega ~ lkj_corr_cholesky(4);
    for (d in 1:D) target += induced_dirichlet_lpdf(c_points[d] | rep_vector(1, n_cut + 1), 0);

    for (n in 1:N) target += trunc_norm(y[n], c_points, L_Omega, u[n], D, y_min, y_max);
}
generated quantities {
   corr_matrix[D] Omega;
   Omega = multiply_lower_tri_self_transpose(L_Omega);
//    vector[2] p = Phi(10 - c_points[2,2]);
}
