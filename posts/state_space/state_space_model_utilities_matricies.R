mat_self_mult <- function(mat, n){
  if (n == 1) mat
  else{
    mat_res <- mat
    for (i in 1:(n - 1)){
      mat_res <- mat %*% mat_res
    }
    mat_res
  }
}

iden <- function(n){
  i <- matrix(0, nrow=n, ncol=n)
  diag(i) <- 1
  i
}

## Assumes that transition and variances are static
generate_full_state_matricies <- function(a_1, p_1, sig_std, eta_std, T_mat, Z_vec_left_pad, X_mat, n_pts){
  if(length(a_1) != nrow(p_1)) stop('Impossible to generate latent state from `a_1` and `p_1`')
  # Transition matrix
  T_mat_full <- matrix(0, nrow=(n_pts + 1)*nrow(T_mat), ncol=ncol(T_mat)*(n_pts + 1))
  row_res <- nrow(T_mat); col_res <- ncol(T_mat)
  
  for (i in 1:(floor(ncol(T_mat_full) / col_res))){
    for(j in i:(floor(nrow(T_mat_full) / row_res))){
      start_row <- (j - 1)*row_res + 1; end_row <- (j)*row_res
      start_col <- (i - 1)*col_res + 1; end_col <- (i)*col_res
      if (i == j){
        T_mat_full[start_row:end_row, start_col:end_col] <- iden(row_res)
      }
      else {
        T_mat_full[start_row:end_row, start_col:end_col] <- mat_self_mult(T_mat, j-i)
      }
    } 
  }
  
  # Design Matrix
  Z_mat <- cbind(Z_vec_left_pad |> rep(nrow(X_mat)) |> matrix(nrow=nrow(X_mat), byrow=TRUE), 
                 X_mat)

  Z_mat_full <- matrix(0, nrow=n_pts, ncol=ncol(Z_mat)*(n_pts + 1))
  for (i in 1:n_pts){
    start_col <- (i - 1)*ncol(Z_mat) + 1; end_col <- i*ncol(Z_mat)
    Z_mat_full[i, start_col:end_col] <- Z_mat[i,]
  }
  # P_1_mat
  P_1_star_mat_full <- matrix(0, nrow=(n_pts+1)*length(a_1), ncol=(n_pts+1)*length(a_1))
  P_1_star_mat_full[1:length(a_1), 1:length(a_1)] <- p_1
  
  # Assume that errors are all 1d  
  H_mat_full <- Q_mat_full <-  matrix(0, nrow=n_pts, ncol=n_pts)
  diag(Q_mat_full) <- eta_std
  diag(H_mat_full) <- sig_std
  
  # R matrix
  R_mat_full <- matrix(0, ncol=n_pts, nrow=(n_pts+1) * length(a_1))
  for (i in 1:n_pts){
    R_mat_full[length(a_1) + (i - 1)*length(a_1) + 1,i] <- 1
  }
  
  a_1_star <- c(a_1, rep(0, n_pts*length(a_1)))

  list(
    'T' = T_mat_full,
    'Z' = Z_mat_full,
    'Q' = Q_mat_full,
    'H' = H_mat_full,
    'R' = R_mat_full,
    'a1*' = a_1_star,
    'p1*' = P_1_star_mat_full
  )
}
