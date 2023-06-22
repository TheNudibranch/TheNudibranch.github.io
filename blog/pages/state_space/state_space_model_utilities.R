.coalesce <- function(...){
  c(...)[[1]]
}

`%<-%` <- function(lhs, rhs){
  sub <- substitute(lhs)
  res <- list()
  for (i in 1:(length(sub) - 1)){
    sym <- as.symbol(sub[[i+1]])
    bquote(.(sym) <- .(rhs[[i]])) |> eval(envir = parent.frame())
  }
}


iden <- function(n){
  mat <- matrix(0,nrow=n, ncol=n)
  diag(mat) <- 1
  mat
}

## Assume that model can only accept seasons and regression, no linear
gen_Z_list <- function(n, include_slope=FALSE, x_mat=NULL, n_seasons=NULL){
  if(!is.null(x_mat) & (.coalesce(nrow(x_mat), -1) != n))
      stop('`x_mat` rows needs to be same `n`')
  if(.coalesce(n_seasons, 3) <= 2) stop('`n_seasons` must be greater than 2')
  
  season_pad <- if (.coalesce(n_seasons, 2) <= 2) NULL else c(1, rep(0,n_seasons-2))
  slope <- if(!include_slope) NULL else 1
  z_list <- lapply(1:n, \(x) c(1, slope, season_pad, x_mat[x,]))
  z_list
}

gen_Z_list(n = 50, x_mat = x_mat, include_slope = TRUE,n_seasons=NULL)

gen_T_mat <- function(include_slope=FALSE, n_regress=NULL, n_seasons=NULL){
  if(.coalesce(n_seasons, 3) <= 2) stop('`n_seasons` must be greater than 2')
  
  mat_len <- 1 + include_slope + .coalesce(n_regress,0) + .coalesce(n_seasons - 1,0)
  T_mat <- matrix(0, nrow=mat_len, ncol=mat_len)
  T_mat[1,1] <- 1
  if(include_slope) T_mat[1,2] <- T_mat[2,2] <- 1
  if(!is.null(n_seasons)){
    start_ind <- 2 + include_slope
    T_mat[start_ind, start_ind:(n_seasons-2 + start_ind)] <- -1
    
    for (i in start_ind:(start_ind + n_seasons - 3)){
      T_mat[i+1,i] <- 1
    }
  }
  if(!is.null(n_regress)){
    T_mat[(mat_len-n_regress + 1):mat_len, (mat_len-n_regress + 1):mat_len] <- iden(n_regress)
  }
  T_mat
}

gen_T_mat(FALSE, n_seasons = 3, n_regress = 2)

## Assume that slope and season is allowed to vary with time, not regression coef
gen_R_mat <- function(include_slope=FALSE, n_regress=NULL, n_seasons=NULL){
  if(.coalesce(n_seasons, 3) <= 2) stop('`n_seasons` must be greater than 2')
  
  n_col <- 1 + include_slope + !is.null(n_seasons)
  n_row <- 1 + include_slope + .coalesce(n_regress,0) + .coalesce(n_seasons - 1,0)
  R_mat <- matrix(0, nrow=n_row, ncol=n_col)
  R_mat[1,1] <- 1
  if(include_slope) R_mat[2,2] <- 1
  if(!is.null(n_seasons)) R_mat[2 + include_slope, 2 + include_slope] <- 1
  R_mat
}

gen_R_mat(TRUE, n_regress=2, n_seasons=3)

gen_Q_mat <- function(var_vec){
  Q_mat <- matrix(0,nrow=length(var_vec), ncol=length(var_vec))
  diag(Q_mat) <- var_vec
  Q_mat
}

forward_backward_pass <- function(y, a_1, P_1, var_vec, noise_var, params = list()){
  if ('const_mats' %in% names(params)) {
    c(Z_list, T_mat, R_mat, Q_mat) %<-% params[['const_mats']]
  }
  else{
    if (sum(c('include_slope', 'x_mat', 'n_seasons') %in% names(params)) != 3) 
      stop('Need to supply all 3 arguments to params if `const_mats not supplied')
    c('include_slope', 'x_mat', 'n_seasons') %<-% params[c('include_slope', 'x_mat', 'n_seasons')]
    Z_list <- gen_Z_list(length(y), include_slope = include_slope, 
                         x_mat=x_mat, n_seasons=n_seasons)
    T_mat <- gen_T_mat(include_slope = include_slope, n_regress=ncol(x_mat), 
                       n_seasons = n_seasons)
    R_mat <- gen_R_mat(include_slope = include_slope, n_regress=ncol(x_mat), 
                       n_seasons = n_seasons)
    Q_mat <- gen_Q_mat(var_vec=var_vec)
  }
  
  forw_list <- list(
    'F' = list(),
    'K' = list(),
    'a' = list(),
    'P' = list(),
    'v' = list(),
    'L' = list()
  )
  forw_list[['a']][[1]] <- a_1
  forw_list[['P']][[1]] <- P_1
  for (n in 1:length(y)){
    forw_list[['v']][[n]] <- c(y[[n]] - Z_list[[n]] %*% forw_list[['a']][[n]])
    forw_list[['F']][[n]] <- c(Z_list[[n]] %*% forw_list[['P']][[n]] %*% Z_list[[n]] + noise_var)
    forw_list[['K']][[n]] <- T_mat %*% forw_list[['P']][[n]] %*% 
      (Z_list[[n]] * (1 / forw_list[['F']][[n]]))
    forw_list[['L']][[n]] <- T_mat - forw_list[['K']][[n]] %*% Z_list[[n]]
    
    forw_list[['P']][[n + 1]] <- (T_mat %*% forw_list[['P']][[n]] %*%
      t(T_mat - (forw_list[['K']][[n]] %*% Z_list[[n]])) ) + R_mat %*% Q_mat %*% t(R_mat)
    
    forw_list[['a']][[n + 1]] <- T_mat %*% forw_list[['a']][[n]] + 
      (forw_list[['K']][[n]] * forw_list[['v']][[n]])
  }
  
  back_list <- list(
    'alpha_smth' = list(),
    'r' = list()
  )
  # r goes from 0 to n, not 1 to n + 1 (like a)
  back_list[['r']] <- lapply(1:(n+1), \(x) rep(0, length(a_1)))
  for (i in length(y):1){
    # We are actually writing to r_{n-w} for the first iteration, not r_n
    # Again, this is becauase r is defined from 0 to n, so r[[n]] actually grabs r_{n-1}
    back_list[['r']][[i]] <- Z_list[[i]] * (1 / forw_list[['F']][[i]]) * forw_list[['v']][[i]] + 
      t(forw_list[['L']][[i]]) %*% back_list[['r']][[i + 1]]
    
    back_list[['alpha_smth']][[i]] <- forw_list[['a']][[n]] + 
      forw_list[['P']][[i]] %*% back_list[['r']][[i]]
  }
  list('back' = back_list, 
       'const_mats' = list(
         'Z' = Z_list,
         'T' = T_mat,
         'R' = R_mat,
         'Q' = Q_mat
       ), 
       'forw' = forw_list)
}

evolve_from_start <- function(alpha_1, n, var_vec, noise_var, params = list()){
  if ('const_mats' %in% names(params)) {
    c(Z_list, T_mat, R_mat, Q_mat) %<-% params[['const_mats']]
  }
  else{
    if (sum(c('include_slope', 'x_mat', 'n_seasons') %in% names(params)) != 3) 
      stop('Need to supply all 3 arguments to params if `const_mats not supplied')
    c('include_slope', 'x_mat', 'n_seasons') %<-% params[c('include_slope', 'x_mat', 'n_seasons')]
    Z_list <- gen_Z_list(n, include_slope = include_slope, 
                         x_mat=x_mat, n_seasons=n_seasons)
    T_mat <- gen_T_mat(include_slope = include_slope, n_regress=ncol(x_mat), 
                       n_seasons = n_seasons)
    R_mat <- gen_R_mat(include_slope = include_slope, n_regress=ncol(x_mat), 
                       n_seasons = n_seasons)
    Q_mat <- gen_Q_mat(var_vec=var_vec)
  }
  
  evolve_state <- list(
    'y' = list(),
    'alpha' = list(alpha_1),
    'q_samps' = lapply(1:n, \(x) MASS::mvrnorm(1, rep(0, length(var_vec)), Q_mat)),
    'noise_samps' = lapply(1:n, \(x) rnorm(1,0,sqrt(noise_var)))
  )
  for (i in 1:n){
    evolve_state[['y']][[i]] <- Z_list[[i]] %*% evolve_state[['alpha']][[i]] + evolve_state[['noise_samps']][[i]]
    evolve_state[['alpha']][[i + 1]] <- T_mat %*% evolve_state[['alpha']][[i]] + 
      R_mat %*% evolve_state[['q_samps']][[i]]
  }
  list('evolve_state' = evolve_state, 'const_mats' = list(
    'Z' = Z_list,
    'T' = T_mat,
    'R' = R_mat,
    'Q' = Q_mat
  ))
  
}

MASS::mvrnorm(1, rep(0,3), iden(3)) |> is.vector()

simulate_state <- function(smoothed_state, )

a <- forward_backward_pass(y_vec, a_1=rep(0,7), P_1=iden(7), var_vec=rep(1,3),
                           noise_var=1, params=list(x_mat=x_mat, include_slope=TRUE, n_seasons=4))
a <- forward_backward_pass(y_vec, a_1=rep(0,3), P_1=P_1, var_vec=c(1),
                           noise_var=0.1, params=list(x_mat=x_mat, include_slope=FALSE, n_seasons=NULL))
y_forw <- evolve_from_start(alpha_1 = a$back$alpha_smth[[300]], n=length(y_vec), var_vec = rep(1,1),
                  noise_var = 1, params = list(const_mats = a$const_mats))

plot(x1, type='l', col='blue', ylim=c(min(x1,x2, y_vec), max(x1,x2, y_vec))); lines(x2, col='red')
lines(y_vec, lwd=3)
y_forw$evolve_state$y |> unlist() |> lines(lwd=3, lty=2)



a <- forward_backward_pass(y_vec, a_1=rep(0,3), P_1=P_1, var_vec=c(1),
                           noise_var=0.1, x_mat=x_mat, include_slope=FALSE)

a <- list('a' = 2, 'b' = 'a', 'c' = list(1))
a[c('a', 'b')]
a['a', 'b']
  