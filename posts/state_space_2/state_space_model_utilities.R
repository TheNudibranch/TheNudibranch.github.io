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
  slope <- if(!include_slope) NULL else 0
  z_list <- lapply(1:n, \(x) c(1, slope, season_pad, x_mat[x,]))
  z_list
}

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
      t(forw_list[['L']][[n]])) + R_mat %*% Q_mat %*% t(R_mat)
    
    forw_list[['a']][[n + 1]] <- T_mat %*% forw_list[['a']][[n]] + 
      (forw_list[['K']][[n]] * forw_list[['v']][[n]])
  }
  
  back_list <- list(
    'alpha_smth' = list(),
    'r' = list(),
    'v_smth'=list()
  )
  # r goes from 0 to n, not 1 to n + 1 (like a)
  back_list[['r']] <- lapply(1:(n+1), \(x) rep(0, length(a_1)))
  for (i in length(y):1){
    # We are actually writing to r_{n-w} for the first iteration, not r_n
    # Again, this is becauase r is defined from 0 to n, so r[[n]] actually grabs r_{n-1}
    back_list[['r']][[i]] <- Z_list[[i]] * (1 / forw_list[['F']][[i]]) * forw_list[['v']][[i]] + 
      t(forw_list[['L']][[i]]) %*% back_list[['r']][[i + 1]]
    
    back_list[['alpha_smth']][[i]] <- forw_list[['a']][[i]] + 
      forw_list[['P']][[i]] %*% back_list[['r']][[i]]
    
    back_list[['v_smth']][[i]] <- c(y[[i]] - Z_list[[i]] %*% back_list[['alpha_smth']][[i]])
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

generate_all_const_mats <- function(var_vec, params){
  c('include_slope', 'x_mat', 'n_seasons') %<-% params[c('include_slope', 'x_mat', 'n_seasons')]
  Z_list <- gen_Z_list(nrow(x_mat), include_slope = include_slope, 
                       x_mat=x_mat, n_seasons=n_seasons)
  T_mat <- gen_T_mat(include_slope = include_slope, n_regress=ncol(x_mat), 
                     n_seasons = n_seasons)
  R_mat <- gen_R_mat(include_slope = include_slope, n_regress=ncol(x_mat), 
                     n_seasons = n_seasons)
  Q_mat <- gen_Q_mat(var_vec=var_vec)
}

simulate_state <- function(smoothed_state, a_1, P_1, var_vec, noise_var, params){
  alpha_1 <- MASS::mvrnorm(1, mu=a_1, Sigma=P_1)
  plus_system <- evolve_from_start(alpha_1, length(smoothed_state), var_vec=var_vec, noise_var=noise_var,
                              params = params)
  plus_system_smoothed <- forward_backward_pass(unlist(plus_system$evolve_state$y), a_1=a_1, P_1=P_1,
                                                var_vec=var_vec, noise_var=noise_var, params=plus_system)
  lapply(1:length(smoothed_state), 
         \(x) plus_system$evolve_state$alpha[[x]] - plus_system_smoothed$back$alpha_smth[[x]] +  smoothed_state[[x]])
}

simulate_n_obs <- function(n_sim, smoothed_state, a_1, P_1, var_vec, noise_var, params){
  y_sim_list <- list()
  params <- list('const_mats' = generate_all_const_mats(var_vec, params))
  for (i in 1:n_sim){
    alpha_sim <- simulate_state(smoothed_state, a_1, P_1, var_vec, noise_var, params)
    y_sim_list[[i]] <- lapply(alpha_sim, \(x) params$T_mat %*% x) |> unlist()
  }
}

time_series_err <- function(mat, prob_range = c(0.5, 0.75, 0.9)){
  if(length(dim(mat)) != 2) stop('`mat` needs to be a 2-d matrix')
  message(sprintf('Assuming expression `mat` is a time series consisting of %s posterior draws and %s variables.',nrow(mat), ncol(mat)))
  
  n_probs <- length(prob_range)
  cmap <- colormap::colormap(colormap=c('#BCBCDC', '#9999C7', '#7C7CB9', '#5050A2', '#27278F', '#00007C'), 
                             nshades=n_probs)
  
  quant <- mat |> apply(2, \(x, y=prob_range){
    upper <- 0.5 + y/2
    lower <- 0.5 - y/2
    c(quantile(x,lower), quantile(x, upper)) |> sort() |> unname()
  })
  
  lines(colMeans(mat), lwd=2)
  
  for (i in 1:n_probs){
    if (i == 1){
      y_upper <- quant[n_probs + i, ]
      y_lower <- quant[n_probs - i + 1, ]
      polygon(c(seq(ncol(mat)), rev(seq(ncol(mat)))), c(y_lower, rev(y_upper)), col=adjustcolor(cmap[i], 0.3), border=NA)
    }
    else {
      y_upper_upper <- quant[n_probs + i, ]
      y_upper_lower <- quant[n_probs + i - 1, ]
      polygon(c(seq(ncol(mat)), rev(seq(ncol(mat)))), c(y_upper_lower, rev(y_upper_upper)), col=adjustcolor(cmap[i], 0.3),border=NA)
      
      y_lower_upper <- quant[n_probs - i + 2, ]
      y_lower_lower <- quant[n_probs - i + 1, ]
      polygon(c(seq(ncol(mat)), rev(seq(ncol(mat)))), c(y_lower_lower, rev(y_lower_upper)), col=adjustcolor(cmap[i], 0.3), border=NA)
      
    }
  }
}



