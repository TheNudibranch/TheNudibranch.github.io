library(cmdstanr)
library(kableExtra)
knitr::opts_chunk$set(cache = F)
options(mc.cores = parallel::detectCores())

state_space_utils <- new.env()
source('state_space_model_utilities.R', state_space_utils)


season_length <- 7; number_seasons <- 15
n_tot <- season_length * number_seasons
x1 <- (cumsum(rnorm(n_tot, 0,0.5)) + rnorm(n_tot, 0,.01)) |> abs()
x2 <- (cumsum(rnorm(n_tot, 0,0.5)) + rnorm(n_tot, 0,.01)) |> abs()
x_mat <- cbind(x1,x2)
beta_vec <- c(-1,1)

ss <- cos(seq(0,2*pi, length.out=season_length))*1
ss <- ss - mean(ss)
ss_vec <- c(ss)[-length(ss)]
ss_var <- 0.01^2;

slope_start <- 0; slope_var <- 1e-30; slope_vec <- c(slope_start)

mu_start <- rnorm(1,0,1); mu_var <- 0.01^2; mu_vec <- c(mu_start)

noise_var <- 0.5

y_vec <- ss_comp <-  c()
# i <- 1
for (i in 1:n_tot){
  ss_comp <- c(ss_vec[1], ss_comp)
  y_vec <- c(y_vec, mu_vec[i] + (x_mat %*% beta_vec)[i] + ss_vec[1] + rnorm(1,0,sqrt(noise_var)))
  ss_vec <- c(-sum(ss_vec) + rnorm(1, 0, sqrt(ss_var)), ss_vec[-length(ss_vec)])
  slope_vec <- c(slope_vec, slope_vec[i] + rnorm(1, 0, sqrt(slope_var)))
  mu_vec <- c(mu_vec, mu_vec[i] + slope_vec[i] + rnorm(1, 0,sqrt(mu_var)))
}

# Add 20% of series mean to the last 10 iterations
is_new_prod_live <- seq_along(y_vec) >= length(y_vec) - 9 
y_vec[is_new_prod_live] <- abs(y_vec[is_new_prod_live])*0.5 + 
  y_vec[is_new_prod_live]


y_vec_no_prod <- y_vec[!is_new_prod_live]

k_obj <- state_space_utils$forward_backward_pass(
  y_vec_no_prod, a_1=rep(0,9), 
  P_1 = state_space_utils$iden(9)*0.5,
  var_vec = c(mu_var, ss_var), 
  noise_var = c(noise_var),
  params = list('include_slope'=F, x_mat=cbind(x1,x2)[!is_new_prod_live,], n_seasons=7)
)

filtered <- y_vec_no_prod - (k_obj$forw$v |> unlist())
smoothed <- y_vec_no_prod - (k_obj$back$v_smth |> unlist())

sim <- state_space_utils$simulate_state(
  k_obj$back$alpha_smth, a_1=rep(0,9), 
  P_1 = state_space_utils$iden(9)*0.5,
  var_vec = c(mu_var, ss_var), noise_var = c(noise_var),
  params = list('include_slope'=F, x_mat=cbind(x1,x2)[!is_new_prod_live,], n_seasons=7)
)


state_space_utils <- new.env()
source('state_space_model_utilities.R', state_space_utils)

y_sim <- list()
alpha_sim <- list()
z_list <- state_space_utils$gen_Z_list(length(y_vec_no_prod), include_slope=F,   
                                       x_mat=cbind(x1,x2)[!is_new_prod_live,], n_seasons=7)

for (i in 1:100){
  sim <- state_space_utils$simulate_state(
    k_obj$back$alpha_smth, a_1=rep(0,9), 
    P_1 = state_space_utils$iden(9)*0.5,
    var_vec = c(mu_var, ss_var), noise_var = c(noise_var),
    params = list('include_slope'=F, x_mat=cbind(x1,x2)[!is_new_prod_live,], n_seasons=7)
  )
  y_sim[[i]] <- lapply(1:length(y_vec_no_prod), \(x) z_list[[x]] %*% sim[[x]])
}

plot(y_vec_no_prod, type='b', pch=16)
lapply(1:length(y_vec_no_prod),\(x) z_list[[x]] %*% k_obj$back$alpha_smth[[x]]) |> unlist() |> lines()
lapply(1:length(y_vec_no_prod), \(x) z_list[[x]] %*% sim[[x]]) |> unlist() |> lines()
y_sim |> unlist() |> matrix(nrow=length(y_vec_no_prod), byrow = F) |> rowMeans() |> lines()
