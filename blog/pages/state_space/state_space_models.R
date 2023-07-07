library(rstan)
library(tidyverse)
rstudioapi::getActiveDocumentContext()$path |> dirname() |> setwd()

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE) 
#################
## Generate some data
#################
season_length <- 10; number_seasons <- 6
n_tot <- season_length * number_seasons
x1 <- cumsum(rnorm(n_tot, 0,2)) + rnorm(n_tot, 0,1)
x2 <- cumsum(rnorm(n_tot, 0,1)) + rnorm(n_tot, 0,1)
x_mat <- cbind(x1,x2)
beta_vec <- c(-2,2)

ss <- cos(seq(0,2*pi, length.out=season_length)) * 20 + rnorm(season_length, 0, 4)
ss <- ss - mean(ss)
ss_vec <- c(ss)[-length(ss)]
ss_var <- 0.3;

slope_start <- -0.5; slope_var <- 1e-15; slope_vec <- c(slope_start)

mu_start <- rnorm(1,10,1); mu_var <- 1; mu_vec <- c(mu_start)

y_vec <- c()
# i <- 1
for (i in 1:n_tot){
  y_vec <- c(y_vec, mu_vec[i] + (x_mat %*% beta_vec)[i] + ss_vec[1])
  ss_vec <- c(-sum(ss_vec) + rnorm(1, 0, sqrt(ss_var)), ss_vec[-length(ss_vec)])
  slope_vec <- c(slope_vec, slope_vec[i] + rnorm(1, 0, sqrt(slope_var)))
  mu_vec <- c(mu_vec, slope_vec[i] + rnorm(1, 0,sqrt(mu_var)))
}

plot(x1, type='l', col='blue', ylim=c(min(x1,x2, y_vec), max(x1,x2, y_vec))); lines(x2, col='red')
lines(y_vec, lwd=3)

#################
## State Space Models
#################
mod <- stan_model('state_space.stan')

data <- list(
  N=n_tot,
  y = y_vec,
  has_slope=1,
  n_seasons=10,
  n_covar=2,
  X_mat = x_mat
  # X_mat=matrix(1,n_tot,1)
)


a <- sampling(mod, data=data)
b <- summary(a)$summary %>% as.data.frame() %>% rownames_to_column() %>% filter(!grepl('k_obj', rowname))

plot(y_vec, type='l', lwd=3)
b %>% filter(grepl('trend_comp\\[1,', rowname)) %>% .$mean %>% lines()
b %>% filter(grepl('trend_comp\\[2,', rowname)) %>% .$mean %>% lines()
b %>% filter(grepl('trend_comp\\[3,', rowname)) %>% .$mean %>% lines()

