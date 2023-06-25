library(cmdstanr)

rstudioapi::getActiveDocumentContext()$path |> dirname() |> setwd()

#################
## Generate some data
#################
n_tot <- 300;
x1 <- cumsum(rnorm(n_tot, 0,2)) + rnorm(n_tot, 0,1)
x2 <- cumsum(rnorm(n_tot, 0,1)) + rnorm(n_tot, 0,1)
x_mat <- cbind(x1,x2)

slope_start <- 0; sig_eta <- 0.1; sig_slope <- 1e-15; sig_alpha <- 1; beta <- c(-2, 0);
a1 <- rnorm(1,10, 1); alpha_vec <- c(a1); slope_vec <- c(slope_start); y_vec <- c(alpha_vec + rnorm(1,0,sig_eta) + (x_mat %*% beta)[1])
for (i in 2:n_tot){
  alpha_vec <- append(alpha_vec, alpha_vec[i-1] + slope_vec[i-1] + rnorm(1,0,sqrt(sig_alpha)));
  slope_vec <- append(slope_vec, slope_vec[i-1] + rnorm(1,0,sqrt(sig_slope)))
  y_vec <- append(y_vec, alpha_vec[i] + (x_mat %*% beta)[i] + rnorm(1,0,sqrt(sig_eta)))
}

plot(x1, type='l', col='blue', ylim=c(min(x1,x2, y_vec), max(x1,x2, y_vec))); lines(x2, col='red')
lines(y_vec, lwd=3)

#################
## Working on Utilities
#################



#################
## State Space Models
#################
mod <- cmdstan_model('state_space.stan')

data <- list(
  N=n_tot,
  y = y_vec,
  has_slope=1,
  n_seasons=0,
  n_covar=0,
  X_mat=matrix(NA,300,0)
)

v <- matrix(2, 300, 0)
dim(v)


a <- mod$sample(data=data, seed=1234,fixed_param = TRUE)
mod$