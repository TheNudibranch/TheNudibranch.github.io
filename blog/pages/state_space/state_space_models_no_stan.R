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

slope_start <- 0; sig_slope <- 0.01; sig_obs <- 0.1; sig_mu <- 1; sig_season <- 0.01; beta <- c(-2, 0)

#################
## Working on Utilities
#################



#################
## State Space Models
#################
T_mat <- matrix(0,nrow=2,ncol=2)
diag(T_mat) <- c(1,1)

n_pts <- n_tot
p_1 <- matrix(c(1,0,0,1e-9), nrow=2, byrow = T)
a <- generate_full_state_matricies(c(1, 0), p_1, sig_std = 0.1, eta_std=1, T_mat, c(1), x_mat[,1,drop=FALSE], n_pts = n_pts)

q_star <- a$`p1*` +  a$R %*% a$Q %*% t(a$R)
omega <- a$Z %*% a$T %*% q_star %*% t(a$T) %*% t(a$Z) + a$H
mu <- a$Z %*% a$T %*% a$`a1*`
u <- solve(omega) %*% (y_vec - mu)

alpha_smooth <- a$T %*% a$`a1*` + a$T %*% q_star %*% t(a$T) %*% t(a$Z) %*% u

alpha_smooth |> matrix(ncol=2, byrow=T)
