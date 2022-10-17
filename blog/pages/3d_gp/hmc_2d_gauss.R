library(tidyverse)
library(viridisLite)
library('mvtnorm')
mu <- c(4,5); sigma <- c(1,0.5,0.5,1) %>% matrix(., nrow=2, byrow=T)
m_mat <- c(1,0,0,1) %>% matrix(., nrow=2, byrow=T)

gradient <- function(x_vec){
  return(solve(sigma)%*%(x_vec - mu))
}



leap_frog <- function(starting_x,starting_mom, step_size, L){
  momentum <- starting_mom
  curr_step <- 0
  curr_x <- starting_x
  for (i in 1:L){
    mom_half <- momentum + (step_size / 2) * gradient(curr_x)
    curr_x <- curr_x + step_size*(m_mat %*% mom_half)
    momentum <- mom_half + (step_size / 2) * gradient(curr_x)
    curr_step <- curr_step + step_size
  }
  return(cbind(curr_x, momentum))
}


hmc_step <- function(starting_x, step_size, L){
  starting_mom <- rmvnorm(n=1, mean=c(0,0)) %>% t()
  proposed_mat <- leap_frog(starting_x, starting_mom, step_size, L)
  alpha_log <- dmvnorm(proposed_mat[,1], mean=mu, sigma=sigma, log = T) + 
    dmvnorm(proposed_mat[,2], mean=c(0,0), log=T) - 
    (dmvnorm(starting_x, mean=mu, sigma=sigma, log = T) + 
       dmvnorm(t(starting_mom), mean=c(0,0), log=T))
  if(runif(1) < exp(alpha_log)){
    return(proposed_mat[,1])
  }
  else{
    starting_x
  }
}

hmc <- function(starting_x, step_size, L, chain_length, n_burn=500){
  path <- matrix(NA, ncol=2, nrow=chain_length + 1 + n_burn)
  path[1,] <- starting_x
  for (i in 1:chain_length){
    path[i+1,] <- hmc_step(path[i,], step_size, L)
  }
  return(path[-(1:n_burn),])
}

base_plot <- rmvnorm(1e3, mu, sigma)

chain <- hmc(c(4,5), 0.01, 100, 1e3) 
(chain[-nrow(chain),] != lead(chain)[-nrow(chain),]) %>% sum(.) %>% `/`(2) %>% `/`(nrow(chain))

base_plot %>% plot(.,col=adjustcolor('grey',0.4), pch=16)
hmc(c(8,7), 0.01, 20, 3e3) %>% points(., col=viridis(nrow(.)), pch=16)

