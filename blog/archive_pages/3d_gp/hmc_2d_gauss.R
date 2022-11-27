library(tidyverse)
library(viridisLite)
library('mvtnorm')
mu <- c(4,5); sigma <- c(1,-0.7,-0.7,1) %>% matrix(., nrow=2, byrow=T)
m_mat <- c(1,0,0,1) %>% matrix(., nrow=2, byrow=T)

gradient <- function(x_vec){
  return(solve(sigma)%*%(x_vec - mu))
}

50*50

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
  for (i in 1:(chain_length+n_burn)){
    path[i+1,] <- hmc_step(path[i,], step_size, L)
  }
  return(path[-(1:n_burn),])
}

base_plot <- rmvnorm(1e3, mu, sigma)

chain <- hmc(c(4,5), 0.01, 100, 1e3) 
(chain[-nrow(chain),] != lead(chain)[-nrow(chain),]) %>% sum(.) %>% `/`(2) %>% `/`(nrow(chain))

base_plot %>% plot(.,col=adjustcolor('grey',0.4), pch=16)
hmc(c(0,8), 0.01, 60, 1e3) %>% points(., col=viridis(nrow(.)), pch=16)




library(ggplot2)
library(gganimate)
p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()
anim <- p + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 0.1)


ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point(aes(group = seq_along(Species))) + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)
faithfuld |> View()

c_y <- c(-0.08688898675237944,-0.012303916979681433,3.7790386747278126,-3.1932757403901597)
p_y <- c(-0.08869440776364163,2.3982243506341403,3.3459728131191135,-3.334587010987924)
c_m <- c(0.01872290999106177,2.2336461842771196,-1.105737519201798,0.4463573083117613)
p_m <- c(-0.022563483823720868,2.7811288283299387,0.19109314120911625,-0.7399761446777857)

mu <- c(0.00005019010633364083,-0.000027628767930417268,1.5767045643321986,-0.8679480416554808)
cov <- c(2.0999999993836664,0.0012268535374577085,0.001207491580609006,0.0000021720111752035256,0.0012268535374577085,2.099999999887107,0.0000021720111752035273,0.0012233069922560425,0.001207491580609006,0.0000021720111752035273,1.4917523447021037,0.04581763628862271,0.0000021720111752035256,0.0012233069922560425,0.04581763628862271,1.9885879474185106)
cov_mat <- matrix(cov, nrow=4, ncol=4, byrow = T)


(dmvnorm(p_y, mean=mu, sigma=cov_mat, log = T) + dmvnorm(p_m, mean=rep(0,4), sigma=diag(4), log = T)) - 
  (dmvnorm(c_y, mean=mu, sigma=cov_mat, log = T) + dmvnorm(c_m, mean=rep(0,4), sigma=diag(4), log = T))
  
solve(cov_mat) %*% (c_y - mu)
