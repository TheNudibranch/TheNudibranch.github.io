data {
  int <lower=1> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real beta;
  real<lower=0> sigma;
}
model {
  beta ~ normal(0,1);
  sigma ~ exponential(1);
  y ~ normal(x*beta, sigma);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | x[n]*beta, sigma);
  }
}
