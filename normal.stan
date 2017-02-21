data {
  int<lower = 1> n_obs;
  vector[n_obs] y;
  real mu_mn;
  real<lower = 0> mu_sd;
}
parameters {
  real mu;
  real<lower = 0> sigma;
}
model {
  y ~ normal(mu, sigma);
  // equivalent to:
  // for(i in 1:n_obs){
  //   y[i] ~ normal(mu, sigma);
  // }
  sigma ~ normal(0.0, 10.0);
  mu ~ normal(mu_mn, mu_sd);
}
