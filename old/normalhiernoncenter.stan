data {
  int<lower = 1> n_obs;
  int<lower = 1> n_group;
  matrix[n_group, n_obs] y; // each row is a group's vector
  real mu_mn;
  real<lower = 0> mu_sd;
}
parameters {
  real mu;
  vector[n_group] eps_group;
  real<lower = 0> sigma_mu;
  real<lower = 0> sigma_y;
}
model {
  for(j in 1:n_group){
    y[j] ~ normal(mu + sigma_mu*eps_group[j], sigma_y); 
  }
  eps_group ~ normal(0, 1.0); // prior now on eps_group
  sigma_y ~ normal(0.0, 10.0);
  sigma_mu ~ normal(0.0, 10.0);
  mu ~ normal(mu_mn, mu_sd);
}
