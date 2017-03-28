data {
  int<lower = 1> n_obs;
  int<lower = 1> n_group;
  matrix[n_group, n_obs] y; // each row is a group's vector
  real mu_mn;
  real<lower = 0> mu_sd;
}
transformed data {
  matrix[n_group, n_obs] y_std;
  for(j in 1:n_group){
    y_std[j] = (y[j] - mean(y[j]))/sd(y[j]);
  }
}
parameters {
  real mu;
  vector[n_group] eps_group;
  real<lower = 0> sigma_mu;
  real<lower = 0> sigma_y;
}
model {
  for(j in 1:n_group){
    y_std[j] ~ normal(mu + sigma_mu*eps_group[j], sigma_y); // y[j] is the j'th row
  }
  eps_group ~ normal(0.0, 1.0);
  sigma_y ~ normal(0.0, 10.0);
  sigma_mu ~ normal(0.0, 10.0);
  mu ~ normal(mu_mn, mu_sd);
}

