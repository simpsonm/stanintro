data {
  int n_obs;
  vector[n_obs] y;
  real mu_prior_mn;
  real<lower = 0> mu_prior_sd;
  real<lower = 0> sig_prior_scale;
  real<lower = 0> sig_prior_df;
}
parameters {
  real mu;
  real<lower = 0> sigma;
}
model {
  y ~ normal(mu, sigma); // mean and sd parameterization
  mu ~ normal(mu_prior_mn, mu_prior_sd);
  sigma ~ student_t(sig_prior_df, 0, sig_prior_scale);
}
