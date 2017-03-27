data {
  int<lower = 1> n_obs;
  int<lower = 1> n_cov;
  vector[n_obs] y;
  matrix[n_obs, n_cov] x;
  real beta_prior_mean;
  real<lower = 0> beta_prior_sd;
  real<lower = 0> sig_prior_scale;
  real<lower = 0> sig_prior_df;
} 
parameters {
  vector[n_cov] beta;
  real<lower = 0> sigma;
}
model {
  y ~ normal(x*beta, sigma);
  beta ~ normal(beta_prior_mean, beta_prior_sd);
  sigma ~ student_t(sig_prior_df, sig_prior_scale);
}
