data {
  int<lower = 1> n_obs;
  int<lower = 1> n_cov;
  vector[n_obs] y;
  matrix[n_obs, n_cov] x;
  real beta_prior_mn;
  real<lower = 0> beta_prior_sd;
  real alpha_prior_mn;
  real<lower = 0> alpha_prior_sd;
  real<lower = 0> sig_prior_scale;
  real<lower = 0> sig_prior_df;
}
parameters {
  real alpha;
  vector[n_cov] beta;
  real<lower = 0> sigma;
}
model {
  y ~ normal(alpha + x*beta, sigma);
  beta ~ normal(beta_prior_mn, beta_prior_sd);
  alpha ~ normal(alpha_prior_mn, alpha_prior_sd);
  sigma ~ student_t(sig_prior_df, 0, sig_prior_scale);
}
