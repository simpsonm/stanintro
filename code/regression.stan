data {
  int<lower = 1> n_obs;
  int<lower = 1> n_cov;
  vector[n_obs] y;
  matrix[n_obs, n_cov] x;
  real beta_prior_mn;
  real<lower = 0> beta_prior_sd;
  real alpha_prior_loc;
  real<lower = 0> alpha_prior_scale;
  real<lower = 0> sigma_prior_scale;
  real<lower = 0> sigma_prior_df;
}
parameters {
  real alpha;
  vector[n_cov] beta;
  real<lower = 0> sigma;
}
model {
  y ~ normal(alpha + x*beta, sigma);
  alpha ~ cauchy(alpha_prior_loc, alpha_prior_scale);
  beta ~ normal(beta_prior_mn, beta_prior_sd);
  sigma ~ student_t(sigma_prior_df, 0, sigma_prior_scale);
}
