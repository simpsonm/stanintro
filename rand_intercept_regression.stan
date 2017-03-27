data {
  int<lower = 1> n_obs;
  int<lower = 1> n_cov;
  int<lower = 1> n_state;
  vector[n_obs] y;
  matrix[n_obs, n_cov] x;
  matrix[n_obs, n_state] state;
  real beta_prior_mean;
  real<lower = 0> beta_prior_sd;
  real<lower = 0> sig_prior_scale;
  real<lower = 0> sig_prior_df;
  real mu_alpha_prior_mean;
  real<lower = 0> mu_alpha_prior_sd;
  real<lower = 0> sig_alpha_prior_scale;
  real<lower = 0> sig_alpha_prior_df;
} 
parameters {
  vector[n_cov] beta;
  vector[n_state] alpha;
  real mu_alpha;
  real<lower = 0> sigma_alpha;
  real<lower = 0> sigma;
}
model {
  y ~ normal(state*alpha + x*beta, sigma);
  alpha ~ normal(mu_alpha, sigma_alpha)
  beta ~ normal(beta_prior_mean, beta_prior_sd);
  sigma ~ student_t(sig_prior_df, sig_prior_scale);
  mu_alpha ~ normal(mu_alpha_prior_mean, mu_alpha_prior_sd);
  sigma_alpha ~ student_t(sig_alpha_prior_df, sig_alpha_prior_scale);
}
