data {
  int<lower = 1> n_obs;
  int<lower = 1> n_cov;
  int<lower = 1> n_state;
  vector[n_obs] y;
  matrix[n_obs, n_cov] x;
  matrix[n_obs, n_state] state;
  vector[state] x_state;
  real beta_prior_mean;
  real<lower = 0> beta_prior_sd;
  real<lower = 0> sig_prior_scale;
  real<lower = 0> sig_prior_df;
  real mu_alpha_prior_mean;
  real<lower = 0> mu_alpha_prior_sd;
  real<lower = 0> sig_alpha_prior_scale;
  real<lower = 0> sig_alpha_prior_df;
  real<lower = 0> phi_prior_scale;
  real<lower = 0> phi_prior_df;  
}
transformed data {
  matrix[n_state, n_state] distance_matrix;
  vector[n_state] ones;

  for(i in 1:n_state){
    for(j in 1:n_state){
      distance_matrix[i,j] = abs(x_state[i] - x_state[j])
    }
  }
  ones = rep_vector(1, n_state);
}
parameters {
  vector[n_cov] beta;
  vector[n_state] alpha;
  real<lower = 0> sigma;
  real mu_alpha;
  real<lower = 0> sigma_alpha;
  real<lower = 0> phi;
}
transformed parameters{
  cov_matrix[n_state] cov_state;
  cov_state = sigma_alpha^2*exp(-distance_matrix/phi);
}
model {
  y ~ normal(state*mu + x*beta, sigma);
  mu ~ multi_normal(mu_alpha*ones, cov_state);
  beta ~ normal(beta_prior_mean, beta_prior_sd);
  sigma ~ student_t(sig_prior_df, sig_prior_scale);
  mu_alpha ~ normal(mu_alpha_prior_mean, mu_alpha_prior_sd);
  sigma_alpha ~ student_t(sig_alpha_prior_df, sig_alpha_prior_scale);
  phi ~ student_t(phi_prior_df, phi_prior_scale);
}
