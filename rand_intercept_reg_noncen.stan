data {
  int<lower = 1> n_obs;
  int<lower = 1> n_cov;
  int<lower = 1> n_state;
  vector[n_obs] y;
  matrix[n_obs, n_cov] x;
  matrix[n_obs, n_state] state;
}
transformed data {
  vector[n_obs] y_cs;
  matrix[n_obs, n_cov] x_cs;
  real y_mn;
  real<lower = 0> y_sd;
  vector[n_cov] x_mn;
  vector<lower = 0>[n_cov] x_sd;

  // center and scale y
  y_mn = mean(y);
  y_sd = sd(y);
  y_cs = (y - y_mn)/y_sd;

  // center and scale x
  for(i in 1:n_cov){
    x_mn[i] = mean(x[,i]);
    x_sd[i] = sd(x[,i]);
    x_cs[,i] = (x[,i] - x_mn[i]) / x_sd[i];
  }
}
parameters {
  vector[n_cov] beta_raw;    // "noncentered" coefficients
  real<lower = 0> sigma;
  vector[n_state] alpha_raw; // "noncentered" intercepts
  real mu_alpha;
  real<lower = 0> sigma_alpha;
}
model {
  vector[n_state] alpha;
  vector[n_cov] beta;
  alpha = mu_alpha + sigma_alpha * alpha_raw;
  beta = 10*beta_raw;

  y_cs ~ normal(state*alpha + x_cs*beta, sigma);
  alpha_raw ~ normal(0, 1);
  beta_raw ~ normal(0, 1);
  sigma ~ student_t(5, 0, 10);
  mu_alpha ~ normal(0, 10);
  sigma_alpha ~ student_t(5, 0, 10);
}