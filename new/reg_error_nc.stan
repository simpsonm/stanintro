data {
  int<lower = 1> n_obs;
  int<lower = 1> n_cov;
  vector[n_obs] y;
  vector<lower = 0>[n_obs] y_se;
  matrix[n_obs, n_cov] x;
  real beta_prior_mn;
  real<lower = 0> beta_prior_sd;
  real alpha_prior_loc;
  real<lower = 0> alpha_prior_scale;
  real<lower = 0> sig_prior_scale;
  real<lower = 0> sig_prior_df;
}
transformed data {
  vector[n_obs] y_cs;
  vector<lower = 0>[n_obs] y_cs_se;
  matrix[n_obs, n_cov] x_cs;
  real y_mn;
  real<lower = 0> y_sd;
  vector[n_cov] x_mn;
  vector<lower = 0>[n_cov] x_sd;
  // center and scale y
  y_mn = mean(y);
  y_sd = sd(y);
  y_cs = (y - y_mn)/y_sd;
  y_cs_se = y_se/y_sd;
  // center and scale x
  for(i in 1:n_cov){
    x_mn[i] = mean(x[,i]);
    x_sd[i] = sd(x[,i]);
    x_cs[,i] = (x[,i] - x_mn[i]) / x_sd[i];
  }
}
parameters {
  real alpha_cs;
  vector[n_cov] beta_cs;
  vector[n_obs] theta_cs_raw;
  real<lower = 0> sigma_cs;
}
transformed parameters{
  vector[n_obs] theta_cs;
  theta_cs = theta_cs_raw*sigma_cs + alpha_cs + x_cs*beta_cs;
}
model {
  y_cs ~ normal(theta_cs, y_cs_se);
  theta_cs_raw ~ normal(0, 1);
  beta_cs ~ normal(beta_prior_mn, beta_prior_sd);
  alpha_cs ~ cauchy(alpha_prior_loc, alpha_prior_scale);
  sigma_cs ~ student_t(sig_prior_df, 0, sig_prior_scale);
}
generated quantities {
  real alpha;
  vector[n_cov] beta;
  real<lower = 0> sigma;
  vector[n_obs] theta;

  beta = (beta_cs ./ x_sd) * y_sd;
  alpha = alpha_cs * y_sd - dot_product(x_mn, beta) + y_mn;
  sigma = sigma_cs * y_sd;
  theta = theta_cs * y_sd + y_mn;
}
