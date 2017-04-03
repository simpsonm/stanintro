data {
  int<lower = 1> n_obs;
  int<lower = 1> n_cov;
  vector[n_obs] y;
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
  matrix[n_obs, n_cov] x_cs;
  real y_mn;
  real<lower = 0> y_sd;
  vector[n_cov] x_mn;
  vector<lower = 0>[n_cov] x_sd;
  vector[n_cov] x_mnsd; // x_mn / x_sd
  vector[n_cov] beta_cs_prior_mn;
  vector<lower = 0>[n_cov] beta_cs_prior_sd;
  real alpha_cs_prior_loc;
  real<lower = 0> alpha_cs_prior_scale;
  real<lower = 0> sig_cs_prior_scale;

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

  // priors on _cs parameters
  x_mnsd = x_mn ./ x_sd;
  beta_cs_prior_mn = x_sd * beta_prior_mn / y_sd;
  beta_cs_prior_sd = x_sd * beta_prior_sd / y_sd;
  alpha_cs_prior_loc = (alpha_prior_loc - y_mn) / y_sd;
  alpha_cs_prior_scale = alpha_prior_scale / y_sd;
  sig_cs_prior_scale = sig_prior_scale / y_sd;
}

parameters {
  real alpha_cs;
  vector[n_cov] beta_cs;
  real<lower = 0> sigma_cs;
}
model {
  y_cs ~ normal(alpha_cs + x_cs*beta_cs, sigma_cs);
  beta_cs ~ normal(beta_cs_prior_mn, beta_cs_prior_sd);
  alpha_cs ~ normal(alpha_cs_prior_loc + dot_product(x_mnsd, beta_cs), alpha_cs_prior_scale);
  sigma_cs ~ student_t(sig_prior_df, 0, sig_cs_prior_scale);
}
generated quantities {
  real alpha;
  vector[n_cov] beta;
  real<lower = 0> sigma;

  beta = (beta_cs ./ x_sd) * y_sd;
  alpha = alpha_cs * y_sd - dot_product(x_mn, beta) + y_mn;
  sigma = sigma_cs * y_sd;
}
