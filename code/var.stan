data {
  int<lower = 1> N_obs;
  int<lower = 1> N_var;
  int<lower = 1, upper = N_obs - 1> N_lag;
  vector[N_var] y[N_obs]; // N_obs vectors of N_var variables
}
transformed data {
  vector[N_var] y_obs[N_obs - N_lag];
  for(t in 1:(N_obs - N_lag)){
    y_obs[t] = y[t + N_lag];
  }
}
parameters {
  matrix[N_var, N_var] M[N_lag];
  cholesky_factor_corr[N_var] L_corr_err;
  vector<lower = 0>[N_var] sd_err;
  vector[N_var] intercept;
}
transformed parameters {
  cholesky_factor_cov[N_var] L_cov_err;
  L_cov_err = diag_pre_multiply(sd_err, L_corr_err);
}
model {
  // create conditional means and likelihood
  vector[N_var] mu[N_obs - N_lag];
  for(t in 1:(N_obs - N_lag)){
    mu[t] = intercept;
    for(p in 1:N_lag){
      mu[t] = mu[t] + M[j]*y[t + N_lag - p];
    }
  }
  y_obs ~ multi_normal_cholesky(mu, L_cov_err);
  // priors
  for(p in 1:N_lag){
    to_vector(M) ~ normal(0, 1);
  } // only *linear* transformations ok on LHS of ~
  sd_err ~ normal(0, 1);
  L_corr_err ~ lkj_corr_cholesky(1);
}
generated quantities {
  matrix[N_var, N_var] cov_err;
  cov_err = tcrossprod(L_cov_err); // L * L'
}
