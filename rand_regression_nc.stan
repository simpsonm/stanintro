data {
  int<lower = 1> n_obs;
  int<lower = 1> n_cov;
  int<lower = 1> n_group;
  vector[n_obs] y;
  matrix[n_obs, n_cov] x;
  int group[n_obs];
}
transformed data {
  vector[n_obs] y_cs;
  matrix[n_obs, n_cov] x_cs;
  real y_mn;
  real<lower = 0> y_sd;
  real x_mn;
  real<lower = 0> x_sd;

  // center and scale y
  y_mn = mean(y);
  y_sd = sd(y);
  y_cs = (y - y_mn)/y_sd;

  // center and scale x
  x_cs[,1] = x[,1];
  for(i in 2:n_cov){
    x_mn = mean(x[,i]);
    x_sd = sd(x[,i]);
    x_cs[,i] = (x[,i] - x_mn) / x_sd;
  }
}
parameters {
  vector[n_cov] beta_raw[n_group];
  real<lower = 0> sigma;
  vector[n_cov] mu_beta;
  vector<lower = 0>[n_cov] sigma_beta;
}
model {
  vector[n_cov] beta[n_group];
  for(g in 1:n_group){
    beta[g] = beta_raw[g] .* sigma_beta + mu_beta;
    beta_raw[g] ~ normal(0, 1);
  }
  for(i in 1:n_obs){
    y_cs[i] ~ normal(x_cs[i]*beta[group[i]], sigma);
  }
  mu_beta ~ normal(0, 10);
  sigma_beta ~ student_t(5, 0, 10);
  sigma ~ student_t(5, 0, 10);
}
