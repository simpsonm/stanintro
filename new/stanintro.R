library(rstan)

set.seed(234234)

## simulate data with observation errors
n <- 1000
alpha <- 11000
beta <- c(1000, .00025, -3)*10000
x <- cbind(runif(n, -1, 1)*1000, rnorm(n)*1000 + 1000, rgamma(n, 1, 1/1000))
sigma <- 1000
ses <- 10*rep(sigma, n)
theta <- rnorm(n, alpha + x%*%beta, sigma)
y <- rnorm(n, theta, ses)

regdat <- list(n_obs = n, n_cov = length(beta), y = theta, x = x,
               alpha_prior_loc = 10000, alpha_prior_scale = 10000,
               beta_prior_mn = 10000, beta_prior_sd = 10000,
               sig_prior_df = 5, sig_prior_scale = 100000)

## initialize the model; takes 15-30 seconds
regfit0 <- stan("regression.stan", data = regdat, chains = 1, iter = 1)
## ignore compiler warnings

## sample the model
regfit <- stan(fit = regfit0, data = regdat, cores = 4, chains = 4,
              warmup = 2000, iter = 4000, open_progress = FALSE)
## about 10-40 seconds to fit per chain

regcsfit0 <- stan("regression_cs.stan", data = regdat, chains = 1, iter = 1)

regcsfit <- stan(fit = regcsfit0, data = regdat, cores = 4, chains = 4,
                 warmup = 2000, iter = 4000, open_progress = FALSE)
## about
