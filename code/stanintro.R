library(rstan)

## simulate data with observation errors
set.seed(234234)
n <- 100
alpha <- 150
beta <- c(0.05, 100, -.003)
x <- cbind(runif(n, -100, 100), rnorm(n, 10, 1/10), rgamma(n, 1, 1/100))
sigma <- 10
standard.errors <- rep(10*sigma, n)
theta <- rnorm(n, alpha + x%*%beta, sigma)
y <- rnorm(n, theta, standard.errors)

## create list of all variables in the data block
regdat <- list(n_obs = n, n_cov = length(beta), y = y, x = x,
               alpha_prior_loc = 100, alpha_prior_scale = 1000,
               beta_prior_mn = 10, beta_prior_sd = 100,
               sigma_prior_df = 5, sigma_prior_scale = 100)

## initialize the model; takes 15-30 seconds
regfit0 <- stan("regression.stan", data = regdat, chains = 1, iter = 1)
## ignore compiler warnings

## sample the model
regfit <- stan(fit = regfit0, data = regdat, cores = 4, chains = 4,
              warmup = 2000, iter = 4000, open_progress = FALSE)
## about 3 minutes to fit per chain

traceplot(regfit, pars = c("alpha", "beta"))

ggsave("traceplot1.png")

traceplot(regfit, pars = c("alpha", paste("beta[", 1:2, "]", sep=""), "sigma"))

ggsave("traceplot2.png")

summary(regfit, pars = c("alpha", "beta", "sigma"))$summary

## extract the MCMC draws
regfitdraws <- extract(regfit)
str(regfitdraws, 1)


regcsfit0 <- stan("regression_cs.stan", data = regdat, chains = 1, iter = 1)

regcsfit <- stan(fit = regcsfit0, data = regdat, cores = 4, chains = 4,
                 warmup = 2000, iter = 4000, open_progress = FALSE)
## about 4 seconds to fit per chain

regerrordat <- list(n_obs = n, n_cov = length(beta), y = y, x = x, y_se = ses,
                    alpha_prior_loc = 0, alpha_prior_scale = 10,
                    beta_prior_mn = 0, beta_prior_sd = 10,
                    sigma_prior_df = 5, sigma_prior_scale = 10)


regerrorfit0 <- stan("reg_error.stan", data = regerrordat, chains = 1, iter = 1)

regerrorfit <- stan(fit = regerrorfit0, data = regerrordat, cores = 4, chains = 4,
                    warmup = 2000, iter = 4000, open_progress = FALSE)


regerrorncfit0 <- stan("reg_error_nc.stan", data = regerrordat, chains = 1, iter = 1)

regerrorncfit <- stan(fit = regerrorncfit0, data = regerrordat, cores = 4, chains = 4,
                      warmup = 2000, iter = 4000, open_progress = FALSE)
## about 30 seconds to fit per chain
