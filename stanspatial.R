library(rstan)

#####  fit a normal distribution
y <- rnorm(1000, 4.0, 1.2)  # simulate fake data

## create a list of each variable defined in the data block
normaldat <- list(n_obs = length(y), y = y, mu_mn = 0, mu_sd = 10)

## create the log posterior and make sure the sampler initializes
normalfit0 <- stan("normal.stan", data = normaldat, chains = 1, iter = 1)

## create the log posterior and make sure the sampler initializes
## warmup < iter; defaults to 1/2 iter
## iter defaults to 4000
## chains defaults to 4
## cores defaults to 1 - you should set it to less than the number of cores you have
normalfit <- stan(fit = normalfit0, data = normaldat, cores = 4, chains = 4,
                  warmup = 2000, iter = 4000)

traceplot(normalfit) 

summary(normalfit)$summary ## combines all chains together

##### fit a hierarchical model
J <- 3 # number of groups
N <- 100 # sample size per group
mus <- c(1.2, 1.4, 1.6)
sigma <- 2.5
y <- t(sapply(mus, function(x) rnorm(N, x, sigma))) ## need it to be J x N

hierdat <- list(n_obs = N, n_group = J, y = (y - mean(y))/sd(y), mu_mn = 0, mu_sd = 10)

hierfit0 <- stan("normalhier.stan", data = hierdat, chains = 1, iter = 1)

hierfit <- stan(fit = hierfit0, data = hierdat, cores = 4, chains = 4,
                warmup = 2000, iter = 4000)

## lots of divergent transitions - lets check the pairs plot
pairs(hierfit)
pairs(hierfit, pars = c("mu", "mu_group[1]", "sigma_mu"))
?pairs.stanfit  ## to get the help file
## red dots: divergent transitions
## below main diagonal: lower  than median acceptance probability
## above main diagonal: higher than median acceptance probability

## can try increasing target metropolis acceptance probability
## but if divergent transitions are occuring when the acceptance
## probability is already high, this is unlikely to work

hierfit2 <- stan(fit = hierfit0, data = hierdat, cores = 4, chains = 4,
                 warmup = 2000, iter = 4000,
                 control = list(adapt_delta = 0.9))

## still some divergent transitions, and we slowed down the algorithm

## now lets center and scale the data
hierfitcs0 <- stan("normalhiercs.stan", data = hierdat, chains = 1, iter = 1)

hierfitcs <- stan(fit = hierfitcs0, data = hierdat, cores = 4, chains = 4,
                  warmup = 2000, iter = 4000)


hierfitcsnc0 <- stan("normalhiercsnc.stan", data = hierdat, chains = 1, iter = 1)

hierfitcsnc <- stan(fit = hierfitcsnc0, data = hierdat, cores = 4, chains = 4,
                    warmup = 2000, iter = 4000)



## fit a hierarchical model (non-centered parameterization)
hierfitnc0 <- stan("normalhiernoncenter.stan", data = hierdat, chains = 1, iter = 1)

hierfitnc <- stan(fit = hierfitnc0, data = hierdat, cores = 4, chains = 4,
                  warmup = 2000, iter = 4000)
## still divergent transitions, but a lot less
pairs(hierfitnc, pars = c("eps_group[1]", "sigma_mu"))

hierfitnc2 <- stan(fit = hierfitnc0, data = hierdat, cores = 4, chains = 4,
                   warmup = 2000, iter = 4000,
                   control = list(adapt_delta  = 0.9))




## fit a regression

## fit a regression (centered and standardized variables)

## fit a regression with CAR errors

## fit a regression with ICAR errors

## fit a regression with CAR errors (sparse)

## fit a regression with ICAR errors (sparse)

## fit a Gaussian Process

## mixture model?

