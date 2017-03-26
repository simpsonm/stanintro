library(rstan)
library(tigris)
library(dplyr) ## for easy data frame manipulations
set.seed(34345398) ## for reproducing my simulations

coincfull <- read.csv("data/county/income/ACS_15_5YR_S1902_with_ann.csv", skip = 1)
coinc <- select(coincfull, Id, Id2, Geography,
                Mean.income..dollars...Estimate..All.households)
colnames(coinc) <- c("id", "id.co", "name", "income.mean")

coedufull <- read.csv("data/county/edu/ACS_15_5YR_S1501_with_ann.csv", skip = 1)
coedu <- coedufull %>%
  select(Id, Id2, Geography, contains("Percent..Estimate..Population.25.years.and.over")) %>%
  select(-Percent..Estimate..Population.25.years.and.over)
colnames(coedu) <- c("id", "id.co", "name",
                     paste("edu.over25.",
                           c("lessHS", "someHS", "HS", "someCol", "assoc", "bach", "gradprof"),
                           sep = ""))
coedu <- coedu %>% mutate(odds.lessHS = edu.over25.lessHS / (100 - edu.over25.lessHS),
                          odds.someHS = edu.over25.someHS / (100 - edu.over25.someHS),
                          odds.HS = edu.over25.HS / (100 - edu.over25.HS),
                          odds.someCol = edu.over25.someCol / (100 - edu.over25.someCol),
                          odds.assoc = edu.over25.assoc / (100 - edu.over25.assoc),
                          odds.bach = edu.over25.bach / (100 - edu.over25.bach),
                          odds.gradprof = edu.over25.gradprof / (100 - edu.over25.gradprof))

coracefull <- read.csv("data/county/race/ACS_15_5YR_DP05_with_ann.csv", skip = 1)
corace <- coracefull %>%
  select(id = Id, id.co = Id2, name = Geography,
         race.white = Percent..RACE...One.race...White,
         race.black = Percent..RACE...One.race...Black.or.African.American,
         race.natamer = Percent..RACE...One.race...American.Indian.and.Alaska.Native,
         race.asian = Percent..RACE...One.race...Asian,
         race.islander = Percent..RACE...One.race...Native.Hawaiian.and.Other.Pacific.Islander,
         race.other = Percent..RACE...One.race...Some.other.race,
         race.twoplus = Percent..RACE...Two.or.more.races,
         hispanic = Percent..HISPANIC.OR.LATINO.AND.RACE...Total.population...Hispanic.or.Latino..of.any.race.) %>% mutate(race.other = race.other  + race.islander) %>%
  select(-race.islander) %>% mutate(odds.white = race.white / (100 - race.white),
                                    odds.black = race.black / (100 - race.black),
                                    odds.asian = race.asian / (100 - race.asian),
                                    odds.other = race.other / (100 - race.other),
                                    odds.twoplus = race.twoplus / (100 - race.twoplus),
                                    odds.hispanic = hispanic / (100 - hispanic))

Sys.setlocale('LC_ALL','C') ## to deal with a string with weird characters in it
codata <- full_join(coinc, full_join(coedu, corace)) %>% 
  mutate(state = sapply(name, function(x){strsplit(as.character(x), ", ")[[1]][2]}))

codataDC <- codata
codataDC$state[codataDC$state == "District of Columbia"] <- "Virginia"

xstate <- model.matrix(~ state - 1, codata)

x.base <- model.matrix(~ state - 1 + odds.black + odds.asian + odds.other +
                          odds.twoplus + odds.hispanic + odds.lessHS +
                          odds.someHS + odds.HS + odds.someCol + odds.assoc +
                          odds.bach, codataDC)


#####  fit a normal distribution
y <- rnorm(100, 4.0, 1.2)  # simulate fake data

## create a list of each variable defined in the data block
normaldat <- list(n_obs = length(y), y = y,
                  mu_prior_mn = 0, mu_prior_sd = 10,
                  sig_prior_scale = 10, sig_prior_df = 5)

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
ngroup <- 3 # number of groups
nobs <- 25 # sample size per group
mus <- c(1.2, 1.4, 1.6)
sigma <- 2.5
y <- t(sapply(mus, function(x) rnorm(N, x, sigma))) ## need it to be ngroup x nobs

hierdat <- list(n_obs = nobs, n_group = ngroup, y = y, mu_mn = 0, mu_sd = 10)

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

