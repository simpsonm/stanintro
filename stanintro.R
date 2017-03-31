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

regions <- read.csv("data/state-geocodes.csv")
regions$state <- as.character(regions$Name)

codata <- full_join(coinc, full_join(coedu, corace)) %>%
  mutate(state = sapply(name, function(x){strsplit(as.character(x), ", ")[[1]][2]})) %>%
  left_join(regions)

x.base <- model.matrix(~ odds.black + odds.asian + odds.other +
                         odds.twoplus + odds.hispanic + odds.lessHS +
                         odds.someHS + odds.HS + odds.someCol + odds.assoc +
                         odds.bach - 1, codata)

x.educ <- model.matrix(~ odds.lessHS +
                         odds.someHS + odds.HS + odds.someCol + odds.assoc +
                         odds.bach - 1, codata)

x.race <- model.matrix(~ odds.black + odds.asian + odds.other +
                         odds.twoplus + odds.hispanic - 1, codata)


int.state <- model.matrix(~ state - 1, codata)

int.division <- model.matrix(~ Division - 1, codata)

int.region <- model.matrix(~ Region - 1, codata)

## create list of all variables in the data block
regdat <- list(n_obs = nrow(codata), n_cov = ncol(x.base),
               y = codata$income.mean, x = x.base,
               beta_prior_mn = 0, beta_prior_sd = 20000,
               alpha_prior_mn = 60000, alpha_prior_sd = 20000,
               sig_prior_scale = 20000, sig_prior_df = 5)

## initialize: create the model and check data constraints
## (takes a good 15-30 seconds)
regfit0 = stan("regression.stan", data = regdat, chains = 1, iter = 1)
## ignore compiler warnings

## sample the model
regfit = stan(fit = regfit0, data = regdat, cores = 4, chains = 4,
              warmup = 2000, iter = 4000, open_progress = FALSE)
## about 70 seconds to fit

traceplot(regfit, pars = c("alpha", "beta"))

traceplot(regfit, pars = c(paste("beta[", 1:3, "]", sep=""), "sigma"))

summary(regfit, pars = c("alpha", "beta", "sigma"))$summary

regfitdraws <- extract(regfit)
str(regfitdraws, 1)

regfit_cs0 = stan("regression_cs.stan", data = regdat, chains = 1, iter = 1)

## sample the model
regfit_cs = stan(fit = regfit_cs0, data = regdat, cores = 4, chains = 4,
                 warmup = 2000, iter = 4000, open_progress = FALSE)
## about 10 seconds to fit

randintdat <- list(n_obs = nrow(codata), n_cov = ncol(x.base), n_region = length(unique(codata$Division)),
                   y = codata$income.mean, x = x.base, region = codata$Division)

randintfit0 <- stan("rand_intercept_reg.stan", data = randintdat, chains = 1, iter = 1)

randintfit <- stan(fit = randintfit0, data = randintdat, cores = 4, chains = 4,
                   warmup = 2000, iter = 4000, open_progress = FALSE)


randintncfit0 <- stan("rand_intercept_reg_noncen.stan", data = randintdat, chains = 1, iter = 1)

randintncfit <- stan(fit = randintncfit0, data = randintdat, cores = 4, chains = 4,
                     warmup = 2000, iter = 4000, open_progress = FALSE)

randintfit2 <- stan(fit = randintfit, data = randintdat, cores = 4, chains = 4,
                   warmup = 2000, iter = 4000, open_progress = FALSE,
                   control = list(adapt_delta = 0.999))

randintncfit2 <- stan(fit = randintncfit0, data = randintdat, cores = 4, chains = 4,
                      warmup = 2000, iter = 4000, open_progress = FALSE,
                      control = list(adapt_delta = 0.999))



randintncfit0 <- stan("rand_intercept_reg_noncen.stan", data = randintdat, chains = 1, iter = 1)

randintncfit <- stan(fit = randintncfit0, data = randintdat, cores = 4, chains = 4,
                     warmup = 2000, iter = 4000, open_progress = FALSE)





randintdat <- list(n_obs = nrow(codata), n_cov = ncol(x.race), n_region = length(unique(codata$Region)),
                   y = codata$income.mean, x = x.race, region = as.numeric(as.factor(codata$Region)))

randintfit0 <- stan("rand_intercept_reg.stan", data = randintdat, chains = 1, iter = 1)

randintfit <- stan(fit = randintfit0, data = randintdat, cores = 4, chains = 4,
                   warmup = 2000, iter = 4000, open_progress = FALSE)

pairs(randintfit, pars = c("alpha[1]", "sigma_alpha"))

pairs(randintfit)


randintncfit0 <- stan("rand_intercept_reg_noncen.stan", data = randintdat, chains = 1, iter = 1)

randintncfit <- stan(fit = randintncfit0, data = randintdat, cores = 4, chains = 4,
                     warmup = 2000, iter = 4000, open_progress = FALSE)

pairs(randintncfit)

x.full <- model.matrix(~ odds.black + odds.asian + odds.other +
                         odds.twoplus + odds.hispanic + odds.lessHS +
                         odds.someHS + odds.HS + odds.someCol + odds.assoc +
                         odds.bach, codata)


randregdat <- list(n_obs = nrow(codata), n_cov = ncol(x.full), n_group = length(unique(codata$state)),
                   y = codata$income.mean, x = x.full, group = as.numeric(as.factor(codata$state)))


randregfit0 <- stan("rand_regression.stan", data = randregdat, chains = 1, iter = 1)

randregfit <- stan(fit = randregfit0, data = randregdat, cores = 4, chains = 4,
                   warmup = 2000, iter = 4000, open_progress = FALSE)
## 500 seconds, 229 divergent transitions

randregncfit0 <- stan("rand_regression_nc.stan", data = randregdat, chains = 1, iter = 1)

randregncfit <- stan(fit = randregncfit0, data = randregdat, cores = 4, chains = 4,
                     warmup = 2000, iter = 4000, open_progress = FALSE)

## nothing works; fake data is probably where it's at
