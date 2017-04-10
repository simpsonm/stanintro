library(rstan)
library(tigris)
library(dplyr) ## for easy data frame manipulations
set.seed(34345398) ## for reproducing my simulations

coempfull <- read.csv("data/county/emp/ACS_15_5YR_S2301_with_ann.csv", skip = 1)
coemp <- select(coempfull, Id, Id2, Geography,
                lfpar = Labor.Force.Participation.Rate..Estimate..Population.16.years.and.over,
                emprat = Employment.Population.Ratio..Estimate..Population.16.years.and.over) %>%
  mutate(id = Id, id.co = Id2, name = Geography,
         odds.labfor = lfpar / (100 - lfpar), odds.emp = emprat / (100 - emprat))

coincfull <- read.csv("data/county/income/ACS_15_5YR_S1902_with_ann.csv", skip = 1)
coinc <- select(coincfull, Id, Id2, Geography,
                Mean.income..dollars...Estimate..All.households,
                Mean.income..dollars...Margin.of.Error..All.households)
colnames(coinc) <- c("id", "id.co", "name", "income.mean", "income.moe")
coinc$income.se = coinc$income.moe/1.645

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

coageracefull <- read.csv("data/county/race/ACS_15_5YR_DP05_with_ann.csv", skip = 1)
coagerace <- coageracefull %>%
  select(id = Id, id.co = Id2, name = Geography,
         race.white = Percent..RACE...One.race...White,
         race.black = Percent..RACE...One.race...Black.or.African.American,
         race.natamer = Percent..RACE...One.race...American.Indian.and.Alaska.Native,
         race.asian = Percent..RACE...One.race...Asian,
         race.islander = Percent..RACE...One.race...Native.Hawaiian.and.Other.Pacific.Islander,
         race.other = Percent..RACE...One.race...Some.other.race,
         race.twoplus = Percent..RACE...Two.or.more.races,
         hispanic = Percent..HISPANIC.OR.LATINO.AND.RACE...Total.population...Hispanic.or.Latino..of.any.race.,
         male = Percent..SEX.AND.AGE...Total.population...Male,
         age.under5 = Percent..SEX.AND.AGE...Under.5.years,
         age.5to9 = Percent..SEX.AND.AGE...5.to.9.years,
         age.10to14 = Percent..SEX.AND.AGE...10.to.14.years,
         age.15to19 = Percent..SEX.AND.AGE...15.to.19.years,
         age.20to24 = Percent..SEX.AND.AGE...20.to.24.years,
         age.25to34 = Percent..SEX.AND.AGE...25.to.34.years,
         age.35to44 = Percent..SEX.AND.AGE...35.to.44.years,
         age.45to54 = Percent..SEX.AND.AGE...45.to.54.years,
         age.55to59 = Percent..SEX.AND.AGE...55.to.59.years,
         age.60to64 = Percent..SEX.AND.AGE...60.to.64.years,
         age.65to74 = Percent..SEX.AND.AGE...65.to.74.years,
         age.75to84 = Percent..SEX.AND.AGE...75.to.84.years,
         age.over84 = Percent..SEX.AND.AGE...85.years.and.over) %>%
  mutate(race.other = race.other  + race.islander) %>%
  select(-race.islander) %>% mutate(odds.white = race.white / (100 - race.white),
                                    odds.black = race.black / (100 - race.black),
                                    odds.asian = race.asian / (100 - race.asian),
                                    odds.other = race.other / (100 - race.other),
                                    odds.twoplus = race.twoplus / (100 - race.twoplus),
                                    odds.hispanic = hispanic / (100 - hispanic),
                                    odds.male = male / (100 - male),
                                    odds.under5 = age.under5 / (100 - age.under5),
                                    odds.5to9 = age.5to9 / (100 - age.5to9),
                                    odds.10to14 = age.10to14 / (100 - age.10to14),
                                    odds.15to19 = age.15to19 / (100 - age.15to19),
                                    odds.20to24 = age.20to24 / (100 - age.20to24),
                                    odds.25to34 = age.25to34 / (100 - age.25to34),
                                    odds.35to44 = age.35to44 / (100 - age.35to44),
                                    odds.45to54 = age.45to54 / (100 - age.45to54),
                                    odds.55to59 = age.55to59 / (100 - age.55to59),
                                    odds.60to64 = age.60to64 / (100 - age.60to64),
                                    odds.65to74 = age.65to74 / (100 - age.65to74),
                                    odds.75to84 = age.75to84 / (100 - age.75to84),
                                    odds.over84 = age.over84 / (100 - age.over84))

Sys.setlocale('LC_ALL','C') ## to deal with a string with weird characters in it

regions <- read.csv("data/state-geocodes.csv")
regions$state <- as.character(regions$Name)

codata <- full_join(coemp, full_join(coinc, full_join(coedu, coagerace))) %>%
  mutate(state = sapply(name, function(x){strsplit(as.character(x), ", ")[[1]][2]})) %>%
  left_join(regions)

x.base <- model.matrix(~ odds.black + odds.asian + odds.other +
                         odds.twoplus + odds.hispanic + odds.lessHS +
                         odds.someHS + odds.HS + odds.someCol + odds.assoc +
                         odds.bach + odds.5to9 + odds.10to14 + odds.15to19 +
                         odds.20to24 + odds.25to34 + odds.35to44 + odds.45to54 +
                         odds.55to59 + odds.60to64 + odds.65to74 + odds.75to84 +
                         odds.over84 + odds.labfor + odds.emp - 1, codata)

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
## about 250 seconds to fit

traceplot(regfit, pars = c("alpha", "beta"))

traceplot(regfit, pars = c(paste("beta[", 1:3, "]", sep=""), "sigma"))

summary(regfit, pars = c("alpha", "beta", "sigma"))$summary

regfitdraws <- extract(regfit)
str(regfitdraws, 1)

regfit_cs0 = stan("regression_cs.stan", data = regdat, chains = 1, iter = 1)

## sample the model
regfit_cs = stan(fit = regfit_cs0, data = regdat, cores = 4, chains = 4,
                 warmup = 2000, iter = 4000, open_progress = FALSE)
## about 20 seconds to fit

randintdat <- list(n_obs = nrow(codata), n_cov = ncol(x.base),
                   n_region = length(unique(codata$state)),
                   y = codata$income.mean, x = x.base,
                   region = as.numeric(as.factor(codata$state)),
                   y_se = codata$income.se)

randintfit0 <- stan("rand_intercept_reg.stan", data = randintdat, chains = 1, iter = 1)

randintfit <- stan(fit = randintfit0, data = randintdat, cores = 4, chains = 4,
                   warmup = 2000, iter = 4000, open_progress = FALSE)


randintfit0 <- stan("rand_int_reg2.stan", data = randintdat, chains = 1, iter = 1)

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



ngroup <- 50
alphas <- rnorm(ngroup, 10, .1)
beta <- c(1, 0.5, -2)/2
nobs <- 100
groups <- rep(1:50, 2)
x <- cbind(rnorm(nobs, 2, 1), runif(nobs, -1, 1), rgamma(nobs, 1, 1.5))
y <- as.vector(alphas[groups] + x%*%beta + rnorm(nobs))

randintdat <- list(n_obs = nobs, n_cov = length(beta), n_region = length(alphas),
                   y = y, x = x, region = groups)

randintfit0 <- stan("rand_intercept_reg.stan", data = randintdat, chains = 1, iter = 1)

randintfit <- stan(fit = randintfit0, data = randintdat, cores = 4, chains = 4,
                   warmup = 2000, iter = 4000, open_progress = FALSE)



randintncfit0 <- stan("rand_intercept_reg_noncen.stan", data = randintdat, chains = 1, iter = 1)

randintncfit <- stan(fit = randintncfit0, data = randintdat, cores = 4, chains = 4,
                     warmup = 2000, iter = 4000, open_progress = FALSE)





randintdat <- list(n_obs = nrow(codata), n_cov = ncol(x.race), n_region = length(unique(codata$state)),
                   y = codata$income.mean, x = x.race, region = as.numeric(as.factor(codata$state)))

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


randregdat <- list(n_obs = nrow(codata), n_cov = 1, n_group = length(unique(codata$state)),
                   y = codata$income.mean, x = matrix(1, ncol = 1, nrow = nrow(codata)),
                   group = as.numeric(as.factor(codata$state)))


randregfit0 <- stan("rand_regression.stan", data = randregdat, chains = 1, iter = 1)

randregfit <- stan(fit = randregfit0, data = randregdat, cores = 4, chains = 4,
                   warmup = 2000, iter = 4000, open_progress = FALSE)
## 500 seconds, 229 divergent transitions

randregncfit0 <- stan("rand_regression_nc.stan", data = randregdat, chains = 1, iter = 1)

randregncfit <- stan(fit = randregncfit0, data = randregdat, cores = 4, chains = 4,
                     warmup = 2000, iter = 4000, open_progress = FALSE)

## nothing works; fake data is probably where it's at

