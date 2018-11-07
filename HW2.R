library(survival)
library(survminer)
library(flexsurv)
library(dplyr)

katrina <- read.csv("katrina.csv", header = TRUE)

# fitting AFT model
fit <- survreg(Surv(time = hour, event = reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation + slope +
                 age, data = katrina, dist = "weibull")

summary(fit)
exp(coef(fit))

#fitting model with different distributions
library(flexsurv)x
# the syntax in flexsurvreg() is the same as survreg()
# weibull distribution
fit_wb <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation + slope +
                        age, data = katrina, dist = "weibull")

plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "weibull distribution")

# log-logistic distribution
fit_wb <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation + slope +
                        age, data = katrina, dist = "llogis")

plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "log-logistic distribution")

# lognormal distribution
fit_wb <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation + slope +
                        age, data = katrina, dist = "lnorm")

plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "lognormal distribution")


# exponential distribution
fit_wb <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation + slope +
                        age, data = katrina, dist = "exp")

plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "exponential distribution")

# coeff estimate & std errors - fitting weibull

fit_weibull<- survreg(Surv(time = hour, event = reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation + slope +
                       age, data = katrina, dist = "weibull")

# estimated coefficients & estimates
data.frame("weibull" = coef(fit_weibull), "exp_weibull" = exp(coef(fit_weibull)))

# so now for the pumps who didn't have backup, we're going to predict
# the mean time to survive if they HAD gotten it
# to do this, we're assuming that they'll still have the event at the same
# estimated survival probability as they did previously
katrina_nofin <- katrina %>%
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(fit, type = "lp"),
         # add ID variable so we know which subjects they are
         ID = row_number()) %>%
  # next, we're only interested in those who had the event AND no backup
  dplyr::filter(reason == 1, backup == 0) %>%
  # now i'm doing two things:
  # 1. find the survival prob at the time of their event
  # 2. pretending they did have backup (change backup from 0 to 1)
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit$scale,
                                  distribution = fit$dist),
         old_backup = backup,
         backup = old_backup + 1)



# now with that dataset, i need to find their new time
results <- katrina_nofin %>%
  # estimate their new linear predictor value if they had financial aid
  mutate(new_lp = predict(fit, newdata = katrina_nofin, type = "lp"),
         # now, keeping the same survival probability as before, get the time
         # corresponding to the new_lp value
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit$scale,
                             distribution = fit$dist),
         # and compute the difference
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)

results = results[order(-results$pred_time_diff),]

head(results,20)



#bridgecrane- doesn't look good
katrina_nofin <- katrina %>%
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(fit, type = "lp"),
         # add ID variable so we know which subjects they are
         ID = row_number()) %>%
  # next, we're only interested in those who had the event AND no financial aid
  dplyr::filter(reason == 1, bridgecrane == 0) %>%
  # now i'm doing two things:
  # 1. find the survival prob at the time of their event
  # 2. pretending they did have financial aid (change fin from 0 to 1)
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit$scale,
                                  distribution = fit$dist),
         old_bridgecrane = bridgecrane,
         bridgecrane = old_bridgecrane + 1)



# now with that dataset, i need to find their new time
results <- katrina_nofin %>%
  # estimate their new linear predictor value if they had financial aid
  mutate(new_lp = predict(fit, newdata = katrina_nofin, type = "lp"),
         # now, keeping the same survival probability as before, get the time
         # corresponding to the new_lp value
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit$scale,
                             distribution = fit$dist),
         # and compute the difference
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)

results = results[order(-results$pred_time_diff),]

head(results,20)



#servo doesn't look good
katrina_nofin <- katrina %>%
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(fit, type = "lp"),
         # add ID variable so we know which subjects they are
         ID = row_number()) %>%
  # next, we're only interested in those who had the event AND no financial aid
  dplyr::filter(reason == 1, servo == 0) %>%
  # now i'm doing two things:
  # 1. find the survival prob at the time of their event
  # 2. pretending they did have financial aid (change fin from 0 to 1)
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit$scale,
                                  distribution = fit$dist),
         old_servo = servo,
         serv0 = old_servo + 1)



# now with that dataset, i need to find their new time
results <- katrina_nofin %>%
  # estimate their new linear predictor value if they had financial aid
  mutate(new_lp = predict(fit, newdata = katrina_nofin, type = "lp"),
         # now, keeping the same survival probability as before, get the time
         # corresponding to the new_lp value
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit$scale,
                             distribution = fit$dist),
         # and compute the difference
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)

results = results[order(-results$pred_time_diff),]

head(results,20)



# trashrack doesn't look good
katrina_nofin <- katrina %>%
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(fit, type = "lp"),
         # add ID variable so we know which subjects they are
         ID = row_number()) %>%
  # next, we're only interested in those who had the event AND no financial aid
  dplyr::filter(reason == 1, trashrack == 0) %>%
  # now i'm doing two things:
  # 1. find the survival prob at the time of their event
  # 2. pretending they did have financial aid (change fin from 0 to 1)
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit$scale,
                                  distribution = fit$dist),
         old_trashrack = trashrack,
         trashrack = old_trashrack + 1)



# now with that dataset, i need to find their new time
results <- katrina_nofin %>%
  # estimate their new linear predictor value if they had financial aid
  mutate(new_lp = predict(fit, newdata = katrina_nofin, type = "lp"),
         # now, keeping the same survival probability as before, get the time
         # corresponding to the new_lp value
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit$scale,
                             distribution = fit$dist),
         # and compute the difference
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)

results = results[order(-results$pred_time_diff),]

head(results,20)
