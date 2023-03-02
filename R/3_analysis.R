# Analysis

library(tidyverse)
library(survival)
library(ggsurvfit)
library(RMark)
source("./R/functions.R")

df_lh <- readRDS("./data/interim/df_lh.RDS")
df_obs <- readRDS("./data/interim/df_obs.RDS")

# KM survival

df_lh <- df_lh %>%
  dplyr::select(Ringnummer, Geslacht, m_time, s_time, l_time) %>%
  mutate(s_time2 = s_time - m_time,
         l_time2 = l_time - m_time) %>%
  mutate(time = ifelse(s_time2 < l_time2, s_time2, l_time2),
         event = ifelse(s_time2 < l_time2, 1, 0))   # censored = 0, dead = 1

df_lh

km_fit1 <- survfit(Surv(time, event) ~ 1, data = df_lh)

Surv(df_lh$time, df_lh$event)

summary(km_fit1)

plot(km_fit1, main = "Kaplan-Meier estimate (95% CI)",
     xlab = "time", ylab = "survival function")

km_fit1b <- survfit2(Surv(time, event) ~ 1, data = df_lh)

ggsurvfit(km_fit1b) +
  labs(x = "Days", y = "Overall survival probability") +
  add_confidence_interval() +
  scale_y_continuous(limits = c(0, 1))


km_fit2 <- survfit(Surv(time, event) ~ Geslacht, data = df_lh)

summary(km_fit2)

plot(km_fit2, main = "Kaplan-Meier estimate (95% CI)",
     xlab = "time", ylab = "survival function")

km_fit2b <- survfit2(Surv(time, event) ~ Geslacht, data = df_lh)

ggsurvfit(km_fit2b) +
  labs(x = "Days", y = "Overall survival probability") +
  add_confidence_interval() +
  scale_y_continuous(limits = c(0,1))

survdiff(Surv(time, event) ~ Geslacht, data = df_lh)

# => geen aanwijzing dat er verschil is tussen mannelijke of vrouwelijke individuen

# -> gebied als covariaat nemen

#Prepare format for LDLDLD 'known fate model RMArk'

v_obs <- df_obs$obs
v_mark <- df_lh$m_time
obs <- sort(unique(c(v_obs, v_mark)))

df_kf <- convert_to_RMark(obs = obs, m_time = df_lh$m_time,
                 s_time = df_lh$s_time, l_time = df_lh$l_time)

df_kf <- df_kf %>%
  mutate(sex = ifelse(df_lh$Geslacht == "vrouw", 1, 2))
# RMark - Known fate model
# Process data
processed <- process.data(df_kf, model = "Known")

# Create default design data
ddl <- make.design.data(processed)

#  Define range of models for S
S.dot = list(formula = ~1)
S.sex = list(formula = ~sex)
S.time = list(formula = ~time)


# Create model list and run assortment of models
model.list = create.model.list("Known")
results <- mark.wrapper(model.list, data = processed, ddl = ddl,
                        invisible = FALSE,threads = 1)

results
results[[1]]$results$real

S_kf <- (results[[1]]$results$real$estimate)^length(obs)
S_kf_lcl <- (results[[1]]$results$real$lcl)^length(obs)
S_kf_ucl <- (results[[1]]$results$real$ucl)^length(obs)


#### Multi state   KM



# Staggered introduction /  left censoring
# - nog even verder lezen, maar normaal gezien corrigeerd dit voor de overldedn
# populatie


# 48 Roeselare   (5 door vangst overleden + 1 te weinig data)
# 34 Middelkerke  (4 overleden door vangst)

# first nest stop is einde van eerste periode
# dat moment is verschillende per individu -> multi state known fat
# ? wat met individuen waarvan we 1st nest datum niet kennen?

# - > gemiddelde van 'first nest stop' datum - succesvolle nest 1st nest succes datum
