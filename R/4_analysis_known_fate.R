# Analysis

library(tidyverse)
library(survival)
library(ggsurvfit)
library(RMark)
library(INBOtheme)
source("./R/functions.R")

df_lh <- readRDS("./data/interim/df_lh.RDS")
df_obs <- readRDS("./data/interim/df_obs.RDS")

#view(df_lh)
startdate <- min(df_lh$m_date)
enddate <- max(df_lh$s_date, df_lh$l_date, na.rm = TRUE)

obstime <- seq(from = 0, to = as.numeric(enddate - startdate) + 7, by = 7)

df_kf <- convert_to_RMark(obs = obstime, m_time = df_lh$m_time,
                 s_time = df_lh$s_time, l_time = df_lh$l_time)

df_kf <- df_kf %>%
  mutate(sex = ifelse(df_lh$Geslacht == "vrouw", 1, 2),
         area = case_when(
           df_lh$Gebied == "MIDD"  ~ 1,
           df_lh$Gebied == "ROES" ~ 2,
           df_lh$Gebied == "RAMS" ~ 3))

#view(df_kf)

# Process data
processed <- process.data(df_kf, model = "Known")

# Create default design data
ddl <- make.design.data(processed)

#  Define range of models for S
S.dot = list(formula = ~1)
S.sex = list(formula = ~sex)
S.area = list(formula = ~area)
S.time = list(formula = ~time)
S.timearea = list(formula = ~area + time)
S.sextime = list(formula = ~sex + time)

# Create model list and run assortment of models
model.list = create.model.list("Known")
results <- mark.wrapper(model.list, data = processed, ddl = ddl,
                        invisible = FALSE,threads = 1)


results
results$S.dot$results$beta
results$S.area$results$beta
results$S.sex$results$beta
results$S.time$results$beta
results$S.sextime$results$beta


results[[2]]$results$beta
results[[2]]$results$real
plogis(results[[2]]$results$beta$estimate)

summary(results[[1]])

results[[1]]$results$beta
results[[1]]$results$real


S_kf <- (results[[1]]$results$real$estimate)^length(obstime)
S_kf_lcl <- (results[[1]]$results$real$lcl)^length(obstime)
S_kf_ucl <- (results[[1]]$results$real$ucl)^length(obstime)

results[[3]]$results$real$estimate
prod(results[[3]]$results$real$estimate)
cumprod(results[[3]]$results$real$estimate)

dfres_time <- results[[4]]$results$real$estimate %>%
  as_tibble() %>%
  mutate(time = obstime) %>%
  mutate(cumest = cumprod(value))

ggplot(dfres_time, aes(x = time, y = cumest)) + geom_point() +
  scale_y_continuous(limits = c(0,1))


results[[4]]$model
results[[4]]$model.name
results[[4]]$results

results[[4]]$design.data
pred_times <- data.frame(time = seq(from = 2, to = 16))

results[[5]]$design.data
covariate.predictions(model = results[[5]], data = NULL, indices = c(1))



# Per geslacht

processed <- process.data(df_kf, model = "Known")
ddl <- make.design.data(processed)
kfsex <- mark(data = processed, ddl = ddl, model = "Known",
               model.parameters = list(S = list(formula = ~ sex)))
kfsex$results$real
kfsex$results$beta

invlogit <- function(x) 1 / (1 + exp(-x))

invlogit(kfsex$results$beta$estimate[1])
invlogit(kfsex$results$beta$lcl[1])
invlogit(kfsex$results$beta$estimate[1] + kfsex$results$beta$estimate[2])
invlogit(kfsex$results$beta$lcl[1] + kfsex$results$beta$lcl[2])

summary(kfsex)

kfsex$design.data
pred_sex <- data.frame(sex = c(0, 1))
covariate.predictions(model = kfsex, data = pred_sex, indices = c(1))

kfsex$results$real

#Male

df_male <- filter(df_kf, sex == "2")

processed <- process.data(df_male, model = "Known")
ddl <- make.design.data(processed)
kfmale <- mark(data = processed, ddl = ddl, model = "Known",
               model.parameters = list(S = list(formula = ~time)))
kfmale$results$real
kfmale$results$beta$se

summary(kfmale)
kfmale$design.data

#Female

df_female <- filter(df_kf, sex == "1")

processed <- process.data(df_female, model = "Known")
ddl <- make.design.data(processed)
kffemale <- mark(data = processed, ddl = ddl, model = "Known",
               model.parameters = list(S = list(formula = ~time)))
kffemale$results$real$estimate

df_t_male <- data.frame(time = obstime, est = c(kfmale$results$real$estimate), sex = "male")
df_t_female <- data.frame(time = obstime, est = c(kffemale$results$real$estimate), sex = "female")

df_time_sex <- df_t_male %>%
  bind_rows(df_t_female) %>%
  as_tibble() %>%
  group_by(sex) %>%
  mutate(cumest = cumprod(est)) %>%
  mutate(date = startdate + obstime) %>%
  ungroup

ggplot(df_time_sex, aes(x = date, y = cumest, colour = sex)) + geom_point() +
  geom_line() +
  scale_x_date(date_breaks  = "1 month", date_labels =  "%b") +
  scale_y_continuous(limits = c(0,1)) +
  ggtitle("Cumulatieve overlevingskans") +
  ylab("Overlevingskans (S)")





#MIDD

df_midd <- filter(df_kf, area == "1")

processed <- process.data(df_midd, model = "Known")
ddl <- make.design.data(processed)
kfmidd <- mark(data = processed, ddl = ddl, model = "Known",
               model.parameters = list(S = list(formula = ~time)))
kfmidd$results$real
kfmidd$results$beta$se


summary(kfmidd)
kfmidd$design.data



pred_times <- data.frame(time = obstime[5:20])
covariate.predictions(model = kfmidd, data = pred_times, indices = c(1))


SpredB<-covariate.predictions(bduck.results$S.BirdAge.Weight,data=data.frame(BirdAge=1,Weight=1.2),indices=c(1))
SpredB$estimate$estimate



#ROES

df_roes <- filter(df_kf, area == "2")

processed <- process.data(df_roes, model = "Known")
ddl <- make.design.data(processed)
kfroes <- mark(data = processed, ddl = ddl, model = "Known",
               model.parameters = list(S = list(formula = ~time)))
kfroes$results$real$estimate

df_t_midd <- data.frame(time = obstime, est = c(kfmidd$results$real$estimate), area = "midd")
df_t_roes <- data.frame(time = obstime, est = c(kfroes$results$real$estimate), area = "roes")

df_time <- df_t_midd %>%
  bind_rows(df_t_roes) %>%
  as_tibble() %>%
  group_by(area) %>%
  mutate(cumest = cumprod(est)) %>%
  mutate(date = startdate + obstime) %>%
  ungroup

ggplot(df_time, aes(x = date, y = cumest, colour = area)) + geom_point() +
  geom_line() +
  scale_x_date(date_breaks  = "1 month", date_labels =  "%b") +
  scale_y_continuous(limits = c(0,1)) +
  ggtitle("Cumulatieve overlevingskans") +
  ylab("Overlevingskans (S)")



## Progressief toenemende duur per gebied

# We progressively increase the length of the dataset.
# For each length we calculate the overall survival probability with
# confidence intervals. We do this for each region.

# We start in the second observation (first observation
# only contains markings. No observations)

df_s <- expand.grid(obs = obstime, Gebied = c("MIDD", "ROES"))
df_s <-  df_s %>%
  mutate(S = NA, lcl = NA, ucl = NA) %>%
  mutate(area = case_when(
    Gebied == "MIDD"  ~ 1,
    Gebied == "ROES" ~ 2,
    Gebied == "RAMS" ~ 3)) %>%
  mutate(Datum = startdate + obs)

for (t in 2:length(obstime)) {

  cat(t, obstime[t])

  df_t <- convert_to_RMark(obs = obstime[1:t], m_time = df_lh$m_time,
                            s_time = df_lh$s_time, l_time = df_lh$l_time)

  df_t <- df_t %>%
    mutate(sex = ifelse(df_lh$Geslacht == "vrouw", 1, 2),
           area = case_when(
             df_lh$Gebied == "MIDD" ~ 1,
             df_lh$Gebied == "ROES" ~ 2,
             df_lh$Gebied == "RAMS" ~ 3))

  for(a in c(1,2)) {

     df_d <- df_t %>%
      filter(area == a) %>%
      filter(str_detect(ch, "1"))

    processed <- process.data(df_d, model = "Known")
    ddl <- make.design.data(processed)
    kfd <- mark(data = processed, ddl = ddl, model = "Known",
                   model.parameters = list(S = list(formula = ~1)))

    S <- kfd$results$real$estimate
    lcl <- kfd$results$real$lcl
    ucl <- kfd$results$real$ucl

    df_s[df_s$obs == obstime[t] & df_s$area == a, "S"] <- S^t
    df_s[df_s$obs == obstime[t] & df_s$area == a, "lcl"] <- lcl^t
    df_s[df_s$obs == obstime[t] & df_s$area == a, "ucl"] <- ucl^t
  }
}

saveRDS(df_s, file = "./data/interim/df_s.RDS")

ggplot(df_s, aes(x = Datum, y = S, colour = Gebied, fill = Gebied)) +
  geom_point() + geom_line() +
  geom_smooth(aes(ymin = lcl, ymax = ucl), stat = "identity") +
  scale_x_date(date_breaks  = "1 month", date_labels =  "%b") +
  scale_y_continuous(limits = c(0,1)) +
  theme_inbo() +
  ggtitle("Cumulatieve overlevingskans (S)")




kfmidd$results$beta

s_beta <- kfmidd$results$beta$estimate
s_beta_se <- kfmidd$results$beta$se
s_real <- kfmidd$results$real$estimate

invlogit <- function(x) 1 / (1 + exp(-x))

est5_beta <- s_beta[1] + s_beta[5]
rr <- invlogit(s_beta$estimate[1] + s_beta$estimate[5])

rnorm(n = 1, mean = est5_beta, sd = se_s_beta[5])

plogis(intercept)



invlogit()


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
