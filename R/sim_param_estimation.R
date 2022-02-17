## Simulatie overleving Patrijs

library(tidyverse)
library(survival)
library(KMsurv)
library(RMark)

# Simulatie van de overleving van patrijs om geschikte
# analyse methodes te testen.

# Simulatie 1 eenvoudige simulatie constante jaarlijkse overleving dagelijks

# Parameters
Sj <- 0.8     # Jaarlijkse overleving
n_init <- 500  # Startpopulatie
tdays <- 365     # Total time in days

Sd <- Sj^(1/tdays)  # Dagelijkse overleving
mat <- matrix(data = 0, nrow = n_init, ncol = tdays)
mat[, 1] <- 1

# Simulate daily survival during one year
for (i in 2:tdays) {
  mat[, i] <- mat[, i - 1] * rbinom(n = n_init, size = 1, prob = Sd)
}

nt <- colSums(mat)  # number of individuals surviving during the year
plot(nt)

# Create density curve for simulation
n_sim <- 1000
S_sim <- vector(length = n_sim)
for (s in (1:n_sim)){
  for (i in 2:tdays) {
    mat[, i] <- mat[, i - 1] * rbinom(n = n_init, size = 1, prob = Sd)
  }
  nt <- colSums(mat)  # number of individuals surviving during the year
  S_sim[s] <- nt[tdays] / n_init # Survival after one year
}
hist(S_sim)
S_sim_CI <- quantile(S_sim, probs = c(0.025, 0.975))

# Create dataset in format for KM analysis
n_time <- vector(length = n_init)
for (i in 1:n_init) {
  #n_time[i] <- min(which(mat[i,] == 0))
  n_time[i] <- match(0, mat[i,])
}
n_time[is.finite(n_time) == FALSE] <- tdays   # right truncation max days
n_time <- sort(n_time)
n_event <- ifelse(n_time < 365, 1, 0)
plot(n_time)

###########################################
# Mayfield method
d <- n_init * 1  # Total of individual years
S <- nt[tdays] / d   # Survival after one year
S
m <- 1 - S         # mortality rate
m_CI <- m / (2 * d) * qchisq(p = c(0.025, 0.975), df = 2 * d)

S_CI <- 1 - m_CI # Survival rate
S_CI             # confidence interval

# Met deze methode zijn confidence intervallen < 0 mogelijk!

# Survival function - Kaplan-Meier method
# Met 'left censoring' voor de individuen die de onderzoeksperiode overleven.
km <- Surv(n_time, n_event)
km

km_fit <- survfit(data = km, km ~ 1)
km_fit

summary(km_fit)
str(km_fit)          # full summary of the my.surv object

km_fit$time             # {t_i} (= all times)
summary(km_fit)$time    # {tau_i} (= observed times = uncensored times)
km_fit$n.risk           # {r_i} at {t_i}

tail(summary(km_fit)$surv, 1)

plot(km_fit, main = "Kaplan-Meier estimate (95% CI)",
     xlab = "time", ylab = "survival function")

# tail(summary(km_fit)$surv, 1)    # returns the Kaplan-Meier estimate at {tau_i}
# tail(summary(km_fit)$lower, 1)   # lower pointwise estimates (alternatively, $upper)
# tail(summary(km_fit)$upper, 1)   # lower pointwise estimates (alternatively, $upper)


## Methode known fate models in RMark
# Create LD format for known fate model

it <- 20 # interval_time = time between observations
obs <- seq(from = 1, to = tdays - it, by = it)
ch <- vector(length = n_init)

for (j in 1:n_init) {
  mych <- ""
  for (i in obs) {

    L <- ifelse(mat[j, i] == 1, 1, 0)
    D <- ifelse((mat[j, i] == 1) & min(mat[j, i:(i + it)]) == 0, 1, 0)
    mych <- paste0(mych, L, D)

  }
  ch[j] <- mych
}

ch
mat[,341]

df_ch <- data.frame(ch = ch)

# RMark - Known fate model
# Process data
processed <- process.data(df_ch, model = "Known")

# Create default design data
ddl <- make.design.data(processed)

#  Define range of models for S
S.dot = list(formula = ~1)
S.time = list(formula = ~time)

# Create model list and run assortment of models
model.list = create.model.list("Known")
results <- mark.wrapper(model.list, data = processed, ddl = ddl,
                             invisible = FALSE,threads = 1)

results
results[[1]]$results$real
results[[2]]$results$real

S_kf <- (results[[1]]$results$real$estimate)^18
S_kf_lcl <- (results[[1]]$results$real$lcl)^18
S_kf_ucl <- (results[[1]]$results$real$ucl)^18




df_out <- data.frame(method = c("sim", "Mayfiel", "KapMei", "KnownFate"),
                     S = c(Sj, S, tail(summary(km_fit)$surv, 1), S_kf),
                     ucl = c(S_sim_CI[1], S_CI[1],
                             tail(summary(km_fit)$upper, 1), S_kf_ucl),
                     lcl = c(S_sim_CI[2], S_CI[2],
                             tail(summary(km_fit)$lower, 1), S_kf_lcl))

ggplot(df_out, aes(x = method, y = S, ymin = lcl, ymax = ucl)) +
  geom_point() + geom_errorbar() + ylim(c(-0.15, 1))



# Eventueel ook te vervangen door join live dead recoveries met
# p,F,r = 1 (dus enkel S te schatten).


########################################################################
#
# Simulatie 2: constante jaarlijkse overleving dagelijks, ringperiode
#
# - periode voor aanbrengen van zenders
# - uitval van zenders

# Parameters
Sj <- 0.8       # Jaarlijkse overleving
n_init <- 100   # Totaal aantal gezenderde individuen
max_start <- 50 # Periode (dagen na start project) aanbrengen zenders
tdays <- 365    # Totale duur van het opvolgen (dagen)
ploss <- 0.1    # Jaarlijkse kans op verlies van transmitter

Sd <- Sj^(1/365)        # Dagelijkse overleving
prsy <- (1 - ploss)^(1/365) # Dagelijkse kans op behoud transmitter
mat <- mrs <- matrix(data = 0, nrow = n_init, ncol = tdays) # lege state

# Aanbrengen van de zenders (uniforme verdeling over periode 1 tot max_start)
tel_start <- round(runif(n = n_init, min = 1, max = max_start))
#hist(tel_start)
mat[tel_start == 1, 1] <- 1   # starting telemetry for first time step

# Simulate daily survival + new telemetry till tdays
for (i in 2:tdays) {
  mat[, i] <- mat[, i - 1] * rbinom(n = n_init, size = 1, prob = Sd)
  mat[tel_start == i, i] <- 1  # Start telemetry for time step i
}

nt <- colSums(mat)  # number of individuals surviving during the year
plot(nt)

# Simulate loss of transmitters
mrs <- vector(length = n_init)  # Time to loss

for (j in 1:n_init) {
  lost <- which(rbinom(n = tdays - tel_start[j],
                       size = 1, prob = prsy) == 0)[1]
  mrs[j] <- ifelse(!is.na(lost), lost, tdays)
}

plot(sort(mrs))


# Create dataset in format for KM analysis
n_time <- n_event <- vector(length = n_init)

for (j in 1:n_init) {
  n_time[j] <-
}

for (i in 1:n_init) {
  n_time[i] <- match(0, mat[i,tel_start[i]:tdays])
}
n_time[is.finite(n_time) == FALSE] <- tdays   # right truncation max days
n_time <- sort(n_time)
n_event <- ifelse(n_time < tdays, 1, 0)
plot(n_time)
plot(n_event)



# Survival function - Kaplan-Meier method
# Met 'left censoring' voor de individuen die de onderzoeksperiode overleven.
km <- Surv(n_time, n_event)
km

km_fit <- survfit(data = km, km ~ 1)
km_fit

summary(km_fit)
str(km_fit)          # full summary of the my.surv object

km_fit$time             # {t_i} (= all times)
summary(km_fit)$time    # {tau_i} (= observed times = uncensored times)
km_fit$n.risk           # {r_i} at {t_i}

tail(summary(km_fit)$surv, 1)

plot(km_fit, main = "Kaplan-Meier estimate (95% CI)",
     xlab = "time", ylab = "survival function")

# tail(summary(km_fit)$surv, 1)    # returns the Kaplan-Meier estimate at {tau_i}
# tail(summary(km_fit)$lower, 1)   # lower pointwise estimates (alternatively, $upper)
# tail(summary(km_fit)$upper, 1)   # lower pointwise estimates (alternatively, $upper)


## Methode known fate models in RMark
# Create LD format for known fate model

it <- 20 # interval_time = time between observations
obs <- seq(from = 1, to = tdays - it, by = it)
ch <- vector(length = n_init)

for (j in 1:n_init) {
  mych <- ""
  for (i in obs) {

    L <- ifelse(mat[j, i] == 1, 1, 0)
    D <- ifelse((mat[j, i] == 1) & min(mat[j, i:(i + it)]) == 0, 1, 0)
    mych <- paste0(mych, L, D)

  }
  ch[j] <- mych
}

ch
mat[,341]

df_ch <- data.frame(ch = ch)

# RMark - Known fate model
# Process data
processed <- process.data(df_ch, model = "Known")

# Create default design data
ddl <- make.design.data(processed)

#  Define range of models for S
S.dot = list(formula = ~1)
S.time = list(formula = ~time)

# Create model list and run assortment of models
model.list = create.model.list("Known")
results <- mark.wrapper(model.list, data = processed, ddl = ddl,
                        invisible = FALSE,threads = 1)

results
results[[1]]$results$real
results[[2]]$results$real

S_kf <- (results[[1]]$results$real$estimate)^18
S_kf_lcl <- (results[[1]]$results$real$lcl)^18
S_kf_ucl <- (results[[1]]$results$real$ucl)^18

# Create density curve for simulation
n_sim <- 1000
S_sim <- vector(length = n_sim)
for (s in (1:n_sim)){
  for (i in 2:tdays) {
    mat[, i] <- mat[, i - 1] * rbinom(n = n_init, size = 1, prob = Sd)
  }
  nt <- colSums(mat)  # number of individuals surviving during the year
  S_sim[s] <- nt[tdays] / n_init # Survival after one year
}
hist(S_sim)
S_sim_CI <- quantile(S_sim, probs = c(0.025, 0.975))



df_out <- data.frame(method = c("sim", "Mayfiel", "KapMei", "KnownFate"),
                     S = c(Sj, S, tail(summary(km_fit)$surv, 1), S_kf),
                     ucl = c(S_sim_CI[1], S_CI[1],
                             tail(summary(km_fit)$upper, 1), S_kf_ucl),
                     lcl = c(S_sim_CI[2], S_CI[2],
                             tail(summary(km_fit)$lower, 1), S_kf_lcl))

ggplot(df_out, aes(x = method, y = S, ymin = lcl, ymax = ucl)) +
  geom_point() + geom_errorbar() + ylim(c(-0.15, 1))





########################################################################
#
# Simulatie 3: constante jaarlijkse overleving dagelijks, ringperiode
#
# - periode voor aanbrengen van zenders
# - uitval van zenders

# Parameters
Sj <- 0.15       # Jaarlijkse overleving
n_init <- 50   # Totaal aantal gezenderde individuen
max_start <- 1  # Periode (dagen na start project) aanbrengen zenders
tdays <- 365    # Totale duur van het opvolgen (dagen)
plossy <- 0.0001  # Jaarlijkse kans op verlies van transmitter

Sd <- Sj^(1/365)            # Dagelijkse overleving
plossd <- 1 - ((1 - plossy)^(1/365)) # Dagelijkse kans op behoud transmitter

# Simulatie aanbrengen van de zenders (uniforme verdeling over periode)
set.seed(13)
m_time <- round(runif(n = n_init, min = 1, max = max_start))

# Simulate survival
set.seed(51)
s_time <- m_time + rgeom(n = n_init, prob = 1 - Sd)
#s_time[s_time > tdays] <- tdays

# Simulate loss of transmitters
set.seed(99)
l_time <- m_time + rgeom(n = n_init, prob = plossd)
l_time[l_time > tdays] <- tdays

# Create dataset in format for KM analysis
event_time <- event_type <- vector(length = n_init)

for (j in 1:n_init) {
  if (s_time[j] <= l_time[j]) {
    event_time[j] <- s_time[j] - m_time[j]
    event_type[j] <- 1
  }else{
    event_time[j] <- l_time[j] - m_time[j]
    event_type[j] <- 0
  }
}

event_time
event_type

km <- Surv(event_time, event_type)
km

km_fit <- survfit(data = km, km ~ 1)
km_fit

summary(km_fit)

km_fit$time             # {t_i} (= all times)
summary(km_fit)$time    # {tau_i} (= observed times = uncensored times)
km_fit$n.risk           # {r_i} at {t_i}

tail(summary(km_fit)$surv, 1)
tail(summary(km_fit)$upper, 1)
tail(summary(km_fit)$lower, 1)

plot(km_fit, main = "Kaplan-Meier estimate (95% CI)",
     xlab = "time", ylab = "survival function")


## Methode known fate models in RMark
# Create LD format for known fate model

# Set the observation times
# Here we use a fixed interval. Later we can insert the actual
# observation dates
it <- 20 # interval time between observations (days)
obs <- c(seq(from = 1, to = tdays - it, by = it), tdays)

ch <- vector(length = n_init)
for (j in 1:n_init) {
  chL <- chD <- rep(0, length(obs))  # empty vectors
  if (s_time[j] < l_time[j]) {     # case 1: dead before losing transmitter or end of study
    chL[obs >= m_time[j]] <- 1                # observation with transmitter
    chD[match(TRUE, obs > s_time[j])] <-  1  # dead recovery
    if (!match(1, chD) == length(obs)) {
      chL[(match(1, chD) + 1):length(obs)] <- 0 # individual not seen after its dead
    }

  }else{                         # case 2: individual loses transmitter or end of study
    chL[obs > m_time[j]] <- 1   # starting observations with transmitter
    chL[obs > l_time[j]] <- 0   # observation stop when transmitter is lost
  }
  #cat(j, "\n", chL, "\n" , chD, "\n")
  ch[j] <- paste0(as.vector(rbind(chL, chD)), collapse = "")
}

df_ch <- data.frame(ch = ch)

# RMark - Known fate model
# Process data
processed <- process.data(df_ch, model = "Known")

# Create default design data
ddl <- make.design.data(processed)

#  Define range of models for S
S.dot = list(formula = ~1)
#S.time = list(formula = ~time)

# Create model list and run assortment of models
model.list = create.model.list("Known")
results <- mark.wrapper(model.list, data = processed, ddl = ddl,
                        invisible = FALSE,threads = 1)

results
results[[1]]$results$real
results[[2]]$results$real

S_kf <- (results[[1]]$results$real$estimate)^length(obs)
S_kf_lcl <- (results[[1]]$results$real$lcl)^length(obs)
S_kf_ucl <- (results[[1]]$results$real$ucl)^length(obs)

