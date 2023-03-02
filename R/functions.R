## Functions

# Simulation telemetry data

#' Sy         survival probability
#' n_init     total number of transmitters
#' max_start  maximum days between start of study and last trasmitter added
#' p_loss_y  probability loss of transmitter
#' tot_days total number of days study

sim_tel <- function(Sy = 0.8, n_init = 66, max_start = 50, p_loss_y = 0.1,
                    tot_days = 365){

  # calculate daily probability from yearly probability
  Sd <- Sy^(1/365)            # daily survival probability
  plossd <- 1 - ((1 - p_loss_y)^(1/365)) # daily probability to hold the transmitter

  # Simulate application of transmitters (uniform distribution)
  #set.seed(13)
  m_time <- round(runif(n = n_init, min = 1, max = max_start))

  # Simulate survival
  #set.seed(51)
  s_time <- m_time + rgeom(n = n_init, prob = 1 - Sd)  # dead
  #s_time[s_time > tot_days] <- NA  # survives study

  # Simulate loss of transmitters / end of study
  #set.seed(99)
  l_time <- m_time + rgeom(n = n_init, prob = plossd)   # loss
  l_time[l_time > tot_days] <- tot_days    # end of study

  return(list(m_time = m_time, s_time = s_time, l_time= l_time))
}

# Convert sim data -> survival analysis
#
# Creates two vectors for analysis in survival package
#  - time of events
#  - type of events   (1 = dead, 0 = loss of transmitter)

convert_to_surv <- function(m_time, s_time, l_time) {

  event_time <- event_type <- vector(length = length(m_time))

  for (j in 1:length(m_time)) {
    if (s_time[j] <= l_time[j]) {
      event_time[j] <- s_time[j] - m_time[j]
      event_type[j] <- 1
    }else{
      event_time[j] <- l_time[j] - m_time[j]
      event_type[j] <- 0
    }
  }
  event_type <- event_type[order(event_time)]
  event_time <- event_time[order(event_time)] # Sorteren is niet strikt noodzakelijk

  return(list(event_time = event_time, event_type = event_type))
}


## Convert to RMark
#
# Function to convert survival data to RMark life history format (LDLDLD).
# Set the observation times. Here we use a fixed interval.
# Later, the actual observation dates can be inserted.

convert_to_RMark <- function(obs, m_time, s_time, l_time){

  lastobs <- max(obs)
  n_init <- length(m_time)
  ch <- vector(length = n_init)
  for (j in 1:n_init) {
    chL <- chD <- rep(0, length(obs))  # empty vectors
    if (s_time[j] < l_time[j]) {     # case 1: dead before losing transmitter or end of study
      chL[obs >= m_time[j]] <- 1      # start observations with transmitter
      if (lastobs > s_time[j]) {
        chD[match(TRUE, obs > s_time[j])] <- 1 # 1st obs. is dead recovery
        if (!match(1, chD) == length(obs)) {
          chL[(match(1, chD) + 1):length(obs)] <- 0 # no obs. after dead recovery
        }
      }

    }else{                        # case 2: individual loses transmitter or end of study
      chL[obs >= m_time[j]] <- 1   # starting observations with transmitter
      chL[obs > l_time[j]] <- 0   # observations stop when transmitter is lost
    }
    #cat(j, "\n", chL, "\n" , chD, "\n")
    ch[j] <- paste0(as.vector(rbind(chL, chD)), collapse = "")
  }

  return(data.frame(ch = ch))
}


### Beta distribution specification function ###
betaPr <- function(mu, sd, plot = FALSE)
{
  al <- mu*(mu*(1 - mu)/sd^2 - 1)
  be <- (1 - mu) * al / mu
  #be <- (1 - mu)*(mu*(1 - mu)/sd^2 - 1)
  x <- seq(0,1, 0.01)
  if (plot == TRUE) plot(x, dbeta(x, al, be), type = "l",
                         main = paste("Beta: mean ", mu, " and sd ", sd))
  print(paste("The two parameters of the Beta distribution are alpha =",al," and beta =", be))
  return(c(al, be))
}


### Gamma distribution specification function ###
gammaPr <- function(mu, sd)
{
  shape <- mu^2/sd^2
  rate <- mu/sd^2
  x <- seq(max(0, mu - 4*sd), mu + 4*sd, length.out = 100)
  plot(x, dgamma(x, shape = shape, rate = rate), main = paste("Prior distribution with mean ",mu," and sd ", sd), type = "l")
  print(paste("The two parameters of the Gamma distribution are shape =",shape," and rate =",rate))
  return(c(shape,rate))
}


#### Build population matrix

patrijs_mat <- function(vr = list(
  b = 1,        # kans op broeden
  n1 = 0.46,   # nestsucces eerste legsel
  u1 = 12.3,   # aantal uitgekomen eieren per eerste legsel
  Sk1 = 0.4,   # overleving van kuikens uit een eerste legsel
  h = 0.78,    # kans op herleg
  nh = 0.4,    # nestsucces herlegsel
  uh = 9,      # aantal uitgekomen eieren per herlegsel
  Skh = Sk1,   # overleving van kuikens uit een herlegsel (=1ste legsel)
  Sai1 = 0.78, # overleving hen tijdens incubatie 1e legsel
  Saz1 = 0.8,  # overleving hen zomer zonder herlegsel
  Saih = 1,    # overleving hen tijdens incubatie herlegsel
  Sazh = 0.9,  # overleving hen met herlegsel
  Saw = 0.68,  # overleving adulten tijdens winter
  Sjw = 0.68))   # overleving juvenielen tijdens winter (= Saw)
  {

  ## -> beter als een expressie in de markdown code zetten?

  exPjr <- expression((Sai1 * ((b * n1 * u1 * Sk1) +
                               Saih * (1 - n1) * h * nh * uh * Skh) * Sjw) * 0.5)
  exSab <- expression(Sai1 * ((1 - h + n1 * h) * Saz1 + (1 - n1) * h * Saih * Sazh))

  Pjr <- eval(expr = exPjr, envir = vr)
  Sab <- eval(expr = exSab, envir = vr)

  Saw <- vr[["Saw"]]

  mat <- matrix(c(Pjr,        Pjr,
                  Sab * Saw,  Sab * Saw),
                nrow = 2, ncol = , byrow = TRUE)
  return(mat)
}

# Sensitivity of vital rates calculated by small pertubations
# of each vital rate
# In contrast to popbio::vitalsens which calculates sensitivity using partial
# derivatives
# Part of this code is copied from popbio::vitalsens

vitalsens_pert <- function(elements, vitalrates) {

  pert <- 0.001
  if (is.vector(vitalrates)) {
    vitalrates <- as.list(vitalrates)
  }
  if (!is.list(vitalrates)) {
    stop("Vital rates should be a vector or list")
  }
  if (class(elements)[1] != "expression") {
    stop("Matrix elements should be an expression")
  }
  n <- sqrt(length(elements))
  if (n%%1 != 0) {
    stop(paste("Length of element expression is", length(elements),
               "- Expecting power of 2 like 4,9,16 to form a square matrix"))
  }
  vrs <- try(sapply(elements, eval, vitalrates, NULL), silent = TRUE)
  if (inherits(vrs, "try-error")) {
    vrs <- sub("Error in eval\\(expr, envir, enclos\\) :",
               "", vrs[1])
    stop(paste("Cannot evaluate element expression using given vital rates:",
               vrs))
  }

  res <- data.frame(estimate = unlist(vitalrates), sensitivity = 0,
                    elasticity = 0)

  A <- matrix(vrs, nrow = n, byrow = TRUE)
  lambdaA <- lambda(A)

  for (i in 1:length(vitalrates)) {

    vr_pert <- vitalrates
    vr_pert[[i]] <- vitalrates[[i]] * (1 + pert)

    Apert <- matrix(sapply(elements, eval,vr_pert, NULL), nrow = n, byrow = TRUE)

    lambdaApert <- lambda(Apert)
    elas <- (lambdaApert - lambdaA) / lambdaA * (1 / pert)
    sens <- (lambdaApert - lambdaA) / (vr_pert[[i]] - vitalrates[[i]])
    res[i, 3] <- elas
    res[i, 2] <- sens
  }
  return(res)
}


  # eig <- eigen.analysis(A)
  # deriv.funcs <- sapply(elements, deriv, namevec = names(vitalrates),
  #                       function.arg = TRUE)
  # devs <- lapply(deriv.funcs, function(x) do.call(x, vitalrates))
  # for (i in 1:length(vitalrates)) {
  #   derivs <- matrix(as.numeric(lapply(devs, function(x) attr(x,
  #                                                             "gradient")[i])), nrow = n, byrow = TRUE)
  #   res[i, 2] <- sum(derivs * eig$sensitivities)
  #   res[i, 3] <- vitalrates[[i]]/eig$lambda1 * sum(derivs *
  #                                                    eig$sensitivities)
  # }
  # res
