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
  set.seed(13)
  m_time <- round(runif(n = n_init, min = 1, max = max_start))

  # Simulate survival
  set.seed(51)
  s_time <- m_time + rgeom(n = n_init, prob = 1 - Sd)  # dead
  #s_time[s_time > tot_days] <- NA  # survives study

  # Simulate loss of transmitters / end of study
  set.seed(99)
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

  ch <- vector(length = n_init)
  for (j in 1:n_init) {
    chL <- chD <- rep(0, length(obs))  # empty vectors
    if (s_time[j] < l_time[j]) {     # case 1: dead before losing transmitter or end of study
      chL[obs >= m_time[j]] <- 1               # observations with transmitter
      chD[match(TRUE, obs > s_time[j])] <-  1  # dead recovery
      if (!match(1, chD) == length(obs)) {
        chL[(match(1, chD) + 1):length(obs)] <- 0 # individual cannot be seen after its dead
      }

    }else{                        # case 2: individual loses transmitter or end of study
      chL[obs > m_time[j]] <- 1   # starting observations with transmitter
      chL[obs > l_time[j]] <- 0   # observations stop when transmitter is lost
    }
    #cat(j, "\n", chL, "\n" , chD, "\n")
    ch[j] <- paste0(as.vector(rbind(chL, chD)), collapse = "")
  }

  return(data.frame(ch = ch))
}