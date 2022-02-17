#Functions

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

