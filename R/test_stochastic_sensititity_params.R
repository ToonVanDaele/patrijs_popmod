## Tryout stochastic sensitivity (and elasticity)

library(tidyverse)
library(popbio)
source("R/functions.R")

b <- 1       # kans op broeden
n1 <- 0.46   # nestsucces eerste legsel
u1 <- 12.3   # aantal uitgekomen eieren per eerste legsel
Sk1 <- 0.4   # overleving van kuikens uit een eerste legsel
h <- 0.78    # kans op herleg
nh <- 0.4    # nestsucces herlegsel
uh <- 9      # aantal uitgekomen eieren per herlegsel
Skh <- Sk1   # overleving van kuikens uit een herlegsel (=1ste legsel)
Sai1 <- 0.78 # overleving hen tijdens incubatie 1e legsel
Saz1 <- 0.8  # overleving hen zomer zonder herlegsel
Saih <- 1    # overleving hen tijdens incubatie herlegsel
Sazh <- 0.9  # overleving hen met herlegsel
Saw <- 0.68  # overleving adulten tijdens winter
Sjw <- Saw   # overleving juvenielen tijdens winter gelijk gesteld aan adulten

name_par <- c("n1", "u1", "Sk1", "h", "nh", "uh", "Skh", "Sai1", "Saz1",
              "Saih", "Sazh", "Saw", "Sjw")
par <- vector(length = length(name_par))

par[1] <- n1    # nestsucces eerste legsel
par[2] <- u1    # aantal uitgekomen eieren per eerste legsel
par[3] <- Sk1   # overleving van kuikens uit een eerste legsel
par[4] <- h     # kans op herleg
par[5] <- nh    # nestsucces herlegsel
par[6] <- uh    # aantal uitgekomen eieren per herlegsel
par[7] <- Skh   # overleving van kuikens uit een herlegsel (=1ste legsel)
par[8] <- Sai1  # overleving hen tijdens incubatie 1e legsel
par[9] <- Saz1  # overleving hen zomer zonder herlegsel
par[10] <- Saih # overleving hen tijdens incubatie herlegsel
par[11] <- Sazh # overleving hen met herlegsel
par[12] <- Saw  # overleving adulten tijdens winter
par[13] <- Sjw  # overleving juvenielen tijdens winter gelijk gesteld aan adulten

names(par) <- name_par

mat <- popmat(b = b, n1 = par["n1"], u1 = par["u1"], Sk1 = par["Sk1"], h = par["h"],
       nh = par["nh"], uh = par["uh"], Skh = par["Skh"], Sai1 = par["Sai1"],
       Saz1 = par["Saz1"], Saih = par["Saih"], Sazh = par["Sazh"],
       Saw = par["Saw"], Sjw = par["Sjw"])

lambda1 <- lambda(mat)


# loop voor iedere parameter aanpassing *1.0001 -> elasticiteit berekenen

dfpar <- data.frame(param = name_par, value = par)
dfpar$lambda2 <- dfpar$elas <- NA

for (i in 1:nrow(dfpar)) {
  print(dfpar[i, "param"])
  dpar <- par
  dpar[dfpar[i,"param"]] <- dpar[dfpar[i,"param"]]*1.001
  mat <- popmat(b = b, n1 = dpar["n1"], u1 = dpar["u1"], Sk1 = dpar["Sk1"], h = dpar["h"],
         nh = dpar["nh"], uh = dpar["uh"], Skh = dpar["Skh"], Sai1 = dpar["Sai1"],
         Saz1 = dpar["Saz1"], Saih = dpar["Saih"], Sazh = dpar["Sazh"],
         Saw = dpar["Saw"], Sjw = dpar["Sjw"])
  lambda2 <- lambda(mat)
  dfpar[i, "lambda2"] <- lambda2
  dfpar[i, "elas"] <- (lambda2 - lambda1) / lambda1 * 1000
  }

dfpar


## en nu stochastische sensitiviteit per parameter







matINBO <- matrix(c(Pjr,        Pjr,
                    Sab * Saw,  Sab * Saw),
                  nrow = 2, ncol = , byrow = TRUE)

lambda(matINBO)

a1 <- eigen.analysis(matINBO)
a1$sensitivities
a1$elasticities

d <- 1.00001

matINBO2 <- matrix(c(Pjr * d,        Pjr,
                    Sab * Saw,  Sab * Saw),
                  nrow = 2, ncol = , byrow = TRUE)

a2 <- eigen.analysis(matINBO2)
a2$lambda1

((a2$lambda1 - a1$lambda1) / a1$lambda1) * 100000



