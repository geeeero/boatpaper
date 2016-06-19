# Illustration prior-data conflict in Beta-Binomial model

source("pdc-betadens-setup.R")

# ---------------- original values from ijar paper -------
# one prior updated to posteriors with same variance
sgpr <- c(8, 0.75) # n0, y0
data1 <- c(12,16)  #  s,  n
data2 <- c( 0,16)
# set of priors
setpr <- BinomialLuckModel(n0=c(1,8), y0=c(0.7, 0.8))
setpos1 <- setpr
setpos2 <- setpr
data(setpos1) <- BinomialData(s=data1[1], n=data1[2])
data(setpos2) <- BinomialData(s=data2[1], n=data2[2])

# ---------------- make the graphs -------
source("pdc-betadens-figures.R")

# ---------------- values from ipmu paper -------
data1 <- c(4,8)  #  s,  n
data2 <- c(8,8)
# set of priors
setpr <- BinomialLuckModel(n0=c(3,8), y0=c(0.25, 0.75))
setpos1 <- setpr
setpos2 <- setpr
data(setpos1) <- BinomialData(s=data1[1], n=data1[2])
data(setpos2) <- BinomialData(s=data2[1], n=data2[2])

# ---------------- make the graphs -------
source("pdc-betadens-figures-ipmu.R")


#