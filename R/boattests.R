###########################################
# tests for functions in boatfunctions.R  #
###########################################

# rotatefu
test1 <- list(x = seq(0, 10, by=0.1), y = rep(0,101))
test1rot1 <- rotatefu(test1, yc = 0.6)
test1rot2 <- rotatefu(test1, yc = 0.8, rotcntr=c(0,0))
domainplotter(xlims=c(-2,10))
lines(test1, lwd=1.5)
lines(test1rot1, lwd=1.5, col=2)
lines(test1rot2, lwd=1.5, col=3)

# updatefu
test1up <- updatefu(test1, data=list(tau=3, n=5))
test1rot1up <- updatefu(test1rot1, data=list(tau=3, n=5))
test1rot2up <- updatefu(test1rot2, data=list(tau=3, n=5))
lines(test1up, lty=2)
lines(test1rot1up, lty=2, col=2)
lines(test1rot2up, lty=2, col=3)

# boatcont
bsp1 <- list(xp = c(-2,6), a = 2, b = 1/4, yc = 0.5, data = list(tau = 0, n = 0))
bsp2 <- list(xp = c(-2,6), a = 2, b = 1/4, yc = 0.6, data = list(tau = 3, n = 5))
domainplotter(xlims=c(-2,10))
xseq <-seq(-2,6,by=0.1)
lines(xseq, boatcont(seq(-2,6,by=0.1), bsp1))
#lines(xseq, boatcont(seq(-2,6,by=0.1), bsp2), col=2)

# boatfu
lines(boatfu(boatobj=bsp1), col=4)
lines(boatfu(boatobj=bsp1, wh=-1), col=4)
lines(boatfu(boatobj=bsp2))
lines(boatfu(boatobj=bsp2, wh=-1))
lines(boatfu(boatobj=bsp2, prior=F), lty=2)
lines(boatfu(boatobj=bsp2, prior=F, wh=-1), lty=2)

# boatplotter
boatplotter(bsp1)
boatplotter(bsp1, xlims=c(-2,10), ylims=c(-6,6))
boatplotter(bsp2, seqx=5)
boatplotter(bsp2, prior=F)

# miktonormal
mtn1 <- miktonormal(test1)
mtn2 <- miktonormal(test1rot1)
mtn3 <- miktonormal(test1rot2)
plot(mtn1, type="l")
lines(mtn2, col=2)
lines(mtn3, col=4)

# normaltomik
par(mfrow=c(1,2))
domainplotter(xlims=c(-2,10))
lines(test1, lwd=1.5)
lines(test1rot1, lwd=1.5, col=2)
lines(test1rot2, lwd=1.5, col=4)
domainplotter(xlims=c(-2,10))
lines(normaltomik(mtn1), lwd=1.5)
lines(normaltomik(mtn2), col=2)
lines(normaltomik(mtn3), col=4)
par(mfrow=c(1,1))


# normalplotter
bsp3 <- list(xp = c(-1,7), a = 1, b = 1/2, yc = 0.6, data = list(tau = 6, n = 10))
par(mfrow=c(1,2))
boatplotter(bsp3, prior = F, col = 2)
boatplotter(bsp3, add = T)
normalplotter(bsp3, prior = F, col = 2)
normalplotter(bsp3, add = T)
par(mfrow=c(1,1))

