###########################################
# tests for functions in boatfunctions.R  #
###########################################

bsp1 <- list(xp = c(-2,6), a = 2, b = 1/4, yc = 0.5, data = list(tau = 0, n = 0))

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

par(mfrow=c(1,1))
boatplotter(bsp1)


