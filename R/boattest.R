###########################################
# Code for graphs                         #
###########################################

setwd("/home/gero/work/boatpaper/R")
source("boatfunctions.R")
#install.packages("luck", repos="http://R-Forge.R-project.org")
library("luck")

setEPS()

# display the domain \Eta
postscript("boatshape-domain.ps", width=5, height=5)
domainplotter(xlims=c(-2,8), ylims=c(-5,5))
dev.off()

postscript("boatshape-vertical.ps", width=10.5, height=5)
par(mfrow=c(1,2))
domainplotter(xlims=c(-2,8), ylims=c(-5,5))
lines(c(1,1),c(-0.6,0.6),lwd=2)
lines(c(4,4),c(-0.6,0.6),lwd=2)
lines(c(7,7),c(-0.6,0.6),lwd=2)
domainplotter(xlims=c(-2,8), ylims=c(-5,5))
lines(c(1,1),c(-0.6,0.6),lwd=2)
lines(c(4,4),c( 0.6,1.8),lwd=2)
lines(c(7,7),c( 1.8,3  ),lwd=2)
par(mfrow=c(1,1))
dev.off()

boat1 <- list(xp = c( 1,6), a = 2, b = 0.7, yc = 0.5, data = list(tau = 5, n = 10))
boat1 <- list(xp = c(0,14), a = 2, b = 1/4, yc = 0.6, data = list(tau = 2.5, n = 5))
boat1 <- list(xp = c(1,6), a = 2, b = 0.8, yc = 0.5, data = list(tau = 4, n = 8))
boatplotter(boat1)
boatplotter(boat1, prior=F)
boatplotter(boat1, add=T)
normalplotter(boat1, prior=F)

postscript("boatshape-prior.ps", width=10.5, height=5)
par(mfrow=c(1,2))
boat1 <- list(xp = c(1,6), a = 2, b = 0.8, yc = 0.5, data = list(tau = 4, n = 8))
boatplotter(boat1, xlims=c(-2,8))
par(mar=c(5,4.5,4,2)+0.1)
normalplotter(boat1, xlims=c(-2,8))
par(mfrow=c(1,1))
dev.off()


# in Mik's world
postscript("boatshape-posterior-mik.ps", width=10, height=6)
boat1 <- list(xp = c(1,6), a = 2, b = 0.8, yc = 0.5, data = list(tau = 8, n = 8))
boatplotter(boat1, prior=F, xlims=c(0,23), ylims=c(-5,10))
boatplotter(boat1, add=T)
boat1$data <- list(tau = 4, n = 8)
boatplotter(boat1, prior=F, add=T)
boat1$data <- list(tau = 8, n = 16)
boatplotter(boat1, prior=F, add=T)
boat1$data <- list(tau = 16, n = 16)
boatplotter(boat1, prior=F, add=T)
dev.off()

# in normal world
postscript("boatshape-posterior-normal.ps", width=10.5, height=5)
boat1 <- list(xp = c(1,6), a = 2, b = 0.8, yc = 0.5, data = list(tau = 8, n = 8))
par(mar=c(5,4.5,4,2)+0.1)
normalplotter(boat1, prior=F, xlims=c(0,23))
normalplotter(boat1, add=T)
boat1$data <- list(tau = 4, n = 8)
normalplotter(boat1, prior=F, add=T)
boat1$data <- list(tau = 8, n = 16)
normalplotter(boat1, prior=F, add=T)
boat1$data <- list(tau = 16, n = 16)
normalplotter(boat1, prior=F, add=T)
dev.off()

# plot for strong prior-data agreement reaction
expoprior     <- function(eta, etal, b, n = NULL) exp(b*(eta-etal))
expoposterior <- function(eta, etal, b, n)        exp(b*(eta-etal-n))
leftside      <- function(eta, b)                 1 + b*(eta + 2)

eta0finder <- function(expfu, linfu, etal, b, n, rootinterval = c(-2, 100)){
  rootfu <- function(eta){
    expres <- do.call(expfu, list(eta=eta, etal=etal, b=b, n=n)) 
    linres <- do.call(linfu, list(eta=eta, b=b))
    expres - linres
  }
  res <- uniroot(rootfu, rootinterval)
  list(x=res$root, y=do.call(linfu, list(eta=res$root, b=b)))  
}

etavec <- seq(0,15,by=0.01)
etal <- 2
b <- 0.25
n <- 5

par(mar=c(5,4,1,1)+0.1)
#pdf("prior-vs-posterior-eta0u.pdf", width=8, height=6)
postscript("prior-vs-posterior-eta0u.eps", width=8, height=5)
plot(etavec, expoprior(etavec,etal,b), ylim=c(-1,8), type="l",
     yaxp=c(0,8,8), 
     xlab=expression(eta[0]), ylab="tangent equation sides")
lines(etavec, expoposterior(etavec,etal,b,n))
lines(etavec, leftside(etavec,b))
#abline(1+2*b,b)
#abline(v=c(etal,etal+n))
abline(h=0)
eta0u0 <- eta0finder(expoprior, leftside, etal, b, n)
eta0un <- eta0finder(expoposterior, leftside, etal, b, n)
eta0u0pn <- eta0finder(expoposterior, function(eta, b) eta0u0$y, etal, b, n)
points(eta0u0)
points(eta0un)
points(eta0u0pn)
lines(rep(eta0u0$x,2), c(eta0u0$y,-0.1), lty=2)
lines(rep(eta0un$x,2), c(eta0un$y,-0.1), lty=2)
lines( c(eta0u0$x, rep(eta0u0pn$x, 2)), c(rep(eta0u0$y, 2), -0.1), lty=2)
text(x=c(eta0u0$x, eta0un$x, eta0u0pn$x), y=rep(-0.1,3),
     labels=c(expression(eta[0]^u(0)),
              expression(eta[0]^u(n)),
              expression(eta[0]^u(0) + n)), pos=1)
dev.off()
par(mar=c(5,4,4,2)+0.1)


# predictive probability plot (PPP)
# using example from fig 4 ("boatshape-posterior-mik.ps") with n=8

pppn <- 8
pppboat1 <- list(xp = c(1,6), a = 2, b = 0.8, yc = 0.5, data = list(tau = 0, n = pppn))
svec <- seq(0,8, by=0.5)



bsp1 <- list(xp = c( 1,6), a = 2, b = 1/4, yc = 0.6, data = list(tau = 0, n = 0))
bsp1 <- list(xp = c(-1,7), a = 1, b = 1/2, yc = 0.5, data = list(tau = 6, n = 10))

boatplotter(bsp1)
boatplotter(bsp1, prior=F)
boatplotter(bsp1, add=T)

boatplotter(bsp1, prior = F, add=T, col = 2)

normalplotter(bsp1)
normalplotter(bsp1, prior=F)
normalplotter(bsp1, add=T)


# Beispiel, bei dem priori und posteriori touchpoints in der Mitte der Kontur sind (nicht an Enden)
bsp1 <- list(xp = c(0,14), a = 2, b = 1/4, yc = 0.6, data = list(tau = 2.5, n = 5))

#par(mar=c(4,5,0,0))
pdf("boatshape-talk130306.pdf", width=8, height=4)
talk <- list(xp = c(1,6), a = 2, b = 1/2, yc = 0.5, data = list(tau = 8, n = 8))
normalplotter(talk, prior=F, xlims=c(0,23), xlabs="", ylabs="")
normalplotter(talk, add=T)
talk$data <- list(tau = 4, n = 8)
normalplotter(talk, prior=F, add=T)
talk$data <- list(tau = 8, n = 16)
normalplotter(talk, prior=F, add=T)
talk$data <- list(tau = 16, n = 16)
normalplotter(talk, prior=F, add=T)
mtext(text = bquote(n^(0)), 1, line = 2, adj = 0.5, col="blue") 
mtext(text = bquote(y^(0)), 2, line = 2, adj = 0.5, col="red")
dev.off()

#