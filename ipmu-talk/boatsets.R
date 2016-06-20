# boatshape figures for ipmu2016 talk

#source("pdc-betadens.R") # for colour definitions etc.
source("../R/boatfunctions.R")

seqlen <- 201
setprmikl <- normaltomik(list(x=seq(n0(setpr)[1], n0(setpr)[2], length.out=seqlen),
                              y=rep(y0(setpr)[1], seqlen)))
setprmiku <- normaltomik(list(x=seq(n0(setpr)[2], n0(setpr)[1], length.out=seqlen),
                              y=rep(y0(setpr)[2], seqlen)))
setpos1mikl <- list(x=setprmikl$x + data1[2],
                    y=setprmikl$y + data1[1] - data1[2]/2) # 4,8
setpos1miku <- list(x=setprmiku$x + data1[2],
                    y=setprmiku$y + data1[1] - data1[2]/2) # 4,8
setpos2mikl <- list(x=setprmikl$x + data2[2],
                    y=setprmikl$y + data2[1] - data2[2]/2) # 8,8
setpos2miku <- list(x=setprmiku$x + data2[2],
                    y=setprmiku$y + data2[1] - data2[2]/2) # 8,8

pdf("figs/mikdomain.pdf", width=6, height=2)
par(mfrow=c(1,2), mar=c(3,3.5,.1,.1), cex.lab=1.1, cex.axis=.8, mgp=c(2,.7,0), tcl=-.3)
normaldomain(xlims=c(0,10), seqx=201)
polygon(c(n0(setpr)[1], n0(setpr)[1], n0(setpr)[2], n0(setpr)[2]),
        c(y0(setpr)[1], y0(setpr)[2], y0(setpr)[2], y0(setpr)[1]),
        col = rgb(0.000,0.675,0.510,0.5), border = tuegreen)
domainplotter(xlims=c(-2,8), seqx=201)
polygonxylist(setprmikl, setprmiku, col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
dev.off()

pdf("figs/rectinmik0.pdf", width=6, height=3)  
par(mfrow=c(1,2), mar=c(3,3.5,.1,.1), cex.lab=1.1, cex.axis=.8, mgp=c(2,.7,0), tcl=-.3)
domainplotter(xlims=c(-2,15), seqx=201)
polygonxylist(setprmikl, setprmiku, col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
domainplotter(xlims=c(-2,15), seqx=201)
polygonxylist(setprmikl, setprmiku, col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
dev.off()

pdf("figs/rectinmik1.pdf", width=6, height=3)  
par(mfrow=c(1,2), mar=c(3,3.5,.1,.1), cex.lab=1.1, cex.axis=.8, mgp=c(2,.7,0), tcl=-.3)
domainplotter(xlims=c(-2,15), seqx=201)
polygonxylist(setprmikl, setprmiku, col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
polygonxylist(setpos1mikl, setpos1miku, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
domainplotter(xlims=c(-2,15), seqx=201)
polygonxylist(setprmikl, setprmiku, col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
polygonxylist(setpos2mikl, setpos2miku, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
dev.off()

# first time boatshape

pdf("figs/boatshape-alone.pdf", width=6, height=3)  
par(mfrow=c(1,2), mar=c(3,3.5,.1,.1), cex.lab=1.1, cex.axis=.8, mgp=c(2,.7,0), tcl=-.3)
boat1 <- list(xp = c(1,6), a = 1.5, b = 0.9, yc = 0.5, data = list(tau = 8, n = 8))
boatplotter(boat1, prior=T, xlims=c(0,7), ylims=c(-3,3), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
normalplotter(boat1, prior=T, xlims=c(2,9), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
dev.off()

pdf("figs/boatshape-posterior-mik-ipmu0.pdf", width=6, height=3)
par(mfrow=c(1,2), mar=c(3,3.5,1.5,.1), cex.lab=1.1, cex.axis=.8, mgp=c(2,.7,0), tcl=-.3)
# prior & post 1
boat1 <- list(xp = c(1,6), a = 1.5, b = 0.9, yc = 0.5, data = list(tau = 4, n = 8))
boatplotter(boat1, xlims=c(-2,15), ylims=c(-3,8), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
mtext("strong agreement", side=3, line=0.5)
# prior & post 2
boat1 <- list(xp = c(1,6), a = 1.5, b = 0.9, yc = 0.5, data = list(tau = 8, n = 8))
boatplotter(boat1, xlims=c(-2,15), ylims=c(-3,8), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
mtext("prior-data conflict", side=3, line=0.5)
dev.off()

pdf("figs/boatshape-posterior-mik-ipmu1.pdf", width=6, height=3)
par(mfrow=c(1,2), mar=c(3,3.5,1.5,.1), cex.lab=1.1, cex.axis=.8, mgp=c(2,.7,0), tcl=-.3)
# prior & post 1
boat1 <- list(xp = c(1,6), a = 1.5, b = 0.9, yc = 0.5, data = list(tau = 4, n = 8))
boatplotter(boat1, xlims=c(-2,15), ylims=c(-3,8), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
boatplotter(boat1, prior=F, add=T, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
mtext("strong agreement", side=3, line=0.5)
# prior & post 2
boat1 <- list(xp = c(1,6), a = 1.5, b = 0.9, yc = 0.5, data = list(tau = 8, n = 8))
boatplotter(boat1, xlims=c(-2,15), ylims=c(-3,8), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
boatplotter(boat1, prior=F, add=T, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
mtext("prior-data conflict", side=3, line=0.5)
dev.off()

pdf("figs/boatshape-posterior-nor-ipmu1.pdf", width=6, height=3)
par(mfrow=c(1,2), mar=c(3,3.5,1.5,.1), cex.lab=1.1, cex.axis=.8, mgp=c(2,.7,0), tcl=-.3)
# prior & post 1
boat1 <- list(xp = c(1,6), a = 1.5, b = 0.9, yc = 0.5, data = list(tau = 4, n = 8))
normalplotter(boat1, xlims=c(0,17), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
normalplotter(boat1, prior=F, add=T, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
mtext("strong agreement", side=3, line=0.5)
# prior & post 2
boat1 <- list(xp = c(1,6), a = 1.5, b = 0.9, yc = 0.5, data = list(tau = 8, n = 8))
normalplotter(boat1, xlims=c(0,17), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
normalplotter(boat1, prior=F, add=T, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
mtext("prior-data conflict", side=3, line=0.5)
dev.off()

pdf("figs/boatshape-posterior-hpd-ipmu1.pdf", width=6, height=3)
par(mfrow=c(1,2), mar=c(3,3.5,1.5,.1), cex.lab=1.1, cex.axis=.8, mgp=c(2,.7,0), tcl=-.3)
# prior & post 1
boat1 <- list(xp = c(1,6), a = 1.5, b = 0.9, yc = 0.5, data = list(tau = 4, n = 8))
normalplotter(boat1, xlims=c(0,17), addluck=T, usymmcred=0.5, usymmcredoffset=0.2, col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
normalplotter(boat1, prior=F, add=T, addluck=T, usymmcred=0.5, usymmcredoffset=0.2, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
mtext("strong agreement", side=3, line=0.5)
# prior & post 2
boat1 <- list(xp = c(1,6), a = 1.5, b = 0.9, yc = 0.5, data = list(tau = 8, n = 8))
normalplotter(boat1, xlims=c(0,17), addluck=T, usymmcred=0.5, usymmcredoffset=0.2, col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
normalplotter(boat1, prior=F, add=T, addluck=T, usymmcred=0.5, usymmcredoffset=0.2, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
mtext("prior-data conflict", side=3, line=0.5)
dev.off()

pdf("figs/boatshape-posterior-mik-ipmu2.pdf", width=6, height=3)
par(mfrow=c(1,2), mar=c(3,3.5,1.5,.1), cex.lab=1.1, cex.axis=.8, mgp=c(2,.7,0), tcl=-.3)
# prior & post 1
boat1 <- list(xp = c(1,6), a = 1.2, b = 0.9, yc = 0.75, data = list(tau = 6, n = 8))
boatplotter(boat1, xlims=c(-2,15), ylims=c(-3,8), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
mtext("strong agreement", side=3, line=0.5)
# prior & post 2
boat1 <- list(xp = c(1,6), a = 1.2, b = 0.9, yc = 0.75, data = list(tau = 2, n = 8))
boatplotter(boat1, xlims=c(-2,15), ylims=c(-3,8), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
mtext("prior-data conflict", side=3, line=0.5)
dev.off()

pdf("figs/boatshape-posterior-mik-ipmu3.pdf", width=6, height=3)
par(mfrow=c(1,2), mar=c(3,3.5,1.5,.1), cex.lab=1.1, cex.axis=.8, mgp=c(2,.7,0), tcl=-.3)
# prior & post 1
boat1 <- list(xp = c(1,6), a = 1.2, b = 0.9, yc = 0.75, data = list(tau = 6, n = 8))
boatplotter(boat1, xlims=c(-2,15), ylims=c(-3,8), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
boatplotter(boat1, prior=F, add=T, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
mtext("strong agreement", side=3, line=0.5)
# prior & post 2
boat1 <- list(xp = c(1,6), a = 1.2, b = 0.9, yc = 0.75, data = list(tau = 2, n = 8))
boatplotter(boat1, xlims=c(-2,15), ylims=c(-3,8), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
boatplotter(boat1, prior=F, add=T, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
mtext("prior-data conflict", side=3, line=0.5)
dev.off()

pdf("figs/boatshape-posterior-nor-ipmu3.pdf", width=6, height=3)
par(mfrow=c(1,2), mar=c(3,3.5,1.5,.1), cex.lab=1.1, cex.axis=.8, mgp=c(2,.7,0), tcl=-.3)
# prior & post 1
boat1 <- list(xp = c(1,6), a = 1.2, b = 0.9, yc = 0.75, data = list(tau = 6, n = 8))
normalplotter(boat1, xlims=c(0,17), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
normalplotter(boat1, prior=F, add=T, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
mtext("strong agreement", side=3, line=0.5)
# prior & post 2
boat1 <- list(xp = c(1,6), a = 1.2, b = 0.9, yc = 0.75, data = list(tau = 2, n = 8))
normalplotter(boat1, xlims=c(0,17), col=tuegreen, fillcol=rgb(0.000,0.675,0.510,0.5))
normalplotter(boat1, prior=F, add=T, col=tuedarkblue, fillcol=rgb(0.063,0.063,0.451,0.4))
mtext("prior-data conflict", side=3, line=0.5)
dev.off()

#