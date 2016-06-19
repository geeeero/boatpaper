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

#