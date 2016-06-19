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
  
domainplotter(xlims=c(-2,15), seqx=201)
polygonxylist(setprmikl, setprmiku)
polygonxylist(setpos1mikl, setpos1miku)

domainplotter(xlims=c(-2,15), seqx=201)
polygonxylist(setprmikl, setprmiku)
polygonxylist(setpos2mikl, setpos2miku)

#