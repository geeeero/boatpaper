library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(luck)
source("../../lund_1512/lund-1512/course/04-01_BinomialData.R")
source("../../lund_1512/lund-1512/course/04-02_Binomial.R")

tuered <- rgb(0.839,0.000,0.290)
tueblue <- rgb(0.000,0.400,0.800)
tueyellow <- rgb(1.000,0.867,0.000)
tuegreen <- rgb(0.000,0.675,0.510)
tuewarmred <- rgb(0.969,0.192,0.192)
tueorange <- rgb(1.000,0.604,0.000)
tuedarkblue <- rgb(0.063,0.063,0.451)

bottomlegend <- theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.title = element_blank())
rightlegend <- theme(legend.title = element_blank())
nolegend <- guides(fill="none", color="none")

pdcscale <- scale_color_manual(values=c(tuegreen, tuedarkblue), name=element_blank())
pdcscale2 <- scale_fill_manual(values=c(tuegreen, tuedarkblue), name=element_blank())

fmt_dcimals <- function(decimals=0){
  function(x) format(x,nsmall = decimals,scientific = FALSE)
}

updateLuckY <- function (n0, y0, tau, n){ (n0*y0+tau)/(n0+n) }
updateLuckN <- function (n0, n){ n0+n }

nyupdate <- function (pr, data){
  nn <- updateLuckN(pr[1], data[2])
  yn <- updateLuckY(pr[1], pr[2], data[1], data[2])
  c(nn,yn)
}

luck4cny <- function(luck, posterior=FALSE){
  c1 <- c(n0(luck)[1], y0(luck)[1])
  c2 <- c(n0(luck)[1], y0(luck)[2])
  c3 <- c(n0(luck)[2], y0(luck)[1])
  c4 <- c(n0(luck)[2], y0(luck)[2])
  if(posterior){
    c1 <- nyupdate(c1, c(tau(data(luck)), n(data(luck))))
    c2 <- nyupdate(c2, c(tau(data(luck)), n(data(luck))))
    c3 <- nyupdate(c3, c(tau(data(luck)), n(data(luck))))
    c4 <- nyupdate(c4, c(tau(data(luck)), n(data(luck))))
  }
  list(c1=c1, c2=c2, c3=c3, c4=c4)
}

dbetany <- function(x, ny, ...){
  dbeta(x, shape1=ny[1]*ny[2], shape2=ny[1]*(1-ny[2]), ...)
}

pbetany <- function(x, ny, ...){
  pbeta(x, shape1=ny[1]*ny[2], shape2=ny[1]*(1-ny[2]), ...)
}
