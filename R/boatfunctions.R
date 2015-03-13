###########################################
# function for boat shapes in Mik's world #
###########################################

# boat object defined like this:
bsp1 <- list(xp = c(-2,6), a = 2, b = 1/4, yc = 0.5, data = list(tau = 0, n = 0))
# xp: start and end of set on eta_0 axis (before rotating)
# a:  width of shape for eta_0 upper -> infty
# b:  bulkyness of shape
# yc: central ray of shape, 0 < yc < 1
# data: contains tau = number of successes, n = numberof trials

# makes the rotation in "Mik's World" for a list(x = *, y = *) object from
# 0.5-centered things to things centered around yc
rotatefu <- function(xylist, yc, rotcntr = c(-2,0)){
  atz <- atan(yc - 0.5)
  x <- cos(atz)*(xylist$x - rotcntr[1]) - sin(atz)*(xylist$y - rotcntr[2]) + rotcntr[1]
  y <- sin(atz)*(xylist$x - rotcntr[1]) + cos(atz)*(xylist$y - rotcntr[2]) + rotcntr[2]
  return(list(x = x, y = y))
}

# makes the update step in "Mik's World" for a list(x = *, y = *) object and a
# data object list(tau = *, n = *) where tau is the number of successes in n trials
updatefu <- function(xylist, data){
  x <- xylist$x + data$n
  y <- xylist$y + 1/2*(data$tau - (data$n-data$tau))
  return(list(x = x, y = y))
}

# gives the ray (x,y) coordinates for x (vector) and the yc-value for the ray
rayfu <- function(x, ray){
  y <- (1 + 0.5*x)*2*(ray - 0.5)
  return(list(x = x, y = y))
}

# returns upper contour of a boat set with yc = 0.5
# lower contour = -1*upper contour
boatcont <- function(x, boatobj){
  boatobj$a*(1 - exp(-boatobj$b*(x-boatobj$xp[1])))
}

# returns lower [wh=-1] or upper [wh=1, default] x vector and contour y(x) of the boat 
# shape for a boatobj as given in example above
boatfu <- function(x = NULL, boatobj, wh = 1, xlen = 100, fw = TRUE, prior = TRUE){
  # create x vector if none given
  if(is.null(x)){
    if (fw) xvec <- seq(boatobj$xp[1], boatobj$xp[2], length = xlen)
    else    xvec <- seq(boatobj$xp[2], boatobj$xp[1], length = xlen)
  } else xvec <- x
  # upper basic contour
  yvec <- boatcont(x = xvec, boatobj = boatobj)
  # turned into lower contour if wh=-1
  res1 <- list(x = xvec, y = yvec*wh)
  # rotate it according to yc
  res2 <- rotatefu(res1, yc = boatobj$yc)
  # update if posterior set needed
  if(!prior) {
    return(updatefu(res2, data=boatobj$data))
  } else {
    return(res2)
  }
}

domainplotter <- function(xlims, ylims = NULL, seqx = 100, ...){
  ax <- seq(xlims[1], xlims[2], length = seqx)
  if(is.null(ylims)) ylims <- c(-1, 1)*(1 + 0.5*xlims[2])
  plot(ax, rep(0, seqx), xlim = xlims, ylim = ylims, type = "l", col = "grey",
       xlab = bquote(eta[0]), ylab = bquote(eta[1]), ...)
  lines(ax,  1 + 0.5*ax)     
  lines(ax, -1 - 0.5*ax)     
  for(i in seq(0.1, 0.9, by = 0.1))
    lines(ax, (1 + 0.5*ax)*2*(i-0.5), col = "grey")
}

boatplotter <- function(boatobj, prior = TRUE, xlims = NULL, ylims = NULL, minmax = FALSE,
                        seqx = 100, fillcol = "gray", add = FALSE, col = 1, ...){
  if(!prior) {
    data <- boatobj$data
    if(is.null(data$tau) | is.null(data$n)) stop(paste("No data specified in ", quote(boatobj)))
  } else {
    data <- list(tau = 0, n = 0)
  }
  upper <- boatfu(boatobj = boatobj, wh =  1, xlen = seqx, fw = TRUE,  prior = prior)
  lower <- boatfu(boatobj = boatobj, wh = -1, xlen = seqx, fw = FALSE, prior = prior)
  if(!add){ # set up new plot
    if(is.null(xlims)) xlims <- c(-2, boatobj$xp[2] + data$n)
    if(is.null(ylims)) ylims <- c(-1,1)*(1 + 0.5*(boatobj$xp[2] + data$n))
    domainplotter(xlims=xlims, ylims=ylims, seqx=seqx, ...)
  }
  polygon(c(upper$x, lower$x), c(upper$y, lower$y), col = fillcol, border = col, ...)
  if(minmax){
    eta01mm <- eta01minmax(lower=lower, upper=upper, ...)
    points(eta01mm$ymin, cex = 1.5)
    points(eta01mm$ymax, cex = 1.5)
  }
}


# transformation function from Mik's world to 'normal' world
miktonormal <- function(xylist){
  x <- xylist$x + 2
  y <- xylist$y/(xylist$x + 2) + 0.5
  list(x = x, y = y)
}

# transformation function from 'normal' world to Mik's world
normaltomik <- function(xylist){
  x <- xylist$x - 2
  y <- xylist$x*(xylist$y - 0.5)
  list(x = x, y = y)
}

# plot the transformed set in "normal world"
normalplotter <- function(boatobj, prior = TRUE, xlims = NULL, ylims = c(0,1), minmax = FALSE,
                          xlabs = bquote(n^(0)), ylabs = bquote(y^(0)), seqx = 100,
                          fillcol = "gray", add = FALSE, col = 1, ...){
  if(!prior) {
    data <- boatobj$data
    if(is.null(data$tau) | is.null(data$n)) stop(paste("No data specified in ", quote(boatobj)))
  } else {
    data <- list(tau = 0, n = 0)
  }
  uppermik <- boatfu(boatobj = boatobj, wh =  1, xlen = seqx, fw = TRUE,  prior = prior)
  lowermik <- boatfu(boatobj = boatobj, wh = -1, xlen = seqx, fw = FALSE, prior = prior)
  upper <- miktonormal(uppermik)
  lower <- miktonormal(lowermik)
  if(!add){ # set up new plot
    if(is.null(xlims)) xlims <- c(0, boatobj$xp[2] + data$n + 2)
    ax <- seq(xlims[1], xlims[2], length = seqx)
    plot(ax, rep(0, seqx), xlim = xlims, ylim = ylims, type = "l",
         xlab = xlabs, ylab = ylabs, ...)
    lines(ax,  rep(1, seqx))     
    for(i in seq(0.1, 0.9, by = 0.1))
      lines(ax, rep(i, seqx), col = "grey")
  }
  polygon(c(upper$x, lower$x), c(upper$y, lower$y), col = fillcol, border = col, ...)
  if(minmax){
    nymm <- nyminmax(lower=lower, upper=upper, ...)
    points(nymm$ymin, cex = 1.5)
    points(nymm$ymax, cex = 1.5)
  }
}


# gives points on ny contour corresponding to min and max y
nyminmax <- function(lower, upper, minmaxtol = 1e-6){
  minpos <- which.min(lower$y)
  if(abs(lower$y[minpos] - lower$y[length(lower$y)]) < minmaxtol)
    minpos <- length(lower$y)
  minx <- lower$x[minpos]
  miny <- lower$y[minpos]
  maxpos <- which.max(upper$y)
  if(abs(upper$y[maxpos] - upper$y[length(upper$y)]) < minmaxtol)
    maxpos <- length(upper$y)
  maxx <- upper$x[maxpos]
  maxy <- upper$y[maxpos]
  return(list(ymin=list(x=minx, y=miny), ymax=list(x=maxx, y=maxy)))
}

# gives points on eta01 contour corresponding to min and max y,
# i.e., the lower and upper touchpoints
eta01minmax <- function(lower, upper, minmaxtol = 1e-6){
  normlower <- miktonormal(lower)
  normupper <- miktonormal(upper)
  normminmax <- nyminmax(lower=normlower, upper=normupper, minmaxtol=minmaxtol)
  return(list(ymin=normaltomik(normminmax$ymin), ymax=normaltomik(normminmax$ymax)))
}

# for predictive probability plot (PPP)
# returns vector of extreme yn values for given vector of s values (s in [0, n])
ynfinder <- function(boatobj, svec, lower = TRUE, seqx = 100){
  if(lower){
    wh <- -1
  } else {
    wh <- 1
  }
  res <- rep(0, length(svec))
  sboat <- boatobj
  for (i in 1:length(svec)){
    sboat$data$tau <- svec[i]
    mikseq <- boatfu(boatobj = sboat, wh =  wh, xlen = seqx, fw = TRUE,  prior = FALSE)
    normalseq <- miktonormal(mikseq) # returns list(x=,y=)
    if(lower){
      theindex <- which.min(normalseq$y)
    } else {
      theindex <- which.max(normalseq$y)
    }
    #thex <- normalseq$x[theindex]
    #they <- normalseq$y[theindex]
    res[i] <- normalseq$y[theindex]
  }
  res
}

# determine (rectangular) luck model for a given boatshape object
luckenvelope <- function(boatobj, seqx = 100, prior = TRUE){
  uppermik <- boatfu(boatobj = boatobj, wh =  1, xlen = seqx, prior = prior)
  lowermik <- boatfu(boatobj = boatobj, wh = -1, xlen = seqx, prior = prior)
  upper <- miktonormal(uppermik)
  lower <- miktonormal(lowermik)
  ymax <- max(upper$y)
  ymin <- min(lower$y)
  nmax <- max(c(lower$x, upper$x))
  nmin <- min(c(lower$x, upper$x))
  LuckModel(n0 = c(nmin, nmax), y0 = c(ymin, ymax)) 
}

# adding the PPP lines for a luck object
# (notation see ISIPTA'11 paper)
luckppplines <- function(luckobj, n, ...){
  s1 <- n * y0(luckobj)[1]
  s2 <- n * y0(luckobj)[2]
  a  <- n0(luckobj)[1] * y0(luckobj)[1]/(n0(luckobj)[1] + n)
  b  <- n0(luckobj)[2] * y0(luckobj)[2]/(n0(luckobj)[2] + n)
  c  <- (n0(luckobj)[2] * y0(luckobj)[1] + n)/(n0(luckobj)[2] + n)
  d  <- (n0(luckobj)[1] * y0(luckobj)[2] + n)/(n0(luckobj)[1] + n)
  e1 <- y0(luckobj)[1]
  e2 <- (n0(luckobj)[2] * y0(luckobj)[2] + n * y0(luckobj)[1])/(n0(luckobj)[2] + n)
  f1 <- (n0(luckobj)[2] * y0(luckobj)[1] + n * y0(luckobj)[2])/(n0(luckobj)[2] + n)
  f2 <- y0(luckobj)[2]
  lines(c(0, s1, s2, n), c(a, e1, f1, c), ...)
  lines(c(0, s1, s2, n), c(b, e2, f2, d), ...)
}


# to make predictive probability plots (PPP)
# for a boatset and comparing it with luck models
# having the same prior imprecision for y and
# - the same n0 range as the boatset
# - n0 = 1
# - n0 = 2
# boatobj:    the the boat object
# pppn:       sample size
# svecby:     increment for the vector of s values
# luckmodels: if luck models are plotted, too.
pppmaker <- function(boatobj, pppn, svecby = 0.01, luckmodels = TRUE){
  if(luckmodels){
    # luck model with the same prior imprecision for y
    ppplm <- luckenvelope(boatobj)
    # luck model with n0 = 1 and n0 = 2
    ppplm1 <- ppplm -> ppplm2
    n0(ppplm1) <- c(1,1)
    n0(ppplm2) <- c(2,2)
  }
  # plots of the chosen boatset and luckenvelope
  par(mfrow=c(1,2), mar=c(5,4.5,4,1)+0.1)
  # prior sets
  normalplotter(boatobj, main="Prior parameter sets")
  if(luckmodels){
    plot(ppplm, add=T, lty=2, control=controlList(polygonCol=NA, annotate=F))
    lines(n0(ppplm1), y0(ppplm1), col="blue", lwd=2)
    lines(n0(ppplm2), y0(ppplm2), col="red", lwd=2)
  }
  # PPP for boatset
  svec <- seq(0, pppn, by=svecby)
  ylvec <- ynfinder(boatobj = boatobj, svec = svec)
  yuvec <- ynfinder(boatobj = boatobj, svec = svec, lower = FALSE)
  plot(svec, yuvec, type="l", ylim=c(0,1), xlab="s", ylab=expression(y^(n)),
       main = bquote(paste("Posterior imprecision (n=",.(pppn),")")))
  lines(svec, ylvec, type="l")
  if(luckmodels){
    # PPP for the luckenvelopes
    luckppplines(luckobj=ppplm, n=pppn, lty=2)
    luckppplines(luckobj=ppplm1, n=pppn, col="blue")
    luckppplines(luckobj=ppplm2, n=pppn, col="red")
    # prior range (at s=n/2, but could be anywhere)
    lines(rep(pppn*boatobj$yc, 2), y0(ppplm), lwd=5, lend=2)
    # legend
    legend("topleft", c("boatshape", "luckenvelope", "n0 = 1", "n0 = 2"),
           col = c("black", "black", "blue", "red"),
           lty = c(1, 2, 1, 1), lwd = rep(1, 4))
  }
  par(mfrow=c(1,1))
}

#