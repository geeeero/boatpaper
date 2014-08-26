###########################################
# function for boat shapes in Mik's world #
###########################################

# boat object defined like this:
bsp1 <- list(xp = c(-2,6), a = 2, b = 1/4, yc = 0.5, data = list(tau = 0, n = 0))
# xp: start and end of set on eta_0 axis (before rotating)
# a:  width of shape for eta_o upper -> infty
# b:  bulkyness of shape
# yc: central ray of shape, in (0,1)
# data: contains tau = number of successes, n = numberof trials

# makes the rotation in "Mik's World" for a list(x = *, y = *) object from
# 0.5-centered things to things centered around yc
# TODO: add rotationg center as argument, such that rotating and updating may commute
rotatefu <- function(xylist, yc){
  atz <- atan(yc - 0.5)
  x <- cos(atz)*(xylist$x + 2) - sin(atz)*xylist$y - 2
  y <- sin(atz)*(xylist$x + 2) + cos(atz)*xylist$y
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
  boatobj$a*(1-exp(-boatobj$b*(x-boatobj$xp[1])))
}

# returns lower [wh=-1] or upper [wh=1, default] x vector and contour y(x) of the boat 
# shape for a boatobj as given in example above
boatfu <- function(x = NULL, boatobj, wh = 1, xlen = 100, fw = TRUE, prior = TRUE){
  atz <- atan(boatobj$yc - 0.5)
  if(is.null(x)){   # if 
    if (fw) xvec <- seq(boatobj$xp[1], boatobj$xp[2], length = xlen)
    else    xvec <- seq(boatobj$xp[2], boatobj$xp[1], length = xlen)
  } else xvec <- x
  yvec <- boatcont(x = xvec, boatobj = boatobj)
  x <- cos(atz)*(xvec+2) - sin(atz)*yvec*wh - 2
  y <- sin(atz)*(xvec+2) + cos(atz)*yvec*wh
  if(!prior){
    data <- boatobj$data
    x <- x + data$n
    y <- y + 1/2*(data$tau - (data$n-data$tau))
  }
  list(x = x, y = y)
}

domainplotter <- function(xlims, ylims, seqx = 100, ...){
  ax <- seq(xlims[1], xlims[2], length = seqx)
  plot(ax, rep(0, seqx), xlim = xlims, ylim = ylims, type = "l", col = "grey",
       xlab = bquote(eta[0]), ylab = bquote(eta[1]), ...)
  lines(ax,  1 + 0.5*ax)     
  lines(ax, -1 - 0.5*ax)     
  for(i in seq(0.1, 0.9, by = 0.1))
    lines(ax, (1 + 0.5*ax)*2*(i-0.5), col = "grey")
}

boatplotter <- function(boatobj, prior = TRUE, xlims = NULL, ylims = NULL, minmax = TRUE,
                        seqx = 100, fillcol = "gray", add = FALSE, col = 1, ...){
  if(!prior) {
    data <- boatobj$data
    if(is.null(data$tau) | is.null(data$n)) stop(paste("No data specified in ", quote(boatobj)))
  } else {
    data <- list(tau = 0, n = 0)
  }
  upper <- boatfu(boatobj = boatobj, wh =  1, xlen = seqx, fw = TRUE,  prior = prior)
  lower <- boatfu(boatobj = boatobj, wh = -1, xlen = seqx, fw = FALSE, prior = prior)
  #xlabs <- bquote(eta[0])
  #ylabs <- bquote(eta[1])
  if(is.null(xlims)) xlims <- c(-2, boatobj$xp[2] + data$n)
  if(is.null(ylims)) ylims <- c(-1,1)*(1 + 0.5*(boatobj$xp[2] + data$n))
  if(!add){ # set up new plot
    domainplotter(xlims=xlims, ylims=ylims, seqx=seqx, ...)
  }
  polygon(c(upper$x, lower$x), c(upper$y, lower$y), col = fillcol, border = col, ...)
  #if(minmax){
  # minmaxn <- c(nseqf[which.min(lower)], nseqb[which.max(upper)])
  # minmaxy <- c(min(lower), max(upper))
  # points(minmaxn, minmaxy, cex = 1.5)
  #}
}


# transformation function from Mik's world to 'normal' world

miktonormal <- function(xylist){
  x <- xylist$x + 2
  y <- xylist$y/(xylist$x + 2) + 0.5
  list(x = x, y = y)
}


# plot the transformed set in "normal world"
normalplotter <- function(boatobj, prior = TRUE, xlims = NULL, ylims = c(0,1), minmax = FALSE,
                          minmaxtol = 1e-6,
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
  #print(uppermik)
  upper <- miktonormal(uppermik)
  #print(upper)
  lower <- miktonormal(lowermik)
  #xlabs <- bquote(n^(0))
  #ylabs <- bquote(y^(0))
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
    minpos <- which(lower$y == min(lower$y))
    if(abs(lower$y[minpos] - lower$y[length(lower$y)]) < minmaxtol)
      minpos <- length(lower$y)
    minx <- lower$x[minpos]
    miny <- lower$y[minpos]
    maxpos <- which(upper$y == max(upper$y))
    if(abs(upper$y[maxpos] - upper$y[length(upper$y)]) < minmaxtol)
      maxpos <- length(upper$y)
    maxx <- upper$x[maxpos]
    maxy <- upper$y[maxpos]
    points(c(minx,maxx), c(miny,maxy), cex = 1.5)
    return(list(lower=c(minx,miny), upper=c(maxx,maxy)))
  }
}

# for predictive probability plot (PPP)
# returns vector of yn values for given vector of s values (s in [0, n])
ynfinder <- function(boatobj, svec, lower = TRUE, seqx = 100){
  if(lower){
    wh <- -1
  } else {
    wh <- 1
  }
  res <- rep(0, length(svec))
  for (i in 1:length(svec)){
    boatobj$data$tau <- svec[i]
    mikseq <- boatfu(boatobj = boatobj, wh =  wh, xlen = seqx, fw = TRUE,  prior = FALSE)
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
luckenvelope <- function(boatobj, seqx = 100){
  uppermik <- boatfu(boatobj = boatobj, wh =  1, xlen = seqx)
  lowermik <- boatfu(boatobj = boatobj, wh = -1, xlen = seqx)
  upper <- miktonormal(uppermik)
  lower <- miktonormal(lowermik)
  ymax <- max(upper$y)
  ymin <- min(lower$y)
  LuckModel(n0=boatobj$xp + 2, y0 = c(ymin, ymax)) 
}

#