# functions for the mode as posterior inference of interest

# mode as function of n0 and y0 (where n   > 2, 1 < n*y < n-1)
#                          (i.e. where a+b > 2, 1 < a   < a+b-1)
betamode <- function(n, y, trunkate = TRUE){
  # beware of numerical problems!
  # testing if n <= 2
  if(isTRUE(all.equal(n, 2)) || n <= 2)
    stop(paste("No proper mode for n <= 2 (n = ", n, ") \n", sep=""))
  res <- (n*y - 1)/(n-2)
  if(trunkate){
    res[res < 0] <- 0
    res[res > 1] <- 1 
  }
  res
}

# plot region where mode is defined in normal world and miksworld?
# mode undefined for priors with xp[1] < 0 (and strictly also for other conditions)

# as the mode is monotone in y,
# we will find the extremes on the lower and upper contours

modefinder <- function(boatobj, seqx = 200, prior = TRUE){
  lowermik <- boatfu(boatobj = boatobj, wh = -1, xlen = seqx, fw = FALSE,  prior = prior)
  uppermik <- boatfu(boatobj = boatobj, wh =  1, xlen = seqx, fw = FALSE,  prior = prior)
  lowernorm <- miktonormal(lowermik) # returns list(x=n0,y=y0)
  uppernorm <- miktonormal(uppermik) # returns list(x=n0,y=y0)
  lmodes <- betamode(lowernorm$x, lowernorm$y)
  umodes <- betamode(uppernorm$x, uppernorm$y)
  wml <- which.min(lmodes)
  wmu <- which.max(umodes)
  # mode and n, y coordinates for lower mode
  lmode <- list(mode=lmodes[wml], ny=c(lowernorm$x[wml], lowernorm$y[wml]))
  # mode and n, y coordinates for upper mode
  umode <- list(mode=umodes[wmu], ny=c(uppernorm$x[wmu], uppernorm$y[wmu]))
  list(lmode = lmode, umode = umode)
}


modeplotter <- function(boatobj,, prior = FALSE)
  
  

# returns vector of extreme mode values for given vector of s values (s in [0, n])
smodefinder <- function(boatobj, svec, lower = TRUE, seqx = 200, prior = FALSE){
  if(lower){
    wh <- -1
  } else {
    wh <- 1
  }
  res <- rep(0, length(svec))
  sboat <- boatobj
  for (i in 1:length(svec)){
    sboat$data$tau <- svec[i]
    mikseq <- boatfu(boatobj = sboat, wh =  wh, xlen = seqx, fw = TRUE,  prior = prior)
    normalseq <- miktonormal(mikseq) # returns list(x=n0,y=y0)
    if(lower){
      res[i] <- min(betamode(normalseq$x,normalseq$y))
    } else {
      res[i] <- max(betamode(normalseq$x,normalseq$y))
    }
  }
  res
}



pppmodeplotter <- function(boatobj, pppn, svecby = 0.01){
  par(mfrow=c(1,2), mar=c(5,4.5,4,1)+0.1)
  # prior set
  normalplotter(boatobj, main="Prior parameter set")
  # varying s plot
  svec <- seq(0, pppn, by=svecby)
  lvec <- smodefinder(boatobj = boatobj, svec = svec)
  uvec <- smodefinder(boatobj = boatobj, svec = svec, lower = FALSE)
  plot(svec, uvec, type="l", ylim=c(0,1), xlab="s", ylab="mode",
       main = bquote(paste("Posterior imprecision (n=",.(pppn),")")))
  lines(svec, lvec, type="l")
  par(mfrow=c(1,1))
}

#