# functions for the mode as posterior inference of interest

# mode as function of n0 and y0 (where n > 2, 1 < n*y < n-1)
betamode <- function(n,y) (n*y - 1)/(n-2)

# plot region where mode is defined in normal world and miksworld
# mode undefined for priors with xp[1] < 0


# as the mode is monotone in y,
# we will find the extremes on the lower and upper contours

# returns vector of extreme mode values for given vector of s values (s in [0, n])
modefinder <- function(boatobj, svec, lower = TRUE, seqx = 200, prior = FALSE){
  if(lower){
    wh <- -1
  } else {
    wh <- 1
  }
  res <- rep(0, length(svec))
  for (i in 1:length(svec)){
    boatobj$data$tau <- svec[i]
    mikseq <- boatfu(boatobj = boatobj, wh =  wh, xlen = seqx, fw = TRUE,  prior = prior)
    normalseq <- miktonormal(mikseq) # returns list(x=n0,y=y0)
    if(lower){
      res[i] <- min(betamode(normalseq$x,normalseq$y))
    } else {
      res[i] <- max(betamode(normalseq$x,normalseq$y))
    }
  }
  res
}

priormodefinder <- function(boatobj){
  #boatobj$data <- list(tau=0, n=0)
  #lmode <- modefinder(priorboatobj, svec=c(0))
  #umode <- modefinder(priorboatobj, svec=c(0), lower = FALSE)
  
}

modeplotter <- function(boatobj, pppn, svecby = 0.01){
  par(mfrow=c(1,2), mar=c(5,4.5,4,1)+0.1)
  # prior set
  normalplotter(boatobj, main="Prior parameter set")
  # varying s plot
  svec <- seq(0, pppn, by=svecby)
  lvec <- modefinder(boatobj = boatobj, svec = svec)
  uvec <- modefinder(boatobj = boatobj, svec = svec, lower = FALSE)
  plot(svec, uvec, type="l", ylim=c(0,1), xlab="s", ylab="mode",
       main = bquote(paste("Posterior imprecision (n=",.(pppn),")")))
  lines(svec, lvec, type="l")
  par(mfrow=c(1,1))
}

#