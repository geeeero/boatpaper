# functions for the mode as posterior inference of interest

# mode as function of n0 and y0 (where n > 2, 1 < n*y < n-1)
betamode <- function(n,y) (n*y - 1)/(n-2)

# as the mode is monotone in y,
# we will find the extremes on the lower and upper contours

# returns vector of extreme mode values for given vector of s values (s in [0, n])
modefinder <- function(boatobj, svec, lower = TRUE, seqx = 200){
  if(lower){
    wh <- -1
  } else {
    wh <- 1
  }
  res <- rep(0, length(svec))
  for (i in 1:length(svec)){
    boatobj$data$tau <- svec[i]
    mikseq <- boatfu(boatobj = boatobj, wh =  wh, xlen = seqx, fw = TRUE,  prior = FALSE)
    normalseq <- miktonormal(mikseq) # returns list(x=n0,y=y0)
    if(lower){
      res[i] <- min(betamode(normalseq$x,normalseq$y))
    } else {
      res[i] <- max(betamode(normalseq$x,normalseq$y))
    }
  }
  res
}

modeplotter <- function(boatobj, pppn, svecby = 0.01){
  svec <- seq(0, pppn, by=svecby)
  lvec <- modefinder(boatobj = boatobj, svec = svec)
  uvec <- modefinder(boatobj = boatobj, svec = svec, lower = FALSE)
  plot(svec, uvec, type="l", ylim=c(0,1), xlab="s", ylab="mode",
       main = bquote(paste("Posterior imprecision (n=",.(pppn),")")))
  lines(svec, lvec, type="l")
}

#