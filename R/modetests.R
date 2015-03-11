# tests for modefunctions

# small function to plot beta densities and give the mode with betamode() 
betaplotter <- function(a,b, trunkate = TRUE){
  curve(dbeta(x, shape1=a, shape2=b),
        main=bquote(paste("a=",.(a),", b=",.(b),", n=",.(a+b),", y=",.(a/(a+b)), sep="")))
print(betamode(a+b, a/(a+b), trunkate = trunkate))
}

betaplotter(0.3, 0.3)
betaplotter(1  , 0.3)
betaplotter(1.2, 0.8)
betaplotter(1.2, 0.81, trunkate = F)
betaplotter(2  , 0.1 , trunkate = F)
betaplotter(0.1, 2   , trunkate = F)
betaplotter(2  , 0.1)
betaplotter(0.1, 2  )
betaplotter(1.1, 1.1)

# almost equal function
ntest <- seq(1, 3, length.out=11)
almostEqual(2, ntest)
almostEqual(2, ntest+1e-8)
almostEqual(2, ntest+1e-7)

# betamode
betamode(2,0.5)
betamode(2.1,0.5)
betamode(seq(1, 3, length.out=101), rep(0.5, 101))
betamode(seq(3, 4, length.out=101), rep(0.5, 101))
betamode(seq(2, 4, length.out=101), rep(0.5, 101))



#