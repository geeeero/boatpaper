# Illustration prior-data conflict in Beta-Binomial model
# plots of posterior predictive

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

# one prior updated to posteriors with same variance
sgpr <- c(8, 0.75) # n0, y0
data1 <- c(12,16)  #  s,  n
data2 <- c( 0,16)

pos1 <- nyupdate(sgpr, data1)
pos2 <- nyupdate(sgpr, data2)

betavec <- seq(0,1, length.out=201)
# beta cdfs
#bpsgpr <- pbetany(betavec, sgpr)
#bppos1 <- pbetany(betavec, pos1)
#bppos2 <- pbetany(betavec, pos2)
#bpdf <- data.frame(x=betavec, Prior=bpsgpr, "Posterior1"=bppos1, "Posterior2"=bppos2)
#names(bpdf)[c(3,4)] <- c("Posterior 1", "Posterior 2")
#bpdf <- melt(bpdf, "x")
#betapdfcdf <- ggplot(bdf, aes(x, value, group=variable, color=variable)) + geom_line() +
#  bottomlegend + xlab("p") + ylab("") +
#  facet_grid(. ~ Item, scales="free")
#betapdfcdf + pdcscale

betapdf <- rbind(data.frame(x=betavec, value=pbetany(betavec, sgpr), Facet="no conflict", Item="Prior", Item2="Prior"),
                 data.frame(x=betavec, value=pbetany(betavec, pos1), Facet="no conflict", Item="Posterior 1", Item2="Posterior"),
                 data.frame(x=betavec, value=pbetany(betavec, sgpr), Facet="prior-data conflict", Item="Prior", Item2="Prior"),
                 data.frame(x=betavec, value=pbetany(betavec, pos2), Facet="prior-data conflict", Item="Posterior 2", Item2="Posterior"))
betapdf$Item <- ordered(betapdf$Item, levels=c("Prior", "Posterior 1", "Posterior 2"))
betapdf$Item2 <- ordered(betapdf$Item2, levels=c("Prior", "Posterior"))

betaddf <- rbind(data.frame(x=betavec, value=dbetany(betavec, sgpr), Facet="no conflict", Item="Prior", Item2="Prior"),
                 data.frame(x=betavec, value=dbetany(betavec, pos1), Facet="no conflict", Item="Posterior 1", Item2="Posterior"),
                 data.frame(x=betavec, value=dbetany(betavec, sgpr), Facet="prior-data conflict", Item="Prior", Item2="Prior"),
                 data.frame(x=betavec, value=dbetany(betavec, pos2), Facet="prior-data conflict", Item="Posterior 2", Item2="Posterior"))
betaddf$Item <- ordered(betaddf$Item, levels=c("Prior", "Posterior 1", "Posterior 2"))
betaddf$Item2 <- ordered(betaddf$Item2, levels=c("Prior", "Posterior"))

betaplot0 <- ggplot(subset(betaddf, Item2 == "Prior")) + geom_line(aes(x=x, y=value, group=Item2, colour=Item2)) + pdcscale
betaplot0 <- betaplot0 + facet_grid(. ~ Facet) + nolegend + xlab(expression(p)) + ylab("pdf")
pdf("figs/betasgdens0.pdf", width=6, height=3)
betaplot0 + scale_y_continuous(labels = fmt_dcimals(2), limits=c(0,4.65))
dev.off()

betaplot1 <- ggplot(betaddf) + geom_line(aes(x=x, y=value, group=Item2, colour=Item2)) + pdcscale
betaplot1 <- betaplot1 + facet_grid(. ~ Facet) + nolegend + xlab(expression(p)) + ylab("pdf")
pdf("figs/betasgdens.pdf", width=6, height=3)
betaplot1 + scale_y_continuous(labels = fmt_dcimals(2), limits=c(0,4.65))
dev.off()

betaplot2 <- ggplot(betapdf) + geom_line(aes(x=x, y=value, group=Item2, colour=Item2)) + pdcscale
betaplot2 <- betaplot2 + facet_grid(. ~ Facet) + nolegend + xlab(expression(p)) + ylab("cdf")
pdf("figs/betasgpdf.pdf", width=6, height=3)
betaplot2
dev.off()


# better with grid.arrange:
#betapdfcdf      <- betapdfcdf    + theme(plot.margin = unit(c(0,0.5,0,-0.75), "lines")) + theme_bw()
#pdcillupmfcmf   <- pdcillupmfcmf + theme(plot.margin = unit(c(0,0,  0,-0.75), "lines"),
#                                         legend.margin = unit(-0.5, "cm")) + theme_bw()
#pdf("singleprior-pdc2.pdf", width=8, height=5)
#grid.arrange(betapdfcdf, pdcillupmfcmf, nrow=1, ncol=2, widths=c(1,1.2))
#dev.off()


# ------------------- sets of Beta pdfs -------------------------

setpr <- BinomialLuckModel(n0=c(1,8), y0=c(0.7, 0.8))
setpos1 <- setpr
data(setpos1) <- BinomialData(s=12, n=16)
setpos2 <- setpr
data(setpos2) <- BinomialData(s= 0, n=16)

# Beta cdfs
#betavec <- seq(0,1, length.out=201)
#cdfplot(setpos1, xvec=betavec)
#cdfplot(setpos1, xvec=betavec, control=controlList(posterior=TRUE))
#cdfplot(setpos2, xvec=betavec, control=controlList(posterior=TRUE))

betapriolow <- sapply(betavec, function(x){
  optim(par=c(4, 0.7), fn=function(.n0y0, x){
    pbetany(x, .n0y0)
  }, method="L-BFGS-B", control=list(fnscale=1),
  lower=c(n0(setpr)[1], y0(setpr)[1]),
  upper=c(n0(setpr)[2], y0(setpr)[2]), x=x)$value
})

betaprioupp <- sapply(betavec, function(x){
  optim(par=c(4, 0.8), fn=function(.n0y0, x){
    pbetany(x, .n0y0)
  }, method="L-BFGS-B", control=list(fnscale=-1),
  lower=c(n0(setpr)[1], y0(setpr)[1]),
  upper=c(n0(setpr)[2], y0(setpr)[2]), x=x)$value
})

betapriocorners <- sapply(luck4cny(setpr), function(x){
  pbetany(betavec, x)
})

betapos1low <- sapply(betavec, function(x){
  optim(par=c(4, 0.7), fn=function(.n0y0, x){
    pbetany(x, nyupdate(pr=.n0y0, data=c(tau(data(setpos1)), n(data(setpos1)))))
  }, method="L-BFGS-B", control=list(fnscale=1),
  lower=c(n0(setpr)[1], y0(setpr)[1]),
  upper=c(n0(setpr)[2], y0(setpr)[2]), x=x)$value
})

betapos1upp <- sapply(betavec, function(x){
  optim(par=c(4, 0.8), fn=function(.n0y0, x){
    pbetany(x, nyupdate(pr=.n0y0, data=c(tau(data(setpos1)), n(data(setpos1)))))
  }, method="L-BFGS-B", control=list(fnscale=-1),
  lower=c(n0(setpr)[1], y0(setpr)[1]),
  upper=c(n0(setpr)[2], y0(setpr)[2]), x=x)$value
})

betapos1corners <- sapply(luck4cny(setpos1, posterior=TRUE), function(x){
  pbetany(betavec, x)
})

betapos2low <- sapply(betavec, function(x){
  optim(par=c(4, 0.7), fn=function(.n0y0, x){
    pbetany(x, nyupdate(pr=.n0y0, data=c(tau(data(setpos2)), n(data(setpos2)))))
  }, method="L-BFGS-B", control=list(fnscale=1),
  lower=c(n0(setpr)[1], y0(setpr)[1]),
  upper=c(n0(setpr)[2], y0(setpr)[2]), x=x)$value
})

betapos2upp <- sapply(betavec, function(x){
  optim(par=c(4, 0.8), fn=function(.n0y0, x){
    pbetany(x, nyupdate(pr=.n0y0, data=c(tau(data(setpos2)), n(data(setpos2)))))
  }, method="L-BFGS-B", control=list(fnscale=-1),
  lower=c(n0(setpr)[1], y0(setpr)[1]),
  upper=c(n0(setpr)[2], y0(setpr)[2]), x=x)$value
})

betapos2corners <- sapply(luck4cny(setpos2, posterior=TRUE), function(x){
  pbetany(betavec, x)
})

betasetdf <- rbind(data.frame(x=betavec, Lower=betapriolow, Upper=betaprioupp, betapriocorners, Facet="no conflict", Item="Prior", Item2="Prior"),
                   data.frame(x=betavec, Lower=betapos1low, Upper=betapos1upp, betapos1corners, Facet="no conflict", Item="Posterior 1", Item2="Posterior"),
                   data.frame(x=betavec, Lower=betapriolow, Upper=betaprioupp, betapriocorners, Facet="prior-data conflict", Item="Prior", Item2="Prior"),
                   data.frame(x=betavec, Lower=betapos2low, Upper=betapos2upp, betapos2corners, Facet="prior-data conflict", Item="Posterior 2", Item2="Posterior"))
betasetdf$Item <- ordered(betasetdf$Item, levels=c("Prior", "Posterior 1", "Posterior 2"))
betasetdf$Item2 <- ordered(betasetdf$Item2, levels=c("Prior", "Posterior"))

betaset0 <- ggplot(subset(betasetdf, Item2 == "Prior")) + geom_ribbon(aes(x=x, ymin=Lower, ymax=Upper, group=Item2, colour=Item2, fill=Item2), alpha=0.3)
betaset0 <- betaset0 + pdcscale + pdcscale2 +
  geom_line(aes(x=x, y=c1, group=Item2, colour=Item2)) +
  geom_line(aes(x=x, y=c2, group=Item2, colour=Item2)) +
  geom_line(aes(x=x, y=c3, group=Item2, colour=Item2)) +
  geom_line(aes(x=x, y=c4, group=Item2, colour=Item2))
betaset0 <- betaset0 + facet_grid(. ~ Facet) + nolegend + xlab(expression(p)) + ylab("cdf")
pdf("figs/betaset0.pdf", width=6, height=3)
betaset0
dev.off()

betaset1 <- ggplot(betasetdf) + geom_ribbon(aes(x=x, ymin=Lower, ymax=Upper, group=Item2, colour=Item2, fill=Item2), alpha=0.3)
betaset1 <- betaset1 + pdcscale + pdcscale2 +
  geom_line(aes(x=x, y=c1, group=Item2, colour=Item2)) +
  geom_line(aes(x=x, y=c2, group=Item2, colour=Item2)) +
  geom_line(aes(x=x, y=c3, group=Item2, colour=Item2)) +
  geom_line(aes(x=x, y=c4, group=Item2, colour=Item2))
betaset1 <- betaset1 + facet_grid(. ~ Facet) + nolegend + xlab(expression(p)) + ylab("cdf")
pdf("figs/betaset1.pdf", width=6, height=3)
betaset1
dev.off()


#TODO (???):
# ------------------- parameter sets -------------------------


n0vec <- seq(n0(setpr)[1], n0(setpr)[2], length.out=201)
prioparam <- data.frame(n0=n0vec, Lower=y0(setpr)[1], Upper=y0(setpr)[2])
pos1param <- data.frame(n0=updateLuckN(n0vec, n(data(setpos1))),
                        Lower=updateLuckY(n0vec, y0(setpos1)[1], tau(data(setpos1)), n(data(setpos1))),
                        Upper=updateLuckY(n0vec, y0(setpos1)[2], tau(data(setpos1)), n(data(setpos1))))
pos2param <- data.frame(n0=updateLuckN(n0vec, n(data(setpos2))),
                        Lower=updateLuckY(n0vec, y0(setpos2)[1], tau(data(setpos2)), n(data(setpos2))),
                        Upper=updateLuckY(n0vec, y0(setpos2)[2], tau(data(setpos2)), n(data(setpos2))))

paramsetdf <- rbind(data.frame(prioparam, Facet="Prior & Posterior 1", Item="Prior", Item2="Prior"),
                    data.frame(pos1param, Facet="Prior & Posterior 1", Item="Posterior 1", Item2="Posterior"),
                    data.frame(prioparam, Facet="Prior & Posterior 2", Item="Prior", Item2="Prior"),
                    data.frame(pos2param, Facet="Prior & Posterior 2", Item="Posterior 2", Item2="Posterior"))

paramset1 <- ggplot(paramsetdf) + geom_ribbon(aes(x=n0, ymin=Lower, ymax=Upper, group=Item, colour=Item, fill=Item), alpha=0.3)
paramset1 <- paramset1 + facet_grid(Facet ~ .) + guides(linetype="none", colour="none", fill="none")
paramset1 <- paramset1 + xlab(expression(n^(0))) + ylab(expression(y^(0)))
paramset1

paramset1 <- paramset1 + theme(plot.margin = unit(c(0,0.5,0,-0.25), "lines"))
betaset1  <- betaset1  + theme(plot.margin = unit(c(0,0,  0,-0.25), "lines"),
                               legend.margin = unit(-0.5, "cm"))
# parameter sets and cdf sets in one plot
pdf("priorset.pdf", width=8, height=5)
grid.arrange(paramset1, betaset1, nrow=1, ncol=2, widths=c(1,1.2))
dev.off()

paramset2 <- ggplot(paramsetdf) + geom_ribbon(aes(x=n0, ymin=Lower, ymax=Upper, group=Item, colour=Item, fill=Item), alpha=0.3)
paramset2 <- paramset2 + xlab(expression(n^(0))) + ylab(expression(y^(0)))
paramset2

# plot with parameter sets only
pdf("paramsets.pdf", width=6, height=5)
paramset2 + theme(plot.margin = unit(c(0,0.5,0,-0.25), "lines"), legend.margin = unit(-0.5, "cm"))
dev.off()

# in two separate facets
paramset3 <- ggplot(paramsetdf) + geom_ribbon(aes(x=n0, ymin=Lower, ymax=Upper, group=Item2, colour=Item2, fill=Item2), alpha=0.3)
paramset3 <- paramset3 + scale_fill_manual(values = c("#b2df8a", "#1f78b4")) + scale_colour_manual(values = c("#b2df8a", "#1f78b4"))
paramset3 <- paramset3 + facet_grid(. ~ Facet) + xlab(expression(n^(0))) + ylab(expression(y^(0)))

pdf("paramsets.pdf", width=6, height=2.5) # plot.margin: t r b f
paramset3 + theme(plot.margin = unit(c(0,0.5,0,0), "lines"), legend.margin = unit(-0.0, "cm")) + theme_bw()  + rightlegend
dev.off()

#