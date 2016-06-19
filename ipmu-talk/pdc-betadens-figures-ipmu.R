# ------------------- sets of Beta pdfs -------------------------

betapriolow <- sapply(betavec, function(x){
  optim(par=c(4, 0.5), fn=function(.n0y0, x){
    pbetany(x, .n0y0)
  }, method="L-BFGS-B", control=list(fnscale=1),
  lower=c(n0(setpr)[1], y0(setpr)[1]),
  upper=c(n0(setpr)[2], y0(setpr)[2]), x=x)$value
})

betaprioupp <- sapply(betavec, function(x){
  optim(par=c(4, 0.5), fn=function(.n0y0, x){
    pbetany(x, .n0y0)
  }, method="L-BFGS-B", control=list(fnscale=-1),
  lower=c(n0(setpr)[1], y0(setpr)[1]),
  upper=c(n0(setpr)[2], y0(setpr)[2]), x=x)$value
})

betapriocorners <- sapply(luck4cny(setpr), function(x){
  pbetany(betavec, x)
})

betapos1low <- sapply(betavec, function(x){
  optim(par=c(4, 0.5), fn=function(.n0y0, x){
    pbetany(x, nyupdate(pr=.n0y0, data=c(tau(data(setpos1)), n(data(setpos1)))))
  }, method="L-BFGS-B", control=list(fnscale=1),
  lower=c(n0(setpr)[1], y0(setpr)[1]),
  upper=c(n0(setpr)[2], y0(setpr)[2]), x=x)$value
})

betapos1upp <- sapply(betavec, function(x){
  optim(par=c(4, 0.5), fn=function(.n0y0, x){
    pbetany(x, nyupdate(pr=.n0y0, data=c(tau(data(setpos1)), n(data(setpos1)))))
  }, method="L-BFGS-B", control=list(fnscale=-1),
  lower=c(n0(setpr)[1], y0(setpr)[1]),
  upper=c(n0(setpr)[2], y0(setpr)[2]), x=x)$value
})

betapos1corners <- sapply(luck4cny(setpos1, posterior=TRUE), function(x){
  pbetany(betavec, x)
})

betapos2low <- sapply(betavec, function(x){
  optim(par=c(4, 0.5), fn=function(.n0y0, x){
    pbetany(x, nyupdate(pr=.n0y0, data=c(tau(data(setpos2)), n(data(setpos2)))))
  }, method="L-BFGS-B", control=list(fnscale=1),
  lower=c(n0(setpr)[1], y0(setpr)[1]),
  upper=c(n0(setpr)[2], y0(setpr)[2]), x=x)$value
})

betapos2upp <- sapply(betavec, function(x){
  optim(par=c(4, 0.5), fn=function(.n0y0, x){
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
pdf("figs/betaset0-ipmu.pdf", width=6, height=3)
print(betaset0)
dev.off()

betaset1 <- ggplot(betasetdf) + geom_ribbon(aes(x=x, ymin=Lower, ymax=Upper, group=Item2, colour=Item2, fill=Item2), alpha=0.3)
betaset1 <- betaset1 + pdcscale + pdcscale2 +
  geom_line(aes(x=x, y=c1, group=Item2, colour=Item2)) +
  geom_line(aes(x=x, y=c2, group=Item2, colour=Item2)) +
  geom_line(aes(x=x, y=c3, group=Item2, colour=Item2)) +
  geom_line(aes(x=x, y=c4, group=Item2, colour=Item2))
betaset1 <- betaset1 + facet_grid(. ~ Facet) + nolegend + xlab(expression(p)) + ylab("cdf")
pdf("figs/betaset1-ipmu.pdf", width=6, height=3)
print(betaset1)
dev.off()


# ------------------- parameter sets -------------------------

n0vec <- seq(n0(setpr)[1], n0(setpr)[2], length.out=201)
prioparam <- data.frame(n0=n0vec, Lower=y0(setpr)[1], Upper=y0(setpr)[2])
pos1param <- data.frame(n0=updateLuckN(n0vec, n(data(setpos1))),
                        Lower=updateLuckY(n0vec, y0(setpos1)[1], tau(data(setpos1)), n(data(setpos1))),
                        Upper=updateLuckY(n0vec, y0(setpos1)[2], tau(data(setpos1)), n(data(setpos1))))
pos2param <- data.frame(n0=updateLuckN(n0vec, n(data(setpos2))),
                        Lower=updateLuckY(n0vec, y0(setpos2)[1], tau(data(setpos2)), n(data(setpos2))),
                        Upper=updateLuckY(n0vec, y0(setpos2)[2], tau(data(setpos2)), n(data(setpos2))))

paramsetdf <- rbind(data.frame(prioparam, Facet="no conflict", Item="Prior", Item2="Prior"),
                    data.frame(pos1param, Facet="no conflict", Item="Posterior 1", Item2="Posterior"),
                    data.frame(prioparam, Facet="prior-data conflict", Item="Prior", Item2="Prior"),
                    data.frame(pos2param, Facet="prior-data conflict", Item="Posterior 2", Item2="Posterior"))

# in two separate facets
paramset3 <- ggplot(paramsetdf) + geom_ribbon(aes(x=n0, ymin=Lower, ymax=Upper, group=Item2, colour=Item2, fill=Item2), alpha=0.3)
paramset3 <- paramset3 + pdcscale + pdcscale2 
paramset3 <- paramset3 + facet_grid(. ~ Facet) + xlab(expression(n^(0))) + ylab(expression(y^(0)))

pdf("figs/paramsets1-ipmu.pdf", width=6, height=3) # plot.margin: t r b f
print(paramset3 + nolegend)
dev.off()

#