source("processData.R")

library(BlandAltmanLeh)

ba.lp.Se <- bland.altman.stats(res.logit.narm.case$Se, res.probit.narm.case$Se)
plot(ba.lp.Se$means, ba.lp.Se$diffs, col=res.logit.narm.case$GE10, 
     main=expression('Se'['logit']*' vs. Se'['probit']),
     xlab = expression('(Se'['logit']*' + Se'['probit']*')/2'), 
     ylab = expression('Se'['logit']*' - Se'['probit']),pch = 19, cex=0.5)
abline(h = ba.lp.Se$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
abline(h = 0, lwd = 2)
legend(x = "topright", legend = c("3-9",">=10"), fill = 1:2)
lw <- loess(ba.lp.Se$diffs ~ ba.lp.Se$means)
j <- order(ba.lp.Se$means)
lines(ba.lp.Se$means[j],lw$fitted[j],col="green",lwd=2)


ba.lc.Se <- bland.altman.stats(res.logit.narm.case$Se, res.cloglog.narm.case$Se)
plot(ba.lc.Se$means, ba.lc.Se$diffs, col=res.logit.narm.case$GE10, 
     main=expression('Se'['logit']*' vs. Se'['cloglog']),
     xlab = expression('(Se'['logit']*' + Se'['cloglog']*')/2'), 
     ylab = expression('Se'['logit']*' - Se'['cloglog']),pch = 19, cex=0.5)
abline(h = ba.lc.Se$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
abline(h = 0, lwd = 2)
legend(x = "topright", legend = c("3-9",">=10"), fill = 1:2)
lw <- loess(ba.lc.Se$diffs ~ ba.lc.Se$means)
j <- order(ba.lc.Se$means)
lines(ba.lc.Se$means[j],lw$fitted[j],col="green",lwd=2)


ba.pc.Se <- bland.altman.stats(res.probit.narm.case$Se, res.cloglog.narm.case$Se)
plot(ba.pc.Se$means, ba.pc.Se$diffs, col=res.probit.narm.case$GE10, 
     main=expression('Se'['probit']*' vs. Se'['cloglog']),
     xlab = expression('(Se'['probit']*' + Se'['cloglog']*')/2'), 
     ylab = expression('Se'['probit']*' - Se'['cloglog']),pch = 19, cex=0.5)
abline(h = ba.pc.Se$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
abline(h = 0, lwd = 2)
legend(x = "topright", legend = c("3-9",">=10"), fill = 1:2)
lw <- loess(ba.pc.Se$diffs ~ ba.pc.Se$means)
j <- order(ba.pc.Se$means)
lines(ba.pc.Se$means[j],lw$fitted[j],col="green",lwd=2)




ba.lp.Sp <- bland.altman.stats(res.logit.narm.case$Sp, res.probit.narm.case$Sp)
plot(ba.lp.Sp$means, ba.lp.Sp$diffs, col=res.logit.narm.case$GE10, 
     main=expression('Sp'['logit']*' vs. Sp'['probit']),
     xlab = expression('(Sp'['logit']*' + Sp'['probit']*')/2'), 
     ylab = expression('Sp'['logit']*' - Sp'['probit']),pch = 19, cex=0.5)
abline(h = ba.lp.Sp$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
abline(h = 0, lwd = 2)
legend(x = "topright", legend = c("3-9",">=10"), fill = 1:2)
lw <- loess(ba.lp.Sp$diffs ~ ba.lp.Sp$means)
j <- order(ba.lp.Sp$means)
lines(ba.lp.Sp$means[j],lw$fitted[j],col="green",lwd=2)


ba.lc.Sp <- bland.altman.stats(res.logit.narm.case$Sp, res.cloglog.narm.case$Sp)
plot(ba.lc.Sp$means, ba.lc.Sp$diffs, col=res.logit.narm.case$GE10, 
     main=expression('Sp'['logit']*' vs. Sp'['cloglog']),
     xlab = expression('(Sp'['logit']*' + Sp'['cloglog']*')/2'), 
     ylab = expression('Sp'['logit']*' - Sp'['cloglog']),pch = 19, cex=0.5)
abline(h = ba.lc.Sp$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
abline(h = 0, lwd = 2)
legend(x = "topright", legend = c("3-9",">=10"), fill = 1:2)
lw <- loess(ba.lc.Sp$diffs ~ ba.lc.Sp$means)
j <- order(ba.lc.Sp$means)
lines(ba.lc.Sp$means[j],lw$fitted[j],col="green",lwd=2)


ba.pc.Sp <- bland.altman.stats(res.probit.narm.case$Sp, res.cloglog.narm.case$Sp)
plot(ba.pc.Sp$means, ba.pc.Sp$diffs, col=res.probit.narm.case$GE10, 
     main=expression('Sp'['probit']*' vs. Sp'['cloglog']),
     xlab = expression('(Sp'['probit']*' + Sp'['cloglog']*')/2'), 
     ylab = expression('Sp'['probit']*' - Sp'['cloglog']),pch = 19, cex=0.5)
abline(h = ba.pc.Sp$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
abline(h = 0, lwd = 2)
legend(x = "topright", legend = c("3-9",">=10"), fill = 1:2)
lw <- loess(ba.pc.Sp$diffs ~ ba.pc.Sp$means)
j <- order(ba.pc.Sp$means)
lines(ba.pc.Sp$means[j],lw$fitted[j],col="green",lwd=2)