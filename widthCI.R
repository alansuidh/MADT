source("processData.R")

res.logit.narm.case1$Se.wid <- res.logit.narm.case1$Se.ub-res.logit.narm.case1$Se.lb
res.probit.narm.case1$Se.wid <- res.probit.narm.case1$Se.ub-res.probit.narm.case1$Se.lb
res.cloglog.narm.case1$Se.wid <- res.cloglog.narm.case1$Se.ub-res.cloglog.narm.case1$Se.lb
res.logit.narm.case1$Sp.wid <- res.logit.narm.case1$Sp.ub-res.logit.narm.case1$Sp.lb
res.probit.narm.case1$Sp.wid <- res.probit.narm.case1$Sp.ub-res.probit.narm.case1$Sp.lb
res.cloglog.narm.case1$Sp.wid <- res.cloglog.narm.case1$Sp.ub-res.cloglog.narm.case1$Sp.lb

Se.mean.case1 <- (res.logit.narm.case1$Se+res.probit.narm.case1$Se+res.cloglog.narm.case1$Se)/3
Sp.mean.case1 <- (res.logit.narm.case1$Sp+res.probit.narm.case1$Sp+res.cloglog.narm.case1$Sp)/3

ptg.wid.Se.probit.case1 <- log(res.probit.narm.case1$Se.wid/res.logit.narm.case1$Se.wid)
ptg.wid.Se.cloglog.case1 <- log(res.cloglog.narm.case1$Se.wid/res.logit.narm.case1$Se.wid)
ptg.wid.Sp.probit.case1 <- log(res.probit.narm.case1$Sp.wid/res.logit.narm.case1$Sp.wid)
ptg.wid.Sp.cloglog.case1 <- log(res.cloglog.narm.case1$Sp.wid/res.logit.narm.case1$Sp.wid)

ind1 <- which(ptg.wid.Se.probit.case1 < -log(2)|ptg.wid.Se.probit.case1 > log(2))
ind2 <- which(ptg.wid.Se.cloglog.case1 < -log(2)|ptg.wid.Se.cloglog.case1 > log(2))
ind <- union(ind1,ind2)

plot(Se.mean.case1[-ind],exp(ptg.wid.Se.probit.case1[-ind]),pch=19,cex=0.3,log = 'y',
     ylim = c(3/4,4/3),col="red", main = "Meta-analyses with 3-9 Studies",
     xlab = "Average Sensitivity of 3 Links",
     ylab = "Percentage of Width of 95% CI of Sensitivity Compared with Logit Link")
lw <- loess(ptg.wid.Se.probit.case1[-ind] ~ Se.mean.case1[-ind])
j <- order(Se.mean.case1[-ind])
lines(Se.mean.case1[-ind][j],exp(lw$fitted[j]) ,col="red",lwd=2)
abline(h = 1, lwd = 2)

points(Se.mean.case1[-ind],exp(ptg.wid.Se.cloglog.case1[-ind]),pch=19,cex=0.3,col="blue")
lw <- loess(ptg.wid.Se.cloglog.case1[-ind] ~ Se.mean.case1[-ind])
j <- order(Se.mean.case1[-ind])
lines(Se.mean.case1[-ind][j],exp(lw$fitted[j]) ,col="blue",lwd=2)
legend(x = "topl", legend = c("probit","cloglog"), fill = c("red","blue"))


ind1 <- which(ptg.wid.Sp.probit.case1 < -log(2)|ptg.wid.Sp.probit.case1 > log(2))
ind2 <- which(ptg.wid.Sp.cloglog.case1 < -log(2)|ptg.wid.Sp.cloglog.case1 > log(2))
ind <- union(ind1,ind2)

plot(Sp.mean.case1[-ind],exp(ptg.wid.Sp.probit.case1[-ind]),pch=19,cex=0.3,log = 'y',
     ylim = c(3/4,4/3),col="red", main = "Meta-analyses with 3-9 Studies",
     xlab = "Average Specificity of 3 Links",
     ylab = "Percentage of Width of 95% CI of Specificity Compared with Logit Link")
lw <- loess(ptg.wid.Sp.probit.case1[-ind] ~ Sp.mean.case1[-ind])
j <- order(Sp.mean.case1[-ind])
lines(Sp.mean.case1[-ind][j],exp(lw$fitted[j]) ,col="red",lwd=2)
abline(h=1, lwd = 2)

points(Sp.mean.case1[-ind],exp(ptg.wid.Sp.cloglog.case1[-ind]),pch=19,cex=0.3,col="blue")
lw <- loess(ptg.wid.Sp.cloglog.case1[-ind] ~ Sp.mean.case1[-ind])
j <- order(Sp.mean.case1[-ind])
lines(Sp.mean.case1[-ind][j],exp(lw$fitted[j]) ,col="blue",lwd=2)
legend(x = "topl", legend = c("probit","cloglog"), fill = c("red","blue"))





res.logit.narm.case2$Se.wid <- res.logit.narm.case2$Se.ub-res.logit.narm.case2$Se.lb
res.probit.narm.case2$Se.wid <- res.probit.narm.case2$Se.ub-res.probit.narm.case2$Se.lb
res.cloglog.narm.case2$Se.wid <- res.cloglog.narm.case2$Se.ub-res.cloglog.narm.case2$Se.lb
res.logit.narm.case2$Sp.wid <- res.logit.narm.case2$Sp.ub-res.logit.narm.case2$Sp.lb
res.probit.narm.case2$Sp.wid <- res.probit.narm.case2$Sp.ub-res.probit.narm.case2$Sp.lb
res.cloglog.narm.case2$Sp.wid <- res.cloglog.narm.case2$Sp.ub-res.cloglog.narm.case2$Sp.lb

Se.mean.case2 <- (res.logit.narm.case2$Se+res.probit.narm.case2$Se+res.cloglog.narm.case2$Se)/3
Sp.mean.case2 <- (res.logit.narm.case2$Sp+res.probit.narm.case2$Sp+res.cloglog.narm.case2$Sp)/3

ptg.wid.Se.probit.case2 <- log(res.probit.narm.case2$Se.wid/res.logit.narm.case2$Se.wid)
ptg.wid.Se.cloglog.case2 <- log(res.cloglog.narm.case2$Se.wid/res.logit.narm.case2$Se.wid)
ptg.wid.Sp.probit.case2 <- log(res.probit.narm.case2$Sp.wid/res.logit.narm.case2$Sp.wid)
ptg.wid.Sp.cloglog.case2 <- log(res.cloglog.narm.case2$Sp.wid/res.logit.narm.case2$Sp.wid)

ind1 <- which(ptg.wid.Se.probit.case2 < -log(2)|ptg.wid.Se.probit.case2 > log(2))
ind2 <- which(ptg.wid.Se.cloglog.case2 < -log(2)|ptg.wid.Se.cloglog.case2 > log(2))
ind <- union(ind1,ind2)

plot(Se.mean.case2[-ind],exp(ptg.wid.Se.probit.case2[-ind]),pch=19,cex=0.3,log = 'y',
     ylim = c(3/4,4/3),col="red", main = "Meta-analyses with over 10 Studies",
     xlab = "Average Sensitivity of 3 Links",
     ylab = "Percentage of Width of 95% CI of Sensitivity Compared with Logit Link")
lw <- loess(ptg.wid.Se.probit.case2[-ind] ~ Se.mean.case2[-ind])
j <- order(Se.mean.case2[-ind])
lines(Se.mean.case2[-ind][j],exp(lw$fitted[j]) ,col="red",lwd=2)
abline(h = 1, lwd = 2)

points(Se.mean.case2[-ind],exp(ptg.wid.Se.cloglog.case2[-ind]),pch=19,cex=0.3,col="blue")
lw <- loess(ptg.wid.Se.cloglog.case2[-ind] ~ Se.mean.case2[-ind])
j <- order(Se.mean.case2[-ind])
lines(Se.mean.case2[-ind][j],exp(lw$fitted[j]) ,col="blue",lwd=2)
legend(x = "topl", legend = c("probit","cloglog"), fill = c("red","blue"))


ind1 <- which(ptg.wid.Sp.probit.case2 < -log(2)|ptg.wid.Sp.probit.case2 > log(2))
ind2 <- which(ptg.wid.Sp.cloglog.case2 < -log(2)|ptg.wid.Sp.cloglog.case2 > log(2))
ind <- union(ind1,ind2)

plot(Sp.mean.case2[-ind],exp(ptg.wid.Sp.probit.case2[-ind]),pch=19,cex=0.3,log = 'y',
     ylim = c(3/4,4/3),col="red", main = "Meta-analyses with over 10 Studies",
     xlab = "Average Specificity of 3 Links",
     ylab = "Percentage of Width of 95% CI of Specificity Compared with Logit Link")
lw <- loess(ptg.wid.Sp.probit.case2[-ind] ~ Sp.mean.case2[-ind])
j <- order(Sp.mean.case2[-ind])
lines(Sp.mean.case2[-ind][j],exp(lw$fitted[j]) ,col="red",lwd=2)
abline(h=1, lwd = 2)

points(Sp.mean.case2[-ind],exp(ptg.wid.Sp.cloglog.case2[-ind]),pch=19,cex=0.3,col="blue")
lw <- loess(ptg.wid.Sp.cloglog.case2[-ind] ~ Sp.mean.case2[-ind])
j <- order(Sp.mean.case2[-ind])
lines(Sp.mean.case2[-ind][j],exp(lw$fitted[j]) ,col="blue",lwd=2)
legend(x = "topl", legend = c("probit","cloglog"), fill = c("red","blue"))