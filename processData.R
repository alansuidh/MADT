rm(list = ls())
load("Result.RData")

studyNumGE <- function(n) return(study.num >= n)
studyNumL <- function(n) return(study.num < n)
nonZeroCellGE <- function(n){
  res <- logical()
  for(i in 1:k){
    temp <- dt[index[i]:(index[i+1]-1),]
    c1 <- sum(temp[,"TP"]!=0)
    c2 <- sum(temp[,"FP"]!=0)
    c3 <- sum(temp[,"FN"]!=0)
    c4 <- sum(temp[,"TN"]!=0)
    if(c1<n|c2<n|c3<n|c4<n){
      res[i] <- F
    }else{
      res[i] <- T
    }
  }
  return(res)
}

res.logit$GE10 <- factor(studyNumGE(10))
res.probit$GE10 <- factor(studyNumGE(10))
res.cloglog$GE10 <- factor(studyNumGE(10))

case <- which(studyNumGE(3))
case1 <- which(studyNumGE(3)&studyNumL(10))
case2 <- which(studyNumGE(10))


res.logit.case <- res.logit[case,]
res.probit.case <- res.probit[case,]
res.cloglog.case <- res.cloglog[case,]
res.logit.case1 <- res.logit[case1,]
res.probit.case1 <- res.probit[case1,]
res.cloglog.case1 <- res.cloglog[case1,]
res.logit.case2 <- res.logit[case2,]
res.probit.case2 <- res.probit[case2,]
res.cloglog.case2 <- res.cloglog[case2,]

na.logit.case <- which(is.na(res.logit.case$mu0))
na.probit.case <- which(is.na(res.probit.case$mu0))
na.cloglog.case <- which(is.na(res.cloglog.case$mu0))
na.logit.case1 <- which(is.na(res.logit.case1$mu0))
na.probit.case1 <- which(is.na(res.probit.case1$mu0))
na.cloglog.case1 <- which(is.na(res.cloglog.case1$mu0))
na.logit.case2 <- which(is.na(res.logit.case2$mu0))
na.probit.case2 <- which(is.na(res.probit.case2$mu0))
na.cloglog.case2 <- which(is.na(res.cloglog.case2$mu0))

c(length(na.logit.case),length(na.probit.case),length(na.cloglog.case))/length(case)
c(length(na.logit.case1),length(na.probit.case1),length(na.cloglog.case1))/length(case1)
c(length(na.logit.case2),length(na.probit.case2),length(na.cloglog.case2))/length(case2)

na.any.case <- union(na.logit.case,union(na.probit.case,na.cloglog.case))
na.any.case1 <- union(na.logit.case1,union(na.probit.case1,na.cloglog.case1))
na.any.case2 <- union(na.logit.case2,union(na.probit.case2,na.cloglog.case2))

res.logit.narm.case <- res.logit.case[-na.any.case,]
res.probit.narm.case <- res.probit.case[-na.any.case,]
res.cloglog.narm.case <- res.cloglog.case[-na.any.case,]
res.logit.narm.case1 <- res.logit.case1[-na.any.case1,]
res.probit.narm.case1 <- res.probit.case1[-na.any.case1,]
res.cloglog.narm.case1 <- res.cloglog.case1[-na.any.case1,]
res.logit.narm.case2 <- res.logit.case2[-na.any.case2,]
res.probit.narm.case2 <- res.probit.case2[-na.any.case2,]
res.cloglog.narm.case2 <- res.cloglog.case2[-na.any.case2,]