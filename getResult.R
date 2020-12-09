source("getData.R")
source("MADTGLMM.R")

options(warn = 2)

res.logit <- metadt.glmm(dt[index[1]:(index[2]-1),],link = "logit")
for(i in 2:k){
  err <- F
  tryCatch(res.logit <- rbind(res.logit,metadt.glmm(dt[index[i]:(index[i+1]-1),],link = "logit")),
           error = function(e){err <<- T})
  if(err){
    res.logit <- rbind(res.logit,NA);
    next
  }
}

res.probit <- metadt.glmm(dt[index[1]:(index[2]-1),],link = "probit")
for(i in 2:k){
  err <- F
  tryCatch(res.probit <- rbind(res.probit,metadt.glmm(dt[index[i]:(index[i+1]-1),],link = "probit")),
           error = function(e){err <<- T})
  if(err){
    res.probit <- rbind(res.probit,NA);
    next
  }
}

res.cloglog <- metadt.glmm(dt[index[1]:(index[2]-1),],link = "logit")
for(i in 2:k){
  err <- F
  tryCatch(res.cloglog <- rbind(res.cloglog,metadt.glmm(dt[index[i]:(index[i+1]-1),],link = "cloglog")),
           error = function(e){err <<- T})
  if(err){
    res.cloglog <- rbind(res.cloglog,NA);
    next
  }
}
res.cloglog[1,] <- NA

options(warn = 1)

res.logit$study.num <- study.num
res.probit$study.num <- study.num
res.cloglog$study.num <- study.num

res.probit$id <- 1:k
res.logit$id <- 1:k
res.cloglog$id <- 1:k

rm(err)
rm(i)

save.image("~/Desktop/MADT/Result.RData")