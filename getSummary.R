rm(list = ls())
dt <- read.csv("dt.csv")

dt$sample.size <- dt$TP + dt$FP + dt$FN + dt$TN
n <- nrow(dt)

index.sr <- vector()
index.sr[1] <- 1
sr.num <- 1
for(i in 2:n){
  if(dt[i,1] != dt[i-1,1]){
    sr.num <- sr.num + 1
    index.sr[sr.num] <- i
  }
}
sr.size <- vector()
for(i in 1:(sr.num-1))  sr.size[i] <- dt[index.sr[i+1],2]-dt[index.sr[i],2]
sr.size[sr.num] <- dt[n,2] - dt[index.sr[sr.num],2] + 1



index.ma <- vector()
index.ma[1] <- 1
ma.num <- 1
for(i in 2:n){
  if(dt[i,2] != dt[i-1,2]){
    ma.num <- ma.num + 1
    index.ma[ma.num] <- i
  }
}
ma.size <- vector()
for(i in 1:(ma.num-1))  ma.size[i] <- index.ma[i+1] - index.ma[i]
ma.size[ma.num] <- n - index.ma[ma.num] + 1


study.num <- n
study.size <- dt$sample.size

## Print the Summaries

sr.num
summary(sr.size)

ma.num
summary(ma.size)

study.num
summary(study.size)
