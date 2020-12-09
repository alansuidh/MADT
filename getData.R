rm(list = ls())
dt <- read.csv("dt.csv")
# dt <- read.csv("dt.largest.csv")
dt <- dt[,-c(1,3:9)]
n <- nrow(dt)
index <- vector()
index[1] <- 1
k <- 1
for(i in 2:n){
  if(dt[i,1] != dt[i-1,1]){
    k <- k + 1
    index[k] <- i
  }
}
index[k+1] <- n + 1
study.num <- vector()
for(i in 1:k) study.num[i] <- index[i+1]-index[i]
dt <- dt[,-1]
rm(i)