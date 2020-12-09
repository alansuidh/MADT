library(lme4)
metadt.glmm <- function(dt,link){
  num <- nrow(dt)
  study <- rep(1:num,each=2)
  trt <- rep(c(1,0),num)
  ctl <- rep(c(0,1),num)
  e <- numeric(2*num)
  n <- numeric(2*num)
  for(i in 1:num){
    e[2*i-1] <- as.numeric(dt[i,"TP"])
    n[2*i-1] <- as.numeric(dt[i,"TP"]+dt[i,"FN"])
  }
  for(i in 1:num){
    e[2*i] <- as.numeric(dt[i,"TN"])
    n[2*i] <- as.numeric(dt[i,"TN"]+dt[i,"FP"])
  }
  model <- glmer(cbind(e,(n-e))~factor(ctl)+((trt+ctl-1)|study), family = binomial(link))
  
  if(link == "logit"){
    back.trans <- function(x) plogis(x)
  }
  if(link == "probit"){
    back.trans <- function(x) pnorm(x)
  }
  if(link == "cloglog"){
    back.trans <- function(x) 1 - exp(-exp(x))
  }
  back.trans <- Vectorize(back.trans)
  
  mu0 <- summary(model)$coefficients[1,1]
  mu.se <- (summary(model)$coefficients[1,2])
  nu0 <- summary(model)$coefficients[1,1] + summary(model)$coefficients[2,1]
  nu.se <- sqrt((summary(model)$coefficients[1,2])^2+
                  (summary(model)$coefficients[2,2])^2+
                  2*summary(model)$vcov@factors$correlation[1,2]*
                  summary(model)$coefficients[1,2]*
                  summary(model)$coefficients[2,2])
  Se <- back.trans(mu0)
  Sp <- back.trans(nu0)
  Se.lb <- back.trans(mu0 - qnorm(0.975) * mu.se)
  Se.ub <- back.trans(mu0 + qnorm(0.975) * mu.se)
  Sp.lb <- back.trans(nu0 - qnorm(0.975) * nu.se)
  Sp.ub <- back.trans(nu0 + qnorm(0.975) * nu.se)
  sigma_mu <- sqrt(summary(model)$varcor$study[1,1])
  sigma_nu <- sqrt(summary(model)$varcor$study[2,2])
  rho <- summary(model)$varcor$study[1,2]/(sigma_mu*sigma_nu)
  res <- data.frame(mu0=mu0,nu0=nu0,sigma_mu=sigma_mu,sigma_nu=sigma_nu,
                    rho=rho,Se=Se,Se.lb=Se.lb,Se.ub=Se.ub,Sp=Sp,Sp.lb=Sp.lb,Sp.ub=Sp.ub)
  return(res)
}