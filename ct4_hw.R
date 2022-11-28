########################
# homework: 
rm(list = ls()) # clean the global environment
dev.off() # erase all the previous plots
# press ctrl+L to erase text in console

#######################################################################
b0 = 1
b1 = -3
n=10 # sample size
mc = 10000
beta = matrix(0,mc,2)
se = matrix(0,mc,2)
for (i in 1:mc){
  # simulate a linear regression model
  set.seed(i) # for replicability
  x = rnorm(n,5,4)
  u = rnorm(n,0,sqrt(10))
  
  y = b0 + b1*x + u #simulated linear regression model
  ols = lm(y~x)
  beta[i,] = ols$coefficients
  # compute the (X'X)^(-1)
  xx = solve(t(as.matrix(cbind(rep(1,n),x))) %*% as.matrix(cbind(rep(1,n),x)))
  # compute the teoretical value of the OLS variances (conditional on x)
  se[i,2] = sqrt(10*xx[2,2])
  se[i,1] = sqrt(10*xx[1,1])
}


y1 = (beta[,1]-b0)/se[,1]
y2 = (beta[,2]-b1)/se[,2]

par(mfrow = c(1,2))

hist(y1,breaks = 50,freq = F, main = "Normalized distribution of estimated intercept")
lines(seq(-5,5,by = 0.2),dnorm(seq(-5,5,by = 0.2)),col = "blue",lwd = 3)
lines(seq(-5,5,by = 0.2),dt(seq(-5,5,by = 0.2),n-2),col = "red",lwd = 3)
legend('topleft',legend = c("N(0,1)", "t(n-k)"), 
       lty = c(1,1), col = c('blue','red'), lwd = c(2,2))

hist(y2,breaks = 50,freq = F, main = "Normalized distribution of estimated slope")
lines(seq(-5,5,by = 0.2),dnorm(seq(-5,5,by = 0.2)),col = "blue",lwd = 3, )
lines(seq(-5,5,by = 0.2),dt(seq(-5,5,by = 0.2),n-2),col = "red",lwd = 3)
legend('topleft',legend = c("N(0,1)", "t(n-k)"), 
       lty = c(1,1), col = c('blue','red'), lwd = c(2,2))

#######################################################################
b0 = 1
b1 = -3
n=10 # sample size
mc = 10000
beta = matrix(0,mc,2)
se = matrix(0,mc,2)
for (i in 1:mc){
  # simulate a linear regression model
  set.seed(i) # for replicability
  x = rnorm(n,5,4)
  u = rnorm(n,0,sqrt(10))
  
  y = b0 + b1*x + u #simulated linear regression model
  ols = lm(y~x)
  beta[i,] = ols$coefficients
  # compute the (X'X)^(-1)
  xx = solve(t(as.matrix(cbind(rep(1,n),x))) %*% as.matrix(cbind(rep(1,n),x)))
  # compute the teoretical value of the OLS variances (conditional on x)
  suhat = sum(ols$residuals^2)/(n-2)
  se[i,2] = sqrt(suhat*xx[2,2])
  se[i,1] = sqrt(suhat*xx[1,1])
}


y1 = (beta[,1]-b0)/se[,1]
y2 = (beta[,2]-b1)/se[,2]

par(mfrow = c(1,2))

hist(y1,breaks = 50,freq = F, main = "Normalized distribution of estimated intercept")
lines(seq(-5,5,by = 0.2),dnorm(seq(-5,5,by = 0.2)),col = "blue",lwd = 3)
lines(seq(-5,5,by = 0.2),dt(seq(-5,5,by = 0.2),n-2),col = "red",lwd = 3)
legend('topleft',legend = c("N(0,1)", "t(n-k)"), 
       lty = c(1,1), col = c('blue','red'), lwd = c(2,2))

hist(y2,breaks = 50,freq = F, main = "Normalized distribution of estimated slope")
lines(seq(-5,5,by = 0.2),dnorm(seq(-5,5,by = 0.2)),col = "blue",lwd = 3, )
lines(seq(-5,5,by = 0.2),dt(seq(-5,5,by = 0.2),n-2),col = "red",lwd = 3)
legend('topleft',legend = c("N(0,1)", "t(n-k)"), 
       lty = c(1,1), col = c('blue','red'), lwd = c(2,2))

#######################################################################
b0 = 1
b1 = -3
n=5000 # sample size
mc = 10000
beta = matrix(0,mc,2)
se = matrix(0,mc,2)
for (i in 1:mc){
  # simulate a linear regression model
  set.seed(i) # for replicability
  x = rnorm(n,5,4)
  u = rnorm(n,0,sqrt(10))
  
  y = b0 + b1*x + u #simulated linear regression model
  ols = lm(y~x)
  beta[i,] = ols$coefficients
  # compute the (X'X)^(-1)
  xx = solve(t(as.matrix(cbind(rep(1,n),x))) %*% as.matrix(cbind(rep(1,n),x)))
  # compute the teoretical value of the OLS variances (conditional on x)
  suhat = sum(ols$residuals^2)/(n-2)
  se[i,2] = sqrt(suhat*xx[2,2])
  se[i,1] = sqrt(suhat*xx[1,1])
}


y1 = (beta[,1]-b0)/se[,1]
y2 = (beta[,2]-b1)/se[,2]

par(mfrow = c(1,2))

hist(y1,breaks = 50,freq = F, main = "Normalized distribution of estimated intercept")
lines(seq(-5,5,by = 0.2),dnorm(seq(-5,5,by = 0.2)),col = "blue",lwd = 3)
lines(seq(-5,5,by = 0.2),dt(seq(-5,5,by = 0.2),n-2),col = "red",lwd = 3)
legend('topleft',legend = c("N(0,1)", "t(n-k)"), 
       lty = c(1,1), col = c('blue','red'), lwd = c(2,2))

hist(y2,breaks = 50,freq = F, main = "Normalized distribution of estimated slope")
lines(seq(-5,5,by = 0.2),dnorm(seq(-5,5,by = 0.2)),col = "blue",lwd = 3, )
lines(seq(-5,5,by = 0.2),dt(seq(-5,5,by = 0.2),n-2),col = "red",lwd = 3)
legend('topleft',legend = c("N(0,1)", "t(n-k)"), 
       lty = c(1,1), col = c('blue','red'), lwd = c(2,2))
