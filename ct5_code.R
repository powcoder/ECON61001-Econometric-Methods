
########################################################################
###                         Time Series                              ###
########################################################################
rm(list = ls()) # clean the global environment
dev.off() # erase all the previous plots
# press ctrl+L to erase text in console

# a) ########################################################################

u1 = 0 + rnorm(1)
u2 = 0.1 + 0.8*u1 + rnorm(1)
u3 = 0.1 + 0.8*u2 + rnorm(1)

#simulate an autoregressive process of order one: AR(1)
T = 500 #length of the series
u = matrix(0,T,1) 
e  = rnorm(T) # white noise error term with mean 0 and std 1
# specify a starting value
u[1] = 0.1/(1-0.8) + e[1]
#comupte the rest of the process iteratively
for (t in 2:T){
  u[t] = 0.1 + 0.8*u[t-1] + e[t] 
}

#create a function which simulates an AR(1) process to save typing later
ar1  = function(c,rho, T){
  u = matrix(0,T,1) 
  e  = rnorm(T) # white noise error term with mean 0 and std 1
  # specify a starting value
  u[1] = c/(1-rho) + e[1]
  #comupte the rest of the process iteratively
  for (t in 2:T){
    u[t] = c + rho*u[t-1] + e[t]
  }
  return(u)
}

#plot the time series together with its unconstional mean c/(1-rho)
plot(u,type = "l",xlab = "time",lwd = 2, col = "darkgreen", main = "AR(1)")
abline(h = 0.1/(1-0.8),lwd = 2)

# autocovariance
# of order 1:
x1 = u[-T] #skip the last element
x2 = u[-1] #skip the first element
# Sigma t-1
cov(x1,x2)
# of order 2:
x1 = u[-c(T,T-1)] #skip the last 2 elements
x2 = u[-c(1:2)] #skip the first 2 elements
cov(x1,x2) # Sigma t-2

# ususally we work with autocorrelations:
cov(x1,x2)/var(u)

# a quick plot for Sample autocorrelations (autocovariances standardized by the variance)
acf(u)
# if the autocorrelation is above the confidence interval, 
# that means that it is significantly different from zero

# compare to the standard ols y = 1 - 3 x + u, x~N(5,4), u~N(0,10)
acf(1-3*rnorm(T,5,4)+rnorm(T,0,sqrt(10)))

# b) ########################################################################
# The model above can be implemented to examine Exmaple 4.3 from the lecture notes
# In order to find the Sigma matrix, we need to practice creating the autocorrelations:
n = 5
matrix(1:n, nrow = n, ncol = n, byrow = TRUE)
matrix(1:n, nrow = n, ncol = n, byrow = TRUE) - (1:n)
abs(matrix(1:n, nrow = n, ncol = n, byrow = TRUE) - (1:n) )
0.8^abs(matrix(1:n, nrow = n, ncol = n, byrow = TRUE) - (1:n))

# c) ########################################################################
# Simulate a bivariate regression model with AR(1) error
set.seed(42) # for replicability
T=500 # sample size
x<-rnorm(T,5,1) # note the second argument in simulating a random normal variable is std
u = ar1(0,0.8,T)
b0 = 1
b1 = -3
y = b0 + b1*x + u #simulated linear regression model
# Finding an OLS solution
Y = as.matrix(y)
ones = rep(1, T)
X = as.matrix(cbind(ones, x))
beta = solve( t(X) %*% X) %*% t(X) %*% Y
# Finding Sigma for autocorrelated errors
exponent <- abs(matrix(1:T, nrow = T, ncol = T, byrow = TRUE) - (1:T))
S = 0.8^exponent*(1/(1-0.8^2))
# Finding a GLS solution
beta2 = solve(t(X) %*% solve(S) %*% X) %*% t(X) %*% solve(S)%*% Y

# d) ########################################################################

mc = 1000
T = 100 #length of the series
bols = matrix(0,mc,1)
bgls= bols

# Finding Sigma for autocorrelated errors
exponent <- abs(matrix(1:T, nrow = T, ncol = T, byrow = TRUE) - (1:T))
S = 0.8^exponent*(1/(1-0.8^2))

for (i in 1:mc){
  set.seed(i)
  #simulate an autoregressive process of order one: AR(1)
  x<-rnorm(T,5,4) # note the second argument in simulating a random normal variable is std
  u = ar1(0,0.9,T)
  b0 = 1
  b1 = -3
  y = b0 + b1*x + u #simulated linear regression model
  # Finding an OLS solution
  Y = as.matrix(y)
  ones = rep(1, T)
  X = as.matrix(cbind(ones, x))
  beta = solve( t(X) %*% X) %*% t(X) %*% Y
  bols[i] = beta[2]
  # Finding a GLS solution
  beta2 = solve(t(X) %*% solve(S) %*% X) %*% t(X) %*% solve(S)%*% Y
  bgls[i] = beta2[2]
}
# plot the distribution
par(mfrow = c(1,2))

hist(bols,breaks = 30,freq = F, main = "Distribution of OLS", xlim = c(-3.15, -2.85))
# add the population parameter value
abline(v = -3,lwd = 2, col = "red")

hist(bgls,breaks = 30,freq = F, main = "Distribution of GLS", xlim = c(-3.15, -2.85))
# add the population parameter value
abline(v = -3,lwd = 2, col = "red")

# Here we clearle see that the OLS is not the most efficient estimator 






