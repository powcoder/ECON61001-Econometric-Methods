# This line removes all variables from the workspace.
rm(list = ls()) 

#Set working directory according to folders on YOUR PC
#setwd("C:/Users/Ekaterina/Dropbox/! TEACHING/ECON61001")

# a) ########################################################################
# for replicability of the simulation results it is important to set the seed,
# or in other words to fix the random number generator
set.seed(42) # try different numbers here and you will see different results
n=500
x<-rnorm(n,5,4)
u = rnorm(n,0,0.5)
b0 = 1
b1 = -3

y = b0 + b1*x + u

# We want two plots next to each other, i.e. one row and two columns.
par(mfrow=c(1,2))

# First, we plot the scatter plot of y against x
plot(x, y, ylab = 'y',xlab = 'x', main = 'Data')

# In the second plot, we plot a histogram of the dependent variable.
hist(y, breaks = 20, xlab = 'y', main = 'y Distribution')
#dev.off()

# b) ########################################################################
# first define a funciton for the residual sum of squares
rss <- function(a0,a1){
  uhat = y-a0-a1*x
  ss = sum(uhat^2)
  return(ss)
}

#check whether it works
rss(1,2) 

# create a grid of parameter values for 3D plotting
a0 <- seq(-30, 30, length= 50)
a1 <- a0
z <- outer(a0, a1, Vectorize(rss))

# Library upload (not, you might need to install the plotly package first)
# install.packages("plotly")
library(plotly)

# Plot the RSS surface
axx <- list(title = "alpha 0")
axy <- list(title = "alpha 1")
axz <- list(title = "RSS")
p <- plot_ly(x = a0, y = a1, z = z, type = "surface")
p %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis = axz))

# c) ########################################################################

#Zoom in the parameter space
a0 <- seq(-5, 5, length= 50)
a1 <- a0

# residual sum of squares for different a0 values, fixing a1 = -3
rss_a0 <- matrix(0,50,1)
for (i in 1:50){
  rss_a0[i] = sum((y-a0[i]+3*x)^2)
}
dx = a0[-1] - a0[-50]
dy = rss_a0[-1] - rss_a0[-50]

# We want two plots next to each other, i.e. one row and two columns.
par(mfrow=c(1,2))
plot(a0[-1], dy/dx, type = 'l',col = "dark red",lwd = 3, ylab = 'derivative',xlab = 'a0', main = 'd RSS/d a0 for a1 = -3')
abline(h = 0)  
  
# residual sum of squares for different a0 values, fixing a0 = 1
rss_a1 <- matrix(0,50,1)
for (i in 1:50){
  rss_a1[i] = sum((y-1 - a1[i]*x)^2)
}
dx = a1[-1] - a1[-50]
dy = rss_a1[-1] - rss_a1[-50]

# We want two plots next to each other, i.e. one row and two columns.
plot(a1[-1], dy/dx, type = 'l',col = "dark red",lwd = 3,  ylab = 'derivative',xlab = 'a1', main = 'd RSS/d a1 for a0 = 1')
abline(h = 0)  

#dev.off()

# d) ########################################################################
# define a function which solves a bivariate OLS problem
ols<-function(y,x){
  ones <- rep(1,length(x))
  X = matrix(c(ones, x), ncol = 2)
  beta = solve(t(X) %*% X) %*% t(X) %*% as.matrix(y)
  return(beta)
}
# check whether it works
ols(y,x)

# simulate 1000 new datasets and save the estimated coefficients for all of them
beta<-matrix(0,2,1000)
for (i in 1:1000){
  set.seed(i)
  x<-rnorm(n,5,4)
  u = rnorm(n,0,0.5)
  y = b0 + b1*x + u
  beta[,i]<-ols(y,x)
}

# We want two plots next to each other, i.e. one row and two columns.
par(mfrow=c(1,2))
hist(beta[1,], breaks = 20, xlab = 'b0',main = 'Histogram of beta 0')
hist(beta[2,], breaks = 20, xlab = 'b1',main = 'Histogram of beta 1')
#dev.off()

# e) ########################################################################

# we now repeat d) for a different standard deviation of u

# simulate 1000 new datasets and save the estimated coefficients for all of them
beta<-matrix(0,2,1000)
for (i in 1:1000){
  set.seed(i)
  x<-rnorm(n,5,4)
  u = rnorm(n,0,10)
  y = b0 + b1*x + u
  beta[,i]<-ols(y,x)
}

# We want two plots next to each other, i.e. one row and two columns.
par(mfrow=c(1,2))
hist(beta[1,], breaks = 20, xlab = 'b0',main = 'Histogram of beta 0')
hist(beta[2,], breaks = 20, xlab = 'b1',main = 'Histogram of beta 1')
#dev.off()


