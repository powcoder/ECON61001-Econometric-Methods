########################
# homework: Proposition 2.1

# This line removes all variables from the workspace.
rm(list = ls()) 

n=1000
b = 1

# simulate 1000 draws and save the two estimates
beta<-matrix(0,2,1000)
for (i in 1:1000){
  set.seed(i)
  x<-rnorm(n,5,10)
  u = rnorm(n,0,10)
  y =  b*x + u
  # beta hat
  beta[1,i]<-sum(x*y)/sum(x^2)
  #beta tilde
  beta[2,i]<-mean(y)/mean(x)
}

# Illustrate unbiasedness with the help of histogram
par(mfrow=c(1,2))
hist(beta[1,], breaks = 20, xlab = 'b hat',main = 'Histogram of beta hat')
abline(v=1, col="red", lwd=3)
hist(beta[2,], breaks = 20, xlab = 'b tilde',main = 'Histogram of beta tilde')
abline(v=1, col="red", lwd=3)
#dev.off()

# Efficiency of the OLS estimator
print('Is beta hat more efficient than beta tilde?')
var(beta[1,]) < var(beta[2,])
