
########################################################################
###                        WLLN and CLT                              ###
########################################################################
rm(list = ls()) # clean the global environment
dev.off() # erase all the previous plots
# press ctrl+L to erase text in console

# a) ########################################################################
# to get an idea about different distrubutions look at the THEORETICAL denisities
x = seq(-5,5,length = 1000) # normal and Student-t are defined on R
y = seq(0.001,5,length = 1000) # Chi2 and Gamma are defined on R+ only

par(mfrow=c(2,2))
plot(x, dnorm(x,mean = 3, sd = 1), type="l", lty=1, xlab="x value", ylab="Density", main="N(3,1)", lwd = 3)
plot(x, dt(x,5), type="l", lty=1, xlab="x value", ylab="Density", main="t(5)", lwd = 3)
plot(y,dchisq(y,2), type="l", lty=1, xlab="x value", ylab="Density", main="chi2(2)", lwd = 3)
plot(y,dgamma(y,1,2), type="l", lty=1, xlab="x value", ylab="Density", main="Gamma(1,2)", lwd = 3)

# b) ########################################################################
n = 10000
set.seed(42)
# simulate a N(3,1) random variable
u1 = rnorm(n,0,1)+3
# simulate a student t(5) random variable
u2 = rt(n,5)
# simulate a chi-squared (2) random variable
u3 = rchisq(n,2)
# simulate a Gamma(1,2) distribution
u4 = rgamma(n,1,2)

# create a function which computes a moving average sample mean
ma<-function(x){
  return(cumsum(x)/seq(1,length(x)))
}

# create 2x2 plots
par(mfrow=c(2,2))
# plot the sample average of N(3,1)
plot(ma(u1), type = "l", xlab = "n", ylab = "sample mean", col= "cyan4", lwd = 3, main = "N(3,1)")
# add the line for the theoretical mean
abline(h=3,lwd =2)

# plot the sample average of t(5)
plot(ma(u2), type = "l", xlab = "n", ylab = "sample mean", col= "violet", lwd = 3, main = "t(5)")
# add the line for the theoretical mean
abline(h=0,lwd =2)

# plot the sample average of chi2(2)
plot(ma(u3), type = "l", xlab = "n", ylab = "sample mean", col= "mediumpurple", lwd = 3, main = "chi2(2)")
# add the line for the theoretical mean
abline(h=2,lwd =2)

# plot the sample average of Gamma(1,2)
plot(ma(u4), type = "l", xlab = "n", ylab = "sample mean", col= "firebrick", lwd = 3, main = "Gamma(1,2)")
# add the line for the theoretical mean
abline(h=0.5,lwd =2)

#c) ########################################################################
# in order to verify CLT me need to characterize the whole distribution of 
# the sample mean

mu <-matrix(0,1000,4)
for (i in 1:1000){
  # simulate a N(3,1) random variable
  u1 = rnorm(n,0,1)+3
  # simulate a student t(5) random variable
  u2 = rt(n,5)
  # simulate a chi-squared (2) random variable
  u3 = rchisq(n,2)
  # simulate a Gamma(1,2) distribution
  u4 = rgamma(n,1,2)
  mu[i,] = c(mean(u1),mean(u2),mean(u3),mean(u4))
}
# collect theoretical means
mu0 = c(3,0,2,0.5)
# collect theoretical variances
s0 = c(1,5/3,3,0.25)

# simulated distribution of the stabilizing transformation
par(mfrow = c(1,1))
hist(sqrt(n)*(mu[,1]-mu0[1])/sqrt(s0[1]),main = "N(3,1)", xlab = "x",freq = F,col = "cornflowerblue")
lines(seq(-3,3,by = 0.2),dnorm(seq(-3,3,by = 0.2)),col = "blue",lwd = 3)

# now plot the histograms for all 4 distributions in a loop
# define the grid
par(mfrow = c(2,2))
#define the plot names in a vector to iterate over
nm = c("N(3,1)","t(5)","chi2(2)","Gamma(1,2)")
for (i in 1:4){
  hist(sqrt(n)*(mu[,i]-mu0[i])/sqrt(s0[i]),main = nm[i], xlab = "x",freq = F,col = "cornflowerblue")
  lines(seq(-3,3,by = 0.2),dnorm(seq(-3,3,by = 0.2)),col = "blue",lwd = 3)
}

#d) ########################################################################
# Bonus: Cauchy distribution
# simulate a student t(1) random variable
u5 = rt(n,1)
par(mfrow = c(1,1))
plot(ma(u5), type = "l", xlab = "n", ylab = "sample mean", col= "red", lwd = 3, main = "t(1)")
abline(h =0, lwd = 2)
# PS: try running the plot many times




