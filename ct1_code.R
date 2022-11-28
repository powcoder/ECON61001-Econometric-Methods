# This line removes all variables from the workspace.
rm(list = ls()) 

#Set working directory according to folders on YOUR PC
#setwd("C:/Users/Ekaterina/Dropbox/! TEACHING/ECON61001")

# a) ########################################################################

#Make yourself familiar with the 'Help' documentation, either directly in RStudio or online.
#For example, get help about the function seq
help("seq")
# Online help about function plot:
#https://www.rdocumentation.org/packages/graphics/versions/3.6.1/topics/plot

# b) ########################################################################
# Step 1: create x, which is a sequence of numbers from -10 to 10 with a step of 0.5
x = seq(-10,10,by=0.5)
# Step 2: create y, y=x^2
y = x^2
# Step 3: plot a parabola
par(mfrow = c(1,1))
plot(x,y,type='l',col = "dark red", lwd=5, xlab = 'x', ylab = 'y', main = 'Parabola')

# c) ########################################################################
# Simulate a random normal vector
x = rnorm(n=1000, mean = 5, sd = sqrt(10))
hist(x, breaks = 20, xlab = 'x', main = 'Empirical Distribution of x')

# d) ########################################################################
# random sample of x
x1 = sample(x, 100, replace = FALSE)
mu1 = mean(x1)

# e) ########################################################################
# repeat d) a for loop, see mean distribution
mu = matrix(0,500,1)
for (i in 1:500){
  xi = sample(x, 100, replace = FALSE)
  mu[i] = mean(xi)
}
hist(mu, breaks = 20, xlab = 'mu', main = 'Empirical Distribution of mu')

# empirical variance of the distribution 
var(mu)

#theoretical variance of the distribution: sigma2/n
10/100


##################################################
# This line removes all variables from the workspace.
rm(list = ls()) 

# f) At first, we import the data.
Data = read.csv('attend.csv')

# g) ########################################################################

# The covariance matrix of all variables is obtained via the function 'cov'
cov_mat = cov(Data)

# The sample covariance between 'termgpa' and 'attend' can be read from the 
# covariance matrix. In this case, it is in the first row, fourth column.
cov_termgpa_attend = cov_mat[1,4]

# h) ########################################################################

# A linear model can be fit using the function 'lm'.
model_1 = lm(termgpa ~ attend, data = Data)

# Summary statistics for the model are obtained via the 'summary' function.
summary(model_1)

# i) ########################################################################

# The coefficients of a model are 
model_1_coefs = model_1$coefficients

# The slope parameter on 'attend' is the second element.
beta_attend_1 = model_1_coefs[2]

# The sample variance is obtained via the function 'var'
var_attend = var(Data$attend)
# Note that this is equal to the 4th diagonal element of the covariance 
# matrix 'cov_mat'.

# To compare 'beta_attend_1' with the ratio 'cov_termgpa_attend/var_attend',
# we print both.
print(beta_attend_1)
print(cov_termgpa_attend/var_attend)

# j) ########################################################################

# We use again the function 'lm'
model_2 = lm(termgpa ~ attend + ACT + priGPA, data = Data)

# Regression summary
summary(model_2)

# k) ########################################################################

# At first, we need to define the matrices 'X' and 'Y'. 'Y' is the nx1 vector 
# of 'termgpa'.
Y = as.matrix(Data$termgpa)

# The 'X' matrix contains the variables 'attend', 'priGPA' and 'ACT' and has 
# a column of ones at the beginning, which is for the intercept. The 
# dimensions of the nxk matrix are
n = length(Data$termgpa)
k = 4

# A vector of ones is obtained using the function 'rep'. As first argument
# we specify what to repeat and as second argument we specify how often.
ones = rep(1, n)

# The matrix is created via the function 'matrix'. 
X = matrix(c(ones, Data$attend, Data$ACT, Data$priGPA), ncol = k)

# The transpose of a matrix is obtained via the functionn 't'. The inverse is
# obtained via the function 'solve'. Then the OLS estimate for the parameters 
# is given by 
beta_2 = solve(t(X) %*% X) %*% t(X) %*% Y

# l) ########################################################################

# We want three plots next to each other, i.e. one row and three columns.
par(mfrow=c(1,3))

# First, we plot the predicted against the real values of 'termgpa'
plot(Data$termgpa, predict(model_2),
     ylab = 'Predicted Performance (termgpa)', 
     xlab = 'Actual Performance (termgpa)', main = 'Predicted vs. Real')

# In the second plot, we plot the residuals of the regression.
plot(Data$termgpa-predict(model_2),
     ylab = 'Residuals', main = 'Residuals')

# In the third plot, we plot a histogram of the residuals.
hist(Data$termgpa-predict(model_2), breaks = 20,
     xlab = 'termgpa residuals', main = 'Residual Distribution')
dev.off()




