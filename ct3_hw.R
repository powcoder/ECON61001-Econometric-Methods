########################################################################
###                        OLS inference I                           ###
########################################################################

# This line removes all variables from the workspace.
rm(list = ls()) 

#Set working directory according to folders on YOUR PC

# a) ########################################################################

# At first, we import the data again.
Data = read.csv('attend.csv')
# We use again the function 'lm'
model_2 = lm(termgpa ~ attend + priGPA + ACT, data = Data)

# Regression summary
summary(model_2)

# The coefficient on attend can be obtained from model_2.
beta_attend_2 = model_2$coefficients[2]

# The function 'vcov' returns the coefficient variance-covariance matrix of 
# a model. The estimated variance are the diagonal elements. To obtain the
# standard deviation of the coefficient on attend, we take the square root
# of the second diagonal element.
std_beta_attend_2 = sqrt(vcov(model_2)[2,2])

# The t-statistic is the given by
tstat = abs(beta_attend_2 - 0.02) / std_beta_attend_2

# The test statistic is t-distributed. The quantiles of the t distribution
# are obtained via the function 'qt'. As first argument, it takes the 
# probability, as second argument the degrees of freedom. We use a two-
# sided test, so we are interested in the 97.5% quantile.
n  = length(Data$termgpa)
k = 4
critval = qt(0.975, n - k)

# We reject the H0 that beta_attend = 0.02 if the test statistic is larger
# than the critical value
reject_H0 = tstat > critval

# b) ########################################################################
# The F tests for significance of the models test, whether the slope 
# parameters are jointly significant. The results can be read from the 
# regression summaries.
model_1 = lm(termgpa ~ attend, data = Data)

# Summary statistics for the model are obtained via the 'summary' function.
summary(model_1)$fstatistic
summary(model_2)$fstatistic
#The quantiles of the F distribution are obtained via the function 'qf'. 
#As first argument, it takes the probability, 
#as second and third arguments the degrees of freedom.
qf(0.95,1,678)
qf(0.95,3,676)

# c) ########################################################################
# install the car package first (!!!) 
#install.packages("car")
library(car)
# formulate the linear hypothesis in terms of the names you see in summary
myH0 = c("2*attend - ACT = 0")
linearHypothesis(model_2,myH0)

# d) ########################################################################
# attendance rate can be reformulated as
# attend =(termgpa -a - b_ACT*ACT - b_pGPA*priGPA)/b_attend
beta2 = model_2$coefficients
attend_hat = (3.5 - beta2[1] - beta2[4]*mean(Data$ACT) - beta2[3]*(mean(Data$priGPA)-1))/beta2[2]
# as you can see, predictions for "unusual" values of x may result in "unsual" forecasts

# e) ########################################################################

# Predictions can be made via the 'predict' function. As fist argument, 
# specify the model, as second argument the new data on which the predictions
# shall be based.
gpa_student1 = predict(model_2, newdata = data.frame(attend = 90, 
                                                     priGPA = 2.5, 
                                                     ACT = mean(Data$ACT)))
gpa_student2 = predict(model_2, newdata = data.frame(attend = 80, 
                                                     priGPA = 3.5, 
                                                     ACT = mean(Data$ACT)))

# f) ########################################################################
# SST is the same for both models as the dependent variable is the same
SST = sum((Data$termgpa - mean(Data$termgpa))^2)

# model 1
#note, average fitted value is the same as the average dependent variable
SSE_1 = sum((model_1$fitted.values - mean(Data$termgpa))^2)
SSR_1 = sum((model_1$residuals - mean(model_1$residuals))^2)
R2_1 = SSE_1/SST # 1 - SSR_1/SST

# model 2
SSE_2 = sum((model_2$fitted.values - mean(Data$termgpa))^2)
SSR_2 = sum((model_2$residuals - mean(model_2$residuals))^2)
R2_2 = SSE_2/SST # 1 - SSR_1/SST

# Based on the coefficient of determination the second model is preferred to the first model.
# However, remember, R2 increases by construction when you add more explanatory variables.
# See the proof in the lecture notes, Ch 2.

# g) ########################################################################

# Adjusted R2 takes into account the difference in the number of variables k

summary(model_1)$adj.r.squared
summary(model_2)$adj.r.squared

#compare to R2_1 and R2_2

# h) ########################################################################

# A scatterplot is obtained via the function 'plot'
plot(Data$attend, Data$termgpa, main = 'Performance and Attendence Rate', 
     xlab = 'Attendance Rate [%]', ylab = 'Performance (GPA)')

# Given a bivariate regression model, a regression line can be drawn using 
# the function 'abline'. The arguments 'col' and 'lwd' control color and 
# linewidth. The argument lty specifies the line type. It takes integers as 
# values.
abline(model_1, lty = 1, col = 'red', lwd = 2)

# for the predictions of model 2, we create a sequence for 'attend' from 0 to
# 100 using the function 'seq'. The third argument specifies the step size.
x = seq(0, 100, 0.1)

# For simplicity, we store the estimated coefficients as 'beta2'
beta2 = model_2$coefficients

# using the 'lines' function, we add a regression line for model 2.
lines(x, beta2[1] + beta2[2]*x + beta2[3]*mean(Data$priGPA) + 
        beta2[4]*mean(Data$ACT), lty = 2, col = 'blue', lwd = 2)

# Finally, we add a legend. x and y specify the position, 'legend' the text
# and lty, col and lwd the line types, colors and widths.
legend(x = 5, y = 3.9, 
       legend = c("Predictions based on Model 1", 
                  "Predictions based on Model 2 (priGPa and ACT at the mean)"), 
       lty = c(1,2), col = c('red', 'blue'), lwd = c(2,2))


########################################################################
###                       OLS inference II                           ###
########################################################################

# for the interpretations see a separate pdf

# This line removes all variables from the workspace.
rm(list = ls()) 
library(wooldridge)
### (i)
data("hprice1")
help(hprice1)

ols = lm(lprice~bdrms+sqrft,data = hprice1)
res =summary(ols)

# hint: quantiles of a t-distribution
qt(0.975,85)

### (ii)
qt(0.95,85)

###(iii)
#t-statistic
t=(res$coefficients[2,1]-0.05)/res$coefficients[2,2]

#2-sided p-value 
2*pt(t,85)
# or
2*(1-pt(abs(t),85))


###(iv)
#t-statistic
t=(res$coefficients[3,1]-0.0005)/res$coefficients[3,2]

#1-sided p-value 
pt(t,85)
#or
1-pt(abs(t),85)


