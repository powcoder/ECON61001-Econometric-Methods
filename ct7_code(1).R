
########################################################################
###                     Instrumental Variables                       ###
########################################################################
rm(list = ls()) # clean the global environment
dev.off() # erase all the previous plots
# press ctrl+L to erase text in console

# a) ########################################################################
library(wooldridge)
data("wage2")
#help(wage2)

# ols regression
ols = lm(lwage~educ, data=wage2)
summary(ols)
# education is likely to be endogenous

# regressing wage on the number of siblings
ols1= lm(lwage~sibs, data=wage2)
summary(ols1)
#It shows that, controlling for no other factors, one more sibling in the family
#is associated with monthly salary that is about 2.8% lower. The t statistic on
#sibs is about -4.73 (significantly negative). 

# iv regression
#install.packages("AER")
library(AER)
iv=ivreg(lwage~educ|sibs, data = wage2)
summary(iv)


# b) ########################################################################
#It could be that older children are given priority for higher education, and
#families may hit budget constraints and may not be able to afford as much
# for children born later
summary(lm(educ~brthord,data=wage2))
#The equation predicts that every one-unit increase in brthord 
#reduces predicted education by about .28 years. 
#In particular, the difference in predicted education for a first-born and 
# a fourth-born child is about .85 years.

# c) ########################################################################
iv2=ivreg(lwage~educ|brthord, data = wage2)
summary(iv2)
#This is much higher than the OLS estimate (.060) and 
# even above the estimate when sibs is used as an IV for educ (.122).
# Because of missing data on brthord, we are using fewer observations than in the previous analyses.

#compare to ols and iv with sibs
summary(ols)$coefficients
summary(iv)$coefficients

# d) ########################################################################
summary(lm(educ~sibs+brthord,data=wage2))

#The t-statistic is about -2.67, which rejects H0 fairly strongly.
#Therefore we conclude that the identification assumptions appears to hold.
# e) ########################################################################

iv3=ivreg(lwage~educ+sibs|brthord+sibs, data = wage2)
summary(iv3)
#95% CI for beta(educ)
confint(iv3)
#is very wide and include the value zero

# f) ########################################################################
ols2 = lm(educ~sibs+brthord,data=wage2)
# fitted values
educ.hat = ols2$fitted.values
cor(educ.hat,wage2$sibs[!is.na(wage2$brthord)])

#This means that, for the purposes of using IV, multicollinearity is a serious problem
# here, and is not allowing us to estimate beta educ with much precision.



