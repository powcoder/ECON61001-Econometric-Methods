########################
# homework: derivative

# This line removes all variables from the workspace.
rm(list = ls()) 

# generate data
x = seq(-10,10,by=0.5)
y = x^2

#compute differences
dx = x[-1] - x[-41]
dy = y[-1] - y[-41]

# compute derivative
d = dy/dx

#plot derivative
par(mfrow = c(1,1))
plot(x[-1],d,type='l',col = "dark red", lwd=5, xlab = 'x', ylab = 'dy/dx', main = 'Derivative')