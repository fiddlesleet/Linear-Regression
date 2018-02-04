library(ggplot2)

#################################
# AUTOMATICALLY CHECK ASSUMPTIONS
#################################
library(gvlma)
par(mfrow=c(2,2))  # draw 4 plots in same window
model <- lm(dist ~ speed, data=cars)
gvlma(model)
# from the normal QQ plot, we see points 23, 35, and 49 are marked as outliers.
# remove and rebuild model 
model <- lm(dist ~ speed, data=cars[-c(23, 35, 49), ])
gvlma(model)

####################
# ASSUMPTIONS 1-BY-1
####################

# Assumption 0: Normality of residuals
# Note: If Maximum Likelihood used instead of OLS, residual normality also implies the Ys & Xs are normally distributed
# Method: qqnorm plot
# Here, the QQNorm plot is in top right. If points lie exactly on the line, it is a perfectly
#    normal dist. However, some deviation to be expected, esp near the ends
par(mfrow=c(2,2))
model <- lm(dist ~ speed, data = cars)
plot(model) # deviation should be smaller than it is here

# Assumption 1: The mean of residuals is 0 (or very close)
model <- lm(dist ~ speed, data = cars)
mean(model$residuals) # -> ~ 0

# Assumption 2: Homoscedasticity of residuals, meaning they have = variance
par(mfrow=c(2,2))
model <- lm(mpg ~ disp, data = mtcars)
# Here, the 2 left plots show how the residuals vary as the fitted val increases.
# The lines should be approximately flat if the disturbances are homoscedastic
plot(model) # heteroscedastic: the lines aren't flat
# compare with
model <- lm(dist ~ speed, data = cars[1:20,])
plot(model) # homoscedasticity

# Assumption 3: No autocorrelation of residuals 
# Method 1: Visualize with ACF plot: The leftmost vertical line shows the correlation of the
#   residual with itself, so will always be 1. From there, the correlation from the next 
#   vertical line onward will drop to a near-0 value below the dashed blue significance level line.
data(economics)
model <- lm(pce ~ pop, data = economics)
acf(model$residuals) # autocorrelation: the vertical lines don't drop below the significance level line
# Method 2: Runs test for randomnes
library(lawstat)
lawstat::runs.test(model$residuals) # p ~ 0, so reject H_0 that it is random; accept H_A: definite pattern in the residuals
# Method 3: Use Durbin-Watson test
library(lmtest)
lmtest::dwtest(model) # accept H_A

# FIXING AUTO CORRELATION
# Add lag1 of residual as an X variable to the original model. 
library(DataCombine)
data(economics)
econ <- data.frame(economics, resid_mod1=model$residuals)
econ_1 <- slide(econ, Var = "resid_mod1", NewVar = "lag1", slideBy = -1)
econ_2 <- na.omit(econ_1)
model2 <- lm(pce ~ pop + lag1, data = econ_2)
# Check using ACF
acf(model2$residuals)
# runs test
runs.test(model2$residuals) # p-value = 0.3362. Can’t reject null hypothesis that it is random, so assume residuals are not autocorrelated.
lmtest::dwtest(model2) # p-value: 0.6672

# Assumption 5: x var and residuals are uncorrelated
# Method: run correlation test on x and residuals
model <- lm(dist ~ speed, data = cars)
cor.test(cars$speed, model$residuals) # p = 1 is high, so H_0 can't be rejected and assumption 6 holds true

# Assumption 6: The # of observations must be greater than number of Xs
# Method: Look at the data

# Assumption 7: Variability in X values is positive, meaning the X values in the given sample must 
#   not all be the same, or even close to the same
var(cars$speed) # 27.95918 > 0, so assumption holds true

# Assumption 8: No perfect multicollinearity between Xs
# Method: Use Variance Inflation Factor (VIF)
# - VIF computed for every X in the linear model
# - If VIF is high, means the info in that variable is already explained by other X vars in the model,
#       so this variable X is redundant
# - Want: VIF < 2, or at least X < 4
library(car)
model <- lm(mpg ~., data=mtcars)
vif(model)

# FIXING MULTICOLLINEARITY
# METHOD 1: Iteratively remove the X Var with the highest VIF
# remove disp, with VIF 21.62
model <- lm(mpg ~ cyl + hp + drat + wt + qsec + vs + am + gear + carb, data=mtcars)
vif(model)
# remove cyl, with VIF = 14.28
model <- lm(mpg ~ hp + drat + wt + qsec + vs + am + gear + carb, data=mtcars)
vif(model)
summary(model)
# remove hp, with VIF = 6.02
model <- lm(mpg ~ drat + wt + qsec + vs + am + gear + carb, data=mtcars)
vif(model)
summary(model) # adjusted R^2 = .8221 
# remove wt, with VIF = 5.10
model <- lm(mpg ~ drat + qsec + vs + am + gear + carb, data=mtcars)
vif(model)
summary(model) # # adjusted R^2 drops to 0.7735  
# remove gear, with VIF = 4.34
model <- lm(mpg ~ drat + qsec + vs + am  + carb, data=mtcars)
vif(model)
summary(model)
# remove qsec, with VIF = 3.73
model <- lm(mpg ~ drat + vs + am  + carb, data=mtcars)
vif(model)
summary(model)
# remove drat, with VIF = 2.62
model <- lm(mpg ~ vs + am  + carb, data=mtcars) # Now all VIF < 2, but adjusted R^2 has dropped to .7586
vif(model)
summary(model)

# Method 2: Check corr between all vars, keeping only one of all highly correlated pairs 
library(corrplot)
corrplot(cor(mtcars[, -1]))

