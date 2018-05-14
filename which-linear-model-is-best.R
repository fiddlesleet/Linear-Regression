library(leaps)

model_compare <- regsubsets(hp ~., data = mtcars)
summary(model_compare)
plot(model_compare, scale = 'adjr2') # adjusted R^2 

# build the top options
lm1 <- lm(hp ~ mpg + disp + wt + carb, data = mtcars)
lm2 <- lm(hp ~ mpg + disp + wt + vs + carb, data = mtcars)

# compare the top options' Mean Squared Error (MSE). Want lowest value. 
mean(lm1$residuals^2)
mean(lm2$residuals^2)

# compare the top options' AIC, also looking for lowest value
library(MASS)
s1 <- stepAIC(lm1, direction = "both")
s1$anova
