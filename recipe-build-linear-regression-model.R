library(olsrr)

############################################################
# WORKFLOW
# 1. BUILD, SUMMARIZE MODEL
# 2. VARIABLE CONTRIBUTIONS: SHOULD WE ADD ANOTHER VARIABLE? 
############################################################

#####################################################################################
# 1. BUILD, SUMMARIZE MODEL
#####################################################################################

# summarizes residuals, ANOVA, parameter estimates in nice chart 
ols_regress(mpg ~ disp + hp + wt + qsec, data = mtcars)

# all subset regression: tests all 2^k possible subsets of the set of k potential independent variables
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
# ols_all_subset(model)
# view as plot: potential R^2, potential adjusted R^2, C(p), AIC, SBIC, SBC, 
#plot(ols_all_subset(model))

# best subset regression: select the subset of predictors that do the best at meeting some well-
#    defined objective criteria, having the largest R^2 value or the smallest MSE, Mallow's C(p) or AIC
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_best_subset(model)
# plot: show panel of fit criteria for the best subsets: R^2, Adj. R^2, C(p), AIC, SBIC, SBC
plot(ols_best_subset(model))

# Stepwise forward regression: Build regression model from a set of candidate predictor vars,
#   by entering predictors based on p-vals, in stepwise manner until there is no var left
#   to enter. The model will include all the candidate predictor vars. 
# - If details = TRUE, each step is displayed
model <- lm(y ~ ., data = surgical)
ols_step_forward(model)
#plot
# plot(ols_step_forward(model))
# show each step
#ols_step_forward(model, details = TRUE)

# Stepwise backward regression: Build regression model from set of candidate predictor vars,
#    by removing predictors based on p-values, in stepwise manner until there is no var left
#    to remove. 
# If details = TRUE, each step is displayed
model <- lm(y ~ ., data = surgical)
ols_step_backward(model)
#plot(ols_step_backward(model))
# detailed output:
#ols_step_backward(model, details = TRUE)

# Stepwise regression
# Build regression model from a set of candidate predictor variables by entering AND 
# removing predictors based on p-values, in a stepwise manner until there is no variable 
# left to enter or remove any more.
# - if details = TRUE, each step is displayed
model <- lm(y ~ ., data = surgical)
# summarize
ols_stepwise(model) 
# plot
plot(ols_stepwise(model))
# detailed output
# ols_stepwise(model, details = TRUE)

# Stepwise AIC Forward Regression: Build regression model from set of candidate predictors
#    by entering predictors based on Akaike Information Criteria (AIC), in a stepwise
#    manner until no vars left to enter.
# - If details = TRUE, each step displayed
model <- lm(y ~., data = surgical)
ols_stepaic_forward(model)
#plot(ols_stepaic_forward())
# ols_stepaic_forward(model, details = TRUE)

# Stepwise AIC Backward Regression
# Build regression model from a set of candidate predictor variables by removing predictors 
# based on Akaike Information Criteria, in a stepwise manner until there is no variable 
# left to remove any more.
# - If details = TRUE, each step displayed
model <- lm(y ~., data = surgical)
k <- ols_stepaic_backward(model)
# summary
k
# plot
plot(k)
# detailed output
# k <- ols_stepaic_backward(model, details = TRUE)

# Stepwise AIC Regression: Build regression model from a set of candidate predictor variables 
#   by entering and removing predictors based on Akaike Information Criteria (AIC), 
#   in a stepwise manner until there is no variable left to enter or remove any more. 
#   The model should include all the candidate predictor variables. 
# If details = TRUE, each step is displayed.
model <- lm(y ~., data = surgical)
ols_stepaic_both(model)
# plot(ols_stepaic_both(model))
# ols_stepaic_both(model, details = TRUE)

#####################################################################################
# 2. VARIABLE CONTRIBUTIONS: SHOULD WE ADD ANOTHER VARIABLE? 
# - Added variable plot: shows marginal importance of predictor variable X_k
# - Residual plus component plot: shows non-linearity in relationships between X & Y; 
#         suggests possible transformations for linearizing the data
#####################################################################################

# If the plot shows a non-random pattern, consider adding the new predictor to the model
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_rvsr_plot(model, mtcars$drat)

# -----------------------------------------------------------------------------------
# Added variable plot: Gives info on marginal importance of predictor var X_k
# - Shows the marginal importance of X_k in reducing residual variability
# Steps:
# - Regress Y, the response var of the model, on all predictors other than X, and store
#        the Y residuals
# - Regress X on all the other varibles included in the model (X residuals)
# - Make scatter plot of Y residuals and X residuals
# Interpret: A strong linear relationship in the added variable plot indicates increased
#     impotance of X to the model
# -----------------------------------------------------------------------------------
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_avplots(model)

# -----------------------------------------------------------------------------------
# RESIDUAL PLUS COMPONENT PLOT: SHOWS WHETHER ANY NON-LINEARITY IS PRESET IN THE 
#    RELATIONSHIP BETWEEN X & Y; SUGGESTS POSSIBLE TRANSFORMATIONS FOR LINEARIZING THE DATA
# CALCULATE:
# - Regress Y on all Vars including X; store the residuals (e)
# - Multiply e with regression coefficient
# - Construct scatter plot of eX and X
# -----------------------------------------------------------------------------------

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_rpc_plot(model)


#####################################################################################
