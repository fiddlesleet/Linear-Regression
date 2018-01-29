library(olsrr)

############################################################
# WORKFLOW
# 1. DETECT OUTLIERS
# 2. MEASURES OF INFLUENCE DIAGNOSTICS
# 3. BUILD, SUMMARIZE MODEL
# 4. VARIABLE CONTRIBUTIONS: SHOULD WE ADD ANOTHER VARIABLE? 
############################################################

#####################################################################################
# 1. DETECT OUTLIERS
# - Studentized Residual Plot to detect outliers
# - Standardized Residual Plot to detect outliers
# - Deleted Studentized Residual vs Fitted Values Plot: Graph for detecting outliers
#####################################################################################

# -----------------------------------------------------------------------------------

## STUDENTIZED RESIDUAL PLOT: A PLOT TO DETECT OUTLIERS
# - More effective for detecting outlying Y observations than standardized residuals.
# - A studentized deleted residual, aka an externally studentized residual, is the 
#     deleted residual divided by its estimated standard deviation.
# - If an observation has a studentized residual that is larger than 3 (in absolute 
#     value), it is an outlier.
# -----------------------------------------------------------------------------------

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_srsd_plot(model)

# -----------------------------------------------------------------------------------

## STANDARDIZED RESIDUAL (AKA INTERNALLY STUDENTIZED) PLOT: A PLOT TO DETECT OUTLIERS
# - A standardized residual is the residual divided by the estimated standard deviation
# - Less effective for detecting outlying Y observations than studentized residuals.
# -----------------------------------------------------------------------------------

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_srsd_chart(model)

# -----------------------------------------------------------------------------------

## DELETED STUDENTIZED RESIDUAL vs. FITTED VALUES PLOT: Graph for detecting outliers
# -----------------------------------------------------------------------------------
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_dsrvsp_plot(model)


#####################################################################################
# 2. MEASURES OF INFLUENCE DIAGNOSTICS
# - Cook's D bar plot
# - DFBETAs Panel
# - Difference in Fits (DFFIT)
# - Studentized Residuals vs Leverage plot
# - Hadi plot
#####################################################################################

# -----------------------------------------------------------------------------------

## COOK'S D BAR BLOT: identify influential data points in the model
# -Takes into account both the x and y values of the observation, aka depends on both the
#   residual & the leverage
# Steps to compute:
# 1. Delete observations one at a time
# 2. refit the regression model on the remaining (n-1) observations
# 3. Examine how much all of the fitted values change when the ith observation is deleted
# -----------------------------------------------------------------------------------

# cook's d bar chart
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_cooksd_barplot(model)

# cook's d chart: chart of cook's distance to detect observations that strongly influence
#   fitted values of the model
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_cooksd_chart(model)

# -----------------------------------------------------------------------------------

## DFBETAs Panel: Measures the difference in each parameter estimate, with and 
#     without the influential point.
# -If there are n observations and k variables, there will be n*k DFBETAs.
# -Large DFBETA value indicates influential observation
# -Belsey, Kuh, and Welsch recommend 2 as a general cutoff value to indicate influential
#    observations, and 2/sqrt(n) as a size-adjusted cutoff
# -----------------------------------------------------------------------------------

# panel
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_dfbetas_panel(model)

# -----------------------------------------------------------------------------------

## DIFFERENCE IN FITS (DFFIT): USED TO IDENTIFY INFLUENTIAL DATA POINTS.
# - The scaled difference between the ith fitted value from the full data,
#     and the ith fitted value obtained by deleting the ith observation.
# - STEPS TO COMPUTE:
# - Delete observations one at a time
# - Refit the regression model on the remaining observations
# - Check how much the fitted values cahnge when the ith observation is deleted
# AN OBSERVATION IS "INFLUENTIAL" IF ITS DFFITS IS GREATER THAN:
# 2 * (sqrt(p + 1) / (n - p 0 1)); 
# where: p = # of predictors, including the intercept
#        n = n of observations
# -----------------------------------------------------------------------------------

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_dffits_plot(model)

# -----------------------------------------------------------------------------------

## STUDENTIZED RESIDUALS vs LEVERAGE PLOT: DETECT INFLUENTIAL OBSERVATIONS
# -----------------------------------------------------------------------------------
model <- lm(read ~ write + math + science, data = hsb)
ols_rsdlev_plot(model)

# -----------------------------------------------------------------------------------

## HADI PLOT: HADI'S MEASURE OF INFLUENCE BASED ON THE FACT INFLUENTIAL OBSERVATIONS
#     CAN BE PRESENT IN THE RESPONSE VARIABLE OR PREDICTORS, OR BOTH. 
# - Detects influential observation according to Hadi's measure
# -----------------------------------------------------------------------------------
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_hadi_plot(model)


#####################################################################################
# 3. BUILD, SUMMARIZE MODEL
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
# 4. VARIABLE CONTRIBUTIONS: SHOULD WE ADD ANOTHER VARIABLE? 
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
