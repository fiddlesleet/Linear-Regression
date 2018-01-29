# STANDARD REGRESSION ASSUMPTIONS
# Here:
# 1. Normality assumption: The error has normal distribution
# 2. The errors have mean zero
# In other recipes:
# 3. Homoscedasticity/constant variance assumption: The errors have same but unknown variance
# 4. Independent errors: The errors are independent of each other

############################################################
# WORKFLOW
# 1. GENERATE PANEL OF ALL REGRESSION DIAGNOSTICS IN ONE GO
# 2. MODEL FIT DIAGNOSTICS: INTERPRETING CORRELATIONS, GOODNESS OF FIT
# 3. RESIDUAL DIAGNOSTICS
############################################################

#####################################################################################
# 1. GENERATE PANEL OF ALL REGRESSION DIAGNOSTICS IN ONE GO
#####################################################################################

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_diagnostic_panel(model)

#####################################################################################
# 2. MODEL FIT DIAGNOSTICS
# - Interpreting Correlations (zero order, part, partial)
# - Assessing Goodness of Fit
# - Lack of Fit F Test
#####################################################################################

# -----------------------------------------------------------------------------------
## ITERPRETING CORRELATIONS CHART
# - Correlation: Measures the relative importance of an independent var in determining Y.
#     Explains how much each variable contributes to the model's overall $^2, relative to
#     other vars. 
# - Zero Order: The Pearson Correlation Coeff. between dep. var and the indep. vars
# - Part: The unique contribution of an independent variable. How much will R^2 decrease if
#         that variable is removed from the model?
# - Partial: How much of the variance in Y, which is not estimated by the other independent
#           vars, is estimated by the specific variable? 
# -----------------------------------------------------------------------------------
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_correlations(model)

# -----------------------------------------------------------------------------------

## ASSESSING FIT: OBSERVED VS PREDICTED (FITTED) VALUES
# - Ideally, all points should be close to the regressed diagnola line
# - If the model has high R^2, then all points should be close to this line
# - The lower the R^2, the weaker the goodness of fit of the model
# -----------------------------------------------------------------------------------
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_ovsp_plot(model)

# -----------------------------------------------------------------------------------

## F TEST FOR LACK OF FIT: How much of the error is due to lack of model fit
# Calculate: Decompose residual sum of squares into parts due to:
# - lack of fit
# - random variation
# Interpretation: If most of the error is ue to lack of fit, not random error,
#    build a different model. 
# Note that this test only works with simple linear regression, over data at least one independent
#    variable has multiple y values. 
# -----------------------------------------------------------------------------------
model <- lm(mpg ~  disp, data = mtcars)
ols_pure_error_anova(model)


#####################################################################################
# 3. RESIDUAL DIAGNOSTICS
# - Check normality assumption of residuals
# - Histogram of Residuals
# - Residual vs Fitted Values plot
# - Residual fit spread plot
#####################################################################################

# -----------------------------------------------------------------------------------

## CHECK NORMALITY ASSUMPTION
# -----------------------------------------------------------------------------------

# plot residuals
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_rsd_qqplot(model)
# test residual normality
ols_norm_test(model)
# check correlation between observed residuals and expected residuals under normality
ols_corr_test(model)

# -----------------------------------------------------------------------------------

## Residual Histogram: detect violation of normality
# -----------------------------------------------------------------------------------
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_rsd_hist(model)

# -----------------------------------------------------------------------------------
## RESIDUAL VS. FITTED VALUES PLOT: Check if there is non-linearity, unknown error variance, or outliers
# Characteristics of a well behaved residual vs fitted plot:
# 1. The residuals spread randomly around the 0 line indicating that the relationship is linear.
# 2. The residuals form an approximate horizontal band around the 0 line indicating homogeneity of error variance.
# 3. No one residual is visibly away from the random pattern of the residuals indicating that there are no outliers.
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_rvsp_plot(model)

# -----------------------------------------------------------------------------------
# RESIDUAL FIT SPREAD PLOT: Detects non-linearity, influential observations & outliers
# - Side-by-side quantile plots show the centered fit and residuals
# - Shows how much variation in the data is explained by the fit, and how much variation
#    remains in the residuals
# - For inappropriate models, residuals' spread > centered fit's spread
# -----------------------------------------------------------------------------------
model <- lm(mpg ~ disp + wt + wt + qsec, data = mtcars)
ols_rfs_plot(model)


