############################################################
# WORKFLOW
# 1. GENERATE PANEL OF ALL REGRESSION DIAGNOSTICS IN ONE GO
# 2. MODEL FIT DIAGNOSTICS: INTERPRETING CORRELATIONS, GOODNESS OF FIT
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
