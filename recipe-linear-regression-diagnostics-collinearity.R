library(olsrr)

# STANDARD REGRESSION ASSUMPTIONS
# Here: 
# 4. Independent errors: The errors are independent of each other
# In other recipes: 
# 1. Normality assumption: The error has normal distribution
# 2. The errors have mean zero
# 3. Homoscedasticity/constant variance assumption: The errors have same but unknown variance

############################################################
# WORKFLOW
# 1. COLLINEARITY DIAGNOSTICS
############################################################

#####################################################################################
# 1. COLLINEARITY DIAGNOSTICS
# - Variable Inflation Factors (VIF) & Tolerance
# - Condition Index
# - Diagnose collinearity
#####################################################################################

# - Collinearity implies 2 vars are near perfect linear combinations of one another
# MULTICOLLINEARITY: involves >2 vars
# - In the presence of multicollinearity, regresson estimates are unstable, and have
#     high standard errors

# -----------------------------------------------------------------------------------

## VARIABLE INFLATION FACTORS (VIF) & Tolerance: Measure inflation in parameter 
#      varience caused by collinearities among the predictors
# - Measures how much Beta_k is inflated by correlation among predictors in the model
# - VIF = 1: no correlation between kth predictor and the other predictors, so Beta_k
#            is not inflated at all
# Rule of thumb: If VIF > 4, warrants investigation; 
#                If VIF > 10, means serious multicollinearity requiring correction
# How to Calculate: 
# - Regress the kth predictor over the rest of the predictors in the model
# - Compute {R^2}_k
# Formula: VIF = 1 / (1 - {R^2}_k) = 1 / tolerance
# Tolerance: percent of variance in the predictor that can't be accounted for by other predictors
#           Tolerance = 1 - {R^2}_k
# -----------------------------------------------------------------------------------
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_vif_tol(model)

# -----------------------------------------------------------------------------------

## CONDITION INDEX: Decompose a correlation matrix into linear combinations of vars
# - Choose the linear combos so the first has the largest possible variance, the second
#    has the second largest possible variance (subject to being uncorrelated with the first),
#    the third has the next largest possible variance (subject to being uncorrelated with
#    the first and second), and so on....
# - The variance of each selected linear combo is the eigenvalue
# - Collinearity is spotted by finding 2+ variables that have >= half their variance
#     that corresponds to large condition indices 
# RULE OF THUMB: Condition indicies >= 30 are 'large'
# -----------------------------------------------------------------------------------
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_eigen_cindex(model)

# -----------------------------------------------------------------------------------
## Diagnose Collinearity
# -----------------------------------------------------------------------------------
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_coll_diag(model)