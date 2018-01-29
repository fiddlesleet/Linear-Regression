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
# 1. REVIEW: COLLINEARITY DEFINITIONS
# 2. REVIEW: COLLINEARITY CONSEQUENCES
# 3. REVIEW: HOW TO DETECT COLLINEARITY
# 4. COLLINEARITY DIAGNOSTICS
############################################################

#####################################################################################

## 1. COLLINEARITY DEFINITIONS
# - Collinearity implies 2 vars are near perfect linear combinations of one another
# - MULTICOLLINEARITY: involves >2 vars
# - Occurs when 2+ independent variables are approximately linearly related
#####################################################################################


#####################################################################################

## 2. COLLINEARITY CONSEQUENCES
# - In the presence of multicollinearity, regresson estimates are unstable, and have
#     high standard errors, meaning:
# - OLS estimators are still unbiased, but may have large variances and covariance,
#     making precise estimation difficult. So, confidence intervals tend to be wider.
# - t ratios of 1+ coeffs tend to be statistically insignificant
# - even though some regression coeffs are statistically insignificant, R^2 may be very high
# - The OLS estimators and their standard errors can be sensitive to small chances in data

#####################################################################################

#####################################################################################

## 3. REVIEW: HOW TO DETECT COLLINEARITY
# - Check corr. between each pair of independent vars. High corrs may be source of 
#       multicollinearity. However, pair-wise correlation is sufficient, but not necessary.
# - Multiple regressions with high R^2 but most coeffs with low p-val may suggest collinearity.
# - Use VIF: the larger the VIF of X_j, the more collinear X_j is
#           - if VIF > 10, then R^2 of X_j > .9, and X_j is highly colinear
# - Farrar-Glauber 3-part test (F-G test):
#       1. A Chi-square test for whether the Xs are orthogonal 
#       2. F test for the location of multicollinearity 
#       3. t-test for the pattern of multicollinearity 
#
#####################################################################################


#####################################################################################
# 4. COLLINEARITY DIAGNOSTICS
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
