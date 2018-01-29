library(olsrr)

# STANDARD REGRESSION ASSUMPTIONS
# Here:
# 1. Normality assumption: The error has normal distribution
# 2. The errors have mean zero
# In other recipes:
# 3. Homoscedasticity/constant variance assumption: The errors have same but unknown variance
# 4. Independent errors: The errors are independent of each other

############################################################
# WORKFLOW: RESIDUAL (ERROR) DIAGNOSTICS
# 1. REFERENCE: GENERATE PANEL OF ALL REGRESSION DIAGNOSTICS IN ONE GO
# 2. DO THE ERRORS (RESIDUALS) HAVE MEAN = 0?
# 3. ARE THE RESIDUALS NORMALLY DISTRUBUTED? 
############################################################

#####################################################################################
# 1. GENERATE PANEL OF ALL REGRESSION DIAGNOSTICS IN ONE GO
#####################################################################################

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_diagnostic_panel(model)

#####################################################################################
# 2. DO THE ERRORS (RESIDUALS) HAVE MEAN = 0?
#####################################################################################
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
mean(model$residuals)

#####################################################################################
# 3. CHECK IF RESIDUALS ARE NORMALLY DISTRIBUTED
# - Histogram of Residuals
# - Residual QQ plot
# - Normality Tests: Shapiro-Wilk, Kolmogorov-Smirnov, Cramer-von Mises, Anderson-Darling
# - Test correlation between observed residuals and expected residuals under normality
# - Residual vs. fitted values plot
# - Residual fit spread plot
#####################################################################################

## CHECK IF RESIDUALS ARE NORMALLY DISTRUBUTED 
# -----------------------------------------------------------------------------------
# 1. Make histogram of the residuals 
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_rsd_hist(model)

# 2. Residual QQ plot
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_rsd_qqplot(model)

# 3. Summarizr Normality Tests: 
#       Shapiro-Wilk, Kolmogorov-Smirnov, Cramer-von Mises, Anderson-Darling
ols_norm_test(model)

# 4. Test correlation between observed residuals and expected residuals under normality.
ols_corr_test(model)

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


