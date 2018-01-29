library(olsrr)

# STANDARD REGRESSION ASSUMPTIONS
# Here:
# 3. Homoscedasticity/constant variance assumption: The errors have same but unknown variance
# In other recipes:
# 1. Normality assumption: The error has normal distribution
# 2. The errors have mean zero
# 4. Independent errors: The errors are independent of each other

############################################################
# WORKFLOW
# 1. CHECK FOR HETEROSCEDASTICITY
############################################################

#####################################################################################
# 1. TESTS FOR HETEROSCEDASTICITY
# - Test 1: Bartlett Test
# - Test 2: Breusch Pagan
# - Test 3: Score Test
# - Test 4: F TEST
#####################################################################################

## CONSEQUENCES OF HETEROSCEDASTICITY 
# - The OLS estimators and regression predictions based on them remain unbiased and consistent
# - The OLS estimators are no longer the best linear unbiased estimators (BLUE) because they
#   are no longer efficient; so the regression predictions will be inefficient too
# - Because of the inconsistency of the covariance matrex of the estimated regression coefficient
#   tests of hypotheses, (t-test, F-test) are no longer valid

# -----------------------------------------------------------------------------------

## Test 1: Bartlett Test: Tests if variance among samples is equal. 
#  - Sensitive to departures from normality.
#  - Can be used on 2 continuous variables, 1 continuous & one grouping variable, a
#     formula or a linear model
# -----------------------------------------------------------------------------------

# using grouping variable
model <- lm(mpg ~ disp + hp, data = mtcars)
resid <- residuals(model)
cyl <- as.factor(mtcars$cyl)
ols_bartlett_test(resid, group_var = cyl)

# using variables
ols_bartlett_test(hsb$rad, hsb$write)

# using a formula
mt <- mtcars
mt$cyl <- as.factor(mt$cyl)
ols_bartlett_test(mpg ~ cyl, data = mt)

# using a linear model
mtcars$cyl <- as.factor(mtcars$cyl)
model <- lm(mpg ~ cyl, data = mtcars)
ols_bartlett_test(model)

# -----------------------------------------------------------------------------------

## Test 2: Breusch Pagan test: test for herteroskedasticity (non-constant error variance).
# - ASSUMES THE ERROR TERMS ARE NORMALLY DISTRIBUTED
# - tests whether the variance of the errors from a regression is dependent on the 
# - values of the independent variables. It is a chi^2 test.
# -----------------------------------------------------------------------------------

# using fitted values of the model
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_bp_test(model)

# using independent variables of the model
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_bp_test(model, rhs = TRUE)

# using independent variables of the model and performing multiple tests
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_bp_test(model, rhs = TRUE, multiple = TRUE)

# Breusch Pagan with p-value adjustment: The Bonferroni test
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = 'bonferroni')

# Breusch Pagan with p-value adjustment: The Sidak test
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = 'sidak')

# Breusch Pagan with p-value adjustment: Holm's test
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = 'holm')

# -----------------------------------------------------------------------------------

# TEST 3: SCORE TEST
# - ASSUMES ERRORS ARE INDEPENDENT AND IDENTICALLY DISTRIBUTED (i.i.d)
# -----------------------------------------------------------------------------------

# using fitted values of the model
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_score_test(model)

# using independent variables of the model
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_score_test(model, rhs = TRUE)

# specifying variables
mmodel <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_score_test(model, vars = c('disp', 'hp'))

# -----------------------------------------------------------------------------------

## TEST 4: F TEST 
# - ASSUMES THE ERRORS ARE INDEPENDENT AND IDENTICALY DISTRIBUTED
# -----------------------------------------------------------------------------------

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_f_test(model)

# using independent variables of the model
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_f_test(model, rhs = TRUE)

# specifying variables
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_f_test(model, vars = c('disp', 'hp'))
