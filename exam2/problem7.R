# Load data
condo <- read.table("exam2/condo.txt", header = FALSE)
colnames(condo) <- c("SalePrice", "Floor", "ElevDist", "OceanView",
                     "EndUnit", "Furnished")

# 1) Model and Assumptions (code: define model)
full_model <- lm(SalePrice ~ Floor + ElevDist + OceanView + EndUnit + Furnished,
                 data = condo)
full_model


# 2) Scatter plot matrix and correlation matrix
pairs(condo)
cor(condo)


# 3) Test for regression relationship (overall F-test)

anova(full_model)
summary(full_model)


# 4) Test individual parameters

summary(full_model)


# 5) Check multicollinearity (VIF)

library(car)
vif(full_model)


# 6) Stepwise and Best Subsets Selection

# Stepwise (both directions)
step_model <- step(full_model, direction = "both")

# Best subsets using leaps
library(leaps)
best_subsets <- regsubsets(SalePrice ~ Floor + ElevDist + OceanView +
                           EndUnit + Furnished, data = condo,
                           nbest = 5)
summary(best_subsets)


# 7) Multicollinearity in reduced model

vif(step_model)


# 8) Outlier and influence diagnostics for reduced model
par(mfrow = c(2,2))
plot(step_model)
influence.measures(step_model)


# 9) Check assumptions for reduced model
# Residual plots
par(mfrow = c(2,2))
plot(step_model)
# Normality test
shapiro.test(residuals(step_model))
# Homoscedasticity test
library(lmtest)
bptest(step_model)


# 10) Suggestions for fixes (code placeholder)
# Box-Cox transformation (if needed)
library(MASS)
boxcox(step_model)

# Robust regression (if needed)
library(MASS)
rlm(SalePrice ~ Floor + ElevDist + OceanView + EndUnit + Furnished, data=condo)
