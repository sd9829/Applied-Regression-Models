### ===============================
### Soft Drink Delivery Time Data
### ===============================

df <- data.frame(
  y = c(16.68, 11.50, 12.03, 14.88, 13.75, 18.11, 8.00, 17.83, 79.24, 21.50,
        40.33, 21.00, 13.50, 19.75, 24.00, 29.00, 15.35, 19.00, 9.50, 35.10,
        17.90, 52.32, 18.75, 19.83, 10.75),
  x1 = c(7,3,3,4,6,7,2,7,30,5,
         16,10,4,6,9,10,6,7,3,17,
         10,26,9,8,4),
  x2 = c(560,220,340,80,150,330,110,210,1460,605,
         688,215,255,462,448,776,200,132,36,770,
         140,810,450,635,150)
)

# matrix plot
pairs(df[, c("x1", "x2")],
      main = "Matrix Plot of Predictors (x1 and x2)")

# correlation matrix
cor_matrix <- cor(df[, c("x1", "x2")])
cor_matrix

# c. Check for collinearity issues (VIF, condition index)
library(car)
model <- lm(y ~ x1 + x2, data = df)
vif(model)

# eigenvalues
library(car)
kappa(model)              # condition number
kappa(model, exact = TRUE)

# ridge regression
library(MASS)

# Fit ridge regression over a grid of k values
k_values <- seq(0, 10, 0.1)
ridge_mod <- lm.ridge(y ~ x1 + x2, data = df, lambda = k_values)

# Ridge trace plot
plot(ridge_mod,
     main = "Ridge Trace for Soft Drink Delivery Time Data",
     xlab = "Ridge Penalty (k)",
     ylab = "Standardized Coefficients")

best_k <- ridge_mod$lambda[which.min(ridge_mod$GCV)]
best_k

best_model <- lm.ridge(y ~ x1 + x2, data = df, lambda = best_k)
coef(best_model)

