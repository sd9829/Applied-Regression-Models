# -----------------------------------------------------
# Problem: House Price Data Analysis (Multiple Regression)
# -----------------------------------------------------

# Load required libraries
library(readxl)
library(car)   # for partial regression plots

# Read data (file must be in the same directory as this script)
house <- read_excel("Homework4/data-table-B4.XLS")

# Fit multiple regression model
model <- lm(y ~ ., data = house)

# Summary of model
summary(model)

# -----------------------------------------------------
# (a) Normal probability plot of residuals
# -----------------------------------------------------
res <- resid(model)

qqnorm(res, main = "Normal Probability Plot of Residuals")
qqline(res, col = "red", lwd = 2)

# Interpretation:
# Points should roughly follow the red line for residuals to be normal.

# -----------------------------------------------------
# (b) Residuals vs Predicted Response
# -----------------------------------------------------
fitted_vals <- fitted(model)

plot(fitted_vals, res,
     main = "Residuals vs Predicted Response",
     xlab = "Predicted Response (Fitted Values)",
     ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Interpretation:
# Random scatter → linear model appropriate.
# Pattern or funnel shape → nonlinearity or heteroscedasticity.

# -----------------------------------------------------
# (c) Partial regression plots
# -----------------------------------------------------
avPlots(model, main = "Partial Regression Plots")

# Interpretation:
# Check each plot; a roughly straight line indicates that variable contributes linearly.
# A flat or random pattern suggests that variable may not be significant.

# -----------------------------------------------------
# (d) Studentized and R-student residuals
# -----------------------------------------------------
# Standardized (studentized internal) residuals
stud_res <- rstandard(model)

# Externally studentized (R-student) residuals
rstudent_res <- rstudent(model)

# Combine results for viewing
residual_table <- cbind(
  house,
  Standardized_Residual = round(stud_res, 3),
  RStudent_Residual = round(rstudent_res, 3)
)

print(residual_table)

# Interpretation:
# Studentized residuals > |2| indicate potential outliers.
# R-student residuals (> |3|) are stronger evidence of influential points.
