# -----------------------------------------
# Problem: Solar Energy Data (Problem 2.3)
# Parts (a) and (b)
# -----------------------------------------

# Load required library
library(readxl)

# Read data (adjust sheet name if necessary)
solar <- read_excel("Homework4/data-table-B2(1).XLS")

# View first few rows to confirm structure
head(solar)

# Suppose 'Energy' is the response variable (y) and 'Temp' is the predictor (x)
# Replace column names if different in your file
model <- lm(y ~ x1, data = solar)

# Display model summary
summary(model)


# ----- (a) Normal Probability Plot of Residuals -----
residuals_model <- resid(model)

# Q-Q plot (Normal Probability Plot)
qqnorm(residuals_model, main = "Normal Probability Plot of Residuals")
qqline(residuals_model, col = "red", lwd = 2)

# Interpretation (after plotting):
# If residuals roughly follow the straight red line,
# the normality assumption is reasonable.


# ----- (b) Residuals vs Predicted Values -----
fitted_values <- fitted(model)

plot(fitted_values, residuals_model,
     main = "Residuals vs Predicted Response",
     xlab = "Predicted Response (Fitted Values)",
     ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Interpretation:
# If residuals are randomly scattered around 0 without a pattern,
# it suggests constant variance and linearity are reasonable.
