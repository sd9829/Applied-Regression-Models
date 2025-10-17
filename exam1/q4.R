#--------------------------------------------------
# SOAP SUDS EXPERIMENT - LINEAR MODEL ANALYSIS
#--------------------------------------------------

# Data
x <- c(4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0)
y <- c(32, 43, 45, 51, 53, 61, 62)

#-----------------------------------------------
# (a) Fit the simple linear regression model
#-----------------------------------------------
model_linear <- lm(y ~ x)
summary(model_linear)

#-----------------------------------------------
# (b) Display fitted values and residuals
#-----------------------------------------------
residuals_linear <- resid(model_linear)
fitted_values <- fitted(model_linear)

# Create a table for inspection
residual_table <- data.frame(x, y, Fitted = fitted_values, Residual = residuals_linear)
print(residual_table)

#-----------------------------------------------
# (c) Residual Analysis (Visual)
#-----------------------------------------------

# Reset graphics device to avoid margin issues
dev.off()

# Plot residuals vs fitted values
par(mar = c(5, 5, 2, 2))  # Set proper margins
plot(fitted_values, residuals_linear,
     main = "Residuals vs Fitted Values (Linear Model)",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

#-----------------------------------------------
# (d) Check if a quadratic term improves the model
#-----------------------------------------------
model_quadratic <- lm(y ~ x + I(x^2))
summary(model_quadratic)

# Compare linear vs quadratic model using ANOVA
anova(model_linear, model_quadratic)

#-----------------------------------------------
# (e) Diagnostic Plots
#-----------------------------------------------
# Open a new plotting window for better layout
x11()  # (use windows() if on Windows)
par(mfrow = c(2, 2))
plot(model_linear)


#-----------------------------------------------
# Plot data points and fitted curves
#-----------------------------------------------

# Original data
plot(x, y,
     main = "Soap Suds Height vs. Product Amount",
     xlab = "Grams of Product (X)",
     ylab = "Suds Height (Y)",
     pch = 19, col = "blue", cex = 1.3)

# Linear regression line
abline(model_linear, col = "red", lwd = 2)

# Add quadratic curve
x_seq <- seq(min(x), max(x), length.out = 100)
y_quad <- predict(model_quadratic, newdata = data.frame(x = x_seq))
lines(x_seq, y_quad, col = "darkgreen", lwd = 2, lty = 2)

# Add legend
legend("topleft",
       legend = c("Observed Data", "Linear Fit", "Quadratic Fit"),
       col = c("blue", "red", "darkgreen"),
       pch = c(19, NA, NA),
       lty = c(NA, 1, 2),
       bty = "n")
