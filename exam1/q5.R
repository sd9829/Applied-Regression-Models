#-------------------------------------------------------
# Gasoline Mileage - Residual Diagnostic Plots
#-------------------------------------------------------

# Data input
auto_data <- data.frame(
  Y = c(18.90,17.00,20.00,18.25,20.07,11.20,22.12,21.47,34.70,30.40,
        16.50,36.50,21.50,19.70,20.30,17.80,14.39,14.89,17.80,16.41,
        23.54,21.47,16.59,31.90,29.40,13.27,23.90,19.73,13.90,13.27,
        13.77,16.50),
  X1 = c(350,350,250,351,225,440,231,262,89.7,96.9,350,85.3,171,258,
         140,302,500,440,350,318,231,360,400,96.9,140,460,133.6,318,
         351,351,360,350),
  X2 = c(3910,3860,3510,3890,3365,4215,3020,3180,1905,2320,3885,2009,
         2655,3375,2700,3890,5290,5185,3910,3660,3050,4250,3850,2275,
         2150,5430,2535,4370,4540,4715,4215,3660)
)

# Fit regression model
model <- lm(Y ~ X1 + X2, data = auto_data)

# Set layout for all 3 plots in one window
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2.5, 1.5))

# (1) Normal probability plot
qqnorm(resid(model), main = "Normal Q-Q Plot of Residuals")
qqline(resid(model), col = "red", lwd = 2)

# (2) Residuals vs Fitted
plot(fitted(model), resid(model),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# (3) Residuals vs Predictors
plot(auto_data$X1, resid(model),
     main = "Residuals vs X1 (Displacement)",
     xlab = "Displacement (in^3)",   # Replaced the problematic "³" with "^3"
     ylab = "Residuals",
     pch = 19, col = "purple")
abline(h = 0, col = "red", lwd = 2)

plot(auto_data$X2, resid(model),
     main = "Residuals vs X2 (Weight)",
     xlab = "Weight (lbs)",
     ylab = "Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

# Reset layout
par(mfrow = c(1, 1))
