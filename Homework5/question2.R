#-----------------------------------------------
# Byers and Williams: Temperature vs Viscosity Study
#-----------------------------------------------

# Data
temperature <- c(24.9, 35.0, 44.9, 55.1, 65.2, 75.2, 85.2, 95.2)
viscosity <- c(1.133, 0.9772, 0.8532, 0.7550, 0.6723, 0.6021, 0.5420, 0.5074)

data <- data.frame(temperature, viscosity)

#-----------------------------------------------
# (a) Scatter Diagram
#-----------------------------------------------
plot(data$temperature, data$viscosity,
     main = "Scatter Plot: Temperature vs Viscosity",
     xlab = "Temperature (C)",
     ylab = "Viscosity (mPa.s)",
     pch = 19, col = "blue")

#-----------------------------------------------
# (b) Fit Linear Model
#-----------------------------------------------
model_linear <- lm(viscosity ~ temperature, data = data)
summary(model_linear)

# Residual plots for linear model
par(mfrow = c(2, 2))
plot(model_linear)

#-----------------------------------------------
# (c) Transform Viscosity (Exponential Relationship)
#-----------------------------------------------
# ln(viscosity) = b0 + b1 * temperature
data$log_viscosity <- log(data$viscosity)
model_exp <- lm(log_viscosity ~ temperature, data = data)
summary(model_exp)

# Residual plots for exponential model
par(mfrow = c(2, 2))
plot(model_exp)
