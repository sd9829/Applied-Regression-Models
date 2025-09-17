# ===============================================================
# Byers & Williams (1987) - Temperature vs Viscosity
# ===============================================================

# set up the data
temperature <- c(24.9, 35.0, 44.9, 55.1, 65.2, 75.2, 85.2, 95.2)
viscosity <- c(1.1330, 0.9772, 0.8532, 0.7550, 0.6723, 0.6021, 0.5420, 0.5074)
data <- data.frame(temperature, viscosity)

# ===============================================================
# a. Fit the simple linear regression model (prediction equation)
# ===============================================================

visc.lm <- lm(viscosity ~ temperature, data = data)
summary(visc.lm)   # shows slope, intercept, R^2, p-values

# scatter plot with regression line
plot(viscosity ~ temperature, data = data, pch = 19, col = "blue",
     main = "Viscosity vs Temperature",
     xlab = "Temperature (°C)", ylab = "Viscosity (mPa·s)")
abline(visc.lm, col = "red", lwd = 2)

# ===============================================================
# b. Perform a complete analysis of the model
#    - check model significance, fit, and assumptions
# ===============================================================

anova(visc.lm)       # F-test for regression significance
rstudent(visc.lm)    # externally studentized residuals

# residual diagnostics plots
par(mfrow = c(2,2))
plot(visc.lm)         # residuals vs fitted, QQ plot, etc.
par(mfrow = c(1,1))

# correlation of residuals with fitted values
cor(resid(visc.lm), fitted(visc.lm))

# ===============================================================
# c. Calculate and plot the 95% confidence and prediction bands
# ===============================================================

# create smooth sequence of x-values
new_temp <- data.frame(temperature = seq(min(data$temperature), max(data$temperature), length.out = 100))

# predict mean (confidence) and new obs (prediction)
conf_band <- predict(visc.lm, newdata = new_temp, interval = "confidence", level = 0.95)
pred_band <- predict(visc.lm, newdata = new_temp, interval = "prediction", level = 0.95)

# plot with both bands
plot(data$temperature, data$viscosity, pch = 19,
     xlab = "Temperature (°C)", ylab = "Viscosity (mPa·s)",
     ylim = range(c(data$viscosity, pred_band)), # ensures full range visible
     main = "Regression with 95% Confidence and Prediction Bands")
abline(visc.lm, col = "blue", lwd = 2)

# add confidence band (red dashed)
lines(new_temp$temperature, conf_band[,2], col = "red", lty = 2)
lines(new_temp$temperature, conf_band[,3], col = "red", lty = 2)

# add prediction band (green dotted)
lines(new_temp$temperature, pred_band[,2], col = "green", lty = 3)
lines(new_temp$temperature, pred_band[,3], col = "green", lty = 3)

legend("topright", legend = c("Regression Line","95% CI","95% PI"),
       col = c("blue","red","green"), lty = c(1,2,3), bty = "n")

predict(visc.lm, newdata = data.frame(temperature = c(24.9, 35.0, 44.9, 55.1, 65.2, 75.2, 85.2, 95.2)),
        interval = "confidence")
predict(visc.lm, newdata = data.frame(temperature = c(24.9, 35.0, 44.9, 55.1, 65.2, 75.2, 85.2, 95.2)),
        interval = "prediction")
