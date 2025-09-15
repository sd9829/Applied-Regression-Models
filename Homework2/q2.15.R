# Note to professor/grader - I use Pycharm to write R code and it only runs by highlighting certain parts. so if you
# dont see something you expect to see on a plot, I run the lines one block after another, but I hear it is an IDE
# issue. So if your IDE behaves similarly, I wanted you to know! Thank you.

temperature <- c(24.9, 35.0, 44.9, 55.1, 65.2, 75.2, 85.2, 95.2)
viscosity <- c(1.1330, 0.9772, 0.8532, 0.7550, 0.6723, 0.6021, 0.5420, 0.5074)
data <- data.frame(temperature, viscosity)

# a. Estimate the prediction equation.

visc.lm <- lm(viscosity ~ temperature, data = data)
summary(visc.lm)

# scatter plot with regression line
plot(viscosity ~ temperature, data = data, pch = 19, col = "black",
     main = "Viscosity vs Temperature",
     xlab = "Temperature (°C)", ylab = "Viscosity (mPa·s)")
abline(visc.lm, col = "red", lwd = 2)

# b. Perform a complete analysis of the model

anova(visc.lm)       # F-test for regression significance
rstudent(visc.lm)

# residual diagnostics plots
par(mfrow = c(2,2))
plot(visc.lm)
par(mfrow = c(1,1))

# correlation of residuals and fitted values
cor(resid(visc.lm), fitted(visc.lm))

# c. Calculate and plot the 95% confidence and prediction bands

new_temp <- data.frame(temperature = seq(min(data$temperature), max(data$temperature), length.out = 100))

# predict confidence and prediction
conf_band <- predict(visc.lm, newdata = new_temp, interval = "confidence", level = 0.95)
pred_band <- predict(visc.lm, newdata = new_temp, interval = "prediction", level = 0.95)

# plot with both bands
plot(data$temperature, data$viscosity, pch = 19,
     xlab = "Temperature (°C)", ylab = "Viscosity (mPa·s)",
     ylim = range(c(data$viscosity, pred_band)),
     main = "Regression with 95% Confidence and Prediction Bands")
abline(visc.lm, col = "black", lwd = 2)

# confidence band - red
lines(new_temp$temperature, conf_band[,2], col = "red", lty = 2)
lines(new_temp$temperature, conf_band[,3], col = "red", lty = 2)

#prediction band - green
lines(new_temp$temperature, pred_band[,2], col = "green", lty = 3)
lines(new_temp$temperature, pred_band[,3], col = "green", lty = 3)

legend("topright", legend = c("Regression Line","95% CI","95% PI"),
       col = c("black","red","green"), lty = c(1,2,3), bty = "n")
