temperature <- c(24.9, 35.0, 44.9, 55.1, 65.2, 75.2, 85.2, 95.2)
viscosity <- c(1.1330, 0.9772, 0.8532, 0.7550, 0.6723, 0.6021, 0.5420, 0.5074)

data <- data.frame(temperature, viscosity)

# a. Estimate the prediction equation (fit simple linear regression)
model <- lm(viscosity ~ temperature, data = data)
summary(model)  # shows slope, intercept, t-tests, R^2, p-values

# b. Perform a complete analysis of the model
#    - Check ANOVA table for overall significance (F-test)
#    - Check p-value of slope (t-test)
#    - Examine residual plots for assumptions
anova(model)  # F-test for regression

par(mfrow = c(2, 2))
plot(model)   # creates residuals vs fitted, QQ plot, etc.

# Reset plotting layout
par(mfrow = c(1, 1))

# c. Calculate and plot the 95% confidence and prediction bands
new_temp <- data.frame(temperature = seq(min(data$temperature),
                                         max(data$temperature), length.out = 100))

conf_band <- predict(model, newdata = new_temp, interval = "confidence", level = 0.95)
pred_band <- predict(model, newdata = new_temp, interval = "prediction", level = 0.95)

plot(data$temperature, data$viscosity,
     main="Viscosity vs Temperature with 95% Bands",
     xlab="Temperature (°C)", ylab="Viscosity (mPa·s)", pch=19)
abline(model, col="blue", lwd=2)

# confidence band 
lines(new_temp$temperature, conf_band[,2], col="red", lty=2)
lines(new_temp$temperature, conf_band[,3], col="red", lty=2)

# prediction band
lines(new_temp$temperature, pred_band[,2], col="green", lty=3)
lines(new_temp$temperature, pred_band[,3], col="green", lty=3)

legend("topright", legend=c("Regression line","95% CI","95% PI"),
       col=c("blue","red","green"), lty=c(1,2,3), bty="n")
