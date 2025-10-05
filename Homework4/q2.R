# -----------------------------
# Problem 2 (a) and (b)
# -----------------------------

# Create the dataset
pressure <- c(30, 30, 31, 31, 32, 32, 33, 33, 34, 34, 35, 35, 36, 36)
mileage  <- c(29.5, 30.2, 32.1, 34.5, 36.3, 35.0, 38.2, 37.6,
              37.7, 36.1, 33.6, 34.2, 26.8, 27.4)

tire_data <- data.frame(pressure, mileage)

# ----- (a) Plot the data -----
plot(tire_data$pressure, tire_data$mileage,
     main = "Tire Wear vs. Pressure",
     xlab = "Pressure (psi)",
     ylab = "Mileage (thousands)",
     pch = 19, col = "blue")
lines(lowess(tire_data$pressure, tire_data$mileage), col = "red", lwd = 2)

# Observation:
# The plot should rise from 30–33 psi and then fall after 34 psi,
# indicating a curved (non-linear) relationship.

# ----- (b) Fit the least-squares regression line -----
linear_model <- lm(mileage ~ pressure, data = tire_data)
summary(linear_model)

# Display the fitted regression equation
cat("Estimated regression line:  mileage =",
    round(coef(linear_model)[1], 4), "+",
    round(coef(linear_model)[2], 4), "* pressure\n")

# Plot fitted line on top of data
abline(linear_model, col = "darkgreen", lwd = 2)


# Fit the linear model
lm_model <- lm(mileage ~ pressure, data = tire_data)

# Fit the full (group means) model to test lack of fit
full_model <- lm(mileage ~ as.factor(pressure), data = tire_data)

# Compare models using ANOVA (Lack-of-fit test)
anova(lm_model, full_model)

