library(dplyr)

# data frame from the given table
purity <- c(86.91, 89.85, 90.28, 86.34, 92.58, 87.33, 86.29, 91.86, 95.61, 89.86,
            96.73, 99.42, 98.66, 96.07, 93.65, 87.31, 95.00, 96.85, 85.20, 90.56)

hydrocarbon <- c(1.02, 1.11, 1.43, 1.11, 1.01, 0.95, 1.11, 0.87, 1.43, 1.02,
                 1.46, 1.55, 1.55, 1.55, 1.40, 1.15, 1.01, 0.99, 0.95, 0.98)

data <- data.frame(purity, hydrocarbon)

# a. Fit a simple linear regression model to the data.
model <- lm(purity ~ hydrocarbon, data = data)
summary(model)

# b. Test the hypothesis H0: β1 = 0.
# (test is already given in summary(model) under the p-value for hydrocarbon coefficient)
anova(model)

# c. Calculate R squared.
r_squared <- summary(model)$r.squared
cat("\nR-squared: ", r_squared, "\n")

# d. Find a 95% CI on the slope.
ci <- confint(model, level = 0.95)
cat("\n95% Confidence Intervals:\n")
cat(sprintf("Intercept: [%.2f , %.2f]\n", ci[1,1], ci[1,2]))
cat(sprintf("Slope (x4): [%.2f , %.2f]\n", ci[2,1], ci[2,2]))

# e. Find a 95% CI on the mean purity when the hydrocarbon percentage is 1.00
new_data <- data.frame(hydrocarbon = 1.00)
cat("\n 95% CI on the mean purity when the hydrocarbon percentage is 1.00: \n")
predict(model, newdata = new_data, interval = "confidence", level = 0.95)
