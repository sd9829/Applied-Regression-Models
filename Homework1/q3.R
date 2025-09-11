library(readxl)
data <- read_excel("data-table-B2.XLS")

# column names and first few rows
print(colnames(data))
head(data)

# Fit a simple linear regression model relating total heat flux y (kilowatts)
# to the radial defl ection of the defl ected rays x4 (milliradians).
# y = total heat flux, x4 = radial deflection
model <- lm(y ~ x4, data = data)

# Construct the analysis - of - variance table and test for significance of regression
anova_table <- anova(model)
cat("ANOVA Table\n")
print(anova_table)

# Find a 99% CI on the slope
ci <- confint(model, level = 0.99)
cat("\n99% Confidence Intervals:\n")
cat(sprintf("Intercept: [%.2f , %.2f]\n", ci[1,1], ci[1,2]))
cat(sprintf("Slope (x4): [%.2f , %.2f]\n", ci[2,1], ci[2,2]))

# Calculate R-squared
r_squared <- summary(model)$r.squared
cat("\nR-squared = ", r_squared, "\n")

# Find a 95% CI on the mean heat flux when the radial deflection is 16.5
# milliradians.
new_data <- data.frame(x4 = 16.5)
predicted <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)
cat("\n 95% CI on mean heat flux for x4 = 16.5: \n")
print(predicted)
