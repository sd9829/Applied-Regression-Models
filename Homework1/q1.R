library(readxl)
data <- read_excel("data-table-B1.XLS")

# column names and first few rows
print(colnames(data))
head(data)

# Automatically assign y and x8 based on headers
y_col <- "y"       # Games won
x_col <- "x8"      # Opponent's yards rushing

# Fit a simple linear regression model relating games won y to yards gained
# rushing by opponents x8
model <- lm(formula = as.formula(paste(y_col, "~", x_col)), data = data)

# Construct the analysis-of-variance table and test for significance of regression.
anova_table <- anova(model)
cat("ANOVA Table: \n")
print(anova_table)

# Find a 95% CI on the slope.
cat("\n95% CI for Intercept and Slope: \n")
confint(model, level = 0.95)

# percent of the total variability in y is explained by this model
r_squared <- summary(model)$r.squared
cat("\nR-squared: ", r_squared, "\n")
cat("Percent of variability in y explained by x8: ", r_squared * 100, "%\n")

# Find a 95% CI on the mean number of games won if opponents’ yards
# rushing is limited to 2000 yards
new_data <- data.frame(x8 = 2000)
predicted <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)
cat("\n95% CI on the mean number of games won for x8 = 2000: \n")
print(predicted)
