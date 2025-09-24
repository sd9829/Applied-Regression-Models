library(readxl)
nfl_data <- read_excel("Homework3/data-table-B1.XLS")


# ----- Part a: Fit the multiple linear regression model -----
model <- lm(y ~ x2 + x7 + x8, data = nfl_data)
summary(model)

# ----- Part b: Construct ANOVA table and test significance of regression -----
anova_result <- anova(model)
print("ANOVA Table:")
print(anova_result)

# ----- Part c: Calculate t-statistics for H0: β2 = 0, H0: β7 = 0, H0: β8 = 0 -----
summary_model <- summary(model)
cat("T-values:\n")
print(coef(summary_model)[, "t value"])
cat("P-values:\n")
print(coef(summary_model)[, "Pr(>|t|)"])

# ----- Part d: Calculate R² and Adjusted R² -----
cat("R-squared:", summary_model$r.squared, "\n")
cat("Adjusted R-squared:", summary_model$adj.r.squared, "\n")

# ----- Part e: Partial F-test for contribution of x7 -----
# Fit reduced model without x7
reduced_model <- lm(y ~ x2 + x8, data = nfl_data)

# Perform partial F-test using anova()
partial_f_test <- anova(reduced_model, model)
cat("Partial F-test for x7:\n")
print(partial_f_test)

# Reminder: t² = F when comparing a single coefficient, so t-stat for x7 squared ≈ F-stat for partial F
cat("t^2 for x7:", round(coef(summary_model)["x7", "t value"]^2, 4), "\n")
