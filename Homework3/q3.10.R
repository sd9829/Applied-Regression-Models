# Load necessary libraries
library(readxl)
library(dplyr)     # for data manipulation
library(broom)     # for tidying model outputs
library(ggplot2)   # optional, for residual plots

# Read the dataset (update the path as needed)
wine_data <- read_excel("Homework3/data-table-B11.XLS")

# ----- part a: Fit full model with all regressors -----
full_model <- lm(Quality ~ Clarity + Aroma + Body + Flavor + Oakiness, data = wine_data)
summary(full_model)

# ----- part b: ANOVA to test overall significance of regression -----
anova_full <- anova(full_model)
print(anova_full)

# Check F-statistic and p-value from model summary
summary(full_model)$fstatistic
# You can extract the p-value using:
pf(summary(full_model)$fstatistic[1],
   summary(full_model)$fstatistic[2],
   summary(full_model)$fstatistic[3],
   lower.tail = FALSE)

# ----- part c: t-tests for each coefficient -----
summary(full_model)  # Includes individual t-statistics and p-values
# Interpretation: Look at Pr(>|t|) to see which variables are significant

# ----- part d: Compare R² and R²_adj of full vs reduced model -----
R2_full <- summary(full_model)$r.squared
R2adj_full <- summary(full_model)$adj.r.squared

reduced_model <- lm(Quality ~ Aroma + Flavor, data = wine_data)
summary(reduced_model)

R2_reduced <- summary(reduced_model)$r.squared
R2adj_reduced <- summary(reduced_model)$adj.r.squared

cat("Full Model R²:", R2_full, " | Adjusted R²:", R2adj_full, "\n")
cat("Reduced Model R²:", R2_reduced, " | Adjusted R²:", R2adj_reduced, "\n")

# ----- part e: 95% Confidence Intervals for Flavor coefficient -----
confint(full_model, level = 0.95)["Flavor", ]
confint(reduced_model, level = 0.95)["Flavor", ]
