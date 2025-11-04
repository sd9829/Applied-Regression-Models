# describe the data (part 1 of code)

# Load libraries
library(ggplot2)
library(dplyr)
library(GGally)

# Read data
auto <- read.csv("project/auto-mpg.csv", header = TRUE, sep = ",")

# View basic structure
str(auto)

# Replace "?" with NA in horsepower and convert to numeric
auto$horsepower <- as.numeric(gsub("\\?", NA, auto$horsepower))

# Remove rows with missing values in key columns
auto <- na.omit(auto)

# Select relevant columns
auto_subset <- auto %>% select(mpg, displacement, horsepower, weight, acceleration)

# Summary Statistics
summary(auto_subset)
sapply(auto_subset, sd)  # standard deviations

# Correlation Matrix
cor_matrix <- cor(auto_subset)
print(cor_matrix)

# Scatterplot Matrix
library(ggplot2)
library(tidyr)
library(dplyr)

# Create a long-format dataset for facet plotting
auto_long <- auto_subset %>%
  pivot_longer(cols = -mpg, names_to = "Predictor", values_to = "Value")

# Faceted scatterplots for mpg vs each predictor
ggplot(auto_long, aes(x = Value, y = mpg)) +
  geom_point(aes(color = mpg > mean(mpg)), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  facet_wrap(~ Predictor, scales = "free_x") +
  labs(title = "Relationships between MPG and Key Predictors",
       x = "Predictor Values",
       y = "Miles per Gallon (MPG)",
       color = "Above Average MPG") +
  theme_minimal(base_size = 13)

# Individual Histograms
ggplot(auto_subset, aes(x = mpg)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Distribution of MPG", x = "Miles Per Gallon", y = "Count")

ggplot(auto_subset, aes(x = weight)) +
  geom_histogram(fill = "salmon", color = "black", bins = 20) +
  labs(title = "Distribution of Vehicle Weight", x = "Weight (lbs)", y = "Count")

# Boxplots
ggplot(auto_subset, aes(y = mpg)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of MPG", y = "Miles Per Gallon")

# Scatterplots for Relationships
ggplot(auto_subset, aes(x = weight, y = mpg)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "MPG vs Vehicle Weight", x = "Weight (lbs)", y = "Miles Per Gallon")

ggplot(auto_subset, aes(x = horsepower, y = mpg)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "MPG vs Horsepower", x = "Horsepower", y = "Miles Per Gallon")

ggplot(auto_subset, aes(x = displacement, y = mpg)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "MPG vs Displacement", x = "Engine Displacement", y = "Miles Per Gallon")

# part 2 of code

# Fit linear regression model model
model <- lm(mpg ~ displacement + horsepower + weight + acceleration, data = auto_subset)

# Summary of model (for coefficients, t-tests, R², F-test, p-value)
summary(model)

# ANOVA table for overall model significance
anova(model)

# 95% Confidence Intervals for coefficients
confint(model, level = 0.95)

# Example data for prediction - taken from row 1 in data file
new_car <- data.frame(displacement = 307, horsepower = 130,
                      weight = 3504, acceleration = 12)

# 95% confidence interval for mean prediction
predict(model, newdata = new_car, interval = "confidence", level = 0.95)

# 95% prediction interval for an individual observation
predict(model, newdata = new_car, interval = "prediction", level = 0.95)



