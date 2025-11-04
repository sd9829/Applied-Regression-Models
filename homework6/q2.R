#-----------------------------------------------
# Automobile Gasoline Mileage Analysis
#-----------------------------------------------

# Load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(car)

# Read data
data <- read_excel("homework6/data-table-B3.XLS")

# View structure
str(data)
summary(data)

#------------------------------------------------
# (a) Linear regression model: y ~ weight + transmission
#------------------------------------------------
# Assuming:
# y = Mileage (mpg)
# x10 = Vehicle weight
# x11 = Transmission type (0 = automatic, 1 = manual)

# Rename columns if necessary
colnames(data) <- c("mileage", "weight", "transmission")

# Fit the model
model_a <- lm(mileage ~ weight + transmission, data = data)
summary(model_a)

# ANOVA table
anova(model_a)

# Does transmission affect mileage?
# Check p-value for 'transmission' coefficient from summary output.

#------------------------------------------------
# (b) Add interaction term: y ~ weight * transmission
#------------------------------------------------
model_b <- lm(mileage ~ weight * transmission, data = data)
summary(model_b)

# Compare models
anova(model_a, model_b)

#------------------------------------------------
# Interpretation plots
#------------------------------------------------

# Residual diagnostics
par(mfrow = c(2, 2))
plot(model_b)

# Visualizing interaction
ggplot(data, aes(x = weight, y = mileage, color = factor(transmission))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction Between Weight and Transmission Type",
       x = "Vehicle Weight",
       y = "Gasoline Mileage (mpg)",
       color = "Transmission\n(0=Auto, 1=Manual)") +
  theme_minimal()
