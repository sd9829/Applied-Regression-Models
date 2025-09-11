library(ggplot2)

# Create the dataset
year <- c(1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,
          1986,1987,1988,1989,1990,1991)
days <- c(91,105,106,108,88,91,58,82,81,65,61,48,61,43,33,36)
index <- c(16.7,17.1,18.2,18.1,17.2,18.2,16.0,17.2,18.0,17.2,
           16.9,17.1,18.2,17.3,17.5,16.6)

data <- data.frame(year, days, index)

# a. Make a scatterplot of the data.
ggplot(data, aes(x = index, y = days)) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Scatterplot of Ozone Days vs. Meteorological Index",
       x = "Seasonal Meteorological Index (850-mb Temp)",
       y = "Number of Days Ozone > 0.20 ppm") +
  theme_minimal()

# b. Estimate the prediction equation.
model <- lm(days ~ index, data = data)
summary(model)

# c. Test for significance of regression.
anova(model)

# d. Calculate and plot the 95% confidence and prediction bands.

new_index <- data.frame(index = seq(min(data$index), max(data$index), length.out = 100))

conf_band <- predict(model, newdata = new_index, interval = "confidence", level = 0.95)
pred_band <- predict(model, newdata = new_index, interval = "prediction", level = 0.95)
plot_data <- cbind(new_index, conf_band, pred_band[,2:3])
colnames(plot_data) <- c("index", "fit", "lwr_conf", "upr_conf", "lwr_pred", "upr_pred")

# Print first few rows of the confidence and prediction bands
cat("\n=== 95% Confidence and Prediction Bands (first 10 values) ===\n")
print(head(plot_data, 10))   # prints first 10 rows for testing

# Plot with regression line, confidence band, and prediction band
ggplot() +
  geom_point(data = data, aes(x = index, y = days), size = 3, color = "blue") +
  geom_line(data = plot_data, aes(x = index, y = fit), color = "red", size = 1.2) +
  geom_ribbon(data = plot_data, aes(x = index, ymin = lwr_conf, ymax = upr_conf),
              fill = "green", alpha = 0.3) +
  geom_ribbon(data = plot_data, aes(x = index, ymin = lwr_pred, ymax = upr_pred),
              fill = "orange", alpha = 0.2) +
  labs(title = "Regression with 95% Confidence and Prediction Bands",
       x = "Seasonal Meteorological Index (850-mb Temp)",
       y = "Number of Days Ozone > 0.20 ppm") +
  theme_minimal()
