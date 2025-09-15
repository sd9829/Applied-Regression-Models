# question 2.18 from Textbook

# load data
firm <- c("Miller Lite","Pepsi","Stroh's","Federal Express","Burger King",
          "Coca-Cola","McDonald's","MCI","Diet Cola","Ford","Levi's",
          "Bud Lite","ATT Bell","Calvin Klein","Wendy's","Polaroid","Shasta",
          "Meow Mix","Oscar Meyer","Crest","Kibbles N Bits")

amount <- c(50.1,74.1,19.3,22.9,82.4,40.1,185.9,26.9,20.4,166.2,
            27,45.6,154.9,5,49.7,26.9,5.7,7.6,9.2,32.4,6.1)

impressions <- c(32.1,99.6,11.7,21.9,60.8,78.6,92.4,50.7,21.4,
                 40.1,40.8,10.4,88.9,12,29.2,38,10,12.3,23.4,
                 71.1,4.4)

data <- data.frame(firm, amount, impressions)

# a. Fit the simple linear regression model to these data.
model <- lm(impressions ~ amount, data = data)
summary(model)

# -------------------------------------------------------------
# b. Is there a significant relationship between amount spent
#    and retained impressions?
#    -> Check the slope (amount) p-value in the summary.
# -------------------------------------------------------------
# If the p-value for "amount" is < 0.05, the relationship is significant.

# -------------------------------------------------------------
# c. Construct the 95% confidence and prediction bands.
#    We will create a sequence of "amount" values to cover the data range,
#    then predict with both confidence and prediction intervals.
# -------------------------------------------------------------
new_amount <- data.frame(amount = seq(min(data$amount), max(data$amount), length.out = 100))

conf_band <- predict(model, newdata = new_amount, interval = "confidence", level = 0.95)
pred_band <- predict(model, newdata = new_amount, interval = "prediction", level = 0.95)

# Plot with data, regression line, and bands
plot(data$amount, data$impressions,
     main="Regression with 95% Confidence and Prediction Bands",
     xlab="Advertising Amount (millions)", ylab="Retained Impressions (millions)", pch=19)
abline(model, col="blue", lwd=2)

# Add confidence band (red)
lines(new_amount$amount, conf_band[,2], col="red", lty=2)
lines(new_amount$amount, conf_band[,3], col="red", lty=2)

# Add prediction band (green)
lines(new_amount$amount, pred_band[,2], col="green", lty=3)
lines(new_amount$amount, pred_band[,3], col="green", lty=3)

legend("topleft", legend=c("Regression line","95% CI","95% PI"),
       col=c("blue","red","green"), lty=c(1,2,3), bty="n")

# -------------------------------------------------------------
# d. Give the 95% confidence and prediction intervals for MCI.
#    (MCI has amount spent = 26.9)
# -------------------------------------------------------------
MCI <- data.frame(amount = 26.9)

conf_int_MCI <- predict(model, newdata = MCI, interval = "confidence", level = 0.95)
pred_int_MCI <- predict(model, newdata = MCI, interval = "prediction", level = 0.95)

conf_int_MCI
pred_int_MCI
