# Note to professor/grader - I use Pycharm to write R code and it only runs by highlighting certain parts. so if I
# don't see something you expect to see on a plot, I run the lines one block after another, but I hear it is an IDE
# issue. So if your IDE behaves similarly, I wanted you to know! Thank you.

# setting up data
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

ad.lm <- lm(impressions ~ amount, data = data)
summary(ad.lm)

# Scatter plot with regression line
plot(impressions ~ amount, data = data, pch = 19, col = "blue",
     main = "Advertising vs Retained Impressions",
     xlab = "Advertising Amount (millions)",
     ylab = "Retained Impressions (millions)")
abline(ad.lm, col = "red", lwd = 2)

# b. Is there a significant relationship between the amount a company spends
# on advertising and retained impressions? Justify your answer statistically.
anova(ad.lm)  # overall F-test

# residual diagnostics plots
par(mfrow = c(2,2))
plot(ad.lm)
par(mfrow = c(1,1))

# answer: the t-test for the slope anf the F-test from the anova table and running summary()
# show a p-value < 0.05 so we reject the null hypothesis H0: β1 = 0 and conlcude that
# there is a statistically signnificant linear relaitonship between the amount a company
# spends on advertising and the number of retained impressions per week.


# c. Construct the 95% confidence and prediction bands for these data.
new_amount <- data.frame(amount = seq(min(data$amount), max(data$amount), length.out = 100))

conf_band <- predict(ad.lm, newdata = new_amount, interval = "confidence", level = 0.95)
pred_band <- predict(ad.lm, newdata = new_amount, interval = "prediction", level = 0.95)

plot(data$amount, data$impressions, pch = 19,
     main = "Regression with 95% Confidence and Prediction Bands",
     xlab = "Advertising Amount (millions)",
     ylab = "Retained Impressions (millions)")
abline(ad.lm, col = "blue", lwd = 2)

# confidence band - red
lines(new_amount$amount, conf_band[,2], col = "red", lty = 2)
lines(new_amount$amount, conf_band[,3], col = "red", lty = 2)

# prediction band - green
lines(new_amount$amount, pred_band[,2], col = "green", lty = 3)
lines(new_amount$amount, pred_band[,3], col = "green", lty = 3)

legend("topleft", legend = c("Regression Line","95% CI","95% PI"),
       col = c("blue","red","green"), lty = c(1,2,3), bty = "n")

# d. Give the 95% confidence and prediction intervals for the number of
# retained impressions for MCI.

MCI <- data.frame(amount = 26.9)

predict(ad.lm, newdata = MCI, interval = "confidence", level = 0.95)
predict(ad.lm, newdata = MCI, interval = "prediction", level = 0.95)
