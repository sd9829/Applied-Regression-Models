# load the data
y <- c(16.0, 15.8, 15.6, 15.5, 14.8, 14.0, 13.5, 13.0, 12.0, 11.0)
x <- c(1700, 1720, 1730, 1740, 1750, 1760, 1770, 1780, 1790, 1795)

df <- data.frame(x, y)
df$x2 <- x^2

# fit the model
model <- lm(y ~ x + x2, data=df)
summary(model)

# test for regression
anova(model)

# Test contribution of the quadratic term and linear term using partial F-tests
model_linear <- lm(y ~ x, data=df)
anova(model_linear, model)

# Test contribution of linear term (β₁):
# H0:β1=0,HA:β1≠0
# Fit model without x:
model_quad <- lm(y ~ x2, data=df)
anova(model_quad, model)

# Plot residuals and assess fit
par(mfrow=c(2,2))
plot(model)
