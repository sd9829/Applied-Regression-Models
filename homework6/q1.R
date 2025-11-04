# Fit the model (y = severity, x1 = fall index, x2 = hard hat, x3 = bump cap)
fit <- lm(y ~ x1 + x2 + x3, data = df)

# (b1) Bump cap vs None: look at the coefficient for x3
sum_fit <- summary(fit)
sum_fit$coefficients["x3", ]      # gives estimate, SE, t, two-sided p
df_resid <- fit$df.residual       # = n - 4

# If you need one-sided p for Ha: beta3 < 0
t_b3 <- sum_fit$coefficients["x3", "t value"]
p_two <- sum_fit$coefficients["x3", "Pr(>|t|)"]
p_one_left <- if (t_b3 < 0) p_two/2 else 1 - p_two/2

# (b2) Hard hat vs Bump cap: test beta2 = beta3
install.packages("car")  # if needed
library(car)
linearHypothesis(fit, "x2 = x3")   # gives F, df1=1, df2=n-4, and p-value
