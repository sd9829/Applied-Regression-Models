# FULL MODEL (from SAS parameter table)

full_model <- list(
  intercept = 904.13484,
  rbi = 34.68826,
  runs = 21.79455,
  ko = -26.87467,
  homers = 18.05023,
  obp = -2525.44148,
  sb = -9.36523
)

full_model

# STEPWISE MODEL (based on SAS selection)

stepwise_model <- c("Intercept", "rbi", "contract", "ko", "err")
stepwise_model

1 - pf(245.33, 3, 31)
