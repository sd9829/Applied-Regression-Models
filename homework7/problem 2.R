
# Load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(car)

df <- read_excel("homework7/data-table-B19.XLS")

cor_matrix <- round(cor(df[ , -1]), 3)
cor_matrix

# vif analysis
# vif(lm(y ~ ., data = df))

vif_values <- vif(lm(y ~ ., data = df), qr = TRUE)
vif_values

m_full <- lm(y ~ ., data = df)
alias(m_full)

df_reduced <- subset(df, select = -c(x_7, x_10))
m_reduced <- lm(y ~ ., data = df_reduced)
vif_values <- vif(m_reduced)
vif_values

kappa(m_reduced)
kappa(m_reduced, exact = TRUE)
