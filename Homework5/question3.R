# ------------------------------------------------------------
# Solar Thermal Energy (Table B.2) - Influence Analysis in R
# ------------------------------------------------------------

# Packages
suppressPackageStartupMessages({
  library(readxl)
  library(car)        # nice influence plots
})

# ---- Load data ----
path <- "Homework5/data-table-B2(1).XLS"   # change if your file lives elsewhere
dat  <- as.data.frame(read_excel(path))
names(dat) <- tolower(names(dat))

# Try to auto-detect columns (Energy ~ Temp OR y ~ x1)
if (all(c("energy","temp") %in% names(dat))) {
  yvar <- "energy"; xvar <- "temp"
} else if (all(c("y","x1") %in% names(dat))) {
  yvar <- "y"; xvar <- "x1"
} else {
  stop("Couldn't find expected columns. Make sure the sheet has either (Energy, Temp) or (y, x1).")
}

fml <- as.formula(paste(yvar, "~", xvar))
mod <- lm(fml, data = dat)

cat("\n=== Model Summary ===\n")
print(summary(mod))

# ---- Influence measures ----
n <- nobs(mod)
p <- length(coef(mod))  # includes intercept

h       <- hatvalues(mod)          # leverage (h_ii)
cooks   <- cooks.distance(mod)     # Cook's D
dffits_ <- dffits(mod)             # DFFITS
dfb     <- dfbetas(mod)            # DFBetas (matrix)
covr    <- covratio(mod)           # Covariance ratio
rstud   <- rstudent(mod)           # Externally studentized residuals
rstd    <- rstandard(mod)          # Internally standardized residuals

# ---- Rule-of-thumb cutoffs ----
lev_cut   <- 2*p/n
cooks_cut <- 4/(n - p)
dffits_cut <- 2*sqrt(p/n)
dfbeta_cut <- 2/sqrt(n)
covr_low  <- 1 - 3*p/n
covr_high <- 1 + 3*p/n

# ---- Assemble table ----
out <- data.frame(
  obs = seq_len(n),
  fitted = fitted(mod),
  resid = resid(mod),
  rstudent = rstud,
  leverage = h,
  cooksD = cooks,
  dffits = dffits_,
  covratio = covr
)

# Add DFBetas columns with names
dfb_df <- as.data.frame(dfb)
names(dfb_df) <- paste0("dfb_", names(coef(mod)))
out <- cbind(out, dfb_df)

# Flag observations exceeding thresholds
out$flag_leverage <- h > lev_cut
out$flag_cooks    <- cooks > cooks_cut
out$flag_dffits   <- abs(dffits_) > dffits_cut
out$flag_covratio <- (covr < covr_low) | (covr > covr_high)

# Any DFBetas large?
flag_dfb <- apply(abs(dfb_df) > dfbeta_cut, 1, any)
out$flag_dfbetas <- flag_dfb

cat("\nCutoffs (rules of thumb)\n")
cat(sprintf("High leverage: h_ii > %.3f\n", lev_cut))
cat(sprintf("Large Cook's D: D > %.3f\n", cooks_cut))
cat(sprintf("Large DFFITS: |DFFITS| > %.3f\n", dffits_cut))
cat(sprintf("Large |DFBETA_j| > %.3f (any coefficient)\n", dfbeta_cut))
cat(sprintf("Unusual COVRATIO: outside [%.3f, %.3f]\n", covr_low, covr_high))

cat("\nTop observations by influence\n")
print(out[order(-out$cooksD), ], row.names = FALSE)

# ---- Index plots (which obs stand out?) ----
par(mfrow = c(2,2), mar = c(4.2,4.2,2.2,1.2))
influenceIndexPlot(mod, vars = c("Cook", "Studentized", "Hat"), id.n = 3, main = "Index Plots")
plot(out$leverage, type="h", main="Leverage (hat values)", ylab="h_ii", xlab="Observation")
abline(h = lev_cut, col = 2, lty = 2)
plot(out$cooksD, type="h", main="Cook's Distance", ylab="D", xlab="Observation")
abline(h = cooks_cut, col = 2, lty = 2)
plot(abs(out$dffits), type="h", main="|DFFITS|", ylab="|DFFITS|", xlab="Observation")
abline(h = dffits_cut, col = 2, lty = 2)
par(mfrow = c(1,1))

# ---- Leverage vs Studentized residuals with Cook's contours ----
influencePlot(mod, id.method = "identify", main="Influence Plot: Studentized Residuals vs Leverage")

# ---- Cook's D plot from base 'plot.lm' (panel 4) ----
plot(mod, which = 4)  # adds another view of Cook's distance

# ---- Optional: PRESS residuals & statistic ----
press_resid <- resid(mod)/(1 - h)
PRESS <- sum(press_resid^2)
cat(sprintf("\nPRESS = %.4f\n", PRESS))
