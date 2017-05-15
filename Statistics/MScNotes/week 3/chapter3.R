
############################################################################################
## 0. Read data

adv <- read.table("advertising.csv", header = TRUE, sep = ",", colClasses = c('NULL', NA, NA, NA, NA))
cat("[LOADED: advertisement data]\n", "Measurements: ", names(adv), "\n")
fix(adv)

############################################################################################
## 1. Simple plots and simple regressions

x11()
pairs(adv)

m1 <- lm(adv$Sales ~ adv$TV)
x11()
plot(m1)

############################################################################################
## 2. Digression: no need to likelihood functions, empirical cdf justification

# Empirical cdf 

s <- c(10, 25, 100, 1000, 10000)
x11()
for (i in seq_along(s)) {
  y <- rnorm(s[i])
  e_y <- ecdf(y)
  plot(e_y, main = sprintf("Sample size %d", s[i]), xlim = c(-4, 4), verticals = TRUE)
  readline()
}

# Empirical pdf

s <- c(10, 25, 100, 1000)
x11()
for (i in seq_along(s)) {
  y <- rnorm(s[i])
  plot(y, rep(1, s[i]), ylim = c(0, 1.1), main = sprintf("Sample size %d", s[i]), xlim = c(-4, 4))
  for (j in 1:s[i]) {
    lines(c(y[j], y[j]), c(0, 1), col = "blue", lwd = 1)
  }
  readline()
}

# Non-gaussian errors and least-squares

x_range <- seq(0, 10, 0.01)
f_range <- dexp(x_range) # Laplace with parameter 1
f <- c(f_range[length(f_range):1], f_range) / 2
x <- c(-x_range[length(x_range):1], x_range)
f_gauss <- dnorm(x, 0, sqrt(2)) # Gaussian with variance 2, same variance as the Laplacian above
x11(); plot(x, f, type = "l", lwd = 5, col = "blue", ylab = "pdf", main = "Double Exponential(1) a.k.a. Laplace")
lines(x, f_gauss, lwd = 3, col = "red")

n <- 50
var_error <- 10
num_rep <- 1000
beta_true <- 1
error_l <- 0
error_g <- 0
for (i in 1:num_rep) {
  errors_l <- rexp(n) * (as.numeric(runif(n) < 0.5) * 2 - 1) * sqrt(0.5 * var_error)
  errors_g <- rnorm(n)* sqrt(var_error)
  x <- rnorm(n)
  y_g <- beta_true * x + errors_g
  y_l <- beta_true * x + errors_l
  beta_hat_g <- lm(y_g ~ x)$coefficients[2]
  beta_hat_l <- lm(y_l ~ x)$coefficients[2]
  cat(sprintf("[%d] Estimated beta (Gaussian error): %.2f, estimated beta (Laplacian error): %.2f\n", i, beta_hat_g, beta_hat_l))
  error_g <- error_g + abs(beta_hat_g - beta_true)
  error_l <- error_l + abs(beta_hat_l - beta_true)
}
cat(sprintf("Average error (Gaussian): %.2f\n Average error (Laplacian): %.2f\n", error_g / num_rep, error_l / num_rep))


############################################################################################
## 2. Understanding R2 with synthetic data. Ideas taken from Section 2.2 of Shalizi.

# Check the R2 of m1

summary(m1)

# Synthetic example

n <- 100

X_U_01 <- runif(n)
X_N <- rnorm(n, 0.5, 0.1)
X_U_23 <- runif(n, 2, 3)
X_ALL <- c(X_U_01, X_N, X_U_23)

Y1 <- sqrt(X_U_01) + rnorm(n, 0, 0.05)
Y2 <- sqrt(X_N) + rnorm(n, 0, 0.05)
Y3 <- sqrt(X_U_23) + rnorm(n, 0, 0.05)
Y_ALL <- c(Y1, Y2, Y3)

x <- seq(0, 3, 0.1)
y_true <- sqrt(x)

m1 <- lm(Y1 ~ X_U_01)
m2 <- lm(Y2 ~ X_N)
m3 <- lm(Y3 ~ X_U_23)
m4 <- lm(Y_ALL ~ X_ALL)


x11()
plot(X_U_01, Y1, xlim = c(0, 3), ylim = c(0, 3), xlab = "X", ylab = "Y", main = "Simulations")
points(X_N, Y2, pch = 2, col = "blue")
points(X_U_23, Y3, pch = 0, col = "red")
lines(x, y_true, col = "gray", lwd = "2")

xu <- 3.5
lines(c(0, xu), c(m1$coefficients[1], m1$coefficients[1] + m1$coefficients[2] * xu))
lines(c(0, xu), c(m2$coefficients[1], m2$coefficients[1] + m2$coefficients[2] * xu), col = "blue")
lines(c(0, xu), c(m3$coefficients[1], m3$coefficients[1] + m3$coefficients[2] * xu), col = "red")
lines(c(0, xu), c(m4$coefficients[1], m4$coefficients[1] + m4$coefficients[2] * xu), col = "green", lwd = 3)

cat(sprintf("R2 for each model: %.2f (black), %.2f (blue), %.2f (red)\n", summary(m1)$r.squared, summary(m2)$r.squared, summary(m3)$r.squared))
cat(sprintf("R2 for model with full data: %.2f\n", summary(m4)$r.squared))

############################################################################################
## 3. Residual plots. Contrast advertisement against a good synthetic model

n <- 100
beta_true <- 2
X <- rnorm(n)
Y <- beta_true * X + rnorm(n)
x11(); plot(X, Y, main = "Synthetic case")
m1 <- lm(Y ~ X)
summary(m1)
x11(); plot(m1)

# From the previous model, let artificially enter an outlier by adding extra noise to point #100
# Try this by setting n = 100 and n = 1000

X_copy <- X
Y_copy <- Y
Y_copy[100] <- Y_copy[100] + 10
x11(); plot(X_copy, Y_copy)
m_copy <- lm(Y_copy ~ X_copy)
x11(); plot(m_copy)

# From the previous model, let artificially enter a high leverage point
# Try this by setting n = 100 and n = 1000

n <- 100
X <- rnorm(n)
Y <- beta_true * X + rnorm(n)
m1 <- lm(Y ~ X)

X_new <- max(X) + 1
Y_new <- beta_true * X_new + 10
X <- c(X, X_new)
Y <- c(Y, Y_new)
x11(); plot(X, Y)
m2 <- lm(Y ~ X)
summary(m2)
lines(c(min(X), max(X)), c(m1$coefficients[1] + m1$coefficients[2] * min(X), m1$coefficients[1] + m1$coefficients[2] * max(X)), col = "blue")
lines(c(min(X), max(X)), c(m2$coefficients[1] + m2$coefficients[2] * min(X), m2$coefficients[1] + m2$coefficients[2] * max(X)), col = "red")

## Laplace model

n <- 10000 # Try this also with n = 10000
beta_true <- 1
var_error <- 1
errors_l <- rexp(n) * (as.numeric(runif(n) < 0.5) * 2 - 1) * sqrt(0.5 * var_error)
X_l <- rnorm(n) # Try this also with Uniform(-1, 1) i.e. X_l <- runif(n, -1, 1)
Y_l <- beta_true * X_l + errors_l
m3 <- lm(Y_l ~ X_l)
x11(); plot(Y_l, X_l)
summary(m3)
x11(); plot(m3)
  
# Advertisement data: run it again

m1 <- lm(adv$Sales ~ adv$TV)
x11(); plot(m1)
high_lev <- 179
x11(); plot(adv$TV, adv$Sales, xlab = "TV", ylab = "Sales", main = "High leverage highlighted")
points(adv$TV[high_lev], adv$Sales[high_lev], col = "red", pch = 19, lwd = 5)

m2 <- lm(adv$Sales ~ adv$TV + adv$Radio + adv$Newspaper)
x11()
plot(m2)

############################################################################################
## 4. More on tests

# Using all variables

m2 <- lm(adv$Sales ~ adv$TV + adv$Radio + adv$Newspaper)
summary(m2)

# Looks like newspapers are not predictive of sales. But in which sense?

m3 <- lm(adv$Sales ~ adv$Newspaper)
summary(m3)

############################################################################################
## 5. Predictive and confidence intervals

m1 <- lm(Sales ~ TV, data = adv)

x11()
plot(adv$TV, adv$Sales, xlab = "TV", ylab = "Sales", main = "Confidence interval")
abline(m1, col = "blue", lwd = 5)
newx <- seq(min(adv$TV), max(adv$TV), 0.1)
prd <- predict(m1, newdata = data.frame(TV = newx), interval = c("confidence"), level = 0.95, type = "response")
lines(newx, prd[ ,2],col = "red", lty = 2)
lines(newx, prd[ ,3],col = "red", lty = 2)

x11()
prd_p <- predict(m1, newdata = data.frame(TV = newx), interval = c("prediction"), level = 0.95, type = "response")
plot(adv$TV, adv$Sales, xlab = "TV", ylab = "Sales", main = "Prediction interval")
abline(m1, col = "blue", lwd = 5)
lines(newx, prd_p[ ,2],col = "red", lty = 2)
lines(newx, prd_p[ ,3],col = "red", lty = 2)

adv_smaller <- adv[1:20, ] # Reduced data set, to illustrate what happens to confidence intervals
m2 <- lm(Sales ~ TV, data = adv_smaller)
x11()
plot(adv_smaller$TV, adv_smaller$Sales, xlab = "TV", ylab = "Sales", main = "Confidence interval (sample size = 20)")
abline(m2, col = "blue", lwd = 5)
newx <- seq(min(adv$TV), max(adv$TV), 0.1)
prd2 <- predict(m2, newdata = data.frame(TV = newx), interval = c("confidence"), level = 0.95, type = "response")
lines(newx, prd2[ ,2],col = "red", lty = 2)
lines(newx, prd2[ ,3],col = "red", lty = 2)

############################################################################################
## 6. Non-linear transformations  

m1 <- lm(Sales ~ log(TV), data = adv)
x11(); plot(log(adv$TV), adv$Sales, xlab = "log(TV)", ylab = "Sales")
x11(); plot(m1)

m2 <- lm(log(Sales) ~ TV, data = adv)
x11(); plot(adv$TV, log(adv$Sales), xlab = "TV", ylab = "log(Sales)")
x11(); plot(m2)

m3 <- lm(log(Sales) ~ log(TV), data = adv)
x11(); plot(log(adv$TV), log(adv$Sales), xlab = "log(TV)", ylab = "log(Sales)")
x11(); plot(m3)

m4 <- lm(Sales ~ TV + I(TV^2), data = adv)
tv_range <- seq(min(adv$TV), max(adv$TV), 0.1)
sale_pred <- cbind(rep(1, length(tv_range)), tv_range, tv_range^2) %*% m4$coefficients
x11(); plot(adv$TV, adv$Sales, xlab = "TV", ylab = "Sales")
points(tv_range, sale_pred, col = "blue", lwd = 2)

m1 <- lm(Sales ~ TV + Radio, data = adv)
m12 <- lm(Sales ~ TV + Radio + TV * Radio, data = adv)

############################################################################################
## 7. Discrete inputs

cred <- read.table("credit.csv", header = TRUE, sep = ",")
m1 <- lm(Balance ~ Gender, data = cred)
summary(m1)

# Create dataset with redundant information. It has numerical problems.

new_dat <- cbind(cred$Balance, as.numeric(cred$Gender == " Male"),
                 as.numeric(cred$Gender == "Female"))
m0 <- lm(new_dat[, 1] ~ new_dat[, 2] + new_dat[, 3])
summary(m0)

# Full model: notice encoding of ethnicity

m2 <- lm(Balance ~ ., data = cred)
summary(m2)

# More on collinearity with the credit data

x11();pairs(cred)
x11();plot(cred$Limit, cred$Rating, xlab = "Limit", ylab = "Rating", main = "Credit dataset")
