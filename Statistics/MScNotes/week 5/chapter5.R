
############################################################################################
## 0. Read data

setwd("z:/docs/teaching/G006/data/Chapter5/")
crd <- read.table("Credit.csv", header = TRUE, sep = ",")
cat("[LOADED: credit data]\n", "Measurements: ", names(crd), "\n")


############################################################################################
## 1. Simple plots and simple regressions

x11(); plot(crd)
x11(); plot(crd$Income, crd$Balance, xlab = "Income", ylab = "Balance")
x11(); plot(crd$Age, crd$Balance, xlab = "Age", ylab = "Balance")

crd_model_full <- lm(Balance ~ ., data = crd)
summary(crd_model_full)

crd_model_empty <- lm(Balance ~ 1, data = crd)
summary(crd_model_empty)

############################################################################################
## 2. Forward/backward regressions

crd_fwd <- step(crd_model_empty, scope = list(upper = crd_model_full), direction = "forward")

crd_bwd <- step(crd_model_full, scope = list(lower = crd_model_empty), direction = "backward")

summary(crd_fwd)
summary(crd_bwd)

# A demonstration that it is indeed a local minimum: start search from previous point
crd_redo <- step(crd_bwd, scope = list(lower = crd_model_empty, upper = crd_model_full), direction = "both")
summary(crd_redo)

############################################################################################
## 3. Size vs. complexity

# Here, I generate coefficients for a polynomial of fifth degree from a Normal of standard
# deviation 0.1

num_trials <- 10
x <- seq(0, 10, 0.1)
K <- 100

W <- matrix(rnorm(K * 2, mean = 0, sd = 1), ncol = 2)
x_basis <- matrix(rep(0, K * length(x)), ncol = K)
for (k in 1:K) x_basis[, k] <- 2 / (1 + exp(-W[k, 1] - W[k, 2] * x)) - 1
x11()
for (i in 1:num_trials) {
  beta <- rnorm(K) / sqrt(K)
  y <- x_basis %*% beta 
  plot(x, y, main = "Neural network, random coefficients with sd = 1")
  lines(x, y, lwd = 5, col = "blue")
  par(ask = TRUE)
}

W <- matrix(rnorm(K * 2, mean = 0, sd = 10), ncol = 2)
x_basis <- matrix(rep(0, K * length(x)), ncol = K)
for (k in 1:K) x_basis[, k] <- 2 / (1 + exp(-W[k, 1] - W[k, 2] * x)) - 1
x11()
for (i in 1:num_trials) {
  beta <- rnorm(K) / sqrt(K)
  y <- x_basis %*% beta 
  plot(x, y, main = "Neural network, random coefficients with sd = 10")
  lines(x, y, lwd = 5, col = "blue")
  par(ask = TRUE)
}

W <- matrix(rnorm(K * 2, mean = 0, sd = 100), ncol = 2)
x_basis <- matrix(rep(0, K * length(x)), ncol = K)
for (k in 1:K) x_basis[, k] <- 2 / (1 + exp(-W[k, 1] - W[k, 2] * x)) - 1
x11()
for (i in 1:num_trials) {
  beta <- rnorm(K) / sqrt(K)
  y <- x_basis %*% beta 
  plot(x, y, main = "Neural network, random coefficients with sd = 100")
  lines(x, y, lwd = 5, col = "blue")
  par(ask = TRUE)
}

############################################################################################
## 4. Dangers of ignoring data scale in ridge regression

library(glmnet)

x <- as.matrix(crd[, 1:6]) # Use continuous inputs only, for simplicity
y <- crd[, 11] # Balance

# Do it without standardization first
y_model1 <- glmnet(x, y, alpha = 0, standardize = FALSE)
x11(); plot(y_model1, xvar = "lambda")

# Check variance of inputs
diag(var(x))

# Standardize it
y_model2 <- glmnet(x, y, alpha = 0, standardize = TRUE)
x11(); plot(y_model2, xvar = "lambda")

# Compare

name_inputs <- names(crd)
for (i in 1:6) {
  min_v <- min(y_model1$beta[i, ], y_model2$beta[i, ])
  max_v <- max(y_model1$beta[i, ], y_model2$beta[i, ])
  x11(); plot(y_model1$beta[i, ], type = "l", col = "blue", lwd = 5, main = paste(name_inputs[i], "(red: standardized)"), 
              ylim = c(min_v, max_v), xlab = "Lambda index", ylab = "Coefficients")
  lines(y_model2$beta[i, ], col = "red", lwd = 5)
}

############################################################################################
## 6. Lab 1, Chapter 6 (ISLR)

library(ISLR) # Load the data
library(leaps) # For best subset selection. More flexible library than the standard R functions for subset selection.

View(Hitters)
help(Hitters)

# First observation: missing data

sum(is.na(Hitters$Salary))
dim(Hitters)

Hitters <- na.omit(Hitters)
dim(Hitters)

# Now, let's select a model based on combinatorial search

regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)

# Risky business: allowing now for all combinations up to size 19

regfit.full <- regsubsets(Salary ~ ., Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)

# What is in the summary?

names(reg.summary)

reg.summary$rsq # How the R2 changes

x11(); par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab  = "Number of Variables", ylab = "Adjusted RSq",type = "l")

# Other measures

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)

points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)
which.min(reg.summary$bic)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col ="red", cex = 2, pch = 20)

# Using the package's built in plot functions

x11(); plot(regfit.full, scale = "r2")
x11(); plot(regfit.full, scale = "Cp")
x11(); plot(regfit.full, scale = "bic")

coef(regfit.full, 6)

# Forward and Backward searches

regfit.fwd <- regsubsets(Salary ~ .,  data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd )

# There is some agreement between search algorithms, but notice the difference

coef(regfit.full, 7)
coef(regfit.bwd, 7)
coef(regfit.fwd, 7)

s.full <- summary(regfit.full)
s.fwd <- summary(regfit.fwd)
x11(); plot(s.full$rss, lwd = 5, col = "blue", type = "l", main = "RSS", xlab = "Number of variables", ylab = "RSS")
lines(s.fwd$rss, lwd = 5, col = "red")

############################################################################################
## 7. Lab 2, Chapter 6 (ISLR)

library(glmnet)
library(ISLR) # Load the data

x <- model.matrix(Salary ~ ., Hitters )[, -1] # Useful to remove NAs, and also to transform categories into dummies
y <- na.omit(Hitters$Salary) # In case you are loading it now

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid) # alpha = 0 is ridge regression. Standardization is TRUE by default.

dim(coef(ridge.mod)) # 19 + intercept

# What we will observe next: smaller values of lambda will lead to overall "larger" parameters

ridge.mod$lambda[50]
coef(ridge.mod)[, 50]

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]

# No need to re-run fitting to get parameters at new values of lambda

predict(ridge.mod, s = 50, type = "coefficients")

# Training and test split: 50/50 for the sake of illustration

set.seed(1) # Discussion: what does this do?
train <- sample(1: nrow(x), nrow(x) / 2)
test <- -train
y.test <- y[test]

# Fit

ridge.mod <- glmnet(x[train ,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ]) # Assessment at lambda = 4
sqrt(mean((ridge.pred - y.test)^2)) # Root mean squared error (RMSE)

# What does that mean? It might be useful to have a sense of the scale of the output before observing the input

cat(sprintf("Empirical standard deviation (all data) = %.2f\n", sd(y)))
cat(sprintf("Empirical RMSE with intercept only = %.2f\n", mean(sqrt((ridge.pred - mean(y.test))^2))))
x11(); hist(y, main = "Salary distribution")

# Compare against unregularized least-squares

ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ]) # Assessment at lambda = 0
sqrt(mean((ridge.pred - y.test)^2)) # Root mean squared error (RMSE)

# Now, using cross-validation

set.seed(1)
cv.out <- cv.glmnet (x[train, ], y[train], alpha = 0)
x11(); plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam # Do the above again, without setting the seed!

# Now, test it using the selected regularization level

ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])
sqrt(mean((ridge.pred - y.test)^2))

# Finally: fit the whole data using the selected lambda

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam) #No sparsity, as expected

# Now, Lasso: with the elastic net function glmnet, it is a matter of setting alpha to 1

lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
x11(); plot(lasso.mod) # Possibility of sparsity is evident

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
x11(); plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])
sqrt(mean((lasso.pred - y.test)^2)) # Not really an improvement over ridge regression. However, coefficients are "simpler"

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)
lasso.coef

lasso.coef <- lasso.coef[1:20, 1]
lasso.coef[lasso.coef != 0]

############################################################################################
## 8. Lab, Chapter 7 (ISLR)

library(ISLR) # Load the data
attach(Wage)

View(Wage)

# Linear model

fit <- lm(wage ~ poly(age, 4), data = Wage) # Notice: basis functions for poly are not the "raw" ones
coef(summary(fit))

fit2 <- lm(wage ~ poly(age, 4, raw = TRUE), data = Wage) # Notice: basis functions for poly are not the "raw" ones
coef(summary(fit2))

# Reality check

fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
coef(fit2a)

# Building predictions and visualizing them

agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

x11(); par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex =.5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col =" blue", lty = 3)

# Illustration: degree tests using F statistics (recall the GLM lecture. This complements other methods)

fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# Switch to binomial (logistic) regression

fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE) # Beware that prediction here is given as the log-odds

# As an illustration, let's convert the responses manually (there is a way of doing that directly with "predict")

pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2* preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(age), I((wage > 250) / 5), cex =.5, pch ="|", col = "darkgrey") # Notice the use of 'jitter'
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Now, regression with step functions. "cut" provides an uniform separation across the range

table(cut(age, 4))

fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary (fit)) # Interpretation

# Splines

library(splines)

fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage) # Pre-specified knots
pred <- predict(fit, newdata =list(age = age.grid), se = TRUE)
x11(); plot(age, wage, col = "gray")
lines(age.grid, pred$fit,lwd= 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")

# The following fits a natural spline, where we knots are chosen automatically based on
# degrees of freedom (that gives the number of knots) and the default choice of uniformity 
# with quantiles.

fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = TRUE)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

# Smoothing spline

x11(); plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df = 16)
fit2 <- smooth.spline(age, wage, cv = TRUE)
fit2$df

lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# Local regression

x11(); plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression ")
fit <- loess(wage ~ age, span = .2, data = Wage)
fit2 <- loess(wage ~ age,span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red ", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# GAMs

# In some cases, we act as if we have linear models with a pre-defined dictionary

gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)

# But then we have to think how to convert this to confidence intervals in the
# target function space etc. For smoothing splines, we can make our life simpler
# by using specialized functions.

library(gam)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)

x11(); par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")

# Nested testing (which we did not discuss in full detail in the slides, but
# once again follow the logic outlined in the GLM lecture)

gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage) # Linear in "year"
anova(gam.m1, gam.m2, gam.m3, test = "F")

summary(gam.m3) # Notice: in the tests here, the null hypothesis is a linear model for the respective covariate

# We can use GAMs with other base smoothers. Here, local regression is combined with a smoothing spline.

gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
x11(); par(mfrow = c(1, 3)); plot.gam(gam.lo, se = TRUE, col = "green")

# Notice: we can add interactions within the smoother

gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)
library(akima)
x11();par(mfrow = c(1, 2)); plot(gam.lo.i)

# Logistic GAM

gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
x11(); par(mfrow = c(1, 3))
plot(gam.lr, se = TRUE, col = "green") # Very weird plot

table(education, I(wage > 250)) # Diagnostic

gam.lr.s <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage, 
                                subset = (education !="1. < HS Grad"))
x11(); par(mfrow = c(1, 3)); plot(gam.lr.s, se = TRUE, col = "green")
