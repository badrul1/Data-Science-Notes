
############################################################################################
## 0. Load package containing data

library(ISLR)

cat("[LOADED: Default data]\n", "Measurements: ", names(Default), "\n")
View(Default)
?Default

############################################################################################
## 1. Simple plots and simple logistic regression

x11()
pairs(Default)

x11()
plot(Default$balance, as.numeric(Default$default) - 1, xlab = "Balance", ylab = "Probability of Default")

m0 <- lm(as.numeric(default) - 1 ~ balance, data = Default)
m1 <- glm(default ~ balance, family = binomial, data = Default)
eval_balance <- sort(unique(Default$balance))
prd_linear <- predict(m0, newdata = data.frame(balance = eval_balance), type = "response")
prd_logistic <- predict(m1, newdata = data.frame(balance = eval_balance), type = "response")

lines(eval_balance, prd_linear, type = "l", col = "red", lwd = 5)
lines(eval_balance, prd_logistic, type = "l", col = "blue", lwd = 5)

summary(m1)

############################################################################################
## 2. Maximum likelihood in pictures

# Empirical pdf

theta_hat <- c(0.3, 0.5, 0.9)
for (i in seq_along(theta_hat)) {
  x11()
  plot(c(0, 0), c(0, 1 - theta_hat[i]), xlim = c(-0.1, 1.1), ylim = c(0, 1), xaxt = 'n',
       main = paste(expression(theta), "=", theta_hat[i]), xlab = "y", ylab = "P(Y = y)", type = "l", col = "blue", lwd = 3) 
  axis(side = 1, at = c(0, 1), labels = c("0", "1"))
  lines(c(1, 1), c(0, theta_hat[i]), type = "l", col = "blue", lwd = 3)
}

# Compare against empirical pmf

p_hat <- 0.67
theta_hat <- c(0.3, 0.5, p_hat)
for (i in seq_along(theta_hat)) {
  x11()
  plot(c(0, 0), c(0, 1 - p_hat), xlim = c(-0.1, 1.1), ylim = c(0, 1), xaxt = 'n',
       main = paste(expression(theta), "=", theta_hat[i]), xlab = "y", ylab = "P(Y = y)", type = "l", col = "red", lwd = 20) 
  axis(side = 1, at = c(0, 1), labels = c("0", "1"))
  lines(c(1, 1), c(0, p_hat), type = "l", col = "red", lwd = 20)
  lines(c(0, 0), c(0, 1 - theta_hat[i]), type = "l", col = "blue", lwd = 3)
  lines(c(1, 1), c(0, theta_hat[i]), type = "l", col = "blue", lwd = 3)
}

# Fitting a Gaussian model

true_mu <- 1
true_var <- 2
n <- 100
dat <- rnorm(n, mean = true_mu, sd = sqrt(true_var))
y_support <- seq(min(dat) - 1, max(dat) + 1, 0.1)

mu_attempts <- c(-2, 2, mean(dat))
var_attempts <- c(4, 3, var(dat))
true_pdfs <- dnorm(y_support, mean = true_mu, sd = sqrt(true_var))

x11()
for (i in seq_along(mu_attempts)) {
  llik <- -n * log(var_attempts[i]) - sum((dat - mu_attempts[i])^2) / var_attempts[i]
  pdfs <- dnorm(y_support, mean = mu_attempts[i], sd = sqrt(var_attempts[i]))
  plot(y_support, pdfs, xlim = c(min(y_support), max(y_support)), 
      ylim = c(0, max(true_pdfs)),
      type = "l", col = "blue", lwd = 5, xlab = "y", main = sprintf("Log-likelihood = %.2f", llik))
  lines(dat, rep(max(true_pdfs) / 2, n), type = "h", col = "gray", lwd = 3)
  lines(y_support, pdfs, type = "l", col = "blue", lwd = 5)
  lines(y_support, true_pdfs, type = "l", col = "red", lwd = 3)
  par(ask = TRUE)
}

cat(sprintf("Log likelihood at true parameters = %.2f\n", 
            -n * log(true_var) - sum((dat - true_mu)^2) / true_var))
    
############################################################################################
## 3. Back to logistic regression example

m1 <- glm(default ~ balance, family = binomial, data = Default)
summary(m1)

m2 <- glm(default ~ balance + income, family = binomial, data = Default)
summary(m2)

m3 <- glm(default ~ balance + income + student, family = binomial, data = Default)
summary(m3)

Default2 <- Default
Default2$dummy <- runif(nrow(Default))

m4 <- glm(default ~ balance + income + student + dummy, family = binomial, data = Default2)
summary(m4)

# Look at deviances, trying to understand whether adding variables helped. For example, balance vs balance + income

x_chisq <- seq(0, 50, 0.1) # First, some visualization of the chi-squared
x11()
plot(x_chisq, dchisq(x_chisq, 1), xlab = "x", ylab = "p(x)", main = expression(paste(chi^2, "(1)")), type = "l", lwd = 5, col = "blue")
x11()
plot(x_chisq, dchisq(x_chisq, 10), xlab = "x", ylab = "p(x)", main = expression(paste(chi^2, "(10)")), type = "l", lwd = 5, col = "blue")

model_diff <- m1$deviance - m2$deviance
df <- length(m2$coefficients) - length(m1$coefficients)
cat(sprintf("goodness of fit p-value, m1 vs m2: %.2f\n", pchisq(model_diff, df, lower.tail = FALSE))) # H0: m1, H1: m2. p-value tests whether m1 is "good enough" compared to m2

df <- nrow(Default) - length(m2$coefficients)
cat(sprintf("goodness of fit p-value, m2 stand alone: %.2f\n", pchisq(m2$deviance, df, lower.tail = FALSE))) # H0: m2, H1: not m2. p-value tests whether m2 is "good enough by itself"

y_rubbish <- rnorm(1000) > 0.5 # Contrasted it to data where predictor is useless
x_rubbish <- rnorm(1000)
m_rubbish <- glm(y_rubbish ~ x_rubbish, family = binomial)
df <- length(y_rubbish) - length(m_rubbish$coefficients)
cat(sprintf("goodness of fit p-value, m_rubbish stand alone: %.2f\n", pchisq(m_rubbish$deviance, df, lower.tail = FALSE)))

# First type of residual (uninteresting): output label minus predicted probability

pred_d <- m3$fitted.values
y <- as.numeric(Default$default) - 1
x11()
plot(c(0, 1), c(-1, 1), xlab = "Estimated  Pr (default)", 
     ylab = "Observed - estimated", type = "n", main = "Residual plot", mgp = c(2, .5, 0))
abline(0, 0, col = "gray", lwd = .5)
points(pred_d, y - pred_d)#, pch = 20, cex =. 2)

# Binned residuals (from Gelman and Hill, Chapter 5)

binned.resids <- function (x, y, nclass=sqrt(length(x))){
  breaks.index <- floor(length(x)*(1:(nclass-1))/nclass)
  breaks <- c (-Inf, sort(x)[breaks.index], Inf)
  output <- NULL
  xbreaks <- NULL
  x.binned <- as.numeric (cut (x, breaks))
  for (i in 1:nclass){
    items <- (1:length(x))[x.binned==i]
    x.range <- range(x[items])
    xbar <- mean(x[items])
    ybar <- mean(y[items])
    n <- length(items)
    sdev <- sd(y[items])
    output <- rbind (output, c(xbar, ybar, n, x.range, 2*sdev/sqrt(n)))
  }
  colnames (output) <- c ("xbar", "ybar", "n", "x.lo", "x.hi", "2se")
  return (list (binned=output, xbreaks=xbreaks))
}

# Against probability of default

binn_pred <- binned.resids(pred_d, y - pred_d, nclass = 40)$binned # 40 is a somewhat arbitrary number. Try 400 to see what you get
x11()
plot(range(binn_pred[, 1]), range(binn_pred[, 2], binn_pred[, 6], -binn_pred[, 6]), 
     xlab = "Estimated  Pr(default)", ylab = "Average residual",
     type = "n", main = "Binned residual plot", mgp = c(2, .5, 0))
abline(0, 0, col = "gray", lwd = .5)
lines(binn_pred[, 1], binn_pred[, 6], col = "gray", lwd = .5)
lines(binn_pred[, 1], -binn_pred[, 6], col = "gray", lwd = .5)
points(binn_pred[, 1], binn_pred[, 2], pch = 19, cex = .5)

# Against income

binn_pred <- binned.resids(Default$income, y - pred_d, nclass = 40)$binned
x11()
plot(range(binn_pred[, 1]), range(binn_pred[, 2], binn_pred[, 6], -binn_pred[, 6]), 
     xlab = "Income", ylab = "Average residual",
     type = "n", main = "Binned residual plot", mgp = c(2, .5, 0))
abline(0, 0, col = "gray", lwd = .5)
lines(binn_pred[, 1], binn_pred[, 6], col = "gray", lwd = .5)
lines(binn_pred[, 1], -binn_pred[, 6], col = "gray", lwd = .5)
points(binn_pred[, 1], binn_pred[, 2], pch = 19, cex = .5)

############################################################################################
## 4. Poisson binomial  regression

theta <- 5
x   <- 0:20
pmf <- dpois(x, theta)

x11()
plot(x, pmf, col = "blue", lwd = 5, main = paste("Probability Poisson(", theta, ")", sep = ""))

# Analyze Tube data

Tube <- read.table("tube_counts.csv", header = TRUE)
which_kingsx <- which(Tube$id == 101)
Tube <- Tube[-which_kingsx, ] # Remove King's Cross for simplicity
which_zero <- union(which(Tube$enter == 0), which(Tube$exit == 0))
Tube <- Tube[-which_zero, ] # Remove problematic stations
which_zero <- union(which(Tube$enter < 100), which(Tube$exit < 100))
Tube <- Tube[-which_zero, ] # Remove problematic stations, suspiciously low values
cat("[LOADED: tube data]\n", "Measurements: ", names(Tube), "\n")
View(Tube)

x11(); plot(Tube$position, Tube$exit, xlab = "Distance to King's Cross (km)", ylab = "Number of exits")
x11(); plot(log(Tube$position), Tube$exit, xlab = "Distance to King's Cross (log km)", ylab = "Number of exits")

m1 <- glm(enter ~ position + I(position^2), family = poisson, data = Tube)
summary(m1)
pchisq(m1$deviance, nrow(Tube) - length(m1$coefficients), lower.tail = FALSE) # yikes!

x11(); plot(Tube$position, Tube$exit, xlab = "Distance to King's Cross", ylab = "Number of exits", main = "Fitted model")
pos_eval <- seq(0, max(Tube$position), length.out = 100)
y_hat <- exp(m1$coefficients[1] + m1$coefficients[2] * pos_eval + m1$coefficients[3] * pos_eval^2)
lines(pos_eval, y_hat, col = "blue", lwd = 5)

#dy_hat <- m1$coefficients[2] + 2 * m1$coefficients[3] * pos_eval
#x11(); plot(pos_eval, dy_hat, col = "red", lwd = 5, type = "l", 
#            xlab = "Distance to King's Cross", ylab = "Number of exits", main = "Rate of change of log-expectation")
#
#dy_hat <- y_hat * (m1$coefficients[2] + 2 * m1$coefficients[3] * pos_eval) 
#x11(); plot(pos_eval, dy_hat, col = "red", lwd = 5, type = "l", 
#            xlab = "Distance to King's Cross", ylab = "Number of exits", main = "Rate of change of expectation")

# Analyze fitness

x11()
sort_fit <- sort(m1$fitted.values)
plot(m1$fitted.values, Tube$exit - m1$fitted.values, 
     xlab = "Fitted Values", ylab = "Residuals", main = "95% confidence intervals")
lb <- qpois(0.025, sort_fit)
ub <- qpois(0.975, sort_fit, lower.tail = FALSE)
lines(sort_fit, -lb, col = "gray", lwd = .5)
lines(sort_fit,  ub, col = "gray", lwd = .5)

############################################################################################
## 5. Negative binomial regression

library(MASS) # Install it with install.packages("MASS")

r <- 10
p <- 0.10
x_theta <- 0:500
pmf <- dnbinom(x_theta, r, p)
pdf <- dgamma(x_theta, shape = r, scale = (1 - p) / p)

x11()
plot(x_theta, pdf, col = "blue", lwd = 5, type = "l", xlab = expression(theta),
     main = paste("Gamma(", r, ", ", (1 - p) / p, ")", sep = ""))
x11()
plot(x_theta, pmf, col = "blue", lwd = 5, 
     main = paste("Negative Binomial(", r, ", ", p, ")", sep = ""))

m2 <- glm.nb(enter ~ position + I(position^2), data = Tube)
summary(m2)

# Variance is much higher, comparatively

x11()
sort_fit_poisson <- sort(m1$fitted.values)
sort_fit <- sort(m2$fitted.values)
var_fit <- sort_fit + sort_fit^2 / m2$theta
plot(sort_fit, var_fit, type = "l", col = "blue", lwd = 5, xlab = "Fitted mean", ylab = "Fitted variance")
lines(sort_fit_poisson, sort_fit_poisson, type = "l", col = "red", lwd = 5) # Variance of a Poisson of same mean: comparatively negligible

# Comparison of means: little difference

sort_fit_ix <- sort(m2$fitted.values, index.return = TRUE)$ix
x11(); plot(Tube$position, Tube$exit, xlab = "Distance to King's Cross (km)", ylab = "Number of exits")
lines(Tube$position[sort_fit_ix], m2$fitted.values[sort_fit_ix], col = "blue", lwd = 5)
sort_fit_ix <- sort(m1$fitted.values, index.return = TRUE)$ix
lines(Tube$position[sort_fit_ix], m1$fitted.values[sort_fit_ix], col = "red", lwd = 5)

#b2 <- gam(enter ~ s(position), family = nb(link = "log"), data = Tube, method="REML") 
#x11(); plot(Tube$position, Tube$exit, xlab = "Distance to King's Cross (km)", ylab = "Number of exits")
#sort_fit_ix <- sort(m2$fitted.values, index.return = TRUE)$ix
#lines(Tube$position[sort_fit_ix], b2$fitted.values[sort_fit_ix], col = "blue", lwd = 5)

############################################################################################
## 6. Ordinal regression

library(ordinal)
data(soup)
View(soup)
?soup
x11(); plot(soup$AGEGROUP, soup$SURENESS, xlab = "Age group", ylab = "Soup 'sureness'")

m1 <- clm(SURENESS ~ SOUPTYPE + SOUPFREQ + COLD + GENDER + AGEGROUP, data = soup)
summary(m1)

# Plot of probability as a function of linear responses

logistic <- function(x) {return (1 / (1 + exp(-x)))}

eta <- seq(-5, 5, 0.1)
thresholds <- c(0, 0.5, 1.5, Inf)
K <- length(thresholds)
Pr <- matrix(ncol = K, nrow = length(eta))
Pr[, 1] <- 1 - logistic(eta - thresholds[1])
for (k in 2:K) {
  Pr[, k] <- logistic(eta - thresholds[k - 1]) - logistic(eta - thresholds[k])
}

x11()
plot(eta, Pr[, 1], col = "blue", lwd = 3, type = "l", ylim = c(0, 1),
     xlab = expression(eta), ylab = "Probability", main = paste(K, "ordinal values"))
lines(eta, Pr[, 2], col = "red", lwd = 3, type = "l")
lines(eta, Pr[, 3], col = "green", lwd = 3, type = "l")
lines(eta, Pr[, 4], col = "black", lwd = 3, type = "l")

############################################################################################
## 7. Newton-Raphson 1D illustration

my_g1 <- function(x){-x^3/3 + x^2 + 3 * x}
my_f1 <- function(x){-x^2 + 2 * x + 3} # First derivative of g
my_f1_prime <- function(x){-2 * x + 2} # Second derivative of g
my_g <- my_g1
my_f <- my_f1
my_f_prime <- my_f1_prime
x_range <- seq(-5, 5, 0.1)
y_f <- my_f(x_range)
y_g <- my_g(x_range)

x11()
plot(x_range, y_g, type = "l", col = "blue", lwd = 3, xlab = "x", ylab = "y", main = "Original function")

x_initial <- 0

x11()
plot(x_range, y_f, type = "l", col = "blue", lwd = 3, xlab = "x", ylab = "y")
lines(x = c(min(x_range), max(x_range)), y = c(0, 0), col = "gray")

num_iter <- 10
x_current <- x_initial
y_current <- my_f(x_current)
points(x = x_current, y = y_current, lwd = 10)
points(x = x_current, y = y_current, col = "red", lwd = 5)
title(sprintf("Function value %.2f", y_current))
tol <- 1.e-5
par(ask = TRUE)

for (i in 1:num_iter) {
  plot(x_range, y_f, type = "l", col = "blue", lwd = 3, xlab = "x", ylab = "y")
  title(sprintf("Function value %.2f (Iteration %d)", y_current, i))
  lines(x = c(min(x_range), max(x_range)), y = c(0, 0), col = "gray")
  points(x = x_current, y = y_current, lwd = 10)
  points(x = x_current, y = y_current, col = "red", lwd = 5)
  par(ask = TRUE)
  x_new <- x_current - my_f(x_current) / my_f_prime(x_current)
  y_new <- my_f(x_new)
  if (abs(x_current - x_new) < tol) break
  lines(x = c(x_current, x_new), y = c(y_current, y_new), col = "red")
  x_current <- x_new
  y_current <- y_new
  points(x = x_current, y = y_current, lwd = 10)
  points(x = x_current, y = y_current, col = "red", lwd = 5)
}

plot(x_range, y_f, type = "l", col = "blue", lwd = 3, xlab = "x", ylab = "y")
title(sprintf("Possible solution: %.2f", x_current))
lines(x = c(min(x_range), max(x_range)), y = c(0, 0), col = "gray")
points(x = x_current, y = y_current, lwd = 10)
points(x = x_current, y = y_current, col = "red", lwd = 5)

# Experiment with a different starting point (x_initial <- 4, for example)

############################################################################################
## 8. Section 4.6.2 of ISLR

library(ISLR)
View(Smarket)
?Smarket
summary(Smarket)

cor(Smarket)
cor(Smarket[, -9])

# Volume and Year are the clearly correlated variables
x11(); plot(Smarket$Year, Smarket$Volume, xlab = "Year", ylab = "Volume")
x11(); plot(Smarket$Volume, ylab = "Volume") # Data are order in time anyway

# Fit

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit)

# "Predict"

glm.probs <- predict(glm.fit, type = "response") # What is the default response?
glm.probs[1:10] # Wait, is this the probability of up or down?
contrasts(Smarket$Direction) # check with this

glm.pred <- rep("Down", nrow(Smarket)) # Threshold to get predictions
glm.pred[glm.probs >.5] <- "Up"

# Assess training accuracy

table(glm.pred, Smarket$Direction)
(507+145) / 1250
mean(glm.pred == Smarket$Direction)

# Create a separate test set

train <- (Smarket$Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", nrow(Smarket.2005))
glm.pred[glm.probs >.5] <- "Up"

table(glm.pred, Smarket.2005$Direction)
mean(glm.pred == Smarket.2005$Direction)

mean(Smarket.2005$Direction == "Up") # Default choice

# Trying hard: use the most associated covariates

glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", nrow(Smarket.2005))
glm.pred[glm.probs >.5] <- "Up"

table(glm.pred, Smarket.2005$Direction)
mean(glm.pred == Smarket.2005$Direction) # Not better than default!

# Alternative interpretation: error when we see a prediction of "Up"

106 / (106 + 76) # Good? Maybe not. But an alternative direction to explore.

