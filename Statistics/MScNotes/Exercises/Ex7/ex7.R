library(ISLR)

### Question 6

View(Wage)
?Wage

# (a)

library(boot) # For cross-validation

d_max <- 15 # Going up to degree 15!
cv_out <- rep(0, d_max)
best_cv <- Inf

for (d in 1:d_max) {
  model <- glm(wage ~ poly(age, d), data = Wage) # This is still a least-square fit
  cv_assess <- cv.glm(Wage, model, K = 10) # A bad implementation of leave-one-out 
                                           # cross-validation in a linear model, so we
                                           # go with K-fold CV. See ISLR Chapters 5 and 7.
  cv_out[d] <- cv_assess$delta[1]
  if (cv_out[d] < best_cv) {
    best_model <- model
    best_cv <- cv_out[d]
  }
}

# Try it twice and see cv_out change! But the choice of best model
# should remain approximately the same.

d <- which.min(cv_out)
cat(sprintf("Polynomial choice: %d\n", which.min(cv_out)))
x11(); plot(cv_out, xlab = "Degree", ylab = "CV error", main = "Cross-validation progress")

# Remind yourself of difficulties in fitting this:

x11(); plot(Wage$age, Wage$wage, xlab = "Age", ylab = "Wage")
pred_test <- data.frame(age = seq(min(Wage$age), max(Wage$age), 1))
y_hat <- predict(best_model, pred_test)
y_hat2 <- predict(glm(wage ~ poly(age, 3), data = Wage), pred_test)
lines(pred_test$age, y_hat, lwd = 5, col = "red")
lines(pred_test$age, y_hat2, lwd = 5, col = "blue") # Contrasting it to a simpler model

# (b)

# Similar, now using the cut function instead of poly

# Example

cut(Wage$age, 5)
x11(); hist(Wage$age, xlab = "Age", main = "Histogram")

# Now, fit

model <- glm(wage ~ age, data = Wage) # %&£$@^&! cut function is too dumb to account for d == 1
cv_assess <- cv.glm(Wage, model, K = 10)
cv_out[1] <- cv_assess$delta[1]
best_cv <- cv_out[1] # The code that follows will actually break if the best model is the linear one! (Why?)

for (d in 2:d_max) {
  age_discrete <- cut(Wage$age, d) # Not the greatest way of doing it,
                                   # as we are using the whole data to define
                                   # the cuts. We should use only the training data.
                                   # Unfortunately, cv.glm will have problems with
                                   # that. I will be lazy here and set cuts using
                                   # the whole data. Please be aware this is a hack.
                                   # I'll leave to you a more sensible way of doing this.
  Wage2 <- cbind(Wage, age_discrete)
  model <- glm(wage ~ age_discrete, data = Wage2)
  cv_assess <- cv.glm(Wage2, model, K = 10)
  cv_out[d] <- cv_assess$delta[1]
  if (cv_out[d] < best_cv) {
    best_model <- model
    best_cv <- cv_out[d]
  }
}

d <- which.min(cv_out)
cat(sprintf("Cut resolution choice: %d\n", d))
x11(); plot(cv_out, xlab = "Degree", ylab = "CV error", main = "Cross-validation progress")

age_discrete <- cut(Wage$age, d)

Wage2 <- cbind(Wage, age_discrete)
x11(); plot(Wage2$age, Wage2$wage, xlab = "Age", ylab = "Wage")
pred_test_list <- sort(Wage$age, index.return = TRUE)
pred_test <- data.frame(age_discrete = age_discrete[pred_test_list$ix])
y_hat <- predict(best_model, pred_test)
lines(pred_test_list$x, y_hat, lwd = 5, col = "red")
lines(seq(min(Wage$age), max(Wage$age), 1), y_hat2, lwd = 5, col = "blue") # Contrasting it to the cubic polynomial

### Question 7

library(MASS)
View(Boston)
?Boston

# (a)

model <- lm(nox ~ poly(dis, 3), data = Boston)
summary(model)

test_dis <- seq(min(Boston$dis), max(Boston$dis), 0.1)
y_hat <- predict(model, newdata = list(dis = test_dis))

x11(); plot(Boston$dis, Boston$nox, xlab = "dis", ylab = "nox", main = "Boston housing")
lines(test_dis, y_hat, type = "l", lwd = 5, col = "blue")

x11(); plot(model)

# (b)

for (i in 1:15) {
  model <- lm(nox ~ poly(dis, i), data = Boston)
  rss <- sum(model$residuals^2)
  cat(sprintf("RSS for polynomial of order %d = %.2f\n", i, rss))
}

# (c): nothing new here, see solution from previous exercise

# (d)

library(splines)

summary(Boston$dis)

dis_bs <- bs(Boston$dis, df = 4)
attr(dis_bs, "knots")

model <- lm(nox ~ bs(dis, df = 4), data = Boston)
summary(model)
y_hat <- predict(model, newdata = list(dis = test_dis))

x11(); plot(Boston$dis, Boston$nox, xlab = "dis", ylab = "nox", main = "Boston housing")
lines(test_dis, y_hat, lwd = 5, col = "blue") 
# Notice endpoint. Contrast this with the polynomial basis. Read about the relationship between that and natural splines

# (e)

df_space <- c(4, 10, 50, 100)
col_df_space <- c("gray", "blue", "red", "green")

x11(); plot(Boston$dis, Boston$nox, xlab = "dis", ylab = "nox", main = "Boston housing")

i_count <- 1
for (i in df_space) {
  model <- lm(nox ~ bs(dis, df = i), data = Boston)
  y_hat <- predict(model, newdata = list(dis = test_dis))
  lines(test_dis, y_hat, lwd = 5, col = col_df_space[[i_count]])   
  i_count <- i_count + 1
  #par(ask = TRUE)
  rss <- sum(model$residuals^2)
  cat(sprintf("Degrees of freedom vs RSS: %d, %.2f\n", i, rss))
  line <- readline()
}

# (f)

df_space <- 4:10
best_cv <- Inf
cv_out <- rep(0, length(df_space))
d_count <- 1

library(cvTools) # Another way of doing cross-validation

for (d in df_space) {
  model <- lm(nox ~ bs(dis, df = d), data = Boston)
  call_boston <- call("lm", formula = nox ~ bs(dis, df = d), data = Boston)
  cv_assess <- cvFit(call_boston, data = Boston, y = Boston$nox, K = 10)
  cv_out[d_count] <- cv_assess$cv
  if (cv_out[d_count] < best_cv) {
    best_model <- model
    best_cv <- cv_out[d_count]
  }
  d_count <- d_count + 1
} # Igore warnings. Can you see where they come from?

d <- df_space[which.min(cv_out)]
cat(sprintf("Choice of degrees of freedom: %d\n", d))
x11(); plot(df_space, cv_out, xlab = "Degree of freedom", ylab = "CV error", main = "Cross-validation progress")

y_hat <- predict(best_model, newdata = list(dis = test_dis), se = TRUE)
x11(); plot(Boston$dis, Boston$nox, xlab = "dis", ylab = "nox", main = "Boston housing", col = "gray")
lines(test_dis, y_hat$fit, col ="blue", lwd = 5)
lines(test_dis, y_hat$fit + 2 * y_hat$se, lty = "dashed", col ="blue", lwd = 2)
lines(test_dis, y_hat$fit - 2 * y_hat$se, lty = "dashed", col ="blue", lwd = 2)
# What is the confidence interval over?

### Question 8

library(ISLR)
View(College)
?College

summary(College$Outstate)
n <- nrow(College)
train <- sample(n, round(0.5 * n))

# First, try everything

library(gam)

model <- gam(Outstate ~ Private + s(Apps) + s(Accept) + s(Enroll) + s(Top10perc) +
             s(Top25perc) + s(F.Undergrad) + s(P.Undergrad) + s(Room.Board) + 
             s(Books) + s(Personal) + s(PhD) + s(Terminal) + s(S.F.Ratio) + 
             s(perc.alumni) + s(Expend) + s(Grad.Rate), data = College)
summary(model)

x11()
par(mfrow = c(3, 6))
plot(model, se = TRUE, col = "blue")

x11()
par(mfrow = c(1, 3))
plot(model, se = TRUE, col = "blue", terms = c("s(Apps)", "s(Expend)", "s(Grad.Rate)"))

# Contrast: see what happens marginally

model2 <- gam(Outstate ~ s(Apps), data = College)
x11()
plot(model2, se = TRUE, col = "blue")

# (a) # Do the subset selection using linear models

library(leaps)

# Notice: below I'm using the whole data, not only the training set (the correct answer to
# this question should use the training set only. I'm doing this because I want to contrast
# model3 (see below) against model (see above, before item (a)))

model_sub <- regsubsets(Outstate ~ ., data = College, method = "forward", nvmax = ncol(College))
model.summary <- summary(model_sub)
which(model.summary$which[which.min(model.summary$bic), ]) # The ones shown here are the ones used below
# Observe from the above which "somewhat redundant" variables are removed e.g. Top25perc
# in light of Top10perc

# (b)

# Not the training data yet

model2 <- gam(Outstate ~ Private + s(Apps) + s(Accept) + s(Top10perc) +
               s(F.Undergrad) + s(Room.Board) + 
               s(Personal) + s(PhD) + s(Terminal) + s(S.F.Ratio) + 
               s(perc.alumni) + s(Expend) + s(Grad.Rate), data = College)
summary(model2)
x11()
par(mfrow = c(3, 5))
plot(model2, se = TRUE, col = "blue")
# Why some of the variables here are still not significant despite forward selection kind of
# overfits significance already?

# Finally, training data for the sake of next exercise

College_train <- College[train, ]

model_sub <- regsubsets(Outstate ~ ., data = College, method = "forward", nvmax = ncol(College_train))
model.summary <- summary(model_sub)
which(model.summary$which[which.min(model.summary$bic), ]) # The ones shown here are the ones used below
# I notice that the variables selected are the same as the ones chosen by the whole data

model2 <- gam(Outstate ~ Private + s(Apps) + s(Accept) + s(Top10perc) +
                s(F.Undergrad) + s(Room.Board) + 
                s(Personal) + s(PhD) + s(Terminal) + s(S.F.Ratio) + 
                s(perc.alumni) + s(Expend) + s(Grad.Rate), data = College_train)

# (c)

College_test <- College[-train, ]
yhat <- predict(model2, newdata = College_test)
true_y <- as.numeric(College_test$Outstate)
cat(sprintf("Average absolute error = %.2f\n", mean(abs(yhat - true_y))))
summary(true_y) # To remind ourselves on how to interpret the scale

# (d)

summary(model2) # See what we get here

### Question 9

# (a)

set.seed(2)

n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
betas <- rnorm(3) * 3
x <- cbind(rep(1, n), x1, x2)
epsilon <- rnorm(n)
y <- x %*% betas + epsilon

# (b)-(e), I'm ignoring the abline request and making a plot of my interest

beta_hats <- rep(0, 3)
beta_hats[2] <- 10
beta_hats[3] <- 10 * (-sign(betas[3])) # To make things a bit harder

num_iter <- 10
num_betas <- length(betas)
beta_iter <- matrix(rep(0, num_iter * 3), nrow = 3)
beta_iter[, 1] <- beta_hats

for (i in 2:num_iter) {
  res <- y - beta_hats[2] * x1
  m <- lm(res ~ x2)
  beta_hats[1] <- m$coef[1]
  beta_hats[3] <- m$coef[2]
  res <- y - beta_hats[3] * x2
  m <- lm(res ~ x1)
  beta_hats[1] <- m$coef[1]
  beta_hats[2] <- m$coef[2]
  beta_iter[, i] <- beta_hats
}

beta_hats_global <- as.numeric(lm(y ~ x1 + x2)$coef)

x11()
plot(1:num_iter, beta_iter[2, ], xlab = "Iteration", ylab = expression(hat(beta)), type = "l", lwd = 3, col = "blue", ylim = c(min(beta_iter[2:3, ]), max(beta_iter[2:3, ])))
lines(1:num_iter, beta_iter[3, ], lwd = 3, col = "red")
lines(1:num_iter, rep(beta_hats_global[2], num_iter), lty = "dashed", col = "blue", lwd = 1)
lines(1:num_iter, rep(beta_hats_global[3], num_iter), lty = "dashed", col = "red", lwd = 1)

### Question 10

n <- 1000
p <- 100

x <- matrix(rnorm(n * p), ncol = p)
betas <- rnorm(p) * 3
epsilon <- rnorm(n)
y <- x %*% betas + epsilon

beta_hat_global <- as.numeric(lm(y ~ 0 + x)$coef) # No intercept, to simplify it

num_iter <- 10

beta_hats <- rep(0, p)
beta_iter <- matrix(rep(0, p * num_iter), nrow = p)
beta_iter[, 1] <- beta_hats

for (i in 1:num_iter) {
  for (j in 1:p) {
    res <- y - x[, -j] %*% beta_hats[-j]
    m <- lm(res ~ 0 + x[, j])
    beta_hats[j] <- m$coef[1]
  }  
  beta_iter[, i] <- beta_hats
}

x11(); plot(beta_hats, beta_hat_global, xlab = "Backfitting estimates", ylab = "Least squares estimates")

j <- 15
x11()
plot(1:num_iter, beta_iter[j, ], main = paste("Coefficient", j), xlab = "Iteration", ylab = expression(hat(beta)), type = "l", lwd = 3, col = "blue", ylim = c(min(c(beta_iter[j, ], beta_hat_global[j])), max(c(beta_iter[j, ], beta_hat_global[j]))))
lines(1:num_iter, rep(beta_hat_global[j], num_iter), lty = "dashed", col = "blue", lwd = 1)
