### Question 5

library(ISLR)

# (a)

x11(); pairs(Weekly[, 2:5]) # Just the lags: no obvious association

x11(); qqplot(Weekly[, 2], Weekly[, 3]) # Pairwise qqplots don't show clearly whether things change across weeks

x11(); qqnorm(Weekly[, 2]); qqline(Weekly[, 2]); x11(); hist(Weekly[, 2]) # Evidence of unimodality but "heavy-tails"

years <- unique(Weekly$Year)
cat("Quantiles:\n")
cat("----------\n")
for (y in years) { # Notice: this is NOT computationally efficient! For illustration purposes only!
  q <- quantile(Weekly$Today[Weekly$Year == y])
  cat(sprintf("Year %d: %+.2f %+.2f %+.2f %+.2f %+.2f\n", y, q[1], q[2], q[3], q[4], q[5]))
}

x11(); boxplot(Today ~ Year, data = Weekly, xlab = "Year", ylab = "Today") # Some variability across years, nothing strong

#(b)

contrasts(Weekly$Direction) # Check encoding
model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = "binomial")
summary(model)

#(c)

model.probs <- predict(model, type = "response")
model.pred <- rep("Down", nrow(Weekly))
model.pred[model.probs >.5] <- "Up"

table(model.pred, Weekly$Direction)
cat("Training accuracy: %.2f\n", mean(model.pred == Weekly$Direction))
cat(sprintf("Baseline: %.3f\n", max(mean(Weekly$Direction == "Up"), mean(Weekly$Direction == "Down"))))

#(d)

Weekly_train <- Weekly[Weekly$Year < 2009, ]
model <- glm(Direction ~ Lag2, data = Weekly_train, family = "binomial")
summary(model)

Weekly_test <- Weekly[Weekly$Year >= 2009, ]
model.probs <- predict(model, Weekly_test, type = "response")
model.pred <- rep("Down", nrow(Weekly_test))
model.pred[model.probs >.5] <- "Up"

table(model.pred, Weekly_test$Direction)
cat("Test accuracy: %.2f\n", mean(model.pred == Weekly_test$Direction))
cat(sprintf("Baseline: %.3f\n", max(mean(Weekly_test$Direction == "Up"), mean(Weekly_test$Direction == "Down"))))

#(i)

model <- glm(Direction ~ Lag2 + Lag5 + Lag2 * Lag5, data = Weekly_train, family = "binomial")
summary(model)
model.probs <- predict(model, Weekly_test, type = "response")
model.pred <- rep("Down", nrow(Weekly_test))
model.pred[model.probs >.5] <- "Up"
table(model.pred, Weekly_test$Direction)
cat("Test accuracy: %.2f\n", mean(model.pred == Weekly_test$Direction)) # Conclusions?

### Question 6

#(a)

mpg01 <- rep(0, nrow(Auto))
mpg01[Auto$mpg >= median(Auto$mpg)] <- 1
Auto2 <- cbind(Auto, mpg01)

#(b)

x11(); pairs(Auto2)
x11(); boxplot(cylinders ~ mpg01, xlab = "mpg01", ylab = "cylinders", data = Auto2)
x11(); boxplot(year ~ mpg01, xlab = "mpg01", ylab = "year", data = Auto2)
x11(); boxplot(horsepower ~ mpg01, xlab = "mpg01", ylab = "horsepower", data = Auto2)

#(c)

train_idx <- sample(nrow(Auto2), round(nrow(Auto2) / 2))
Auto_train <- Auto2[train_idx, ]
Auto_test <- Auto2[-train_idx, ]

#(f)

model <- glm(mpg01 ~ cylinders + year + horsepower, data = Auto_train, family = "binomial") # Just a suggestion, we can use something else
summary(model) # Interpret coefficients: e.g., what does it say about year?

model.probs <- predict(model, Auto_test, type = "response")
model.pred <- rep(0, nrow(Auto_test))
model.pred[model.probs >.5] <- 1
table(model.pred, Auto_test$mpg01)
cat("Test accuracy: %.2f\n", mean(model.pred == Auto_test$mpg01)) # Conclusions?

### Question 7

#(a)

police <- read.table("frisk_with_noise.dat", skip = 6, header = TRUE)
# Let's look at violent crimes only
police <- police[police$crime == 1, ]

model <- glm(stops ~ 1, family = "poisson", offset = log(pop), data = police)
summary(model) 
# Interpret the model. "Offset" is just a covariate which is given a fixed coefficient of 1.
# How does it change without the offset? 

#(b)

model <- glm(stops ~ factor(eth), family = poisson, offset = log(pop), data = police)
summary(model)
# What is the baseline ethinicity? Look at the header in the frisk_with_noise.dat file. 
# What can you say then of the coefficients? What about the difference in deviance?

#(c)

model <- glm(stops ~ factor(eth) + factor(precinct), family = poisson, offset = log(pop), data = police)
# Notice! It would be non-sense to treat precint as a continuous variable. Why?
summary(model)

#(d)

library(arm) # Gelman and Hill library

y_hat <- predict(model, type = "response")

residual <- y_hat - police$stops
sigma_hat <- sd(residual)
x11(); residual.plot(y_hat, residual, sigma_hat, main = expression(paste("raw residuals, ", y - hat(y))))

sd_residual <- (y_hat - police$stops) / sqrt(y_hat)
x11(); residual.plot(y_hat, sd_residual, 2, main = expression(paste("standardized residuals, ", (y - hat(y)) / sqrt(hat(y)))))

# The following provide a measure of overdispersion. Under the Poisson model, each squared standardize residual is roughly
# a chi-squared with one degree of freedom. Adding n add would give a chi-squared with n degrees of freedom, but they are
# not really independent (since regression coefficients were estimated from the same data). A better approximation is
# a chi-squared with n - k degrees of freedom, where k is the number of regression coefficients (77 in the model with
# precincts). So under the null of a Poisson GLM, the statistic below should be a chi-squared 
# nrow(police) - length(model$coefficients) = 148 degrees of freedom

df <- nrow(police) - length(model$coefficients)
chisq_police <- sum(sd_residual^2)
cat(sprintf("Observed/expected chi-squared under Poisson: [%.2f, %2.f]\n", chisq_police, df))
cat(sprintf("Estimated overdispersion: %.2f\n", chisq_police / df))
cat(sprintf("Upper tail probability: %.2f\n", pchisq(chisq_police, df, lower.tail = FALSE))) # Probability of a value that is as high or higher than the observed one

#(e)

library(MASS) # R's glm doesn't support negative binomials, which are not "true" GLMs.

# Annoyingly, this implementation doesn't allow for offsets, so comparisons are not direct.

model <- glm.nb(stops ~ factor(eth) + factor(precinct) + log(pop), data = police)
summary(model)

model_p <- glm(stops ~ factor(eth) + factor(precinct) + log(pop), family = poisson, data = police) # Compare to Poisson
summary(model_p)

y_hat <- predict(model, type = "response")
residual <- y_hat - police$stops
sigma_hat <- sd(residual)
x11(); residual.plot(y_hat, residual, sigma_hat, main = expression(paste("raw residuals, ", y - hat(y))))

sd_nb <- sqrt(y_hat + y_hat^2 / model$theta)
sd_residual <- (y_hat - police$stops) / sd_nb
x11(); residual.plot(y_hat, sd_residual, 2, main = expression(paste("standardized residuals, ", (y - hat(y)) / sqrt(hat(y)))))

x11(); plot(y_hat, sd_nb^2, xlab = "Fitted mean", ylab = "Fitted variance", main = "Negative Binomial", col = "blue", pch = 19)

# Seems like variance is really high? for a mean of max(y_hat), we have a variance of (max(y_hat) + max(y_hat)^2 / model$theta)

max_mean <- max(y_hat)
max_var <- max_mean + max_mean / model$theta
cat(sprintf("Max mean and variance in the sample: %.2f and %.2f\n", max_mean, max_var))

# To understand that this is not ridiculous, let's convert this to the parameters used in R's negative binomial functions:
# See also https://stat.ethz.ch/R-manual/R-devel/library/stats/html/NegBinomial.html

nb_prob <- max_mean / max_var
nb_size <- nb_prob * max_mean / (1 - nb_prob)

x_range <- 1:1000
prob_pred <- dnbinom(x_range, size = nb_size, prob = nb_prob)
x11(); plot(x_range, prob_pred, xlab = "x", ylab = "p(x)", main = sprintf("Negative Binomial (mean = %.2f, variance = %.2f)", max_mean, max_var), col = "blue")
lines(x_range, dnorm(x_range, mean = max_mean, sd = sqrt(max_var)), type = "l", col = "red") # "Closest Gaussian"

# Similar, now with another mean

other_mean <- 10
other_var <- other_mean + other_mean / model$theta
nb_prob <- other_mean / other_var
nb_size <- nb_prob * other_mean / (1 - nb_prob)

x_range <- 1:30
prob_pred <- dnbinom(x_range, size = nb_size, prob = nb_prob)
x11(); plot(x_range, prob_pred, xlab = "x", ylab = "p(x)", main = sprintf("Negative Binomial (mean = %.2f, variance = %.2f)", other_mean, other_var), col = "blue")
lines(x_range, dnorm(x_range, mean = other_mean, sd = sqrt(other_var)), type = "l", col = "red") # "Closest Gaussian"

# What do you lose by fitting a standard linear regression model

#(f): Up to you

### Question 8

library(foreign) # To read a DTA file (Stata)

risky <- read.dta("risky_behaviors.dta")
risky$women_alone <- factor(risky$women_alone)
risky$couples <- factor(risky$couples)
risky$fupacts <- round(risky$fupacts)
View(risky)

#(a)

model <- glm(fupacts ~ couples + women_alone, family = poisson, data = risky)
summary(model)

# To get some evidence of overdispersion, we once again repeat the same ideas of the previous solution

residual <- model$fitted.values - risky$fupacts
sd_residual <- residual / sqrt(model$fitted.values)
df <- nrow(risky) - length(model$coefficients)
chisq_risky <- sum(sd_residual^2)
cat(sprintf("Observed/expected chi-squared under Poisson: [%.2f, %2.f]\n", chisq_risky, df))
cat(sprintf("Estimated overdispersion: %.2f\n", chisq_risky / df))
cat(sprintf("Upper tail probability: %.2f\n", pchisq(chisq_risky, df, lower.tail = FALSE)))

#(b)

model <- glm(fupacts ~ couples + women_alone + sex + bs_hiv + bupacts, family = poisson, data = risky)
summary(model)

residual <- model$fitted.values - risky$fupacts
sd_residual <- residual / sqrt(model$fitted.values)
df <- nrow(risky) - length(model$coefficients)
chisq_risky <- sum(sd_residual^2)
cat(sprintf("Observed/expected chi-squared under Poisson: [%.2f, %2.f]\n", chisq_risky, df))

#(c) This is called a "quasilikelihood" approach. Not a "real" model!

model <- glm(fupacts ~ couples + women_alone + sex + bs_hiv + bupacts, family = quasipoisson, data = risky)
summary(model) # Provide an interpretation of the parameters and the so-called dispersion parameter!

#(d) Good question. What do you think?

### Question 9

library(foreign) # To read a DTA file (Stata)
library(MASS) # For ordered logistic regression

nes <- read.dta("nes5200_processed_voters_realideo.dta")
nes2000 <- nes[nes$year == 2000, ]
View(nes2000)

#(a)

model <- polr(partyid3 ~ factor(ideo) + age + factor(income), data = nes2000) # Notice the weird lack of points in category 0. 
#This model behaves strangely, also of difficult interpretration as poutting together "apolitical" in the end of the
#spectrum might not make sense. Instead, focus on levels 1 (democrat), 2 (independent) and 3 (republican) only

sel_rows <- c(which(nes2000$partyid3 == "1. democrats (including leaners)"), which(nes2000$partyid3 == "2. independents"), which(nes2000$partyid3 == "3. republicans (including leaners)"))

summary(model) # Make sense of the mess of parameters here, including NaNs! Hint: use "summary(nes2000$covariate_of_interest)" to get empirical counts
nes2000_ordered <- c(nes2000[sel_rows, ])
model <- polr(factor(partyid3) ~ factor(ideo) + age + factor(income), data = nes2000_ordered, method = "logistic") # Notice: this data
summary(model) # In this model, only the intercepts change

#(b)

voted_main <- rep(0, nrow(nes2000))
voted_main[c(which(nes2000$presvote == "1. democrat"), which(nes2000$presvote == "2. republicans"))] <- 1
nes2000_demrep <- cbind(voted_main, nes2000)
na_rows <- which(is.na(nes2000_demrep$ideo) + is.na(nes2000_demrep$age) + is.na(nes2000_demrep$income) > 0)
nes2000_demrep <- nes2000_demrep[-na_rows, ]
model <- glm(voted_main ~ ideo + age + income, family = binomial, data = nes2000_demrep) # Notice the weird lack of points in category 0
summary(model) # Comment on this

#(c)

# First, a useless plot for contrast

pred_d <- model$fitted.values
x11()
plot(c(0, 1), c(-1, 1), xlab = "Estimated  Pr (default)", 
     ylab = "Observed - estimated", type = "n", main = "Residual plot", mgp = c(2, .5, 0))
abline(0, 0, col = "gray", lwd = .5)
points(pred_d, voted_main[-na_rows] - pred_d)#, pch = 20, cex =. 2)

# Binned plots

x11(); binnedplot(model$fitted.values, nes2000_demrep$voted_main - model$fitted.values) # Provide your comments

#(d) Provide your comments


