
############################################################################################
## 0. A plot of the binomial distribution

n <- 40
theta <- 0.2

x   <- 0:n
pmf <- dbinom(x, n, theta)

x11()
plot(x, pmf, col = "blue", lwd = 5, main = paste("Probability P(", n, ", ", theta, ")", sep = ""))

############################################################################################
## 1. A plot of the binomial distribution

n <- 40
theta <- 0.5
x   <- 0:n
pmf <- dbinom(x, n, theta)

x_obs <- 15
x_test <- 0:x_obs
pmf_test <- dbinom(x_test, n, theta)
sum_x <- sum(pmf_test)

x11()
plot(x, pmf, col = "blue", lwd = 5, main = paste("Probability P(", n, ", ", theta, ")", sep = ""))
points(x_test, pmf_test, col = "red", lwd = 7)

############################################################################################
## 2. Showing the distribution of the p-value under the null 
## (n has to be large for this to be approximately continuous and better to visualize)

N <- 100000
n <- 40000
theta <- 0.5

x <- rbinom(N, n, theta)
pvalues <- rep(0, N)
for (i in 1:N) {
  pvalues[i] <- pbinom(x[i], size = n, prob = theta)
}

x11(); hist(pvalues, main = "Histogram of pvalues")
x11(); qqplot(runif(N), pvalues, xlab = "Uniform(0, 1)", main = "QQPlot against uniform distribution")

############################################################################################
## 3. Showing an example of power curve for the binomial example.

n <- 40
null_theta <- 0.5
threshold <- qbinom(0.05, n, null_theta) # This is done approximately

theta_range <- seq(0.1, 0.5, 0.01)
power_curve <- rep(0, length(theta_range))
for (i in seq_along(theta_range)) {
  power_curve[i] <- pbinom(threshold, size = n, prob = theta_range[i])
}
x11()
plot(theta_range, power_curve, col = "blue", xlab = "Alternative", ylab = "Power", ylim = c(0, 1), lwd = 5, type = "l", main = "Power curve")

x11()
plot(theta_range, 1 - power_curve, col = "blue", xlab = "Alternative", ylab = "Probability of Type II error", ylim = c(0, 1), lwd = 5, type = "l", main = "Type II Error")

theta_range <- seq(0.45, 0.5, 0.01)
power_curve <- rep(0, length(theta_range))
for (i in seq_along(theta_range)) {
  power_curve[i] <- pbinom(threshold, size = n, prob = theta_range[i])
}
x11()
plot(theta_range, power_curve, col = "blue", xlab = "Alternative", ylab = "Power", ylim = c(0, 1), lwd = 5, type = "l", main = "Power curve")

# Two-tailed test

threshold  <- qbinom(0.05, n, null_theta) # This is done approximately
threshold1 <- qbinom(0.025, n, null_theta) # This is done approximately
threshold2 <- qbinom(0.975, n, null_theta) # This is done approximately

theta_range <- seq(0.1, 1, 0.01)
power_curve <- rep(0, length(theta_range))
power_curve2 <- rep(0, length(theta_range))
for (i in seq_along(theta_range)) {
  power_curve[i] <- pbinom(threshold, size = n, prob = theta_range[i])
  power_curve2[i] <- pbinom(threshold1, size = n, prob = theta_range[i]) + 
                     pbinom(threshold2, size = n, prob = theta_range[i], lower.tail = FALSE)
}
x11()
plot(theta_range, power_curve2, col = "blue", xlab = "Alternative", ylab = "Power", ylim = c(0, 1), lwd = 5, type = "l", main = "Power")
lines(theta_range, power_curve, type = "l", col = "green", lwd = 5)

############################################################################################
## 4. Power curves at different sample sizes

ns <- c(40, 1000, 10000)
null_theta <- 0.5
theta_range <- seq(0.1, 0.5, 0.01)
power_curve <- rep(0, length(theta_range))
plot_colors <- list("red", "green", "blue")

x11()
for (i in seq_along(ns)) {
  threshold <- qbinom(0.05, ns[i], null_theta)
  for (j in seq_along(theta_range)) {
    power_curve[j] <- pbinom(threshold, size = ns[i], prob = theta_range[j])
  }
  if (i == 1) {
    plot(theta_range, power_curve, col = plot_colors[[i]], xlab = "Alternative", ylab = "Power", lwd = 5, type = "l", main = "Power curve (sample size increasing)") 
  } else {
    points(theta_range, power_curve, col = plot_colors[[i]], lwd = 5, type = "l")
  }
}

n <- 40
levels <- c(0.01, 0.05, 0.2)
x11()
for (i in seq_along(ns)) {
  threshold <- qbinom(levels[i], n, null_theta)
  for (j in seq_along(theta_range)) {
    power_curve[j] <- pbinom(threshold, size = n, prob = theta_range[j])
  }
  if (i == 1) {
    plot(theta_range, power_curve, col = plot_colors[[i]], xlab = "Alternative", ylab = "Power", lwd = 5, type = "l", main = "Power curve (level changing)") 
  } else {
    points(theta_range, power_curve, col = plot_colors[[i]], lwd = 5, type = "l")
  }
}

############################################################################################
## 5. Visualize the t density function and tail probabilities

source("plot_tails.R") # Also available in the course page
level <- 0.05
n <- 5
plot.dist(level, alternative = "two.tailed", distribution = "t", df = n - 1, main_title = paste("T(", n - 1, ")", sep = ""))
cat(sprintf("Critical region: t <= %.2f, t >= %.2f\n", qt(level / 2, n - 1), qt(level / 2, n - 1, lower.tail = FALSE)))

############################################################################################
## 6. Goodness-of-fit for marginal independence

load("twin_data.RDa") # Data adapted from https://arxiv.org/pdf/0707.3794.pdf

twin_table <- table(twin_data$`Twin 1 Alcohol dependence`, twin_data$`Twin 2 Depression`)
chisq.test(twin_table)

twin_table <- table(twin_data$`Twin 2 Alcohol dependence`, twin_data$`Twin 1 Depression`)
chisq.test(twin_table)

############################################################################################
## 7. Confidence intervals: basics

n <- 50
num_rep <- 20
m <- rep(0, num_rep)
true_mean <- 168
true_var <- 103

for (i in 1:num_rep) {
  dat <- rnorm(n, mean = true_mean, sd = sqrt(true_var))
  m[i] <- mean(dat)
}

library(ggplot2) # Available from CRAN
library(gridExtra)

df_sim <- data.frame(x = 1:num_rep, y = m)

x11()
g <- ggplot(df_sim, aes(x = reorder(rownames(df_sim), 1:num_rep), y = y)) +
       geom_point(size = 2) + geom_hline(yintercept = true_mean) +
       coord_flip() + xlab("Dataset") + ylab(expression(hat(mu))) + ylim(164, 171) +
       theme(axis.title.x = element_text(face = "bold", size = 20), axis.title.y = element_text(face = "bold", size = 20))
grid.arrange(g, ncol = 1)

############################################################################################
## 8. Confidence intervals: coverage

n <- 50
num_rep <- 100
m <- rep(0, num_rep)
L <- rep(0, num_rep)
true_mean <- 168
true_var <- 103
v <- qnorm(0.93)

# Lower bound only

for (i in 1:num_rep) {
  dat <- rnorm(n, mean = true_mean, sd = sqrt(true_var))
  m[i] <- mean(dat)
}
L <- m - v * sqrt(true_var/ n)
U <- rep(Inf, num_rep)
df_sim <- data.frame(x = 1:num_rep, y = m, U = U, L = L)
x11()
g <- ggplot(df_sim, aes(x = reorder(rownames(df_sim), 1:num_rep), y = y)) +
  geom_point(size = 2) + geom_hline(yintercept = true_mean) +
  geom_errorbar(aes(ymax = U, ymin = L)) + ylim(162, 171) +
  coord_flip() + xlab("Dataset") + ylab(expression(hat(mu))) +
  theme(axis.title.x = element_text(face = "bold", size = 20), axis.title.y = element_text(face = "bold", size = 20))
grid.arrange(g, ncol = 1)


# Two-sided interval

v <- qnorm(0.975)
U <- m + v * sqrt(true_var / n)
L <- m - v * sqrt(true_var / n) # qnorm(0.025) is the same as -qnorm(0.975)
df_sim <- data.frame(x = 1:num_rep, y = m, U = U, L = L)
x11()
g <- ggplot(df_sim, aes(x = reorder(rownames(df_sim), 1:num_rep), y = y)) +
  geom_point(size = 2) + geom_hline(yintercept = true_mean) +
  geom_errorbar(aes(ymin = L, ymax = U)) +
  coord_flip() + xlab("Dataset") + ylab(expression(hat(mu))) +
  theme(axis.title.x = element_text(face = "bold", size = 20), axis.title.y = element_text(face = "bold", size = 20))
grid.arrange(g, ncol = 1)

############################################################################################
## 9. CLT refresher

# Let's create a Gamma random variable similar to this data

x <- matrix(UKgas) # Let's pretend this didn't change over time
n <- length(x)
h <- hist(x, plot = FALSE)

scale_x <- var(x) / mean(x)
shape_x <- mean(x) / scale_x
y <- dgamma(0:max(x), shape = shape_x, scale = scale_x)

x11()
plot(h, xlab = "millions of therms", main = "UK Gas consumption (1960-1986)")
lines(x = 0:max(x), y = y * length(x) * diff(h$breaks)[1], lwd = 5, col = "blue")

# Create replications of the same distribution, generate averages

num_rep <- 5000
x_bar <- rep(0, num_rep)
for (i in 1:num_rep) {
  x_bar[i] <- mean(rgamma(n, shape = shape_x, scale = scale_x))
}

x11()
par(mfrow = c(1, 2))
hist(x_bar, xlab = expression(bar(X)), main = "Histogram")
qqnorm(x_bar, xlab = expression(bar(X)))
qqline(x_bar)

# Notice: a sample of 108 here doesn't seem to be enough to make the average "truly" Normal
# try to simulate datasets with more than 108 points
cat(sprintf("Normality test: %.2f\n", shapiro.test(x_bar)$p.value))

############################################################################################
## 10. Bootstrapping demos

n <- 10
dat <- rnorm(n)

x11()
plot(dat, rep(1, length(dat)), ylim = c(0.5, 1.2), yaxt = 'n', xlab = "x", ylab = "", lwd = 5, col = "blue", type = "h", main = "Samples from a N(0, 1)")

x11()
plot(ecdf(dat), lwd = 5, col = "blue", main = sprintf("Empirical CDF (n = %d)", n))

# Converging by adding 10 points at a time

x11()
new_n <- 20
for(i in 1:10) {
  dat <- c(dat, rnorm(new_n))
  plot(ecdf(dat), lwd = 5, col = "blue", main = sprintf("Empirical CDF (n = %d)", length(dat)))
  readline()
}

############################################################################################
## 11. Further bootstrapping

x <- matrix(UKgas)
n <- length(x)
h <- hist(x, plot = FALSE)
x11()
plot(h, xlab = "millions of therms", main = "UK Gas consumption (1960-1986)")

# Let's visualize some resamples

B <- 10
x11()
for(i in 1:B) {
  x_star <- sample(x, n, replace = TRUE)
  hist(x_star, xlab = "millions of therms", ylim = c(0, 50), xlim = c(0, max(x)), main = sprintf("UK Gas consumption (1960-1986), re-sample %d", i))
  readline()
}

# Variance calculation

B <- 1000
x_bar_star <- rep(0, B)
for (b in 1:B) {
  x_star <- sample(x, n, replace = TRUE)
  x_bar_star[b] <- mean(x_star)
}

x11()
hist(x_bar_star, xlab = expression(bar(X)^"*"),
     main = sprintf("Bootstrap distribution of sample average, s.e. = %.2f", sd(x_bar_star)))

m <- mean(x)
se <- sd(x_bar_star)
U <- m + qnorm(0.975) * se
L <- m + qnorm(0.025) * se
cat(sprintf("0.95 Bootstrap + Normal confidence interval: [%.2f, %.2f]\n", L, U))

m <- mean(x)
se <- sd(x) / sqrt(n)
U <- m + qnorm(0.975) * se
L <- m + qnorm(0.025) * se
cat(sprintf("0.95 CLT variance + Normal confidence interval: [%.2f, %.2f]\n", L, U))

# Do similarly for median

x_median_star <- rep(0, B)
for (b in 1:B) {
  x_star <- sample(x, n, replace = TRUE)
  x_median_star[b] <- median(x_star)
}

x11()
hist(x_median_star, xlab = "Median",
     main = sprintf("Bootstrap distribution of sample median, s.e. = %.2f", sd(x_median_star)))

m <- median(x)
se <- sd(x_median_star)
U <- m + qnorm(0.975) * se
L <- m + qnorm(0.025) * se
cat(sprintf("0.95 Bootstrap + Normal confidence interval: [%.2f, %.2f]\n", L, U))

x11()
qqnorm(x_median_star, xlab = "Median")
qqline(x_median_star)

# Demonstrate pivotal bootstrap interval

alpha_level <- 0.05
theta_n <- median(x)
r_star <- rep(0, B)
for (b in 1:B) {
  x_star <- sample(x, n, replace = TRUE)
  r_star[b] <- median(x_star) - theta_n
}

r_star <- sort(r_star)
r_L <- r_star[round(B * (1 - alpha_level / 2))]
r_U <- r_star[round(B * (alpha_level / 2))]

cat(sprintf("0.95 Bootstrap pivot interval: [%.2f, %.2f]\n", theta_n - r_L, theta_n - r_U))
