### Question 2

lambda_0 <- 1
n <- 20
alpha <- 0.05
q_alpha_bottom <- your_answer # Fill this with the correct alpha / 2 quantile!
q_alpha_up     <- your_answer # Fill this with the correct 1 - alpha / 2 quantile!  

num_trials <- 10000 # Number of repeats
num_reject <- 0

for (i in seq_len(num_trials)) {
  x <- rpois(n, lambda_0)
  w <- (mean(x) - lambda_0) / sqrt(var(x) / n) # Try what happens when mean(x) is used instad of var(x)
  if (w < q_alpha_bottom || w > q_alpha_up) {
    num_reject <- num_reject + 1
  }
}

type_I_freq <- num_reject / num_trials
cat(sprintf("Advertised Type I error: %.2f\n", alpha))
cat(sprintf("Type I error frequency (%d trials): %.2f\n", num_trials, type_I_freq))

## Question 5

num_trials <- 1000
min_x <- rep(0, num_trials)

for (i in seq_len(num_trials)) {
  x <- runif(4)
  min_x[i] <- min(x)
}
x_range <- seq(0, 1, 0.05)
true_f <- 4 * (1 - x_range)^3
x11(); hist(min_x, main = "Distribution of the minimum of 4 U(0, 1) variables")
x11(); plot(x_range, true_f, lwd = 5, xlim = c(0, 1), col = "blue", type = "l", xlab = "x", ylab = "p(x)")

## Question 6

# As an example, this takes where the function is evaluated, the variance and sample size

beta_two <- function(alpha, mu, sigma, n) {
  q_bottom <- qnorm(alpha / 2, 0, sigma / sqrt(n))
  q_top    <- qnorm(1 - alpha / 2, 0, sigma / sqrt(n))
  result <- pnorm(q_bottom, mean = mu, sd = sigma / sqrt(n)) + 
            pnorm(q_top, mean = mu, sd = sigma / sqrt(n), lower.tail = FALSE)
  return(result)
}

beta_one <- function(alpha, mu, sigma, n){
  q_top    <- qnorm(1 - alpha, 0, sigma / sqrt(n))
  result   <- pnorm(q_top, mean = mu, sd = sigma / sqrt(n), lower.tail = FALSE)
  return(result)
}

x_range1 <- seq(0, 3, 0.05)
x_range2 <- seq(-3, 3, 0.05)
sigma <- 1
n <- 10
alpha <- 0.05
beta_x1 <- beta_one(alpha, x_range1, sigma, n)
beta_x2 <- beta_two(alpha, x_range2, sigma, n)
x11(); plot(x_range2, beta_x2, ylim = c(0, 1), type = "l", lwd = 5, col = "blue", xlab = "mu", ylab = "power")
lines(x_range1, beta_x1, type = "l", lwd = 5, col = "red")

## Question 10

true_mu <- 1
true_var <- 2
n <- 1000000

k <- 30
I <- rep(0, k + 1)
I[1] <- -Inf
I[k + 1] <- Inf
for (j in 2:k) {
  I[j] <- qnorm((j - 1) / k, true_mu, true_var) 
}

E <- rep(0, k)
for (j in 1:k) {
  # This should be the uniform distribution anyway
  E[j] <- n * (pnorm(I[j + 1], true_mu, true_var) - pnorm(I[j], true_mu, true_var))
}

N <- rep(0, k)
num_trials <- 100
reject <- rep(0, num_trials)
alpha <- 0.05

for (i in seq_len(num_trials)) {
  cat(i, "\n")
  x <- rnorm(n, true_mu, true_var)
  for (j in 1:k) {
    N[j] <- sum((x <= I[j + 1]) * (x >= I[j]))
  }
  chisq <- sum((N - E)^2 / E)
  reject[i] <- pchisq(chisq, k - 1 - 2, lower.tail = FALSE) < alpha
}

## Question 11

min_x <- -5
max_x <- 5
num_bins <- 10 # Try higher values here
x_range <- c(-Inf, seq(-5, 5, (max_x - min_x) / num_bins), Inf)

p <- c(pnorm(x_range[2:length(x_range)]) - 
     pnorm(x_range[1:length(x_range) - 1]), 0)

x_eval <- seq(min_x - 1, max_x + 1, 0.01)
f_eval <- rep(0, length(x_eval))
for (i in seq_along(f_eval)) f_eval[i] <- p[which.max(x_eval[i] < x_range) - 1]

x11(); plot(x_eval, f_eval, type = "l", xlab = "x", ylab = "p(x)", lwd = 5, col = "red")
f_true <- dnorm(x_eval)
lines(x_eval,  f_true * max(f_eval) / max(f_true), lwd = 5, col = "blue")

## Question 12

dat <- read.table("lead.dat", header = TRUE)
t.test(x = dat[, 1], y = dat[, 2], paired = TRUE) # t-test
wilcox.test(x = dat[, 1], y = dat[, 2], paired = TRUE) # Wilcoxon test
# small "wiggle" to artificially remove ties
dat[, 2] <- dat[, 2] + rnorm(nrow(dat)) / 10
wilcox.test(x = dat[, 1], y = dat[, 2], paired = TRUE) # Wilcoxon test

## Question 13: code that helps with the calculations

n <- 20; theta_hat <- 0.75; pnorm((theta_hat - 0.5) * sqrt(n) / 0.5, lower.tail = FALSE)
n <- 200; theta_hat <- 0.575; pnorm((theta_hat - 0.5) * sqrt(n) / 0.5, lower.tail = FALSE)
n <- 2000; theta_hat <- 0.523; pnorm((theta_hat - 0.5) * sqrt(n) / 0.5, lower.tail = FALSE)

r_20 <- qnorm(0.05, mean = 0.5, sd = 0.5 / sqrt(20), lower.tail = FALSE)
r_200 <- qnorm(0.05, mean = 0.5, sd = 0.5 / sqrt(200), lower.tail = FALSE)
r_2000 <- qnorm(0.05, mean = 0.5, sd = 0.5 / sqrt(2000), lower.tail = FALSE)

x_range <- seq(0.5, 1, 0.05)
power_20 <- pnorm(r_20, mean = x_range, sd = 0.5 / sqrt(20), lower.tail = FALSE)
power_200 <- pnorm(r_200, mean = x_range, sd = 0.5 / sqrt(200), lower.tail = FALSE)
power_2000 <- pnorm(r_2000, mean = x_range, sd = 0.5 / sqrt(2000), lower.tail = FALSE)

x11(); plot(x_range, power_20, lwd = 5, col = "blue", xlab = "theta", ylab = "power", type = "l")
lines(x_range, power_200, lwd = 5, col = "red")
lines(x_range, power_2000, lwd = 5, col = "magenta")
