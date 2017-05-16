## Question 3

dat <- read.table("newcomb.dat", header = TRUE)

for (i in 1:3) {
  m <- mean(dat$speed[dat$day == i])
  se <- sd(dat$speed[dat$day == i])
  cat(sprintf("0.99 interval for day %d = [%.2f, %.2f]\n", i, m - 3 * se, m + 3 * se))
}

x11(); qqplot(dat$speed[dat$day == 1], dat$speed[dat$day == 2], xlab = "Day 1", ylab = "Day 2")

m <- mean(dat$speed)
se <- sd(dat$speed)
cat(sprintf("0.99 interval, pooled days = [%.2f, %.2f]\n", m - 3 * se, m + 3 * se))

## Question 6

num_trials <- 1000
n <- 10000
mu_hats <- rep(0, n)
alpha <- 0.05
coverage <- 0
sd_true <- 1

for (i in seq_len(num_trials)) {
  x <- rnorm(n) * sd_true - 1
  mu_hats[i] <- max(0, mean(x))
  l_hat <- mu_hats[i] + qnorm(alpha / 2) * sd_true / sqrt(n)
  u_hat <- mu_hats[i] + qnorm(1 - alpha / 2) * sd_true / sqrt(n)
  cat(sprintf("0.95 confidence interval: [%.2f, %.2f]\n", l_hat, u_hat))
  if (l_hat <= -1 && u_hat >= -1) coverage <- coverage + 1
}

coverage <- coverage / num_trials
cat(sprintf("Coverage of interval: %.2f\n", coverage))

## Question 7

num_problems <- 10000
n <- 1000
h0_holds <- rep(0, num_problems)
passed <- rep(0, num_problems)
covered <- rep(0, num_problems)
alpha <- 0.05

for (i in seq_len(num_problems)) {
  mu <- runif(1) * 2 - 1
  if (mu <= 0) h0_holds[i] <- 1
  x <- rnorm(n, mu, 1)
  p <- pnorm(mean(x) * sqrt(n), 0, 1, lower.tail = FALSE)
  if (p < alpha) {
    passed[i] <- 1
  }
  x_hat <- max(0, mean(x)) # The "informed" estimator
  l_hat <- x_hat + qnorm(alpha / 2) / sqrt(n)
  u_hat <- x_hat + qnorm(1 - alpha / 2) / sqrt(n)
  if (l_hat <= mu && u_hat >= mu) {
    covered[i] <- 1
  }
}

cat(sprintf("Coverage without testing: %.2f\n", mean(covered)))
cat(sprintf("Coverage after testing: %.2f\n", mean(covered[passed == 1])))

## Question 8

alpha <- 0.05

dat <- read.table("placebo.dat", header = TRUE)
n <- nrow(dat)
z <- dat$old - dat$placebo
y <- dat$new - dat$old

theta_hat <- mean(y) / mean(z)
cat(sprintf("Theta hat = %.2f\n", theta_hat))

B <- 1000
theta <- rep(0, B)

for (b in seq_len(B)) {
  dat_b <- sample(n, n, replace = TRUE)
  y_b <- y[dat_b]
  z_b <- z[dat_b]
  theta[b] <- mean(y_b) / mean(z_b)
}
theta <- sort(theta)

cat(sprintf("Bootstrap s.e. estimation = %.2f\n", sd(theta)))
cat(sprintf("Normal-based 0.95 CI = [%.2f, %.2f]\n", theta_hat + qnorm(alpha / 2) * sd(theta), theta_hat + qnorm(1 - alpha / 2) * sd(theta)))
cat(sprintf("Pivotal 0.95 CI = [%.2f, %.2f]\n", 2 * theta_hat - theta[round(B * (1 - alpha / 2))], 2 * theta_hat - theta[round(B * alpha / 2)]))
cat(sprintf("Percentile  0.95 CI = [%.2f, %.2f]\n", theta[round(B * (alpha / 2))], theta[round(B * (1 - alpha / 2))]))

## Question 10

n <- 50
x <- runif(n)

theta_hat <- max(x)

B <- 1000

# Parametric

theta_param <- rep(0, B)
for (b in seq_len(B)) {
  fake_x <- runif(n) * theta_hat
  theta_param[b] <- max(fake_x)
}

# Nonparametric

theta_nonparam <- rep(0, B)
for (b in seq_len(B)) {
  dat_b <- sample(n, n, replace = TRUE)
  theta_nonparam[b] <- max(x[dat_b])
}

x11(); hist(theta_nonparam, main = "Bootstrap samples (Nonparametric)")
x11(); hist(theta_param, main = "Bootstrap samples (Parametric)")

