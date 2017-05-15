
############################################################################################
## 1. Basic density estimation: histograms

dat <- read.table("nhanes.dat")
View(dat)

mu_hat <- mean(dat$height, na.rm = TRUE)
sigma_hat <- sd(dat$height, na.rm = TRUE)

# What is the probability of observing a height 1.90 cm or higher?

chosen_height <- 190
cat(sprintf("p(height > %d) = %.2f (Gaussian model)\n", chosen_height, pnorm(chosen_height, mu_hat, sigma_hat, lower.tail = FALSE)))
cat(sprintf("p(height > %d) = %.2f (Empirical distribution)\n", chosen_height, mean(dat$height > chosen_heigth, na.rm = TRUE)))

# Histogram of NHANES, height data

x11(); p_hat <- hist(dat$height, freq = FALSE, xlab = "height (cm)", main = "Histogram of heights")

# Different histograms

for (i in c(3, 10, 100)) {
  x11()
  hist(dat$height, breaks = i, xlab = "height (cm)", main = paste("Bins =", i))
}

# Confidence intervals. First we define this function (using the result in Theorem 20.10, Wasserman's book)

hBands <- function(x, alpha = 0.05, m = NA, spacing = 0.1) {
  if (is.na(m)) {
    h <- hist(x, plot = FALSE)
  } else {
    h <- hist(x, breaks = m, plot = FALSE)
  }
  m <- length(h$breaks)
  breaks <- h$breaks; breaks[length(breaks)] <- max(x) + 1
  dat <- seq(min(x), max(x), spacing)
  n <- length(dat)
  p_hat <- rep(0, n)
  for (i in seq_len(n)) { # Notice: very inneficient way of finding the density
    bin_i <- which(dat[i] < breaks)[1]
    p_hat[i] <- h$density[bin_i - 1]
  }
  c_m <- 0.5 * qnorm(alpha / (2 * m), lower.tail = FALSE) * sqrt(m / n)
  l <- sqrt(p_hat) - c_m; l[l < 0] <- 0; l <- l^2
  u <- (sqrt(p_hat) + c_m)^2
  return(list(p_hat = p_hat, l = l, u = u, x = dat))
}
  
library(MASS)
View(Boston)
?Boston

h_boston <- hBands(Boston$medv, m = NA) #  Try a chang of m

x11()
plot(h_boston$x, h_boston$p_hat, type = "l", lwd = 5, col = "blue", ylim = c(0, max(h_boston$u)),
     xlab = "Price per district ($1000)", ylab = "pdf", main = "House price distribution")
lines(h_boston$x, h_boston$l, lwd = 5, col = "gray")
lines(h_boston$x, h_boston$u, lwd = 5, col = "gray")

############################################################################################
## 2. Kernel density estimation

kBands <- function(x, x0, xf, spacing = 0.1, h = NA, alpha = 0.05) {
  if (is.na(h)) {
    h <- density(x, kernel = "gaussian")$bw
  }
  dat <- seq(x0, xf, spacing)
  n <- length(dat)
  p_hat <- rep(0, n)
  se <- rep(0, n)
  for (i in seq_len(n)) {
    y <- (dat[i] - x) / h
    Y <- exp(-0.5 * y^2) / sqrt(2 * pi) / h
    p_hat[i] <- mean(Y)
    se[i] <- sqrt(var(Y) / (n - 1))
  }
  m <- (xf - x0) / (3 * h)
  q <- qnorm(0.5 * (1 + (1 - alpha)^(1 / m)))
  l <- p_hat - q * se
  u <- p_hat + q * se
  return(list(p_hat = p_hat, x = dat, h = h, l = l, u = u))
}

# Plot small synthetic dataset

set.seed(1)
x_fake <- rnorm(4)
k_fake <- kBands(x_fake, -3, 3.5, h = NA) # Substitute this with different bandwidth selections h
x11()
max_bar_height <- (exp(0) / sqrt(2 * pi)) / (length(x_fake) * k_fake$h) * 0.8
plot(x_fake, rep(max_bar_height, length(x_fake)), type = "h", lwd = 5, col = "gray", xlim = c(-3, 3.5), ylim = c(0, max(k_fake$p_hat)), xlab = "x", ylab = "pdf", main = "Kernel density estimation")
lines(k_fake$x, k_fake$p_hat, lwd = 5, col = "blue")
for (i in x_fake) {
  x_win <- seq(i - 3 * k_fake$h, i + 3 * k_fake$h, 0.1)
  x_std <- (x_win - i) / k_fake$h
  pdf_x <- (exp(-0.5 * x_std^2) / sqrt(2 * pi)) / (length(x_fake) * k_fake$h)
  lines(x_win, pdf_x, col = "gray")
}

# Boston data, with confidence bars

k_boston <- kBands(Boston$medv, 0, max(Boston$medv) + 10)

x11()
plot(k_boston$x, k_boston$p_hat, type = "l", lwd = 5, col = "blue", ylim = c(0, max(k_boston$u)),
     xlab = "Price per district ($1000)", ylab = "pdf", main = "House price distribution")
lines(k_boston$x, k_boston$l, lwd = 5, col = "gray")
lines(k_boston$x, k_boston$u, lwd = 5, col = "gray")

############################################################################################
## 3. Multivariate Gaussian Models: graphical models

sachs <- read.table("sachs.dat", header = TRUE) # From http://science.sciencemag.org/content/308/5721/523
View(sachs)

# Plot examples

x11(); qqnorm(sachs[, 1]); qqline(sachs[, 1])
x11(); qqnorm(log(sachs[, 1])); qqline(log(sachs[, 1]))

# Transformation as suggested in the "nonparanormal" method, http://jmlr.csail.mit.edu/papers/volume10/liu09a/liu09a.pdf,
# Section 4.

gaussify <- function(x)
{
  p <- ncol(x)
  n <- nrow(x)
  z <- matrix(rep(0, p * n), ncol = p)
  delta_n <- 1 / (4 * n^0.25 * sqrt(pi * log(n)))
  F_hat <- rep(0, n)
  for (i in seq_len(p)) {
    idx <- sort(x[, i], index.return = TRUE)$ix
    F_hat[idx] <- (1:n) / n
    F_tilde <- F_hat
    F_tilde[F_hat < delta_n] <- delta_n
    F_tilde[F_hat > 1 - delta_n] <- 1 - delta_n
    z[, i] <- qnorm(F_tilde)
  }
  return(z)
}

sachs_gauss <- gaussify(sachs)

x11(); qqnorm(sachs_gauss[, 1]); qqline(sachs_gauss[, 1]) # As an example
x11(); plot(sachs[, 1:2], xlab = names(sachs)[1], ylab = names(sachs)[2], main = "Scatterplot before Gaussian transformation")
x11(); plot(sachs_gauss[, 1:2], xlab = names(sachs)[1], ylab = names(sachs)[2], main = "Scatterplot after Gaussian transformation")
x11(); plot(sachs[, 10:11], xlab = names(sachs)[10], ylab = names(sachs)[11], main = "Scatterplot before Gaussian transformation")
x11(); plot(sachs_gauss[, 10:11], xlab = names(sachs)[10], ylab = names(sachs)[11], main = "Scatterplot after Gaussian transformation (still bad...)")

# Structure learning by testing vanishing partial correlations.
# Note: this is HIGHLY INEFFICIENT CODE, for the sake of illustrating the steps more explicitly

alpha_brute <- 0.05
alpha_bonferroni <- alpha_brute / ((ncol(sachs) * ncol(sachs) - 1) / 2)

library(ppcor)
sachs_pcor <- pcor(sachs_gauss)
passed_brute <- sachs_pcor$p.value <= alpha_brute; colnames(passed_brute) <- names(sachs)
passed_bonferroni <- sachs_pcor$p.value <= alpha_bonferroni;  colnames(passed_bonferroni) <- names(sachs)

# Plot these guys

library(igraph)

g_brute <- graph_from_adjacency_matrix(passed_brute, diag = FALSE, mode = "undirected")
g_bonferroni <- graph_from_adjacency_matrix(passed_bonferroni, diag = FALSE, mode = "undirected")
x11(); plot(g_brute, layout = layout_in_circle(g_brute), main = "No corrections")
x11(); plot(g_bonferroni, layout = layout_in_circle(g_bonferroni), main = "With Bonferroni corrections")

# GLASSO (Graphical lasso) example. Notice that cross-validation is not provided.

library(glasso)

sachs_glasso <- glassopath(sachs_gauss)

############################################################################################
## 4. Gaussian mixture model:

K <- 2

mu <- c(-1, 2)
sigma <- 0.5
theta <- c(0.3, 0.7)
y_range <- seq(-5, 5, 0.1)

pdf_y <- rep(0, length(y_range))
for (k in 1:K) {
  pdf_y <- pdf_y + theta[k] * dnorm((y_range - mu[k]) / sqrt(sigma))
}

x11()
plot(y_range, pdf_y, type = "l", lwd = 5, col = "blue", xlab = "y", ylab = "pdf", main = "Maginal Density")

n <- 10000
x <- sample(K, n, replace = TRUE, prob = theta)
y <- rnorm(n) * sqrt(sigma) + mu[x]
x11()
hist(y, main = "Sample")
