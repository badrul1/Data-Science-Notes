
############################################################################################
## 0. Read data

nhanes <- read.table("nhanes.dat")
cat("[LOADED: nhanes data]\n", "Measurements: ", names(nhanes), "\n")

############################################################################################
## 1. Some basic plots

x11()
hist(nhanes$height, xlab = "Height (cm)", main = "Height Histogram")

x11()
h <- hist(nhanes$height[nhanes$sex == 1], col = rgb(1, 0, 0, 0.5), xlab = "Height (cm)", main = "Height Histogram")
hist(nhanes$height[nhanes$sex == 2], col = rgb(0, 0, 1, 0.5), breaks = length(h$breaks), add = T)
legend(x = 200, y = 2000, legend = c("Male", "Female"), fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

############################################################################################
## 2. What can we say about a *population*? Initial explorations.

# Is the "double peak" here real?

x11()
hist(nhanes$height[nhanes$sex == 2], col = rgb(0, 0, 1, 0.5), breaks = length(h$breaks), xlab = "Height (cm)", main = "Female Height")
x11()
hist(nhanes$height[nhanes$sex == 2], col = rgb(0, 0, 1, 0.5), xlab = "Height (cm)", main = "Female Height")

# Sample without replacement

{
idx <- which(!is.na(nhanes$height))
x11()
h <- hist(nhanes$height, xlab = "Height (cm)", main = "Height Histogram (ALL)")
ph <- par("usr")
readline()
n <- 100
for(i in 1:10) {
  sub_idx <- sample(idx, n)
  hist(nhanes$height[sub_idx], breaks = h$breaks, xlim = c(ph[1], ph[2]),  
       xlab = "Height (cm)", main = paste("Height Histogram, Subsample ", i, ", N = ", n, sep = ""))
  readline()
}
}
# (Try the above again, with a different choice of n)

# Using probability: the Gaussian distribution

mu <- 0; v <- 1;
x <- seq(-4, 4, 0.1)
pdf <- exp(-0.5 * (x - mu)^2 / v) / sqrt(2 * pi * v)
cdf <- pnorm(x, mean = mu, sd = sqrt(v))
x11()
plot(x, pdf, type = "l", col = "blue", lwd = 5, main = paste("Density N(", mu, ", ", v, ")", sep = ""))
x11()
plot(x, cdf, type = "l", col = "blue", lwd = 5, main = paste("Distribution N(", mu, ", ", v, ")", sep = ""))

# Graphs: uniform distribution

x <- seq(0, 1, 0.1)
pdf <- rep(1, length(x))
cdf <- x
x11()
plot(x, pdf, type = "l", col = "blue", lwd = 5, ylim = c(0, 1), main = paste("Density U(", 0, ", ", 1, ")", sep = ""))
x11()
plot(x, cdf, type = "l", col = "blue", lwd = 5, ylim = c(0, 1), main = paste("Distribution U(", 0, ", ", 1, ")", sep = ""))

# Demonstration: averages of 100 uniform random variables

x11()
n_U <- 1000
f <- 1
for (i in 1:10) {
  d_U <- matrix(nrow = i * f, ncol = n_U)
  for (j in 1:n_U) {
    d_U[, j] <- runif(i * f)  
  }
  m_U <- colMeans(d_U)
  hist(m_U, xlab = "Average", xlim = c(0, 1), breaks = 10, main = paste("Average of", i * f, "uniform random variables"))  
  #qqnorm(m_U, ylim = c(0, 1))
  #qqline(m_U)
  readline()
}

# Demonstration: different Gaussians

mu <- c(0, 0, 0, -1, 3, 3, 3); v <- c(1, 3, 50, 1, 0.1, 0.01, 0.001)
x <- seq(-6, 6, 0.1)
x11()
for (i in seq_along(mu)) {
  pdf <- exp(-0.5 * (x - mu[i])^2 / v[i]) / sqrt(2 * pi * v[i])
  plot(x, pdf, type = "l", col = "blue", lwd = 5, main = paste("Density N(", mu[i], ", ", v[i], ")", sep = ""))
  readline()
}

############################################################################################
## 3. Fitting and model assessment

# Different trials

x11()
x <- seq(min(nhanes$height, na.rm = TRUE), max(nhanes$height, na.rm = TRUE), 0.1)
mu_hat <- mean(nhanes$height, na.rm = TRUE)
v_hat <- var(nhanes$height, na.rm = TRUE)
mu <- c(120, 190, 170, mu_hat)
v <- c(10, 500, 3, v_hat)
n <- sum(!is.na(nhanes$height))
hist(nhanes$height, prob = TRUE, col = "grey", xlab = "Height (cm)")
p_plot <- par("usr")
for (i in seq_along(mu)) {
  pdf <- exp(-0.5 * (x - mu[i])^2 / v[i]) / sqrt(2 * pi * v[i])
  #pdf <- pdf / max(pdf) * p_plot[4]
  score <- -0.5 * (n * log(2 * pi * v[i]) + sum((nhanes$height - mu[i])^2 / v[i], na.rm = TRUE))
  hist(nhanes$height, prob = TRUE, ylim = c(0, 0.045), col = "grey", xlab = "Height (cm)", main = sprintf("Score = %.2f", score))
  lines(x, pdf, col = "blue", lwd = 5)
  readline()
}

# Is this a good model?

cdf <- pnorm(x, mean = mu_hat, sd = sqrt(v_hat))
x11()
plot(x, cdf, type = "l", add = T, col = "blue", lwd = 5, ylim = c(0, 1), main = sprintf("Distribution N(%.1f, %.1f)", mu_hat, v_hat))
cdf_hat <- ecdf(nhanes$height)
x11()
plot(cdf_hat, xlab = "Height (cm)", main = "Empirical cdf")
lines(x, cdf, col = "blue", lwd = 5)
x11()
qqnorm(nhanes$height) 
qqline(nhanes$height)

############################################################################################
## 4. Multivariate plots

x11()
plot(nhanes$height[nhanes$sex == 1], nhanes$weight[nhanes$sex == 1], col = rgb(1, 0, 0, 0.5), xlab = "Height (cm)", ylab = "Weigth (kg)", main = "Height vs. Weight scatterplot")
points(nhanes$height[nhanes$sex == 2], nhanes$weight[nhanes$sex == 2], col = rgb(0, 0, 1, 0.5))

