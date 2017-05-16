
library(ISLR)

########################################################################################################################
### Question 2

library(MASS)
View(fgl)
?fgl

# (a)

# Examples: which one do you think is "most" reasonable?

hist(fgl$RI, freq = FALSE, breaks = 5,  xlab = "Refractive index", main = "Density estimate")
hist(fgl$RI, freq = FALSE, breaks = 10, xlab = "Refractive index", main = "Density estimate")
hist(fgl$RI, freq = FALSE, breaks = 20, xlab = "Refractive index", main = "Density estimate")

# Examples: which one do you think is "most" reasonable?

plot(density(fgl$RI, kernel = "gaussian", bw = 3), xlab = "Refractive index", main = "Density estimate")
plot(density(fgl$RI, kernel = "gaussian", bw = 1), xlab = "Refractive index", main = "Density estimate")
plot(density(fgl$RI, kernel = "gaussian", bw = 0.1), xlab = "Refractive index", main = "Density estimate")

# (b)

# Let's see how R does

hist(fgl$RI, freq = FALSE, xlab = "Refractive index", main = "Density estimate")
plot(density(fgl$RI, kernel = "gaussian"), xlab = "Refractive index", main = "Density estimate")

# (c) Let's do it just for the kernel density estimator. You do the one for the histogram.
# I assume function "kBands" from chapter6.R has been loaded in memory.

k_ri <- kBands(fgl$RI, -10, 20)

x11()
plot(k_ri$x, k_ri$p_hat, type = "l", lwd = 5, col = "blue", ylim = c(0, max(k_ri$u)),
     xlab = "Refractive index", main = "Density estimate")
lines(k_ri$x, k_ri$l, lwd = 5, col = "gray")
lines(k_ri$x, k_ri$u, lwd = 5, col = "gray")

k_ri_mine <- kBands(fgl$RI, -10, 20, h = 1)
lines(k_ri_mine$x, k_ri_mine$u, lwd = 3, col = "red")

# (d)

classes <- unique(fgl$type)

all_d <- density(fgl$RI, kernel = "gaussian")
x11()
for (i in seq_along(classes)) {
  d <- density(fgl$RI[fgl$type == classes[i]], kernel = "gaussian")
  
  plot(all_d$x, all_d$y, type = "l", lwd = 2, col = "black", 
       xlim = c(min(all_d$x, d$x), max(all_d$x, d$x)), ylim = c(0, max(all_d$y, d$y)),
       xlab = "Refractive index, class", ylab = "Density", main = classes[i])
  lines(d$x, d$y, col = "blue", lwd = 5)
  par(ask = TRUE)
}
par(ask = FALSE)

# Same, but add a Gaussian density estimate to it

x11()
for (i in seq_along(classes)) {
  d <- density(fgl$RI[fgl$type == classes[i]], kernel = "gaussian")
  d_gauss <- dnorm(all_d$x, mean = mean(fgl$RI[fgl$type == classes[i]]), sd = sd(fgl$RI[fgl$type == classes[i]]))
  plot(all_d$x, all_d$y, type = "l", lwd = 2, col = "black", 
       xlim = c(min(all_d$x, d$x), max(all_d$x, d$x)), ylim = c(0, max(all_d$y, d$y)),
       xlab = "Refractive index, class", ylab = "Density", main = classes[i])
  lines(d$x, d$y, col = "blue", lwd = 5)
  lines(all_d$x, d_gauss, col = "red", lwd = 3)
  par(ask = TRUE)
}
par(ask = FALSE)

# Same, now make it proportional to class probability

x11()
for (i in seq_along(classes)) {
  d <- density(fgl$RI[fgl$type == classes[i]], kernel = "gaussian")
  d$y <- d$y * (mean(fgl$type == classes[i]))
  plot(all_d$x, all_d$y, type = "l", lwd = 2, col = "black", 
       xlim = c(min(all_d$x, d$x), max(all_d$x, d$x)), ylim = c(0, max(all_d$y, d$y)),
       xlab = "Refractive index, class", ylab = "Density", main = classes[i])
  lines(d$x, d$y, col = "blue", lwd = 5)
  par(ask = TRUE)
}
par(ask = FALSE)

# (e) 

# Let's visualize two class

i <- 1
j <- 6

d1 <- density(fgl$RI[fgl$type == classes[i]], kernel = "gaussian")
d1$y <- d1$y * (mean(fgl$type == classes[i]))
g1 <- dnorm(all_d$x, mean = mean(fgl$RI[fgl$type == classes[i]]), sd = sd(fgl$RI[fgl$type == classes[i]])) * (mean(fgl$type == classes[i]))
d2 <- density(fgl$RI[fgl$type == classes[j]], kernel = "gaussian")
d2$y <- d1$y * (mean(fgl$type == classes[j]))
g2 <- dnorm(all_d$x, mean = mean(fgl$RI[fgl$type == classes[j]]), sd = sd(fgl$RI[fgl$type == classes[j]])) * (mean(fgl$type == classes[j]))

x11()
plot(d1$x, d1$y, type = "l", lwd = 5, col = "blue", 
     xlim = c(min(all_d$x, d1$x, d2$x), max(all_d$x, d1$x, d2$x)), ylim = c(0, max(d1$y, d2$y, g1, g2)),
     xlab = "Refractive index, class", ylab = "Density", main = paste(classes[i], "vs.", classes[j]))
lines(d2$x, d2$y, col = "red", lwd = 5)

x11()
plot(d1$x, g1, type = "l", lwd = 5, col = "blue", 
     xlim = c(min(all_d$x, d1$x, d2$x), max(all_d$x, d1$x, d2$x)), ylim = c(0, max(d1$y, d2$y, g1, g2)),
     xlab = "Refractive index, class", ylab = "Density", main = paste(classes[i], "vs.", classes[j]))
lines(d2$x, g2, col = "red", lwd = 5)

# In the second plot, it looks like the class corresponding to the red curve is more probable at
# the lower values of the refractive index, which is not true when using the nonparametric estimator.

########################################################################################################################
### Question 3

# This assumes function "gaussify" (chapter6.R) is loaded in memory.

# (b)

sachs <- read.table("sachs.dat", header = TRUE)
sachs_gauss <- gaussify(sachs)

# What do you think of these two variables?

x11(); plot(sachs[, 1:2], xlab = names(sachs)[1], ylab = names(sachs)[2], main = "Scatterplot before Gaussian transformation")
x11(); plot(sachs_gauss[, 1:2], xlab = names(sachs)[1], ylab = names(sachs)[2], 
            xlim = c(-3, 3), ylim = c(-3, 3), main = "Scatterplot after Gaussian transformation")

# Let's generate bivariate Gaussian data with the same correlation and standard marginals:
Sigma_pair <- matrix(c(1, cor(sachs[, 1:2])[2], cor(sachs[, 1:2])[2], 1), ncol = 2)
g <- mvrnorm(nrow(sachs), mu = c(0, 0), Sigma = Sigma_pair)
x11(); plot(g, xlab = "Synthetic 1", ylab = "Synthetic 2", 
            xlim = c(-3, 3), ylim = c(-3, 3), main = "Synthetic data with same correlation")
# This is not too bad. There are artifacts at the lower boundaries, but this due to the truncated cdf estimate.

# Now let's see these two variables instead

x11(); plot(sachs[, 10:11], xlab = names(sachs)[10], ylab = names(sachs)[11], main = "Scatterplot before Gaussian transformation")
x11(); plot(sachs_gauss[, 10:11], xlab = names(sachs)[10], ylab = names(sachs)[11], 
            xlim = c(-3, 3), ylim = c(-3, 3), main = "Scatterplot after Gaussian transformation")

# Let's generate bivariate Gaussian data with the same correlation and standard marginals:
Sigma_pair <- matrix(c(1, cor(sachs[, 10:11])[2], cor(sachs[, 10:11])[2], 1), ncol = 2)
g <- mvrnorm(nrow(sachs), mu = c(0, 0), Sigma = Sigma_pair)
x11(); plot(g, xlab = "Synthetic 1", ylab = "Synthetic 2", 
            xlim = c(-3, 3), ylim = c(-3, 3), main = "Synthetic data with same correlation")
# We can see this is not a convincing match. It looks like there are two clouds of points in the plot generated by the
# transformed data, and there shouldn't. The bottomline is that this Gaussian copula model is good for some pairs
# of proteins but not good for others. We may still thrust correlation as a measure of dependence (further analysis
# required, and not developed here) but maybe a mixture of Gaussian copulas would fit this data better. In practice,
# people tend to ignore this misfit and trust correlations after the Gaussian transformation as a good summary
# (maybe by experience in their own field), but this is not something to be taken lightly in a study with data we
# are not familiar with.

########################################################################################################################
### Question 6: this is doable by hand, I provide here a R implementation

X <- matrix(c(1, 1, 0, 5, 6, 4, 4, 3, 4, 1, 2, 0), ncol = 2)

# (a)

x11(); plot(X[, 1], X[, 2], xlab = expression(X[1]), ylab = expression(X[2]))

# (b)

clusters <- sample(2, nrow(X), replace = TRUE)
cat(clusters, "\n")

# (c)

num_clusters <- 2
centroids <- matrix(rep(0, num_clusters * ncol(X)), nrow = num_clusters)
for (i in 1:num_clusters) {
  centroids[i, ] <- colMeans(X[clusters == i, ]) # Average of each column, for the points in cluster i
  # Notice: this will spell trouble if one cluster is empty!
}

# (d): this is done with a loop for pedagogical purposes. It could potentially be very slow for large
# datasets. Can you write without needing a loop?

for (i in 1:nrow(X)) {
  d <- rep(0, num_clusters)
  for (j in 1:num_clusters) {
    d[j] <- sum((X[i, ] - centroids[j, ])^2)
  }
  clusters[i] <- which.min(d)
}

cat("New clusters:\n")
cat(clusters)

# (e): just a copy and paste of the previous items

while (TRUE) {
  
  old_clusters <- clusters
  
  # Calculate MSE
  mse <- 0
  for (i in 1:nrow(X)) { # Again: no need for a loop here
    mse <- mse + (X[i, ] - centroids[clusters[i], ])^2
  }
  
  # Reassign clusters
  for (i in 1:nrow(X)) {
    d <- rep(0, num_clusters)
    for (j in 1:num_clusters) {
      d[j] <- sum((X[i, ] - centroids[j, ])^2)
    }
    clusters[i] <- which.min(d)
  }
  
  # Recompute centers
  for (i in 1:num_clusters) {
    centroids[i, ] <- colMeans(X[clusters == i, ])
  }
  
  if (all(clusters == old_clusters)) {
    break
  }
  
  cat("New MSE = %f\n", mse)
}

# (f)

x11(); plot(X[clusters == 1, 1], X[clusters == 1, 2], col = "blue", lwd = 5, xlab = expression(X[1]), ylab = expression(X[2]), 
            xlim = c(min(X[, 1]), max(X[, 1])), ylim = c(min(X[, 2]), max(X[, 2])))
points(X[clusters == 2, 1], X[clusters == 2, 2], col = "red", lwd = 5)

########################################################################################################################
### Question 10

library(ISLR)
View(USArrests)
?USArrests

# (a)

pca_arrests <- prcomp(USArrests, scale = TRUE)
pve_r <- pca_arrests$sdev^2; pve_r <- pve_r / sum(pve_r)
cat(pve_r)
x11(); plot(1:4, pve_r, type = "l", col = "blue", lwd = 5, xlab = "Principal Component", ylab = "PVE", main = "Proportion of Variance Explained")

# (b)

# We standardize the data before: so we need to do it here
x <- data.matrix(USArrests)
for (i in 1:ncol(USArrests)) { # No loop is actually necessary!
  x[, i] <- (USArrests[, i] - mean(USArrests[, i])) / sd(USArrests[, i])
}
cov(x) # Checking. Also, notice this is the same as pca_arrests$x

sum_total <- sum(x^2)
pve <- rep(0, ncol(USArrests))
for (i in 1:ncol(USArrests)) {
  pve[i] <- sum((x %*% pca_arrests$rotation[, i])^2) / sum_total
}
x11(); plot(1:4, pve_r, type = "l", col = "blue", lwd = 5, xlab = "Principal Component", ylab = "PVE", main = "Proportion of Variance Explained")

# Bonus:

x11(); biplot(pca_arrests, scale = 0)
pca_arrests$rotation <- -pca_arrests$rotation
pca_arrests$x <- -pca_arrests$x
x11(); biplot(pca_arrests, scale = 0)

########################################################################################################################
### Question 11

# (a)

use_2 <- FALSE # Change it to TRUE to generate a two-dimensional dataset instea
num_classes <- 3
set.seed(1)

if (use_2) {
  # In case we want something that can be visualized
  p <- 2
  n <- 20
  means <- matrix(c(-1, 0, 0, 1, 1, 0), ncol = 3)
  sds <- 0.1 * c(1, 1, 1) # Play with the scale: try 0.1, 0.5, 1
  num_classes <- ncol(means)
} else {
  p <- 50
  n <- 20
  means <- matrix(rnorm(p * num_classes), ncol = num_classes)
  sds <- 1 * rep(1, num_classes) # Play with the scale: try 0.1, 0.5, 1
}

dat <- matrix(rep(0, num_classes * n * p), ncol = p)
lbls <- rep(0, n * num_classes)
for (i in 1:num_classes) {
  pos <- (i - 1) * n + (1:n)
  for (j in 1:p) { # Should I mention again no loops are necessary here?
    dat[pos, j] <- rnorm(n) * sds[i] + means[j, i]
  }
  lbls[pos] <- i
}

# (b)

pca_synth <- prcomp(dat, scale = TRUE)

num_pca <- 2
pca_data <- matrix(rep(0, n * num_classes * num_pca), ncol = num_pca)
for (i in 1:num_pca) {
  pca_data[, i] <- pca_synth$x %*% pca_synth$rotation[, i]
}

plot(pca_data[lbls == 1, ], col = "blue", lwd = 5, 
     xlim = c(min(pca_data[, 1]), max(pca_data[, 1])), ylim = c(min(pca_data[, 2]), max(pca_data[, 2])),
     xlab = "Component 1", ylab = "Component 2", main = "PCA Projections")
points(pca_data[lbls == 2, ], col = "red", lwd = 5)
points(pca_data[lbls == 3, ], col = "green", lwd = 5)

# (c)

k_out <- kmeans(dat, 3, nstart = 20)
print(cbind(k_out$cluster, lbls))

# How to quantify it? PAY ATTENTION TO THIS. In theory we need to try every possible permutation of cluster lbls 
# and see which one provides the best match. This permutation increases exponentially with the number of clusters,
# but there are other alternatives 

library(gtools)

compare_labels <- function(num_clusters, num_classes, clustering, lbls)
{
  if (num_clusters > num_classes) {
    stop("This function is not designed to deal with more clusters than ground truth classes")
  }
  perms <- permutations(num_classes, num_classes) # Warning: this increases exponentially with num_classes
  best_err <- Inf
  for (i in 1:nrow(perms)) {
    draft <- rep(0, length(clustering))
    for (j in 1:num_clusters) {
      draft[clustering == j] <- perms[i, j]
    }
    err <- mean(draft != lbls)
    if (err < best_err) {
      best_err <- err
      best_clustering <- draft
    }
  }
  return(best_clustering)
}


best_clustering <- compare_labels(3, num_classes, k_out$cluster, lbls)
table(best_clustering, lbls) # "Confusion matrix". See ISLR Chapter 4.

# (d)

k_out <- kmeans(dat, 2, nstart = 20)
best_clustering <- compare_labels(2, num_classes, k_out$cluster, lbls)
table(best_clustering, lbls)

# (e). 

k_out <- kmeans(dat, 4, nstart = 20)
print(cbind(k_out$cluster, lbls))
# Up to you on how to modify 'compare_labels'

# (f)

k_out <- kmeans(pca_data, 3, nstart = 20)
best_clustering <- compare_labels(3, num_classes, k_out$cluster, lbls)
table(best_clustering, lbls)

# (g): for this to be interesting, we need to change sds to have different entries. Do it, comment on results.

########################################################################################################################
### Question 12

# Try 3 clusters

ucl <- read.table("ucl.dat")

k_out <- kmeans(ucl, 3, nstart = 20)

x11()
plot(ucl[k_out$cluster == 1, ], col = "blue", lwd = 5, 
     xlim = c(min(ucl[, 1]), max(ucl[, 1])), ylim = c(min(ucl[, 2]), max(ucl[, 2])),
     xlab = expression(X[1]), ylab = expression(X[2]), main = "Clustering Results")
points(ucl[k_out$cluster == 2, ], col = "red", lwd = 5)
points(ucl[k_out$cluster == 3, ], col = "green", lwd = 5)

# Try 20 clusters

K <- 20
k_out <- kmeans(ucl, K, nstart = 20)
x11()
for (k in 1:K) {
  plot(ucl, col = "black", lwd = 1, 
       xlim = c(min(ucl[, 1]), max(ucl[, 1])), ylim = c(min(ucl[, 2]), max(ucl[, 2])),
       xlab = expression(X[1]), ylab = expression(X[2]), main = paste("Clustering", k))
  points(ucl[k_out$cluster == k, ], col = "red", lwd = 5)
  par(ask = TRUE)
}
par(ask = FALSE)

# Do some nonlinear transformations. NONE OF THESE WILL WORK THAT GREAT. A reminder in which
# ways adaptive non-Euclidean clustering methods can be useful (not covered in this course).

ucl2 <- cbind(ucl, ucl^2, ucl^3, cos(ucl), sin(ucl), log(ucl), sqrt(ucl))

k_out <- kmeans(ucl2, 3, nstart = 20)

x11()
plot(ucl[k_out$cluster == 1, ], col = "blue", lwd = 5, 
     xlim = c(min(ucl[, 1]), max(ucl[, 1])), ylim = c(min(ucl[, 2]), max(ucl[, 2])),
     xlab = expression(X[1]), ylab = expression(X[2]), main = "Clustering Results")
points(ucl[k_out$cluster == 2, ], col = "red", lwd = 5)
points(ucl[k_out$cluster == 3, ], col = "green", lwd = 5)

# Do some nonlinear transformation + PCA

pca_ucl2 <- prcomp(ucl2, scale = TRUE)
pca_data <- matrix(rep(0, 2 * nrow(ucl2)), ncol = 2)
for (i in 1:2) {
  pca_data[, i] <- pca_ucl2$x %*% pca_ucl2$rotation[, i]
}

k_out <- kmeans(pca_data, 3, nstart = 20)
x11()
plot(ucl[k_out$cluster == 1, ], col = "blue", lwd = 5, 
     xlim = c(min(ucl[, 1]), max(ucl[, 1])), ylim = c(min(ucl[, 2]), max(ucl[, 2])),
     xlab = expression(X[1]), ylab = expression(X[2]), main = "Clustering Results")
points(ucl[k_out$cluster == 2, ], col = "red", lwd = 5)
points(ucl[k_out$cluster == 3, ], col = "green", lwd = 5)
