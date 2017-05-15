# Taken from https://stat.ethz.ch/pipermail/r-help/2010-November/259019.html

plot.dist <- function(alpha, from = -5, to = 5, n = 1000, filename = NULL,
                      alternative = c("two.tailed", "greater", "lesser"),
                      distribution = c("normal", "t", "F", "chisq", "binomial"),
                      colour = "black", fill = "skyblue2", main_title = NULL, ...) {
  alternative <- match.arg(alternative)
  ## Calculate alpha level given hypothesis
  alt.alpha <- switch(alternative,
                      two.tailed = alpha/2,
                      greater = alpha,
                      lesser = alpha)
  ## use a 'switch' to pick the right functions based on distribution
  my.den <- switch(distribution,
                   normal = dnorm,
                   t = dt,
                   F = df,
                   chisq = dchisq,
                   binomial = dbinom)
  my.dist <- switch(distribution,
                    normal = qnorm,
                    t = qt,
                    F = qf,
                    chisq = qchisq,
                    binomial = qbinom)
  ## Additional arguments passed via '...' e.g., degrees of freedom
  crit.lower <- my.dist(p = alt.alpha, lower.tail = TRUE, ...)
  crit.upper <- my.dist(p = alt.alpha, lower.tail = FALSE, ...)
  ## Calculate alpha (lower) region coordinates
  cord.x1 <- c(from, seq(from = from, to = crit.lower,
                         length.out = 100), crit.lower)
  cord.y1 <- c(0, my.den(x = seq(from = from, to = crit.lower,
                                 length.out = 100), ...), 0)
  ## Calculate alpha (upper) region coordinates
  cord.x2 <- c(crit.upper, seq(from = crit.upper, to = to,
                               length.out = 100), to)
  cord.y2 <- c(0, my.den(x = seq(from = crit.upper, to = to,
                                 length.out = 100), ...), 0)
  ## Logic test to choose which graphic device to open
  if(is.null(filename)) {
    dev.new()
  } else {
    pdf(file = filename)
  }
  ## plot distribution
  curve(my.den(x, ...), from = from, to = to,
        n = n, col = colour, lty = 1, lwd = 2,
        ylab = "Density", xlab = "Values", main = main_title)
  ## Add alpha region(s) based on given hypothesis
  if(!identical(alternative, "greater")) {
    polygon(x = cord.x1, y = cord.y1, col = fill)
  }
  if(!identical(alternative, "lesser")) {
    polygon(x = cord.x2, y = cord.y2, col = fill)
  }
  ## If the PDF device was started, shut it down
  if(!is.null(filename)) {dev.off()}
}