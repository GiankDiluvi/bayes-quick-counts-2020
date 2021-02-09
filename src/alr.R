library(mvtnorm)

mu <- min(log(0.6456/4), log(0.02)) - 0.1
sigma1 <- 2 * (log(0.6456/4) - mu)
sigmaJ <- 2 * (log(0.02) - mu)
y <- mvtnorm::rmvnorm(100000, mean = rep(0, 5), sigma = diag(c(sigma1, sigma1, sigma1, sigma1, sigmaJ)))

colMeans(exp(y))

sp1 <- rowSums(exp(y)) + 1
theta <- exp(y) / sp1
theta <- cbind(theta, 1/sp1)
colMeans(theta)

plot(density(theta[, 1]))
plot(density(theta[, 2]))
plot(density(theta[, 3]))
plot(density(theta[, 4]))
plot(density(theta[, 5]))
plot(density(theta[, 6]))
