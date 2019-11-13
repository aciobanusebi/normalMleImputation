myLogNormal <- function(x,mu,Sigma) {
  library(mvtnorm)
  # d <- nrow(x)
  # -d/2 * log(2*pi) - 1/2*log(det(Sigma)) - 1/2 * t(x - mu) %*% solve(Sigma) %*% (x-mu)
  dmvnorm(as.numeric(x), as.numeric(mu), Sigma, log = TRUE)
}
