getConditionalDistribution <- function(x,mu,Sigma,frontIndexes) {
  x_b <- x[-frontIndexes,,drop=FALSE]
  mu_a <- mu[frontIndexes,,drop=FALSE]
  mu_b <- mu[-frontIndexes,,drop=FALSE]
  Sigma_a <- Sigma[frontIndexes,frontIndexes,drop=FALSE]
  Sigma_b <- Sigma[-frontIndexes,-frontIndexes,drop=FALSE]
  Sigma_c <- Sigma[frontIndexes,-frontIndexes,drop=FALSE]
  
  mu_a_hat <- mu_a + Sigma_c %*% solve(Sigma_b) %*% (x_b - mu_b)
  Sigma_a_hat <- Sigma_a - Sigma_c %*% solve(Sigma_b) %*% t(Sigma_c)
  
  list(
    mu = mu_a_hat,
    Sigma = Sigma_a_hat
  )
}
