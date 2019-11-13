getMarginalDistributionLogPdf <- function(x,mu,Sigma,indexes) {
  x_a <- x[indexes,,drop=FALSE]
  mu_a <- mu[indexes,,drop=FALSE]
  Sigma_a <- Sigma[indexes,indexes,drop=FALSE]
  myLogNormal(x_a,mu_a,Sigma_a)
}
