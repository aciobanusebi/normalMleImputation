logLikelihood <- function(data,params) {
  result <- 0
  for(i in 1:nrow(data)) {
    x_i <- convertToColumnVector(data[i,,drop=FALSE])
    indexes <- which(!is.na(x_i))
    result <- result + getMarginalDistributionLogPdf(x_i,params$mu,params$Sigma,indexes)
  }
  result
}