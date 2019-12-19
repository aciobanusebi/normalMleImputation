execIteration <- function(params,data) {
  nRow <- nrow(data)
  nCol <- ncol(data)
  mu <- params$mu
  Sigma <- params$Sigma
  sum_E_x <- 0
  sum_E_xx <- 0
  for(i in 1:nRow) {
    x_i <- convertToColumnVector(data[i,,drop=FALSE])
    E_x_i <- x_i
    isNaIndexes <- which(is.na(x_i))

    if(length(isNaIndexes)) {
      distr <- getConditionalDistribution(x_i,mu,Sigma,isNaIndexes)
      E_x_i[isNaIndexes] <- distr$mu
      E_xx_i <- E_x_i %*% t(E_x_i)
      E_xx_i[isNaIndexes,isNaIndexes] <- E_xx_i[isNaIndexes,isNaIndexes] + distr$Sigma
    } else {
      E_xx_i <- x_i %*% t(x_i)
    }
    sum_E_x <- sum_E_x + E_x_i
    sum_E_xx <- sum_E_xx + E_xx_i
  }
  mu <- sum_E_x/nRow
  Sigma <- sum_E_xx - sum_E_x %*% t(mu) - mu %*% t(sum_E_x) + nRow * mu %*% t(mu)
  Sigma <- Sigma/nRow

  list(
    mu=mu,
    Sigma=Sigma
  )
}
