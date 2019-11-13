execIteration <- function(params,data) {
  nRow <- nrow(data)
  nCol <- ncol(data)
  mu <- params$mu
  Sigma <- params$Sigma
  E_x <- list()
  E_xx <- list()
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
    E_x[[i]] <- E_x_i
    E_xx[[i]] <- E_xx_i
  }
  mu <- 0
  for(i in 1:nRow) {
    mu <- mu + E_x[[i]]
  }
  mu <- mu/nRow
  
  Sigma <- 0
  for(i in 1:nRow) {
    Sigma <- Sigma + E_xx[[i]] - E_x[[i]] %*% t(mu) - mu %*% t(E_x[[i]]) +  mu %*% t(mu)
  }
  Sigma <- Sigma/nRow
  
  list(
    mu=mu,
    Sigma=Sigma
  )
}
