#' Impute the missing data using the parameters params
#'
#' @param params a list with mu and Sigma; it should be the output from the runEm function
#' @param data a matrix with the columns as attributes and rows as observations
#'
#' @return the data, but with the NA's imputed
#' @export
#'
#' @examples
#' nCol <- 10
#' nRow <- 100
#' data <- matrix(runif(nCol * nRow),nrow=nRow,ncol=nCol)
#' data[1,1:9] <- NA
#' 
#' maxIterations <- 10
#' params <- runEm(data,maxIterations,howToStop = "params")
#' newData <- missingNormalImpute(params, data)
missingNormalImpute <- function (params, data) 
{
  
  nTest <- nrow(data)
  
  v <- params$mu
  
  M <- params$Sigma
  values <- list()
  Sigmas <- list()
  for (i in 1:nTest) {
    s_i <- convertToColumnVector(data[i, , drop = FALSE])
    E_s_i <- s_i
    isNaIndexes <- which(is.na(s_i))
    if (length(isNaIndexes)) {
      distr <- getConditionalDistribution(s_i, v, M, isNaIndexes)
      s_i[isNaIndexes] <- distr$mu
    }
    data[i, ] <- s_i
  }
  data
}