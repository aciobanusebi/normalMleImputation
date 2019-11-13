#' Initialize the parameters for the EM algorithm
#'
#' @param data a matrix with the columns as attributes and rows as observations
#'
#' @return params - a list with mu and Sigma
#' @export
#'
#' @examples
#' nCol <- 10
#' nRow <- 100
#' data <- matrix(runif(nCol * nRow),nrow=nRow,ncol=nCol)
#' data[1,1:9] <- NA
#' 
#' params <- initParams(data)
initParams <- function(data) {
  params <- list()
  params$mu <- convertToColumnVector(colMeans(data,na.rm = T))
  result <- 0
  for(i in 1:nrow(data)) {
    x_i <- convertToColumnVector(data[i,,drop=FALSE])
    if(!any(is.na(x_i))) {
      result <- result + x_i %*% t(x_i)
    }
  }
  params$Sigma <- result
  params
}
