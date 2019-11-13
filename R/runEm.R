
#' Run the EM algorithm
#'
#' This function runs the Expectation Maximization algorithm for obtaining the MLE estimates for a multivariate normal distribution when there are missing values in data.
#' @param data a matrix with the columns as attributes and rows as observations
#' @param maxIterations the maximum number of iterations for the EM algorithm
#' @param oldParams the initialization of parameters: a list with mu and Sigma; if NULL, then we initialize the parameters via the function initParams
#' @param howToStop "loglike" or "params"; check whether the loglikelihood does not change from an iteration to another OR the parameters do not change
#' @param eps used to compare the loglikelihoods/params from two consecutive iterations; if the relative error is < eps then STOP
#'
#' @return params - a list with mu and Sigma - after running the EM algorithm
#' @export
#'
#' @examples
#' nCol <- 10
#' nRow <- 100
#' data <- matrix(runif(nCol * nRow),nrow=nRow,ncol=nCol)
#' data[1,1:9] <- NA
#' 
#' params <- initParams(data)
#' maxIterations <- 10
#' params <- runEm(data,maxIterations,oldParams=params,howToStop = "loglike",eps = 1e-6)
#' 
#' # OR
#' nCol <- 10
#' nRow <- 100
#' data <- matrix(runif(nCol * nRow),nrow=nRow,ncol=nCol)
#' data[1,1:9] <- NA
#' 
#' maxIterations <- 10
#' params <- runEm(data,maxIterations,howToStop = "params")
runEm <- function(data,maxIterations,oldParams=NULL,howToStop = "params",eps = 1e-6) {
  howToStop <- tolower(howToStop)
  if(is.null(oldParams)) {
    oldParams <- initParams(data)
  }
  if(howToStop == "loglike") {
    oldLogLike <- logLikelihood(data,oldParams)
    print(oldLogLike)
  }
  for(i in 1:maxIterations) {
    params <- execIteration(oldParams,data)
    if(howToStop == "loglike") {
      logLike <- logLikelihood(data,params)
      print(logLike)
      if(abs(logLike - oldLogLike)/oldLogLike < eps) {
        print("STOP: loglike")
        return(params)
      }
    } else {
      a <- unlist(oldParams)
      b <- unlist(params)
      if(norm(a - b)/norm(a) < eps) {
        print("STOP: params")
        return(params)
      }
    }
    oldParams <- params
    if(howToStop == "loglike") {
      oldLogLike <- logLike
    }
  }
  print("STOP: maxIterations")
  params
}