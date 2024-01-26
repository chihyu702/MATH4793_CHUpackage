#' CovarianceMatrix
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
CovarianceMatrix = function(x){

  #Number of data sets
  dataSets =  ncol(x)

  variableNames = colnames(x)

  #Number of data points in each vector
  numDataPoints = nrow(x)

  #Vector of the Covariance values
  covarianceVector = vector()

  ##For loops to calculate the covariances of the data sets
  for (i in 1:dataSets) {

    for(j in 1:dataSets){

      covarianceVector = c(covarianceVector,Covariance(x[,i],x[,j]))

    }

  }

  #Returns the covariance values as a matrix
  return(matrix(covarianceVector,dataSets,dataSets,TRUE,list(variableNames,variableNames)))

}
