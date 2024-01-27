#' the sample (biased) covariance matrix
#'
#' @param x a set of quantitative date with multiple vector
#'
#' @return the matrix of the sample covariance
#' @export
#'
#' @examples data <- data.frame(x = rnorm(10), y = rnorm(10)); covMatrix(data)
covMatrix <- function(x) {
  # Number of rows
  nRow <- nrow(x)
  # Number of columns
  p <- ncol(x)
  # Initialize an empty matrix to fill out with covariance values
  covMatrix <- matrix(0, nrow = p, ncol = p)

  # to calculate the covariance of the input data
  for (i in 1:p) {
    for (j in 1:p) {
      # Calculate the sum of product of deviations from the mean
      sumC <- sum((x[, i] - mean(x[, i])) * (x[, j] - mean(x[, j])))
      # Divide by the number of observations to get the covariance
      covV <- sumC / (nRow)
      # assign the covariance to the matrix
      covMatrix[i, j] <- covV
    }
  }

  return(covMatrix)
}


