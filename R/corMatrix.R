#' the sample correlation matrix
#'
#' @param x a set of quantitative date with multiple vector
#'
#' @return the matrix of the sample correlation
#' @export
#'
#' @examples data <- data.frame(x = rnorm(10), y = rnorm(10)); corMatrix(data)
corMatrix <- function(x) {
  # number of row
  nRow <- nrow(x)
  # number of column
  p <- ncol(x)
  # Initialize an empty matrix to fill out with covariance values
  corMatrix <- matrix(0, nrow = p, ncol = p)

  # for loop to calculate each value
  for (i in 1:p) {
    for (j in i:p) {
      xi <- x[, i] - mean(x[, i])
      xj <- x[, j] - mean(x[, j])
      sik <- sum(xi * xj)
      sii <- sum(xi^2)
      skk <- sum(xj^2)

      # calculate the correlation coefficient
      rik <- sik / sqrt(sii * skk)
      # assign to the matrix
      corMatrix[i, j] <- rik
      # rik = rki for all i and k, by definition
      corMatrix[j, i] <- corMatrix[i, j]
    }
  }

  return(corMatrix)
}
