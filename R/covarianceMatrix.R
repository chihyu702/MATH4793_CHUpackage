#' CovarianceMatrix
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
CovarianceMatrix <- function(x) {
  n <- nrow(x)
  p <- ncol(x)
  covMatrix <- matrix(0, nrow = p, ncol = p)

  for (i in 1:p) {
    for (j in i:p) {
      xi <- x[, i] - mean(x[, i])
      xj <- x[, j] - mean(x[, j])
      covMatrix[i, j] <- sum(xi * xj) / n
      covMatrix[j, i] <- covMatrix[i, j]  # Exploit symmetry
    }
  }

  return(covMatrix)
}

