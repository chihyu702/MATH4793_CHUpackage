#' vectorMean
#'
#' @param x a set of quantitative date with multiple vector
#'
#' @return the mean of each vector
#' @export
#'
#' @examples data <- data.frame(x = rnorm(10), y = rnorm(10)); vectorMean(data)
vectorMean<- function(x) {
  # number of column
  col <- ncol(x)
  # create a vector with numeric element
  # the number of the element in this vector would be the number of the column
  means <- numeric(ncol(x))

  # number of row
  row <- nrow(x)

  # calculate column by column
  for (j in 1:col) {
    # initialize the sum of column be 0
    sum <- 0
    # sum up the row
    for (i in 1:row) {
      sum <- sum + x[i, j]
    }

    # record the mean for the specific col
    # mean = sum / number of rows
    means[j] <- sum / row
  }
  return(means)
}
