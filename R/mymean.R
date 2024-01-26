#' mymean
#'
#' @param x a set of quantitative date with multiple vector
#'
#' @return the mean of each vector
#' @export
#'
#' @examples data <- data.frame(x = rnorm(10), y = rnorm(10)); mymean(data)
mymean <- function(x) {
  # calculate the mean of the column vector
  apply(x, 2, function(column) sum(column) / length(column))
}
