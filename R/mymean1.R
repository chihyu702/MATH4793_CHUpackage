#' mymean1
#'
#' @param x the vector of the data
#'
#' @return the mean of the vector
#' @export
#'
#' @examples  data <- data.frame(x = rnorm(10), y = rnorm(10)); apply(data, 2, mymean1)
mymean1 <- function(x) {
  # calculate the mean of the given vector
  sum(x)/length(x)
}
