#' bayesMixture
#'
#' This function produces a plot of the mixture prior, and a plot of the posterior.
#' It also print a named list of Bayesian point and interval estimates of theta for a given tail area of a
#'
#' @param n number of trials in the binomial experiment
#' @param y number of successes
#' @param p the mixing probability, the weight of the first beta distribution
#' @param alpha1 alpha parameter of the first prior Beta distribution
#' @param beta1 beta parameter of the first prior Beta distribution
#' @param alpha2 alpha parameter of the second prior Beta distribution
#' @param beta2 beta parameter of the second prior Beta distribution
#' @param a significance level
#'
#' @importFrom graphics curve
#'
#' @return named list containing Bayesian point and interval estimates
#' @export
#'
#' @examples bayesMixture(n=20, y=12, p=0.3, alpha1=2, beta1=3, alpha2=4, beta2=5, a=0.025)
bayesMixture <- function(n=20, y=12, p=0.3, alpha1=2, beta1=3, alpha2=4, beta2=5, a=0.025) {

  # calculate mixture prior, beta
  prior <- function(theta) {
    p * dbeta(theta, alpha1 , beta1) + (1 - p) * dbeta(theta, alpha2 , beta2)
  }
  # calculate posterior, beta
  posterior <- function(theta) {
    p * dbeta(theta, alpha1 + y, n - y + beta1) + (1 - p) * dbeta(theta,alpha2 + y, n - y + beta2)
  }

  # Plot the mixture prior
  curve(prior, from = 0, to = 1, main = "Prior Distribution", col="blue", ylab = "density", xlab=expression(theta))
  # plot the posterior
  curve(posterior, from = 0, to = 1, main = "Posterior Distribution", col = "red", ylab = "density", xlab=expression(theta))

  # Bayesian point estimate
  mean1 <- (alpha1 + y) / (alpha1 + beta1 + n)
  mean2 <- (alpha2 + y) / (alpha2 + beta2 + n)
  point <- p * mean1 + (1 - p) * mean2
  # Bayesian interval estimates
  lower_bound <- qbeta(a / 2, alpha1 + y, beta1 + n - y)
  upper_bound <- qbeta(1 - a / 2, alpha2 + y, beta2 + n - y)
  interval = c(lower_bound, upper_bound)

  # Print the results
  estimates <- list(Point_Estimate = point, Interval_Estimate = interval)
  print(estimates)
}
