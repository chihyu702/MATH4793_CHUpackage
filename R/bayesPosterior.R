#' bayesPosterior
#'
#' The function produces a plot that has all three densities on the one axes, p(theta), f(y|theta),p(theta|y)
#' It prints a named list containing Bayesian point and interval estimates (intervals made for a given tail area of a)
#'
#'
#' @param n number of trials in the binomial experiment
#' @param y number of successes
#' @param alpha alpha parameter of the prior Beta distribution
#' @param beta beta parameter of the prior Beta distribution.
#' @param a significance level
#'
#' @return named list containing Bayesian point and interval estimates
#' @export
#'
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom stats dbeta
#' @importFrom stats dbinom
#' @importFrom stats qbeta
#'
#' @examples bayesPosterior (n = 10, y = 4, alpha = 1, beta = 1, a = 0.025)
bayesPosterior <- function(n=10, y=4, alpha=1, beta=1, a=0.025) {
  # calculate likelihood, Binomial
  likelihood <- function(theta) {
    dbinom(y, size = n, prob = theta)
    # theta^y * (1 - theta)^(n - y)
  }
  # calculate prior density, Beta
  prior <- function(theta) {
    dbeta(theta, alpha, beta)
  }
  # calculate posterior density, Beat
  posterior <- function(theta){
    dbeta(theta, alpha + y, beta + n - y)
  }

  # Create a sequence of theta values for plotting, set each interval = 0.0001
  theta <- seq(0, 1, 0.0001)

  # Calculate the densities by calling functions
  prior_value <- prior(theta)
  likelihood_value <- likelihood(theta)
  posterior_value <- posterior(theta)
  # check with likelihood with normalize
  # likelihood_normalize <- likelihood_value / max(likelihood_value) * max(prior_value)

  # plot
  plot(theta, prior_value, type = 'l', col = 'blue',
       ylim = c(0, max(c(prior_value, likelihood_value, posterior_value))),
       ylab = 'Density', xlab = 'Theta',
       main = 'Prior, Likelihood, and Posterior Densities')

  lines(theta, likelihood_value, col = 'red', lwd = 2)
  lines(theta, posterior_value, col = 'green', lwd = 2)
  legend("topright", legend = c("Prior", "Likelihood", "Posterior"), col = c("blue", "red", "green"), lty = 1, lwd = 2)

  # Bayesian point and interval estimates
  point <- (alpha + y) / (alpha + beta + n)
  interval <- qbeta(c(a/2, 1 - a/2), alpha + y, n - y + beta)

  # Print the results
  results <- list(Point_Estimate = point, Interval_Estimate = interval)
  print(results)
}
