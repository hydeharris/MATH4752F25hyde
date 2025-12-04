utils::globalVariables("x")

#' Plot N
#'
#' @param mu Mean of the normal
#' @param sigma Standard deviation (positive)
#' @param a Cutoff for the left-tail area
#'
#' @returns A named list: mu, sigma, a, area where area = P(X <= a)
#' @importFrom stats dnorm pnorm
#' @importFrom graphics curve polygon
#' @export
#'
#' @examples
#' myncurve(mu=10,sigma=5, a=6)
myncurve = function(mu, sigma, a){
  x1 <- min(mu - 3*sigma, a)
  x2 <- max(mu + 3*sigma, a)

  curve(dnorm(x, mean = mu, sd = sigma), from = x1, to = x2)

  # Plotting the curve
  xcurve <- seq(x1, a, length.out = 500)
  ycurve <- dnorm(xcurve, mean = mu, sd = sigma)

  # Plotting the area
  polygon(c(x1, xcurve, a), c(0, ycurve, 0), col = "gray")

  # Computation
  area <- pnorm(a, mean = mu, sd = sigma)
  list(mu = mu, sigma = sigma, a = a, area = area)
}
