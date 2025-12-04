#' Simulate a binomial distribution and plot outcomes
#'
#' Runs a Monte Carlo simulation of a binomial distribution by generating
#' repeated Bernoulli samples, then plots the empirical distribution of
#' the number of successes.
#'
#' @param iter Number of simulation iterations (default = 100).
#' @param n Number of Bernoulli trials in each iteration (sample size).
#' @param p Probability of success on each Bernoulli trial (default = 0.5).
#'
#' @importFrom grDevices rainbow
#'
#' @return A barplot of the simulated distribution is displayed.
#'         Invisibly returns nothing.
#' @export
#'
#' @examples
#' # Run a simulation with default settings
#' mybin()
#'
#' # Run with 200 iterations, 20 trials per iteration, success probability 0.3
#' mybin(iter = 200, n = 20, p = 0.3)
mybin = function(iter = 100, n = 10, p = 0.5){
  # Create a matrix with n rows and iter columns to store the samples
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  # Vector to hold the number of successes from each sample
  succ = c()

  # Loop over the number of iterations
  for(i in 1:iter){

    # Generate a sample of size n (1 = success, 0 = failure) with probability p
    sam.mat[, i] = sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))

    # Count the number of successes in this sample and store it
    succ[i] = sum(sam.mat[, i])
  }

  # Create a frequency table of the number of successes (0 to n)
  succ.tab = table(factor(succ, levels = 0:n))

  # Make labels for iteration, sample size, and probability
  iter.lab = paste0("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p = ", p)

  # Combine labels into one string
  lab = paste(iter.lab, n.lab, p.lab, sep = ", ")

  # Draw a barplot of the relative frequencies (proportions)
  barplot(succ.tab / iter,
          col = rainbow(n + 1),
          main = "Binomial simulation",
          sub = lab,
          xlab = "Number of successes")
}
