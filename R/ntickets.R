#' ntickets
#'
#' Calcuulates the optimal number of tickets to sell for a flight with N many seats. To allow airlines to sell the most tickets they can while managing overbooking risk.
#' Uses both a binomial method and a normal approximation.
#'
#' @param N Number of seats available
#' @param gamma Acceptance rate of failure
#' @param p Probability that each passenger shows up
#'
#' @returns A named list containing the discrete solution (nd), the continuous solution (nc), N, p, and gamma.
#' @importFrom stats pbinom uniroot
#' @importFrom graphics abline points
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
ntickets <- function(N, gamma, p){
  max_additional_seats = round(0.1 * N) # A range of ticket sales to test for (surely they wont need to sell more than 10% more tickets over, right?)

  # Binomial Method (Discrete)
  nd <- 0 # number of tickets to sell according to the binomial method.

  n_range <- N:(N+max_additional_seats)
  overbook_prob_binom <- 1 - pbinom(N, size = n_range, prob = p)
  target_diffs <- abs(overbook_prob_binom - gamma) #table of all the simulated differences in the range
  nd <- n_range[which.min(target_diffs)] # get the ticket sale that corresponds to the min difference
  #nd

  # Plotting it
  obj_d <- 1 - gamma - pbinom(N, size = n_range, prob = p)
  plot(n_range, obj_d, type = "s", lwd = 2,
       xlab = "n", ylab = "Objective",
       main = sprintf("Objective Vs n (Discrete - %d)", nd),
       xaxs = "i", yaxs = "i")
  abline(h = 0, col = "red")
  abline(v = nd, col = "red")
  points(nd, obj_d[which.min(abs(obj_d))], pch = 19)

  # Normal Method (Continuous)
  cont_method <- function(n){
    mu <- n * p
    sd <- sqrt(mu * (1 - p))

    (1 - gamma) - pnorm(N + 0.5, n * p, sd)
  }

  n_range <- seq(N, N + max_additional_seats)
  sim_vals <- cont_method(n_range)

  cross <- which(diff(sign(sim_vals)) != 0) # see when the error values are still postive and less than the gamma

  nc <- uniroot(cont_method, interval = c(n_range[cross[1]], n_range[cross[1] + 1]))$root #uniroot continuous search
  nc <- round(nc) # round to the nearest whole number

  # Plotting it
  plot(n_range, sim_vals, type = "l", lwd = 2,
       xlab = "n", ylab = "Objective",
       main = sprintf("Objective Vs n (Continuous - %d)", nc))
  abline(h = 0, col = "green")
  abline(v = nc, col = "green", lty = 2)
  points(nc, 0, pch = 19, col = "green")

  # Results

  #nc
  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  result
}

