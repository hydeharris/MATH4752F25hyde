
#' Birthday
#'
#' @param x the amount of people to check the probability of sharing a birthday
#'
#' @returns the probability that of x many people there is at least two people who share a birthday
#' @export
#'
#' @examples
#' birthday(20:24)
birthday = function(x){
  1 - exp(lchoose(365, x) + lfactorial(x) - x*log(365))
}

