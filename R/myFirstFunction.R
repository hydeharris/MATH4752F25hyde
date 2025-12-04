#' Title
#'
#' @param x a quantitative vector
#'
#' @returns A list with components 'x'
#' @export
#'
#' @examples
#' myFirstFunction(1:10)
myFirstFunction <- function(x) {
  y <- x^2
  plot(x-y)
  list(x=x, y=y)
}

