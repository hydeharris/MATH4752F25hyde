#' SHINY MLE
#'
#' @returns shiny app
#' @export
#' @importFrom shiny runApp
#' @examples
#' \dontrun{shinymle()}
shinymle <- function(){
  shiny::runApp(system.file("SHINY", package="MATH4753F25hyde"), launch.browser = TRUE)
}
