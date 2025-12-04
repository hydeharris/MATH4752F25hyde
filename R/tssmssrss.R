
#' Calculate Total, Model, and Residual Sum of Squares
#'
#' @param dataframe A data frame containing the observed data.
#' @param quad.lm An object of class lm, typically a fitted quadratic regression model.
#' @param col A numeric vector of observed response values must align with quad.lm.
#' @importFrom stats fitted
#' @returns A numeric vector of length 3 with elements: TSS, MSS, RSS
#' @export
#'
#' @examples
#' df <- head(mtcars)  # take first 6 rows for simplicity
#'
#' quad.lm <- lm(mpg ~ poly(hp, 2, raw = TRUE), data = df)
#'
#' tssmssrss(df, quad.lm, df$mpg)
tssmssrss = function(dataframe, quad.lm, col){
  col.qfit <- fitted(quad.lm)
  TSS <- with(dataframe, sum((col - mean(col)) ^ 2))
  MSS <- with(dataframe, sum((col.qfit - mean(col)) ^ 2))
  RSS <- with(dataframe, sum((col - col.qfit) ^2))

  c(TSS,MSS,RSS)
}
