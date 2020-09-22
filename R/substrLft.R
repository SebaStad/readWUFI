#' Subtract n elements from a string x from the left side. Save the subtracted elements.
#'
#' @param x Chracter. A string of characters
#' @param n Numeric. Amount of elements to be subtracted.
#'
#' @return Character. The n first characters of the string x.
#' @export
#'
#' @examples
#' substrLeft("WUFI + R = eternal love", 6)
substrLeft <- function(x, n){
  substr(x, 1, n)
}
