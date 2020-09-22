#' Subtract n elements from a string x from the right side. Save the subtracted elements.
#'
#' @param x Chracter. A string of characters
#' @param n Numeric. Amount of elements to be subtracted.
#'
#' @return Character. The n first characters of the string x.
#' @export
#'
#' @examples
#' substrRight("WUFI + R = eternal love", 4)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
