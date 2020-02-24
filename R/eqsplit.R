
#' Split a vector into a list of equal number of elements
#'
#' @param vec vector to be split
#' @param n integer; size of the split subset
#' @return
#' A list of vectors of size `n' (except for last one, possibly) containing
#' all elements of `vec'.
#' @export
eqsplit <- function(vec,n) {
  split(vec, ceiling(seq_along(vec)/n))
}
