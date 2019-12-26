
#' @export
bins2lev <- function(bins) {
    (tail(bins,-1) + head(bins,-1))/2
}
