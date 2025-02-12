#' Returns the sum of squared deviations from the mean
#'
#' @param data A numeric vector
#'
#' @return The sum of squared deviations from the mean
#' @export
#'
#' @examples
#' sdm(c(1, 2, 3))
sdm <- function(data) {
  sum((data - mean(data)) ^ 2)
}

