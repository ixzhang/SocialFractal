#' Calculate the GVF metric (Goodness of Variance Fit)
#'
#' This function computes the GVF metric, which measures the goodness of fit between
#' observed data and a classification into predefined groups using Jenks breaks. It
#' calculates the proportion of variance explained by the grouped data compared to the
#' original data.
#'
#' @param data A numeric vector of observed values.
#' @param num_breaks An integer specifying the number of groups to classify the data into.
#'
#' @return The GVF metric value, which ranges from 0 to 1. A higher value indicates a better fit.
#' @export
#'
#' @examples
#' # Example usage:
#' data <- rnorm(100, mean = 0, sd = 1)  # Generate a numeric vector
#' gvf_result <- gvf(data, num_breaks = 5)
#' # Output the result
gvf <- function(data, num_breaks) {
  # Check if BAMMtools package is installed and loaded
  if (!requireNamespace("BAMMtools", quietly = TRUE)) {
    stop("BAMMtools package is needed for this function to work.
         Please install it using install.packages(\"BAMMtools\").")
  }

  # Calculate Jenks breaks using BAMMtools package
  breaks <- BAMMtools::getJenksBreaks(data, n = num_breaks)

  # Calculate total sum of squared deviations (sum of squared errors) from the mean
  sdam <- sum((data - mean(data)) ^ 2)

  # Calculate within-group sum of squared deviations
  sdcm <- 0
  for (i in 1:(num_breaks - 1)) {
    subset_data <- data[data > breaks[i] & data <= breaks[i + 1]]
    sdcm <- sdcm + sum((subset_data - mean(subset_data)) ^ 2)
  }

  # Calculate GVF metric
  gvf_value <- (sdam - sdcm) / sdam

  return(gvf_value)
}
