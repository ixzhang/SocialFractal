#' Find the appropriate number of breaks using Jenks natural breaks optimization
#'
#' This function optimizes the number of breaks for a given dataset using the
#' Jenks natural breaks algorithm.
#' It iterates over different numbers of breaks and calculates the Goodness of
#' Variance Fit (GVF) for each combination of individual and time point.
#' The GVF metric helps determine the number of breaks that best represents the
#' data structure while minimizing variance within groups.
#'
#' @param social_networks A 3D array containing social network data over time.
#'                       Dimensions are individuals (first/second dimension),
#'                       and time points (third dimension).
#' @param max_breaks The maximum number of breaks to consider. Default is 5.
#' @return A data frame containing the GVF values for different numbers of
#' breaks across individuals and time points.
#' @export
#'
#' @examples
#' # Example usage:
#' # Assuming 'social_network_data' is a 3D array with dimensions
#' [individuals, individuals, time]
#' result <- get_breaks_num(social_network_data, max_breaks = 5)
#' # Explore the results to determine the optimal number of breaks
get_breaks_num <- function(social_networks, max_breaks = 5) {
  # Extract unique individual IDs and time points from the array dimension names
  individual_ids <- dimnames(social_networks)[[1]]
  time_points <- dimnames(social_networks)[[3]]

  # Generate a data frame to store GVF values for each combination
  # of individual, time point, and breaks
  result_df <- expand.grid(
    ID = individual_ids,
    breaks = 1:max_breaks,
    time = time_points
  )

  # Iterate over each time point
  for (time_idx in seq_along(time_points)) {
    current_time <- time_points[time_idx]
    # Iterate over each individual
    for (individual_idx in seq_along(individual_ids)) {
      current_id <- individual_ids[individual_idx]
      # Extract the vector excluding the current individual's eigenvector
      data_vector <- social_networks[individual_idx, -individual_idx, time_idx]
      # Calculate GVF for each possible number of breaks
      for (breaks in 3:(max_breaks + 2)) {
        # Find the row in result_df that matches the current ID,
        # breaks, and time
        row_index <- which(
          result_df$ID == current_id &
            result_df$breaks == breaks - 2 &
            result_df$time == current_time
        )
        if (length(row_index) > 0) {
          # Calculate GVF using the 'gvf' function
          gvf_value <- gvf(data_vector, breaks)
          result_df$gvf[row_index] <- gvf_value
        }
      }
    }
  }

  # Return the data frame with GVF values
  return(result_df)
}
