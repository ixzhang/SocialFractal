#' Calculate Jenks Breakpoints for Social Network Data
#'
#' This function computes Jenks natural breakpoints for social network data across individuals and time points.
#' The result is a matrix containing breakpoints for each individual at each time point.
#'
#' @param social_networks A 3D array with dimensions [individuals, individuals, time points].
#' @param sex The sex information of the zebra finches in social networks. Default is NA.
#' @param break_num The number of breaks (clusters) to compute. Default is 3.
#' @return A matrix containing the ID, time point, and Jenks breakpoints for each individual-time combination.
#' @export
#'
#' @examples
#' # Example usage:
#' # Assuming 'social_network_data' is a 3D array with dimensions [individuals, individuals, time]
#' sex.information <- zf_ind$sex[match(dimnames(zf_rep1_real)[[1]], zf_ind$QR)
#' breakpoints <- get_breakpoints(social_networks = zf_rep1_real, sex = sex.information, break_num = 3)
#' print(breakpoints)
get_breakpoints <- function(social_networks, sex = NA, break_num = 3) {
  require(BAMMtools)

  # Extract individual IDs and time points from the array's dimension names
  individual_ids <- dimnames(social_networks)[[1]]   # Individuals' identifiers
  time_points <- dimnames(social_networks)[[3]]      # Time points in the data
  num_time_points <- length(time_points)            # Total number of time points
  num_individuals <- length(individual_ids)         # Total number of individuals

  # Generate dynamic column names for Jenks breakpoints
  colnames_jenks <- paste0("jenk", 1:break_num)      # Column names for breakpoints (e.g., jenk1, jenk2, ...)

  # Initialize a data.frame to store the results (ID, time, and breakpoints)
  breakpoints_df <- data.frame(
    ID = character(num_time_points * num_individuals),
    sex = character(num_time_points * num_individuals),
    time = character(num_time_points * num_individuals),
    stringsAsFactors = FALSE
  )
  # Add columns for Jenks breakpoints
  for (col in colnames_jenks) {
    breakpoints_df[[col]] <- numeric(num_time_points * num_individuals)
  }

  social_networks[is.na(social_networks)] <- 0

  # Iterate over each time point
  for (time_idx in 1:num_time_points) {
    # Determine the range of rows in the data.frame for the current time point
    start_row <- (num_individuals * (time_idx - 1)) + 1
    end_row <- num_individuals * time_idx

    # Assign individual IDs and time point to the current block of rows
    breakpoints_df[start_row:end_row, "ID"] <- individual_ids   # Assign individual IDs
    breakpoints_df[start_row:end_row, "sex"] <- sex
    breakpoints_df[start_row:end_row, "time"] <- time_points[time_idx]  # Assign current time point

    # Iterate over each individual within the current time point
    for (individual_idx in 1:num_individuals) {
      # Extract interaction values for the current individual (excluding their own ID)
      interaction_values <- social_networks[individual_idx, -individual_idx, time_idx]

      # Skip individuals with no interactions (all zeros)
      if (sum(interaction_values) == 0) {
        next
      }

      # Calculate Jenks natural breakpoints for the current set of interaction values
      # Note: The function 'getJenksBreaks' returns 'break_num + 2' breakpoints; we extract the middle ones
      jenks_breaks <- getJenksBreaks(interaction_values, break_num + 2)

      # Assign the calculated breakpoints to the corresponding row in the data.frame
      current_row <- start_row + individual_idx - 1
      breakpoints_df[current_row, colnames_jenks] <- rev(jenks_breaks[2:(break_num + 1)])
    }
  }

  # Convert time column to numeric (if necessary)
  breakpoints_df$time <- as.numeric(breakpoints_df$time)

  # Return the data.frame of Jenks breakpoints
  return(breakpoints_df)
}
