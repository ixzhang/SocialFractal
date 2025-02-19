#' Calculate Clumping Rate in Social Networks in Null Model 1
#'
#' This function calculates the clumping rate in a social network based on
#' spatial proximity over time, tests for major social preferences by
#' permuting the identities of individuals on the same perch at the same time.
#' The clumping rate is defined as the ratio of interactions within a given
#' threshold to the total number of interactions.
#'
#'
#' @import stats
#'
#' @param data A data.frame containing spatial coordinates and time information.
#' Columns: id, x, y, time.
#' @param individuals A vector of unique individual identifiers.
#' @param threshold The spatial threshold (in the units of x and y) to
#' determine close contacts.
#' @return A matrix representing the clumping rate between individuals.
#' @export
#'
#' @examples
#' # Example usage:
#' # Assuming 'spatial_data' is a data.frame with columns 'id', 'x', 'y', 'time'
#' # data(raw_data)
#' # data(zf_ind)
#' # cr_matrix <- clumping_rate(raw_data,
#' # individuals = zf_ind$Ind_ID[zf_ind$Replicate == 1],
#' # threshold = 75)
#' # print(cr_matrix)
clumping_rate_null1 <- function(data, individuals, threshold) {
  # Extract unique time points and split data by time
  time_points <- unique(data$time)

  data$time.factor <- as.factor(as.character(data$time))
  data_list <- split(data, f = data$time.factor)

  # Prepare individual identifiers and dimensions for the social network array
  n_individuals <- length(individuals)

  individual_map <- data.frame(
    ID = individuals,
    order = 1:n_individuals
  )

  # Initialize a 3D array to store social network data across time
  social_networks <- array(NA, dim = c(
    n_individuals, n_individuals, length(time_points)
  ))
  rownames(social_networks) <- individuals
  colnames(social_networks) <- individuals

  individual_perch <- data.frame(
    ID = individuals,
    space = 0
  )

  # Populate the social network array with distance matrices for each time point
  for (time_idx in seq_along(time_points)) {
    current_data <- data_list[[time_idx]]

    individual_map$order <- 1:n_individuals

    if (nrow(current_data) > 0) {
      space <- match(current_data$id, individual_perch$ID)
      which_space <- which(!is.na(space))
      individual_perch$space[space[which_space]] <-
        current_data$space[which_space]
    }

    for (
      space_idx in unique(individual_perch$space)[
        unique(individual_perch$space) > 0
      ]
    ) {
      individual_map$order[individual_perch$space == space_idx] <-
        sample(individual_map$order[individual_perch$space == space_idx])
    }

    if (nrow(current_data) > 1) {
      # Remove duplicate IDs and order by ID
      current_data <- current_data[!duplicated(current_data$id), ]

      rownames(current_data) <- current_data$id
      current_data <- current_data[order(current_data$id), ]

      # Compute pairwise Euclidean distances
      distance_matrix <- as.matrix(stats::dist(current_data[, c("x", "y")]))
      diag(distance_matrix) <- NA

      # Map row indices to the global individual order
      position_map <- individual_map$order[match(
        as.numeric(rownames(distance_matrix)), individuals
      )]

      # Handle missing individuals by subsetting
      if (any(is.na(position_map))) {
        distance_matrix <- distance_matrix[
          !is.na(position_map), !is.na(position_map)
        ]
        position_map <- individual_map$order[match(
          as.numeric(rownames(distance_matrix)), individuals
        )]
      }

      # Assign the distance matrix to the social network array
      social_networks[position_map, position_map, time_idx] <- distance_matrix
    }
  }

  # Calculate binary matrices for any contact and close contact
  any_contact <- social_networks
  any_contact[!is.na(any_contact)] <- 1
  any_contact[is.na(any_contact)] <- 0

  close_contact <- social_networks
  close_contact[close_contact <= threshold] <- 1
  close_contact[close_contact > threshold] <- 0

  # Compute the sum of close contacts and any contacts
  close_sum <- apply((close_contact), c(1, 2), sum, na.rm = TRUE)
  any_sum <- apply((any_contact), c(1, 2), sum, na.rm = TRUE)

  # Calculate the clumping rate matrix
  clumping_matrix <- close_sum / any_sum

  clumping_matrix
}
