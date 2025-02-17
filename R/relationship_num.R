#' Calculate Relationship Numbers Based on Social Networks and Breakpoints
#'
#' This function computes the number of relationships in different tiers
#' for each individual across time points.
#' The relationships are categorized based on Jenks breakpoints and
#' gender (same or different sex).
#' @param social_networks A 3D array containing social network data.
#' Dimensions: [individuals, individuals, time]
#' @param breakpoints_df A data.frame containing Jenks breakpoints
#' for each individual and time point.
#'
#' @return A data.frame with relationship counts for each tier and
#' gender category.
#' @export
#'
#' @examples
#' # Example usage:
#' # Assuming 'social_network_data' and 'breakpoints_df' are properly formatted
#' relationships <- relationship_num(social_network_data, breakpoints_df)
#' print(relationships)
relationship_num <- function(social_networks, breakpoints_df) {
  # Extract individual IDs and time points from social_networks
  individual_ids <- dimnames(social_networks)[[1]]
  time_points <- dimnames(social_networks)[[3]]

  # Map individual IDs to their corresponding sex
  sexes <- breakpoints_df$sex[match(individual_ids, breakpoints_df$ID)]

  # Calculate the number of tiers based on breakpoints_df columns
  num_tiers <- ncol(breakpoints_df) - 3
  # Assuming columns are ID, sex, time, jenk1, ..., jenkN

  # Initialize the relationship data.frame with existing columns
  relationship_df <- breakpoints_df[, c("ID", "time", "sex")]

  # Iterate over each row in the relationship data.frame
  for (i in seq_along(relationship_df)) {
    current_id <- relationship_df$ID[i]
    current_sex <- relationship_df$sex[i]
    current_time <- relationship_df$time[i]

    # Find the index of the current ID and time in the social_networks array
    id_idx <- which(individual_ids == current_id)
    time_idx <- which(time_points == current_time)

    # Extract the egocentric network for the current individual at the
    # current time
    egocentric_network <- social_networks[id_idx, -id_idx, time_idx]
    other_sexes <- sexes[-id_idx] # Sexes of other individuals

    # Iterate over each tier to calculate relationship counts
    for (tier in 1:(num_tiers + 1)) {
      if (tier == (num_tiers + 1)) {
        relationship_df[i, paste0("tier", tier)] <-
          sum(
            egocentric_network <= breakpoints_df[i, paste0("jenk", num_tiers)]
          )
        relationship_df[i, paste0("tier", tier, "_homo")] <-
          sum(egocentric_network <= breakpoints_df[i, paste0("jenk", num_tiers)]
              & other_sexes == current_sex)
        relationship_df[i, paste0("tier", tier, "_hetero")] <-
          sum(egocentric_network <= breakpoints_df[i, paste0("jenk", num_tiers)]
              & other_sexes != current_sex)
      } else {
        relationship_df[i, paste0("tier", tier)] <-
          sum(egocentric_network > breakpoints_df[i, paste0("jenk", tier)])
        relationship_df[i, paste0("tier", tier, "_homo")] <-
          sum(egocentric_network > breakpoints_df[i, paste0("jenk", tier)]
              & other_sexes == current_sex)
        relationship_df[i, paste0("tier", tier, "_hetero")] <-
          sum(egocentric_network > breakpoints_df[i, paste0("jenk", tier)]
              & other_sexes != current_sex)
      }
    }
  }

  relationship_df
}
