#' Calculate Multilevel Group Size from Social Networks
#'
#' This function computes the group size across different hierarchical levels
#' for a social network evolving over time.
#' It uses the Louvain community detection algorithm to identify communities at
#' each time point and calculates group sizes based on the membership structure.
#'
#' @import igraph
#'
#' @param social_networks A 3D array containing social network data.
#' Dimensions: individuals, individuals, time points.
#' @return A data.frame with columns for time, level, individual_num, group_num,
#' and group_size (individual_num / group_num).
#' @export
#'
#' @examples
#' # Example usage:
#' # Assuming 'social_network_data' is a 3D array with dimensions
#' # [individuals, individuals, time]
#' social_network_data <- data(zf_rep1_real)
#' group_sizes <- multilevel_group_size(social_network_data)
#' print(group_sizes)
multilevel_group_size <- function(social_networks) {
  # Extract time points from the social network's dimension names
  time_points <- dimnames(social_networks)[[3]]

  # Initialize a data frame to stor group size information
  group_size_df <- data.frame()

  # Iterate over each time point
  for (current_time in seq_along(time_points)) {
    # Extract the current network matrix
    current_network <- social_networks[, , current_time]
    current_network[is.na(current_network)] <- 0

    # Create an igraph graph object
    current_graph <- igraph::graph.adjacency(current_network,
                                             mode = "undirected",
                                             weighted = TRUE)

    # Detect communities using the Louvain algorithm
    clusters_detected <- igraph::cluster_louvain(current_graph)
    levels_detected <- nrow(clusters_detected$memberships)

    # The data frame of group size in current network
    gs_df <- data.frame(
      time = time_points[current_time],
      level = 1:(levels_detected + 1),
      individual_count = clusters_detected$vcount,
      group_count = c(
        clusters_detected$vcount,
        apply(clusters_detected$memberships, 1, FUN = max)
      )
    )

    # Calculate group size
    gs_df$group_size <- gs_df$individual_count / gs_df$group_count

    group_size_df <- rbind(group_size_df, gs_df)
  }

  group_size_df
}
