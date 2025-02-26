#' Robust Community Detection in Social Networks
#'
#' This function calculates the robustness of community structures in social
#' networks using bootstrapping and the Louvain community detection algorithm.
#'
#' @import assortnet
#' @import igraph
#'
#' @param social_networks
#'   A 3D array representing multiple social networks. Each layer corresponds
#'   to a different network.
#' @param n_bootstraps
#'   Number of bootstrap iterations to perform (default: 1000).
#' @param level
#'   The level of community membership to consider (default: 1).
#'
#' @returns
#'   A numeric value representing the robustness of the community structure.
#' @export
#'
#' @examples
#' # Example usage:
#' # robust_rc <- robust_community(social_networks, n_bootstraps = 500)
robust_community <- function(social_networks, n_bootstraps = 1000, level = 1) {

  # Replace missing values with 0 to avoid errors in calculations
  social_networks[is.na(social_networks)] <- 0

  # Calculate the mean network across all layers
  mean_network <- apply(social_networks, c(1, 2), mean)

  # Convert the mean network to an undirected, weighted graph
  mean_graph <- igraph::graph_from_adjacency_matrix(
    mean_network,
    mode = "undirected",
    weighted = TRUE
  )

  # Detect communities in the mean network using the Louvain algorithm
  community_observed <- igraph::cluster_louvain(mean_graph)

  # Initialize matrices to store community membership and presence information
  network_community <- matrix(0,
                              nrow = nrow(social_networks),
                              ncol = ncol(social_networks))
  network_present <- matrix(0,
                            nrow = nrow(social_networks),
                            ncol = ncol(social_networks))

  # Perform bootstrapping to assess community robustness
  for (i in 1:n_bootstraps) {
    # Sample networks with replacement for bootstrapping
    boot_networks <- social_networks[, , sample(1:dim(social_networks)[3],
                                                dim(social_networks)[3],
                                                replace = TRUE)]

    # Calculate the mean network for the bootstrap sample
    mean_boot <- apply(boot_networks, c(1, 2), mean)

    # Detect communities in the bootstrap network
    community_boot <- igraph::cluster_louvain(
      igraph::graph_from_adjacency_matrix(
        mean_boot,
        mode = "undirected",
        weighted = TRUE
      )
    )

    # Update matrices to track community membership and node presence
    network_community <- network_community +
      outer(community_boot$memberships[level, ],
            community_boot$memberships[level, ],
            "==")
    network_present <- network_present +
      outer((rowSums(mean_boot) > 0),
            (rowSums(mean_boot) > 0),
            "*")
  }

  # Calculate the proportion of times communities are consistent
  # across bootstraps
  P <- network_community / network_present
  P[!is.finite(P)] <- 0
  # Handle any non-finite values (e.g., due to division by zero)

  # Calculate the robustness of the community structure using assortativity
  rc <- assortnet::assortment.discrete(
    P,
    community_observed$memberships[level, ]
  )$r

  # Return the robustness measure
  rc
}
