
# PARALLEL DISTANCE COMPUTATION


library(parallel)
library(doParallel)

compute_distance_matrix_parallel <- function(pitcher_ids, metrics, kde_cache,
                                             distance_type = "hellinger",
                                             metric_weights = NULL,
                                             n_cores = NULL,
                                             verbose = TRUE) {
  
  n <- nrow(pitcher_ids)
  
  if (is.null(metric_weights)) {
    metric_weights <- rep(1/length(metrics), length(metrics))
  }
  
  # Detect cores
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 1)
  }
  
  if (verbose) {
    cat(sprintf("  Using %d CPU cores for parallel processing\n", n_cores))
  }
  
  # Create list of all pairwise comparisons
  comparisons <- expand.grid(i = 1:(n-1), j = 2:n)
  comparisons <- comparisons[comparisons$j > comparisons$i, ]
  total_comparisons <- nrow(comparisons)
  
  if (verbose) {
    cat(sprintf("  Total comparisons: %s\n", format(total_comparisons, big.mark = ",")))
  }
  
  # Setup parallel cluster
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # Export necessary objects
  clusterExport(cl, c("compute_kde_distance_cached", "kde_cache", "pitcher_ids", 
                      "metrics", "distance_type", "metric_weights"),
                envir = environment())
  
  start_time <- Sys.time()
  
  # Parallel computation with progress
  if (verbose) cat("  Computing distances in parallel...\n")
  
  results <- foreach(idx = 1:nrow(comparisons), .combine = rbind) %dopar% {
    i <- comparisons$i[idx]
    j <- comparisons$j[idx]
    
    # Compute distance for each metric
    metric_dists <- sapply(metrics, function(m) {
      compute_kde_distance_cached(
        pitcher_ids$id[i], 
        pitcher_ids$id[j], 
        m, 
        kde_cache,
        distance_type
      )
    })
    
    metric_dists[is.na(metric_dists)] <- 0
    combined_dist <- sqrt(sum(metric_weights * metric_dists^2))
    
    c(i, j, combined_dist)
  }
  
  stopCluster(cl)
  
  # Build distance matrix from results
  dist_matrix <- matrix(0, n, n)
  
  for (k in 1:nrow(results)) {
    i <- results[k, 1]
    j <- results[k, 2]
    dist <- results[k, 3]
    
    dist_matrix[i, j] <- dist
    dist_matrix[j, i] <- dist
    
    if (verbose && k %% 50000 == 0) {
      cat(sprintf("\r  Building matrix: %s/%s (%.1f%%)     ",
                  format(k, big.mark = ","),
                  format(nrow(results), big.mark = ","),
                  (k/nrow(results))*100))
      flush.console()
    }
  }
  
  if (verbose) {
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("\n  Parallel computation complete in %s\n", format_time(total_time)))
  }
  
  rownames(dist_matrix) <- paste(pitcher_ids$player_name, pitcher_ids$game_year, pitcher_ids$pitch_type)
  colnames(dist_matrix) <- paste(pitcher_ids$player_name, pitcher_ids$game_year, pitcher_ids$pitch_type)
  
  return(dist_matrix)
}
