#Compute distance matrix updated

compute_distance_matrix <- function(pitcher_ids, metrics, db_path, 
                                    distance_type = "hellinger",
                                    metric_weights = NULL,
                                    verbose = TRUE) {
  
  n <- nrow(pitcher_ids)
  
  # Default: equal weights
  if (is.null(metric_weights)) {
    metric_weights <- rep(1/length(metrics), length(metrics))
  }
  
  # Initialize distance matrix
  dist_matrix <- matrix(0, n, n)
  
  # Calculate total comparisons
  total_comparisons <- (n * (n - 1)) / 2
  comparisons_done <- 0
  
  # Start timer
  start_time <- Sys.time()
  last_update <- start_time
  
  if (verbose) {
    cat(sprintf("  Total comparisons: %s\n", format(total_comparisons, big.mark = ",")))
    cat(sprintf("  Starting at: %s\n", format(start_time, "%H:%M:%S")))
    cat("  Computing distances...\n")
    flush.console()
  }
  
  # Compute pairwise distances
  for (i in 1:(n-1)) {
    
    # IMMEDIATE feedback every pitcher
    if (verbose && i %% 1 == 0) {
      cat(sprintf("\r    Pitcher %d/%d...", i, n-1))
      flush.console()
    }
    
    for (j in (i+1):n) {
      
      # Compute distance for each metric
      metric_dists <- sapply(metrics, function(m) {
        compute_kde_distance(
          pitcher_ids$id[i], 
          pitcher_ids$id[j], 
          m, 
          db_path, 
          distance_type
        )
      })
      
      # Handle NAs
      metric_dists[is.na(metric_dists)] <- 0
      
      # Weighted combination of metrics
      combined_dist <- sqrt(sum(metric_weights * metric_dists^2))
      
      dist_matrix[i, j] <- combined_dist
      dist_matrix[j, i] <- combined_dist
      
      comparisons_done <- comparisons_done + 1
    }
    
    # Detailed progress every 10 pitchers
    if (i %% 10 == 0 || i == n - 1) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      pct_complete <- (comparisons_done / total_comparisons) * 100
      rate <- comparisons_done / elapsed  # comparisons per second
      remaining_comparisons <- total_comparisons - comparisons_done
      remaining_secs <- remaining_comparisons / rate
      eta <- Sys.time() + remaining_secs
      
      if (verbose) {
        cat(sprintf("\r    Progress: %s/%s (%.1f%%) | %.0f comp/sec | Elapsed: %s | ETA: %s     ",
                    format(comparisons_done, big.mark = ","),
                    format(total_comparisons, big.mark = ","),
                    pct_complete,
                    rate,
                    format_time(elapsed),
                    format(eta, "%H:%M:%S")))
        flush.console()
      }
      
      last_update <- Sys.time()
    }
  }
  
  if (verbose) {
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("\n    ✓ Complete in %s (%.0f comp/sec)\n", 
                format_time(total_time),
                total_comparisons / total_time))
  }
  
  # Add row/column names
  rownames(dist_matrix) <- paste(pitcher_ids$player_name, pitcher_ids$game_year, pitcher_ids$pitch_type)
  colnames(dist_matrix) <- paste(pitcher_ids$player_name, pitcher_ids$game_year, pitcher_ids$pitch_type)
  
  return(dist_matrix)
}

# Helper function to format time
format_time <- function(seconds) {
  if (seconds < 60) {
    return(sprintf("%.0fs", seconds))
  } else if (seconds < 3600) {
    mins <- floor(seconds / 60)
    secs <- seconds %% 60
    return(sprintf("%dm %ds", mins, secs))
  } else {
    hours <- floor(seconds / 3600)
    mins <- floor((seconds %% 3600) / 60)
    return(sprintf("%dh %dm", hours, mins))
  }
}
