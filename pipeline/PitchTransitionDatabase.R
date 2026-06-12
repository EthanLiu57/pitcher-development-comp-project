# ==========================================
# PITCH-LEVEL TRANSITION DATABASE
# ==========================================

library(dplyr)

cat("Building pitch-level transition database...\n\n")

# We need the individual pitch distance matrices from Step 2
# These are in results_step2

if (!exists("results_step2")) {
  cat("Loading step2 results...\n")
  results_step2 <- readRDS("~/Downloads/step2_all_results.rds")
}

cat(sprintf("Pitch types available: %s\n\n", paste(names(results_step2), collapse = ", ")))

# ==========================================
# BUILD TRANSITION DATABASE FOR EACH PITCH TYPE
# ==========================================

pitch_transitions_full <- list()

for (pitch_type in names(results_step2)) {
  
  cat(sprintf("========== %s ==========\n", pitch_type))
  
  if (is.null(results_step2[[pitch_type]])) {
    cat("  No data - skipping\n\n")
    next
  }
  
  dist_matrix <- results_step2[[pitch_type]]$dist_matrix
  pitcher_ids <- results_step2[[pitch_type]]$pitcher_ids
  
  cat(sprintf("  Pitchers: %d\n", nrow(pitcher_ids)))
  
  # Find consecutive-year transitions for this pitch type
  pitcher_years <- pitcher_ids %>%
    arrange(pitcher, game_year) %>%
    group_by(pitcher) %>%
    mutate(
      next_year = lead(game_year),
      is_consecutive = (next_year - game_year) == 1
    ) %>%
    filter(!is.na(is_consecutive), is_consecutive) %>%
    ungroup()
  
  cat(sprintf("  Consecutive-year transitions: %d\n", nrow(pitcher_years)))
  
  if (nrow(pitcher_years) == 0) {
    cat("  No transitions - skipping\n\n")
    next
  }
  
  # Build transitions with full KDEs
  pitch_transitions <- list()
  
  for (i in 1:nrow(pitcher_years)) {
    
    pitcher_id <- pitcher_years$pitcher[i]
    year_from <- pitcher_years$game_year[i]
    year_to <- pitcher_years$next_year[i]
    
    # Find indices in pitcher_ids
    idx_from <- which(pitcher_ids$pitcher == pitcher_id & 
                        pitcher_ids$game_year == year_from)
    idx_to <- which(pitcher_ids$pitcher == pitcher_id & 
                      pitcher_ids$game_year == year_to)
    
    if (length(idx_from) == 0 || length(idx_to) == 0) next
    
    # Get KDEs from database
    con <- dbConnect(RSQLite::SQLite(), kde_db)
    
    # Get pitcher-pitch IDs from metadata
    from_id <- dbGetQuery(con, "
      SELECT id FROM pitcher_metadata 
      WHERE pitcher = ? AND game_year = ? AND pitch_type = ?
    ", params = list(pitcher_id, year_from, pitch_type))$id
    
    to_id <- dbGetQuery(con, "
      SELECT id FROM pitcher_metadata 
      WHERE pitcher = ? AND game_year = ? AND pitch_type = ?
    ", params = list(pitcher_id, year_to, pitch_type))$id
    
    if (length(from_id) == 0 || length(to_id) == 0) {
      dbDisconnect(con)
      next
    }
    
    # Get KDEs for all metrics
    kde_transitions <- list()
    
    for (metric in optimized_metrics) {
      
      kde_from <- dbGetQuery(con, "
        SELECT grid_value, density_value
        FROM kde_grids
        WHERE pitcher_id = ? AND metric = ?
        ORDER BY grid_point
      ", params = list(from_id, metric))
      
      kde_to <- dbGetQuery(con, "
        SELECT grid_value, density_value
        FROM kde_grids
        WHERE pitcher_id = ? AND metric = ?
        ORDER BY grid_point
      ", params = list(to_id, metric))
      
      if (nrow(kde_from) > 0 && nrow(kde_to) > 0) {
        kde_transitions[[metric]] <- list(
          grid_from = kde_from$grid_value,
          density_from = kde_from$density_value,
          grid_to = kde_to$grid_value,
          density_to = kde_to$density_value
        )
      }
    }
    
    dbDisconnect(con)
    
    if (length(kde_transitions) > 0) {
      
      # Get player name
      player_name <- pitcher_ids$player_name[idx_from]
      
      pitch_transitions[[length(pitch_transitions) + 1]] <- list(
        pitcher_id = pitcher_id,
        player_name = player_name,
        year_from = year_from,
        year_to = year_to,
        pitch_type = pitch_type,
        idx_from = idx_from,
        idx_to = idx_to,
        kde_transitions = kde_transitions
      )
    }
  }
  
  cat(sprintf("  Valid transitions with KDEs: %d\n\n", length(pitch_transitions)))
  
  pitch_transitions_full[[pitch_type]] <- list(
    pitch_type = pitch_type,
    dist_matrix = dist_matrix,
    pitcher_ids = pitcher_ids,
    transitions = pitch_transitions
  )
}

# Save
saveRDS(pitch_transitions_full, "~/Downloads/pitch_transitions_full_kde.rds")

cat("✓ Pitch-level transition database complete\n")

# Summary
cat("\n========== SUMMARY ==========\n")
for (pt in names(pitch_transitions_full)) {
  n_trans <- length(pitch_transitions_full[[pt]]$transitions)
  cat(sprintf("  %s: %d transitions\n", pt, n_trans))
}

# ==========================================
# PITCH-LEVEL SIMULATION FUNCTION
# ==========================================

simulate_pitch_development <- function(current_pitcher_id,
                                       current_year,
                                       pitch_type,
                                       years_ahead = 1,
                                       k_similar = 20,
                                       n_simulations = 1000,
                                       pitch_transitions_data) {
  
  cat(sprintf("Simulating %s development for pitcher %s (%s)\n", 
              pitch_type, current_pitcher_id, current_year))
  
  if (!(pitch_type %in% names(pitch_transitions_data))) {
    stop(sprintf("Pitch type %s not found", pitch_type))
  }
  
  pt_data <- pitch_transitions_data[[pitch_type]]
  dist_matrix <- pt_data$dist_matrix
  pitcher_ids <- pt_data$pitcher_ids
  transitions <- pt_data$transitions
  
  # Find current pitcher in pitcher_ids
  current_idx <- which(pitcher_ids$pitcher == current_pitcher_id & 
                         pitcher_ids$game_year == current_year)
  
  if (length(current_idx) == 0) {
    stop(sprintf("Pitcher %s (%s) not found for pitch type %s", 
                 current_pitcher_id, current_year, pitch_type))
  }
  
  current_idx <- current_idx[1]
  
  cat(sprintf("Found: %s\n", pitcher_ids$player_name[current_idx]))
  
  # Get current pitcher's KDEs from database
  con <- dbConnect(RSQLite::SQLite(), kde_db)
  
  current_db_id <- dbGetQuery(con, "
    SELECT id FROM pitcher_metadata 
    WHERE pitcher = ? AND game_year = ? AND pitch_type = ?
  ", params = list(current_pitcher_id, current_year, pitch_type))$id
  
  if (length(current_db_id) == 0) {
    dbDisconnect(con)
    stop("Current pitcher KDEs not found in database")
  }
  
  current_kdes <- list()
  
  for (metric in optimized_metrics) {
    
    kde_data <- dbGetQuery(con, "
      SELECT grid_value, density_value
      FROM kde_grids
      WHERE pitcher_id = ? AND metric = ?
      ORDER BY grid_point
    ", params = list(current_db_id, metric))
    
    if (nrow(kde_data) > 0) {
      current_kdes[[metric]] <- list(
        grid = kde_data$grid_value,
        density = kde_data$density_value
      )
    }
  }
  
  dbDisconnect(con)
  
  cat(sprintf("Current pitcher has KDEs for %d metrics\n", length(current_kdes)))
  
  # Find K most similar pitchers
  cat(sprintf("\nFinding %d most similar pitchers...\n", k_similar))
  
  distances <- dist_matrix[current_idx, ]
  distances <- distances[-current_idx]
  
  sorted_idx <- order(distances)
  top_k <- head(sorted_idx, k_similar)
  
  similar_distances <- distances[top_k]
  
  # Weight by similarity
  weights <- exp(-similar_distances^2 / (2 * median(similar_distances)^2))
  weights <- weights / sum(weights)
  
  cat("Top 5 similar pitchers:\n")
  for (i in 1:min(5, length(top_k))) {
    idx <- top_k[i]
    cat(sprintf("  %d. %s (%s) - distance=%.3f, weight=%.3f\n",
                i,
                pitcher_ids$player_name[idx],
                pitcher_ids$game_year[idx],
                similar_distances[i],
                weights[i]))
  }
  
  # Find transitions from similar pitchers
  cat("\nFinding development trajectories...\n")
  
  available_transitions <- list()
  
  for (trans in transitions) {
    
    if (trans$idx_from %in% top_k) {
      # Add weight
      trans$weight <- weights[which(top_k == trans$idx_from)]
      available_transitions[[length(available_transitions) + 1]] <- trans
    }
  }
  
  cat(sprintf("Found %d trajectories from similar pitchers\n\n", 
              length(available_transitions)))
  
  if (length(available_transitions) == 0) {
    stop("No development data available for similar pitchers")
  }
  
  # Run simulations
  cat(sprintf("Running %d simulations...\n", n_simulations))
  
  simulated_pitches <- list()
  transition_weights <- sapply(available_transitions, function(x) x$weight)
  
  for (sim in 1:n_simulations) {
    
    # Sample a transition
    sampled_idx <- sample(1:length(available_transitions),
                          size = 1,
                          prob = transition_weights)
    
    sampled_transition <- available_transitions[[sampled_idx]]
    
    # Apply transition to current pitcher
    simulated_pitch <- list()
    
    for (metric in optimized_metrics) {
      
      current_kde <- current_kdes[[metric]]
      transition_kde <- sampled_transition$kde_transitions[[metric]]
      
      if (!is.null(current_kde) && !is.null(transition_kde)) {
        
        # Calculate transformation
        mean_from <- sum(transition_kde$grid_from * transition_kde$density_from)
        mean_to <- sum(transition_kde$grid_to * transition_kde$density_to)
        delta_mean <- mean_to - mean_from
        
        sd_from <- sqrt(sum(transition_kde$density_from * 
                              (transition_kde$grid_from - mean_from)^2))
        sd_to <- sqrt(sum(transition_kde$density_to * 
                            (transition_kde$grid_to - mean_to)^2))
        delta_sd <- sd_to - sd_from
        
        # Current stats
        current_mean <- sum(current_kde$grid * current_kde$density)
        current_sd <- sqrt(sum(current_kde$density * 
                                 (current_kde$grid - current_mean)^2))
        
        # Apply transformation
        simulated_mean <- current_mean + delta_mean
        simulated_sd <- max(current_sd + delta_sd, 0.01)
        
        # Generate new distribution
        simulated_grid <- current_kde$grid
        simulated_density <- dnorm(simulated_grid,
                                   mean = simulated_mean,
                                   sd = simulated_sd)
        simulated_density <- simulated_density / sum(simulated_density)
        
        simulated_pitch[[metric]] <- list(
          grid = simulated_grid,
          density = simulated_density,
          mean = simulated_mean,
          sd = simulated_sd
        )
      }
    }
    
    simulated_pitches[[sim]] <- simulated_pitch
    
    if (sim %% 100 == 0) {
      cat(sprintf("\r  Simulation %d/%d", sim, n_simulations))
      flush.console()
    }
  }
  
  cat("\n\n✓ Simulations complete\n")
  
  return(list(
    pitcher_id = current_pitcher_id,
    player_name = pitcher_ids$player_name[current_idx],
    year = current_year,
    pitch_type = pitch_type,
    similar_pitchers = data.frame(
      player_name = pitcher_ids$player_name[top_k],
      year = pitcher_ids$game_year[top_k],
      distance = similar_distances,
      weight = weights
    ),
    n_simulations = n_simulations,
    simulated_pitches = simulated_pitches,
    current_kdes = current_kdes
  ))
}

# ==========================================
# PITCH-LEVEL VISUALIZATION
# ==========================================

visualize_pitch_development <- function(sim_results, metric = "release_speed") {
  
  library(ggplot2)
  
  current_kde <- sim_results$current_kdes[[metric]]
  
  if (is.null(current_kde)) {
    cat(sprintf("Metric %s not available\n", metric))
    return(NULL)
  }
  
  # Extract simulated distributions
  sim_data <- data.frame()
  
  for (i in 1:length(sim_results$simulated_pitches)) {
    
    sim_pitch <- sim_results$simulated_pitches[[i]]
    sim_kde <- sim_pitch[[metric]]
    
    if (!is.null(sim_kde)) {
      sim_data <- rbind(sim_data, data.frame(
        grid = sim_kde$grid,
        density = sim_kde$density,
        simulation = i
      ))
    }
  }
  
  # Calculate percentiles
  percentile_data <- sim_data %>%
    group_by(grid) %>%
    summarise(
      p05 = quantile(density, 0.05),
      p25 = quantile(density, 0.25),
      p50 = quantile(density, 0.50),
      p75 = quantile(density, 0.75),
      p95 = quantile(density, 0.95),
      .groups = 'drop'
    )
  
  # Plot
  p <- ggplot() +
    geom_ribbon(data = percentile_data,
                aes(x = grid, ymin = p05, ymax = p95),
                fill = "lightblue", alpha = 0.3) +
    geom_ribbon(data = percentile_data,
                aes(x = grid, ymin = p25, ymax = p75),
                fill = "steelblue", alpha = 0.4) +
    geom_line(data = percentile_data,
              aes(x = grid, y = p50),
              color = "darkblue", size = 1.5) +
    geom_line(data = data.frame(grid = current_kde$grid,
                                density = current_kde$density),
              aes(x = grid, y = density),
              color = "red", size = 1.5, linetype = "dashed") +
    labs(title = sprintf("%s %s Development Projection", 
                         sim_results$player_name,
                         sim_results$pitch_type),
         subtitle = sprintf("%s: Current (red) vs. 1 year ahead (blue)\n%d simulations from %d similar pitchers",
                            metric,
                            sim_results$n_simulations,
                            nrow(sim_results$similar_pitchers)),
         x = metric,
         y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"))
  
  print(p)
  return(p)
}

# ==========================================
# TEST PITCH-LEVEL SIMULATION
# ==========================================

cat("\n========== TESTING PITCH-LEVEL SIMULATION ==========\n\n")

# Find a pitcher with FF data
ff_data <- pitch_transitions_full[["FF"]]

if (!is.null(ff_data) && length(ff_data$transitions) > 0) {
  
  # Use first transition's starting point
  test_pitcher <- ff_data$transitions[[1]]$pitcher_id
  test_year <- ff_data$transitions[[1]]$year_from
  
  cat(sprintf("Test: %s FF (%s)\n\n", test_pitcher, test_year))
  
  sim_pitch <- simulate_pitch_development(
    current_pitcher_id = test_pitcher,
    current_year = test_year,
    pitch_type = "FF",
    k_similar = 20,
    n_simulations = 1000,
    pitch_transitions_data = pitch_transitions_full
  )
  
  # Visualize
  p1 <- visualize_pitch_development(sim_pitch, "release_speed")
  ggsave("~/Downloads/pitch_sim_velocity.png", p1, width = 12, height = 7)
  
  p2 <- visualize_pitch_development(sim_pitch, "release_spin_rate")
  ggsave("~/Downloads/pitch_sim_spin.png", p2, width = 12, height = 7)
  
  cat("\n✓ Pitch-level simulation complete!\n")
}

cat("\n========== COMPLETE ==========\n")
cat("\nYou now have:\n")
cat("  - Arsenal-level simulation (full pitcher development)\n")
cat("  - Pitch-level simulation (individual pitch development)\n")
cat("  - Both with full KDE reconstruction\n")
cat("  - Both with visualization functions\n")
cat("\nReady for Shiny app integration!\n")