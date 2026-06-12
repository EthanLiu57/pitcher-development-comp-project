# ==========================================
# COMPLETE PITCH SIMULATION PIPELINE
# ==========================================

library(RSQLite)
library(dplyr)

cat("========== PITCH-LEVEL SIMULATION SYSTEM ==========\n\n")

# ==========================================
# FUNCTION: SIMULATE PITCH DEVELOPMENT
# ==========================================

simulate_pitch_development <- function(current_pitcher_id,
                                       current_year,
                                       pitch_type,
                                       years_ahead = 1,
                                       k_similar = 20,
                                       n_simulations = 500,
                                       pitch_transitions_data) {
  
  if (!(pitch_type %in% names(pitch_transitions_data))) {
    stop(sprintf("Pitch type %s not found", pitch_type))
  }
  
  pt_data <- pitch_transitions_data[[pitch_type]]
  dist_matrix <- pt_data$dist_matrix
  pitcher_ids <- pt_data$pitcher_ids
  transitions <- pt_data$transitions
  
  # Find current pitcher
  current_idx <- which(pitcher_ids$pitcher == current_pitcher_id & 
                         pitcher_ids$game_year == current_year)
  
  if (length(current_idx) == 0) {
    stop(sprintf("Pitcher %s (%s) not found for %s", 
                 current_pitcher_id, current_year, pitch_type))
  }
  
  current_idx <- current_idx[1]
  
  # Get current KDEs from database
  con <- dbConnect(RSQLite::SQLite(), kde_db)
  
  current_db_id <- dbGetQuery(con, "
    SELECT id FROM pitcher_metadata 
    WHERE pitcher = ? AND game_year = ? AND pitch_type = ?
  ", params = list(current_pitcher_id, current_year, pitch_type))$id
  
  if (length(current_db_id) == 0) {
    dbDisconnect(con)
    stop("Current pitcher KDEs not found")
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
  
  # Find similar pitchers
  distances <- dist_matrix[current_idx, ]
  distances <- distances[-current_idx]
  
  sorted_idx <- order(distances)
  top_k <- head(sorted_idx, k_similar)
  
  similar_distances <- distances[top_k]
  
  weights <- exp(-similar_distances^2 / (2 * median(similar_distances)^2))
  weights <- weights / sum(weights)
  
  # Find transitions
  available_transitions <- list()
  
  for (trans in transitions) {
    if (trans$idx_from %in% top_k) {
      trans$weight <- weights[which(top_k == trans$idx_from)]
      available_transitions[[length(available_transitions) + 1]] <- trans
    }
  }
  
  if (length(available_transitions) == 0) {
    stop("No development data available")
  }
  
  # Run simulations
  simulated_pitches <- list()
  transition_weights <- sapply(available_transitions, function(x) x$weight)
  
  for (sim in 1:n_simulations) {
    
    sampled_idx <- sample(1:length(available_transitions),
                          size = 1,
                          prob = transition_weights)
    
    sampled_transition <- available_transitions[[sampled_idx]]
    
    simulated_pitch <- list()
    
    for (metric in optimized_metrics) {
      
      current_kde <- current_kdes[[metric]]
      transition_kde <- sampled_transition$kde_transitions[[metric]]
      
      if (!is.null(current_kde) && !is.null(transition_kde)) {
        
        mean_from <- sum(transition_kde$grid_from * transition_kde$density_from)
        mean_to <- sum(transition_kde$grid_to * transition_kde$density_to)
        delta_mean <- mean_to - mean_from
        
        sd_from <- sqrt(sum(transition_kde$density_from * 
                              (transition_kde$grid_from - mean_from)^2))
        sd_to <- sqrt(sum(transition_kde$density_to * 
                            (transition_kde$grid_to - mean_to)^2))
        delta_sd <- sd_to - sd_from
        
        current_mean <- sum(current_kde$grid * current_kde$density)
        current_sd <- sqrt(sum(current_kde$density * 
                                 (current_kde$grid - current_mean)^2))
        
        simulated_mean <- current_mean + delta_mean
        simulated_sd <- max(current_sd + delta_sd, 0.01)
        
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
  }
  
  return(list(
    pitcher_id = current_pitcher_id,
    player_name = pitcher_ids$player_name[current_idx],
    year = current_year,
    pitch_type = pitch_type,
    similar_pitchers = data.frame(
      player_name = pitcher_ids$player_name[top_k],
      year = pitcher_ids$game_year[top_k],
      pitcher_id = pitcher_ids$pitcher[top_k],
      distance = similar_distances,
      weight = weights
    ),
    n_simulations = n_simulations,
    simulated_pitches = simulated_pitches,
    current_kdes = current_kdes
  ))
}

# ==========================================
# FUNCTION: STORE PITCH SIMULATION
# ==========================================

store_pitch_simulation <- function(sim_result, sim_con, pitch_type) {
  
  pitcher_id <- sim_result$pitcher_id
  year <- sim_result$year
  player_name <- sim_result$player_name
  
  # Store similar pitchers
  for (j in 1:nrow(sim_result$similar_pitchers)) {
    
    dbExecute(sim_con, "
      INSERT OR REPLACE INTO pitch_similar_pitchers 
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      pitcher_id, year, pitch_type,
      sim_result$similar_pitchers$pitcher_id[j],
      sim_result$similar_pitchers$player_name[j],
      sim_result$similar_pitchers$year[j],
      sim_result$similar_pitchers$distance[j],
      sim_result$similar_pitchers$weight[j],
      j
    ))
  }
  
  # Store distributions
  for (metric in names(sim_result$current_kdes)) {
    
    current_kde <- sim_result$current_kdes[[metric]]
    
    if (is.null(current_kde) || length(current_kde$grid) == 0) next
    
    # Extract simulated distributions
    sim_data <- data.frame()
    
    for (k in 1:length(sim_result$simulated_pitches)) {
      
      sim_pitch <- sim_result$simulated_pitches[[k]]
      sim_kde <- sim_pitch[[metric]]
      
      if (!is.null(sim_kde)) {
        sim_data <- rbind(sim_data, data.frame(
          grid = sim_kde$grid,
          density = sim_kde$density
        ))
      }
    }
    
    if (nrow(sim_data) == 0) next
    
    # Calculate percentiles
    percentiles <- sim_data %>%
      group_by(grid) %>%
      summarise(
        p05 = quantile(density, 0.05, na.rm = TRUE),
        p25 = quantile(density, 0.25, na.rm = TRUE),
        p50 = quantile(density, 0.50, na.rm = TRUE),
        p75 = quantile(density, 0.75, na.rm = TRUE),
        p95 = quantile(density, 0.95, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Store
    for (k in 1:nrow(percentiles)) {
      
      grid_val <- percentiles$grid[k]
      current_dens <- current_kde$density[which.min(abs(current_kde$grid - grid_val))]
      
      dbExecute(sim_con, "
        INSERT OR REPLACE INTO pitch_simulations 
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(
        pitcher_id, player_name, year, pitch_type, metric,
        grid_val, current_dens,
        percentiles$p05[k], percentiles$p25[k], percentiles$p50[k],
        percentiles$p75[k], percentiles$p95[k],
        sim_result$n_simulations,
        nrow(sim_result$similar_pitchers)
      ))
    }
    
    # Summary stats
    current_mean <- sum(current_kde$grid * current_kde$density)
    current_sd <- sqrt(sum(current_kde$density * (current_kde$grid - current_mean)^2))
    
    sim_means <- sapply(sim_result$simulated_pitches, function(p) {
      if (!is.null(p[[metric]])) p[[metric]]$mean else NA
    })
    
    sim_sds <- sapply(sim_result$simulated_pitches, function(p) {
      if (!is.null(p[[metric]])) p[[metric]]$sd else NA
    })
    
    sim_mean <- mean(sim_means, na.rm = TRUE)
    sim_sd_mean <- mean(sim_sds, na.rm = TRUE)
    
    dbExecute(sim_con, "
      INSERT OR REPLACE INTO simulation_summary 
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      pitcher_id, player_name, year, pitch_type, metric,
      current_mean, current_sd,
      sim_mean, sim_sd_mean,
      sim_mean - current_mean,
      sim_sd_mean - current_sd
    ))
  }
}

# ==========================================
# RUN PITCH SIMULATIONS
# ==========================================

cat("Running pitch-level simulations...\n\n")

sim_con <- dbConnect(RSQLite::SQLite(), "~/Downloads/simulation_results.db")

total_combinations <- 0

for (pt in names(pitch_transitions_full)) {
  pt_data <- pitch_transitions_full[[pt]]
  
  pitcher_years <- unique(data.frame(
    pitcher_id = sapply(pt_data$transitions, function(x) x$pitcher_id),
    year_from = sapply(pt_data$transitions, function(x) x$year_from)
  ))
  
  total_combinations <- total_combinations + nrow(pitcher_years)
}

cat(sprintf("Total combinations: %d\n", total_combinations))
cat(sprintf("Estimated time: %.0f minutes\n\n", total_combinations / 30))

start_time <- Sys.time()
processed <- 0
errors <- 0

for (pitch_type in names(pitch_transitions_full)) {
  
  cat(sprintf("\n--- %s ---\n", pitch_type))
  
  pt_data <- pitch_transitions_full[[pitch_type]]
  
  pitcher_years <- unique(data.frame(
    pitcher_id = sapply(pt_data$transitions, function(x) x$pitcher_id),
    year_from = sapply(pt_data$transitions, function(x) x$year_from)
  ))
  
  for (i in 1:nrow(pitcher_years)) {
    
    pitcher_id <- as.character(pitcher_years$pitcher_id[i])
    year <- as.character(pitcher_years$year_from[i])
    
    tryCatch({
      
      sim_result <- simulate_pitch_development(
        current_pitcher_id = pitcher_id,
        current_year = year,
        pitch_type = pitch_type,
        years_ahead = 1,
        k_similar = 20,
        n_simulations = 500,
        pitch_transitions_data = pitch_transitions_full
      )
      
      store_pitch_simulation(sim_result, sim_con, pitch_type)
      
    }, error = function(e) {
      errors <<- errors + 1
    })
    
    processed <- processed + 1
    
    if (processed %% 10 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      rate <- processed / elapsed
      remaining <- (total_combinations - processed) / rate
      eta <- Sys.time() + remaining
      
      cat(sprintf("\r  Progress: %d/%d (%.1f%%) | Errors: %d | ETA: %s     ",
                  processed, total_combinations,
                  100 * processed / total_combinations,
                  errors,
                  format(eta, "%H:%M:%S")))
      flush.console()
    }
  }
}

elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))

cat(sprintf("\n\n✓ Complete in %.1f minutes\n", elapsed))
cat(sprintf("Errors: %d\n", errors))

dbDisconnect(sim_con)

# Verify
sim_con <- dbConnect(RSQLite::SQLite(), "~/Downloads/simulation_results.db")

pitch_count <- dbGetQuery(sim_con, "SELECT COUNT(*) as n FROM pitch_simulations")$n

cat(sprintf("\nPitch simulation rows in database: %s\n", format(pitch_count, big.mark = ",")))

dbDisconnect(sim_con)
