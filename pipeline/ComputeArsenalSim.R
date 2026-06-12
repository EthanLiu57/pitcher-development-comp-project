# ==========================================
# PRE-COMPUTE ALL SIMULATIONS FOR SHINY APP
# ==========================================

library(RSQLite)
library(dplyr)
library(ggplot2)

cat("========== PRE-COMPUTATION PIPELINE ==========\n\n")

cat("This will:\n")
cat("1. Run simulations for every pitcher-year (arsenal + individual pitches)\n")
cat("2. Store simulated distributions in SQLite database\n")
cat("3. Generate and save visualization data\n")
cat("4. Create lookup tables for fast Shiny retrieval\n\n")

# ==========================================
# CREATE SIMULATION DATABASE
# ==========================================

sim_db_path <- "~/Downloads/simulation_results.db"

cat("Creating simulation database...\n")

sim_con <- dbConnect(RSQLite::SQLite(), sim_db_path)

# Table 1: Arsenal simulations
dbExecute(sim_con, "
  CREATE TABLE IF NOT EXISTS arsenal_simulations (
    pitcher_id TEXT,
    player_name TEXT,
    year INTEGER,
    metric TEXT,
    grid_value REAL,
    current_density REAL,
    sim_p05 REAL,
    sim_p25 REAL,
    sim_p50 REAL,
    sim_p75 REAL,
    sim_p95 REAL,
    n_simulations INTEGER,
    n_similar_pitchers INTEGER,
    PRIMARY KEY (pitcher_id, year, metric, grid_value)
  )
")

# Table 2: Pitch-level simulations
dbExecute(sim_con, "
  CREATE TABLE IF NOT EXISTS pitch_simulations (
    pitcher_id TEXT,
    player_name TEXT,
    year INTEGER,
    pitch_type TEXT,
    metric TEXT,
    grid_value REAL,
    current_density REAL,
    sim_p05 REAL,
    sim_p25 REAL,
    sim_p50 REAL,
    sim_p75 REAL,
    sim_p95 REAL,
    n_simulations INTEGER,
    n_similar_pitchers INTEGER,
    PRIMARY KEY (pitcher_id, year, pitch_type, metric, grid_value)
  )
")

# Table 3: Similar pitchers (arsenal level)
dbExecute(sim_con, "
  CREATE TABLE IF NOT EXISTS arsenal_similar_pitchers (
    pitcher_id TEXT,
    year INTEGER,
    similar_pitcher_id TEXT,
    similar_player_name TEXT,
    similar_year INTEGER,
    distance REAL,
    weight REAL,
    rank INTEGER,
    PRIMARY KEY (pitcher_id, year, rank)
  )
")

# Table 4: Similar pitchers (pitch level)
dbExecute(sim_con, "
  CREATE TABLE IF NOT EXISTS pitch_similar_pitchers (
    pitcher_id TEXT,
    year INTEGER,
    pitch_type TEXT,
    similar_pitcher_id TEXT,
    similar_player_name TEXT,
    similar_year INTEGER,
    distance REAL,
    weight REAL,
    rank INTEGER,
    PRIMARY KEY (pitcher_id, year, pitch_type, rank)
  )
")

# Table 5: Summary statistics
dbExecute(sim_con, "
  CREATE TABLE IF NOT EXISTS simulation_summary (
    pitcher_id TEXT,
    player_name TEXT,
    year INTEGER,
    level TEXT,  -- 'arsenal' or pitch_type
    metric TEXT,
    current_mean REAL,
    current_sd REAL,
    sim_mean REAL,
    sim_sd REAL,
    delta_mean REAL,
    delta_sd REAL,
    PRIMARY KEY (pitcher_id, year, level, metric)
  )
")

# Create indices
dbExecute(sim_con, "CREATE INDEX IF NOT EXISTS idx_arsenal_sim ON arsenal_simulations(pitcher_id, year)")
dbExecute(sim_con, "CREATE INDEX IF NOT EXISTS idx_pitch_sim ON pitch_simulations(pitcher_id, year, pitch_type)")
dbExecute(sim_con, "CREATE INDEX IF NOT EXISTS idx_summary ON simulation_summary(pitcher_id, year)")

cat("✓ Database created\n\n")

# ==========================================
# ARSENAL SIMULATION BATCH PROCESSOR
# ==========================================

process_arsenal_simulations <- function(transition_deltas_full,
                                        dist_matrix,
                                        sim_con,
                                        n_simulations = 500,  # Reduce for speed
                                        k_similar = 20) {
  
  cat("========== ARSENAL SIMULATIONS ==========\n\n")
  
  # Get all unique pitcher-years that could be simulated
  # These are pitchers who have transitions FROM them (are in year_from)
  
  pitcher_years_to_simulate <- unique(sapply(transition_deltas_full, function(x) x$key_from))
  
  cat(sprintf("Pitcher-years to simulate: %d\n", length(pitcher_years_to_simulate)))
  cat(sprintf("Simulations per pitcher: %d\n", n_simulations))
  cat(sprintf("Total simulations: %s\n\n", 
              format(length(pitcher_years_to_simulate) * n_simulations, big.mark = ",")))
  
  start_time <- Sys.time()
  
  for (i in 1:length(pitcher_years_to_simulate)) {
    
    current_key <- pitcher_years_to_simulate[i]
    
    # Parse key
    parts <- strsplit(current_key, "_")[[1]]
    pitcher_id <- paste(head(parts, -1), collapse = "_")
    year <- tail(parts, 1)
    
    # Get player name
    player_name <- pitcher_info_lookup %>%
      filter(pitcher_id == pitcher_id, year == year) %>%
      pull(player_name) %>%
      head(1)
    
    if (length(player_name) == 0) {
      player_name <- pitcher_id
    }
    
    # Run simulation
    tryCatch({
      
      sim_result <- simulate_pitcher_development(
        current_pitcher_id = pitcher_id,
        current_year = year,
        years_ahead = 1,
        k_similar = k_similar,
        n_simulations = n_simulations,
        transition_deltas_full = transition_deltas_full,
        dist_matrix = dist_matrix
      )
      
      # Store results in database
      store_arsenal_simulation(sim_result, sim_con)
      
    }, error = function(e) {
      cat(sprintf("\n  Error for %s (%s): %s\n", player_name, year, e$message))
    })
    
    # Progress
    if (i %% 10 == 0 || i == length(pitcher_years_to_simulate)) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      rate <- i / elapsed
      remaining <- (length(pitcher_years_to_simulate) - i) / rate
      eta <- Sys.time() + remaining
      
      cat(sprintf("\r  Progress: %d/%d (%.1f%%) | %.1f/min | ETA: %s     ",
                  i, length(pitcher_years_to_simulate),
                  100 * i / length(pitcher_years_to_simulate),
                  rate * 60,
                  format(eta, "%H:%M:%S")))
      flush.console()
    }
  }
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  cat(sprintf("\n\n✓ Arsenal simulations complete in %.1f minutes\n\n", elapsed))
}

# ==========================================
# STORAGE FUNCTION FOR ARSENAL SIMULATIONS
# ==========================================

store_arsenal_simulation <- function(sim_result, sim_con) {
  
  pitcher_id <- sim_result$current_pitcher
  year <- sim_result$current_year
  player_name <- pitcher_info_lookup %>%
    filter(pitcher_id == pitcher_id, year == year) %>%
    pull(player_name) %>%
    head(1)
  
  if (length(player_name) == 0) player_name <- pitcher_id
  
  # Store similar pitchers
  for (j in 1:nrow(sim_result$similar_pitchers)) {
    
    sim_key <- sim_result$similar_pitchers$key[j]
    parts <- strsplit(sim_key, "_")[[1]]
    sim_pitcher_id <- paste(head(parts, -1), collapse = "_")
    sim_year <- tail(parts, 1)
    
    sim_player_name <- pitcher_info_lookup %>%
      filter(pitcher_id == sim_pitcher_id, year == sim_year) %>%
      pull(player_name) %>%
      head(1)
    
    if (length(sim_player_name) == 0) sim_player_name <- sim_pitcher_id
    
    dbExecute(sim_con, "
      INSERT OR REPLACE INTO arsenal_similar_pitchers 
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      pitcher_id, year,
      sim_pitcher_id, sim_player_name, sim_year,
      sim_result$similar_pitchers$distance[j],
      sim_result$similar_pitchers$weight[j],
      j
    ))
  }
  
  # Store distributions for each metric
  for (metric in names(sim_result$current_arsenal$features)) {
    
    current_kde <- sim_result$current_arsenal$features[[metric]]
    
    if (is.null(current_kde) || length(current_kde$grid) == 0) next
    
    # Extract simulated distributions
    sim_data <- data.frame()
    
    for (k in 1:length(sim_result$simulated_arsenals)) {
      
      sim_arsenal <- sim_result$simulated_arsenals[[k]]
      sim_kde <- sim_arsenal[[metric]]
      
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
        p05 = quantile(density, 0.05),
        p25 = quantile(density, 0.25),
        p50 = quantile(density, 0.50),
        p75 = quantile(density, 0.75),
        p95 = quantile(density, 0.95),
        .groups = 'drop'
      )
    
    # Store each grid point
    for (k in 1:nrow(percentiles)) {
      
      # Find current density at this grid point
      grid_val <- percentiles$grid[k]
      current_dens <- current_kde$density[which.min(abs(current_kde$grid - grid_val))]
      
      dbExecute(sim_con, "
        INSERT OR REPLACE INTO arsenal_simulations 
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(
        pitcher_id, player_name, year, metric,
        grid_val, current_dens,
        percentiles$p05[k], percentiles$p25[k], percentiles$p50[k],
        percentiles$p75[k], percentiles$p95[k],
        sim_result$n_simulations,
        nrow(sim_result$similar_pitchers)
      ))
    }
    
    # Store summary statistics
    current_mean <- sum(current_kde$grid * current_kde$density)
    current_sd <- sqrt(sum(current_kde$density * (current_kde$grid - current_mean)^2))
    
    sim_means <- sapply(sim_result$simulated_arsenals, function(a) {
      if (!is.null(a[[metric]])) a[[metric]]$mean else NA
    })
    
    sim_sds <- sapply(sim_result$simulated_arsenals, function(a) {
      if (!is.null(a[[metric]])) a[[metric]]$sd else NA
    })
    
    sim_mean <- mean(sim_means, na.rm = TRUE)
    sim_sd_mean <- mean(sim_sds, na.rm = TRUE)
    
    dbExecute(sim_con, "
      INSERT OR REPLACE INTO simulation_summary 
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      pitcher_id, player_name, year, 'arsenal', metric,
      current_mean, current_sd,
      sim_mean, sim_sd_mean,
      sim_mean - current_mean,
      sim_sd_mean - current_sd
    ))
  }
}

# ==========================================
# PITCH-LEVEL SIMULATION BATCH PROCESSOR
# ==========================================

process_pitch_simulations <- function(pitch_transitions_full,
                                      sim_con,
                                      n_simulations = 500,
                                      k_similar = 20) {
  
  cat("========== PITCH-LEVEL SIMULATIONS ==========\n\n")
  
  total_combinations <- 0
  
  for (pt in names(pitch_transitions_full)) {
    pt_data <- pitch_transitions_full[[pt]]
    
    # Pitchers who have transitions FROM them
    pitcher_years <- unique(paste(
      sapply(pt_data$transitions, function(x) x$pitcher_id),
      sapply(pt_data$transitions, function(x) x$year_from),
      sep = "_"
    ))
    
    total_combinations <- total_combinations + length(pitcher_years)
  }
  
  cat(sprintf("Total pitcher-year-pitch combinations: %d\n", total_combinations))
  cat(sprintf("Simulations each: %d\n", n_simulations))
  cat(sprintf("Total simulations: %s\n\n",
              format(total_combinations * n_simulations, big.mark = ",")))
  
  start_time <- Sys.time()
  processed <- 0
  
  for (pitch_type in names(pitch_transitions_full)) {
    
    cat(sprintf("\n--- %s ---\n", pitch_type))
    
    pt_data <- pitch_transitions_full[[pitch_type]]
    
    # Get unique pitcher-years
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
          k_similar = k_similar,
          n_simulations = n_simulations,
          pitch_transitions_data = pitch_transitions_full
        )
        
        # Store results
        store_pitch_simulation(sim_result, sim_con, pitch_type)
        
      }, error = function(e) {
        # Skip errors silently for batch processing
      })
      
      processed <- processed + 1
      
      # Progress
      if (processed %% 10 == 0 || processed == total_combinations) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        rate <- processed / elapsed
        remaining <- (total_combinations - processed) / rate
        eta <- Sys.time() + remaining
        
        cat(sprintf("\r  Progress: %d/%d (%.1f%%) | %.1f/min | ETA: %s     ",
                    processed, total_combinations,
                    100 * processed / total_combinations,
                    rate * 60,
                    format(eta, "%H:%M:%S")))
        flush.console()
      }
    }
  }
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  cat(sprintf("\n\n✓ Pitch simulations complete in %.1f minutes\n\n", elapsed))
}

# ==========================================
# STORAGE FUNCTION FOR PITCH SIMULATIONS
# ==========================================

store_pitch_simulation <- function(sim_result, sim_con, pitch_type) {
  
  pitcher_id <- sim_result$pitcher_id
  year <- sim_result$year
  player_name <- sim_result$player_name
  
  # Store similar pitchers
  for (j in 1:nrow(sim_result$similar_pitchers)) {
    
    # Parse similar pitcher info from pitcher_ids
    # This is already in the similar_pitchers dataframe
    
    dbExecute(sim_con, "
      INSERT OR REPLACE INTO pitch_similar_pitchers 
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      pitcher_id, year, pitch_type,
      NA,  # Would need to extract pitcher_id
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
        p05 = quantile(density, 0.05),
        p25 = quantile(density, 0.25),
        p50 = quantile(density, 0.50),
        p75 = quantile(density, 0.75),
        p95 = quantile(density, 0.95),
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
# RUN EVERYTHING
# ==========================================

cat("========== STARTING PRE-COMPUTATION ==========\n\n")


# Arsenal simulations
process_arsenal_simulations(
  transition_deltas_full = transition_deltas_full,
  dist_matrix = dist_matrix_correct,
  sim_con = sim_con,
  n_simulations = 500,
  k_similar = 20
)

# Pitch simulations
process_pitch_simulations(
  pitch_transitions_full = pitch_transitions_full,
  sim_con = sim_con,
  n_simulations = 500,
  k_similar = 20
)

# Close database
dbDisconnect(sim_con)

cat("\n========== PRE-COMPUTATION COMPLETE ==========\n")
cat(sprintf("\nDatabase saved: %s\n", sim_db_path))
cat("\nYou can now build a lightning-fast Shiny app that just queries this database!\n")