library(RSQLite)
library(dplyr)
library(dbplyr)

# ========================================
# SETUP: Create Database Structure
# ========================================

create_kde_database <- function(db_path = "~/Downloads/pitcher_kde_distributions.db") {
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS pitcher_metadata (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      pitcher INTEGER,
      player_name TEXT,
      game_year INTEGER,
      pitch_type TEXT,
      p_throws TEXT,
      n_pitches INTEGER,
      UNIQUE(pitcher, game_year, pitch_type)
    )
  ")
  
  # Table 2: KDE Grid Values
  # This stores the actual density values on a grid
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS kde_grids (
      pitcher_id INTEGER,
      metric TEXT,
      grid_point INTEGER,
      grid_value REAL,
      density_value REAL,
      FOREIGN KEY (pitcher_id) REFERENCES pitcher_metadata(id),
      PRIMARY KEY (pitcher_id, metric, grid_point)
    )
  ")
  
  # Table 3: Distribution Summary Stats
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS distribution_stats (
      pitcher_id INTEGER,
      metric TEXT,
      mean REAL,
      sd REAL,
      min REAL,
      max REAL,
      q25 REAL,
      q50 REAL,
      q75 REAL,
      FOREIGN KEY (pitcher_id) REFERENCES pitcher_metadata(id),
      PRIMARY KEY (pitcher_id, metric)
    )
  ")
  
  # Create indices for fast queries
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_kde_pitcher ON kde_grids(pitcher_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_kde_metric ON kde_grids(metric)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_meta_pitcher ON pitcher_metadata(pitcher)")
  
  dbDisconnect(con)
  
  return(db_path)
}

#  Generate and Store KDEs


populate_kde_database <- function(pitch_data, db_path, grid_points = 50) {
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Define metrics to compute KDEs for
  metrics <- c(
    "release_speed", "release_spin_rate", "bauer_units", "spin_efficiency",
    "induced_vert_break", "horz_break", "total_movement",
    "vaa", "haa", "extension", "release_pos_z", "release_pos_x",
    "gyro_degree", "true_spin", "movement_angle", "perceived_velocity"
  )
  
  # Get unique pitcher-year-pitch combinations
  pitcher_combos <- pitch_data %>%
    filter(!is.na(pitch_type)) %>%
    group_by(pitcher, player_name, game_year, pitch_type, p_throws) %>%
    summarise(n_pitches = n(), .groups = 'drop')
  
  # Process each pitcher-year-pitch combination
  for (i in 1:nrow(pitcher_combos)) {
    
    if (i %% 50 == 0) {
      print(paste("Processing", i, "of", nrow(pitcher_combos)))
    }
    
    # Get this pitcher's data
    pitcher_data <- pitch_data %>%
      filter(
        pitcher == pitcher_combos$pitcher[i],
        game_year == pitcher_combos$game_year[i],
        pitch_type == pitcher_combos$pitch_type[i]
      )
    
    # Insert metadata
    dbExecute(con, "
      INSERT OR IGNORE INTO pitcher_metadata 
      (pitcher, player_name, game_year, pitch_type, p_throws, n_pitches)
      VALUES (?, ?, ?, ?, ?, ?)
    ", params = list(
      pitcher_combos$pitcher[i],
      pitcher_combos$player_name[i],
      pitcher_combos$game_year[i],
      pitcher_combos$pitch_type[i],
      pitcher_combos$p_throws[i],
      pitcher_combos$n_pitches[i]
    ))
    
    # Get the ID
    pitcher_id <- dbGetQuery(con, "
      SELECT id FROM pitcher_metadata 
      WHERE pitcher = ? AND game_year = ? AND pitch_type = ?
    ", params = list(
      pitcher_combos$pitcher[i],
      pitcher_combos$game_year[i],
      pitcher_combos$pitch_type[i]
    ))$id
    
    # For each metric, compute KDE and store
    for (metric in metrics) {
      
      # Get values
      values <- pitcher_data[[metric]]
      values <- values[!is.na(values) & !is.infinite(values)]
      
      if (length(values) < 10) next  # Skip if too few data points
      
      # Compute KDE
      kde <- density(values, n = grid_points, na.rm = TRUE)
      
      # Normalize density to sum to 1 (make it a probability distribution)
      density_normalized <- kde$y / sum(kde$y)
      
      # Store grid points and densities
      kde_df <- data.frame(
        pitcher_id = pitcher_id,
        metric = metric,
        grid_point = 1:grid_points,
        grid_value = kde$x,
        density_value = density_normalized
      )
      
      dbWriteTable(con, "kde_grids", kde_df, append = TRUE)
      
      # Store summary statistics
      stats_df <- data.frame(
        pitcher_id = pitcher_id,
        metric = metric,
        mean = mean(values, na.rm = TRUE),
        sd = sd(values, na.rm = TRUE),
        min = min(values, na.rm = TRUE),
        max = max(values, na.rm = TRUE),
        q25 = quantile(values, 0.25, na.rm = TRUE),
        q50 = quantile(values, 0.50, na.rm = TRUE),
        q75 = quantile(values, 0.75, na.rm = TRUE)
      )
      
      dbWriteTable(con, "distribution_stats", stats_df, append = TRUE)
    }
  }
  
  dbDisconnect(con)
  print("Database populated successfully!")
}


#  Retrieve KDEs for Distance Computation


get_pitcher_kde <- function(pitcher_id, metric, db_path) {
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  kde_data <- dbGetQuery(con, "
    SELECT grid_value, density_value
    FROM kde_grids
    WHERE pitcher_id = ? AND metric = ?
    ORDER BY grid_point
  ", params = list(pitcher_id, metric))
  
  dbDisconnect(con)
  
  return(kde_data)
}

# Get all pitcher IDs for a specific pitch type
get_pitcher_ids <- function(db_path, pitch_type = NULL, min_pitches = 100) {
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  if (is.null(pitch_type)) {
    query <- "SELECT id, pitcher, player_name, game_year, pitch_type 
              FROM pitcher_metadata 
              WHERE n_pitches >= ?"
    ids <- dbGetQuery(con, query, params = list(min_pitches))
  } else {
    query <- "SELECT id, pitcher, player_name, game_year, pitch_type 
              FROM pitcher_metadata 
              WHERE pitch_type = ? AND n_pitches >= ?"
    ids <- dbGetQuery(con, query, params = list(pitch_type, min_pitches))
  }
  
  dbDisconnect(con)
  
  return(ids)
}


# Distributional Distances


compute_kde_distance <- function(pitcher_id1, pitcher_id2, metric, 
                                 db_path, distance_type = "hellinger") {
  
  # Get KDE data for both pitchers
  kde1 <- get_pitcher_kde(pitcher_id1, metric, db_path)
  kde2 <- get_pitcher_kde(pitcher_id2, metric, db_path)
  
  # Handle missing data
  if (nrow(kde1) == 0 | nrow(kde2) == 0) {
    return(NA)
  }
  
  # Extract density values
  p <- kde1$density_value
  q <- kde2$density_value
  
  # Ensure same length 
  n <- min(length(p), length(q))
  p <- p[1:n]
  q <- q[1:n]
  
  # Compute distance based on type
  dist <- switch(distance_type,
                 "hellinger" = sqrt(sum((sqrt(p) - sqrt(q))^2)) / sqrt(2),
                 
                 "total_variation" = 0.5 * sum(abs(p - q)),
                 
                 "kl_divergence" = {
                   # Symmetrized KL (Jensen-Shannon divergence)
                   m <- (p + q) / 2
                   0.5 * sum(p * log((p + 1e-10) / (m + 1e-10))) + 
                     0.5 * sum(q * log((q + 1e-10) / (m + 1e-10)))
                 },
                 
                 "l2" = sqrt(sum((p - q)^2)),
                 
                 "wasserstein" = {
                   # Approximate Wasserstein using grid
                   x1 <- kde1$grid_value
                   x2 <- kde2$grid_value
                   # Simple approximation
                   sum(abs(cumsum(p) - cumsum(q))) * (x1[2] - x1[1])
                 },
                 
                 stop("Unknown distance type")
  )
  
  return(dist)
}

# Compute distance matrix for a set of pitchers
compute_distance_matrix <- function(pitcher_ids, metrics, db_path, 
                                    distance_type = "hellinger",
                                    metric_weights = NULL) {
  
  n <- nrow(pitcher_ids)
  
  # Default: equal weights
  if (is.null(metric_weights)) {
    metric_weights <- rep(1/length(metrics), length(metrics))
  }
  
  # Initialize distance matrix
  dist_matrix <- matrix(0, n, n)
  
  # Compute pairwise distances
  for (i in 1:(n-1)) {
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
    }
    
    if (i %% 50 == 0) {
      print(paste("Distance computation:", i, "of", n))
    }
  }
  
  # Add row/column names
  rownames(dist_matrix) <- paste(pitcher_ids$player_name, pitcher_ids$game_year, pitcher_ids$pitch_type)
  colnames(dist_matrix) <- paste(pitcher_ids$player_name, pitcher_ids$game_year, pitcher_ids$pitch_type)
  
  return(dist_matrix)
}



#  Create database
db_path <- create_kde_database("~/Downloads/pitcher_kde_distributions.db")

#  Populate with KDEs (Do not run again)
populate_kde_database(
  pitch_data = all_pitches,
  db_path = db_path,
  grid_points = 50
)

#  Get pitcher IDs for clustering 
pitcher_ids <- get_pitcher_ids(
  db_path = db_path,
  pitch_type = "FF", 
  min_pitches = 5
)

print(paste("Found", nrow(pitcher_ids), "pitchers"))

# Step 4: Compute distance matrix
metrics_to_use <- core_metrics

dist_matrix <- compute_distance_matrix(
  pitcher_ids = pitcher_ids,
  metrics = metrics_to_use,
  db_path = db_path,
  distance_type = "hellinger"
)

# Step 5: Cluster
hclust_result <- hclust(as.dist(dist_matrix), method = "ward.D2")

# Cut into clusters
clusters <- cutree(hclust_result, k = 8)

# Add clusters to pitcher info
pitcher_ids$cluster <- clusters

# View cluster assignments
pitcher_ids %>%
  select(player_name, game_year, cluster) %>%
  arrange(cluster, player_name)



plot_pitcher_kde_comparison <- function(pitcher_id1, pitcher_id2, metric, db_path) {
  
  library(ggplot2)
  

  con <- dbConnect(RSQLite::SQLite(), db_path)
  meta1 <- dbGetQuery(con, "SELECT * FROM pitcher_metadata WHERE id = ?", 
                      params = list(pitcher_id1))
  meta2 <- dbGetQuery(con, "SELECT * FROM pitcher_metadata WHERE id = ?", 
                      params = list(pitcher_id2))
  dbDisconnect(con)
  
  # Get KDEs
  kde1 <- get_pitcher_kde(pitcher_id1, metric, db_path)
  kde2 <- get_pitcher_kde(pitcher_id2, metric, db_path)
  
  # Combine for plotting
  plot_data <- rbind(
    data.frame(kde1, pitcher = paste(meta1$player_name, meta1$game_year)),
    data.frame(kde2, pitcher = paste(meta2$player_name, meta2$game_year))
  )
  
  # Plot
  ggplot(plot_data, aes(x = grid_value, y = density_value, color = pitcher)) +
    geom_line(size = 1.2) +
    labs(
      title = paste("KDE Comparison:", metric),
      x = metric,
      y = "Density",
      color = "Pitcher"
    ) +
    theme_minimal()
}

plot_pitcher_kde_comparison(1, 2, "release_speed", db_path)



dbDisconnect(con)