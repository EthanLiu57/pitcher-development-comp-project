library(RSQLite)
library(dplyr)
library(dbplyr)

# SETUP: Create Database Structure

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


