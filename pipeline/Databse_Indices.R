
# ADD DATABASE INDICES


add_database_indices <- function(db_path) {
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  cat("Adding database indices...\n")
  
  # Index on pitcher_id + metric (most common query pattern)
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_kde_pitcher_metric 
                  ON kde_grids(pitcher_id, metric)")
  
  # Composite index for the exact query pattern we use
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_kde_lookup 
                  ON kde_grids(pitcher_id, metric, grid_point)")
  
  # Index on pitcher_metadata
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_meta_id 
                  ON pitcher_metadata(id)")
  
  dbDisconnect(con)
  
  cat(" Indices added\n")
}

add_database_indices(kde_db)