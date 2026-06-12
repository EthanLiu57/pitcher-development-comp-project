create_simple_db <- function(db_path = "~/Downloads/pitcher_features.db") {
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Single table with all your features
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS pitcher_features (
      pitcher INTEGER,
      player_name TEXT,
      game_year INTEGER,
      p_throws TEXT,
      age_pit INTEGER,
      PRIMARY KEY (pitcher, game_year)
    )
  ")
  
  # Create index
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_pitcher ON pitcher_features(pitcher, game_year)")
  
  dbDisconnect(con)
  
  cat("✓ Database created\n")
  return(db_path)
}

# Create database
simple_db <- create_simple_db("~/Downloads/pitcher_features.db")

# Load pitcher_wide into database
con <- dbConnect(RSQLite::SQLite(), simple_db)

dbWriteTable(con, "pitcher_features", pitcher_wide, overwrite = TRUE)


n_rows <- dbGetQuery(con, "SELECT COUNT(*) as n FROM pitcher_features")$n
n_cols <- length(dbListFields(con, "pitcher_features"))

cat(sprintf("✓ Loaded %s pitchers with %d features\n", format(n_rows, big.mark = ","), n_cols))

# Show sample
cat("\nSample data:\n")
sample <- dbGetQuery(con, "SELECT * FROM pitcher_features LIMIT 3")
print(names(sample)[1:20])  # First 20 column names

dbDisconnect(con)
# ==========================================
# FIXED: CLUSTER WITH YOUR COLUMN NAMING
# ==========================================

cluster_from_db <- function(db_path, min_total_pitches = 0, verbose = TRUE) {
  
  if (verbose) cat("\n Clustering from Databas\n")
  
  start_time_total <- Sys.time()
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Load data
  if (verbose) cat("Loading features from database...\n")
  pitcher_features <- dbGetQuery(con, "SELECT * FROM pitcher_features")
  dbDisconnect(con)
  
  if (verbose) cat(sprintf("✓ Loaded %d pitchers (%.1fs)\n", 
                           nrow(pitcher_features),
                           as.numeric(difftime(Sys.time(), start_time_total, units = "secs"))))
  
  # Filter by total pitches if needed
  if (min_total_pitches > 0) {
    n_pitches_cols <- grep("^n_pitches_", colnames(pitcher_features), value = TRUE)
    total_pitches <- rowSums(pitcher_features[, n_pitches_cols], na.rm = TRUE)
    pitcher_features <- pitcher_features[total_pitches >= min_total_pitches, ]
    if (verbose) cat(sprintf("✓ Filtered to %d pitchers with %d+ total pitches\n", 
                             nrow(pitcher_features), min_total_pitches))
  }
  
  # Separate IDs
  id_cols <- c("pitcher", "player_name", "game_year", "p_throws")
  if ("age_pit" %in% colnames(pitcher_features)) {
    id_cols <- c(id_cols, "age_pit")
  }
  
  pitcher_ids <- pitcher_features %>%
    select(all_of(id_cols))
  
  feature_cols_method1 <- grep("_js_", colnames(pitcher_features), value = TRUE)
  
  feature_cols_method2 <- grep("_js$", colnames(pitcher_features), value = TRUE)
  
  # Use whichever found columns
  if (length(feature_cols_method1) > 0) {
    feature_cols <- feature_cols_method1
  } else if (length(feature_cols_method2) > 0) {
    feature_cols <- feature_cols_method2
  } else {
    # Fallback: exclude known ID and n_pitches columns
    exclude_patterns <- c("^pitcher$", "^player_name$", "^game_year$", "^p_throws$", 
                          "^age_pit$", "^n_pitches_")
    feature_cols <- colnames(pitcher_features)
    for (pattern in exclude_patterns) {
      feature_cols <- feature_cols[!grepl(pattern, feature_cols)]
    }
  }
  
  if (verbose) cat(sprintf("✓ Found %d feature columns\n", length(feature_cols)))
  
  if (length(feature_cols) == 0) {
    cat("\n⚠ ERROR: No feature columns found!\n")
    cat("Column names sample:\n")
    print(head(colnames(pitcher_features), 20))
    stop("Cannot cluster without features")
  }
  
  # Extract features as matrix
  pitcher_matrix <- pitcher_features %>%
    select(all_of(feature_cols)) %>%
    as.matrix()
  
  if (verbose) cat(sprintf("✓ Using %d features\n", ncol(pitcher_matrix)))
  
  # Only remove columns that are ALL NA
  all_na <- apply(pitcher_matrix, 2, function(x) all(is.na(x)))
  
  if (any(all_na)) {
    if (verbose) cat(sprintf("  Removing %d all-NA columns\n", sum(all_na)))
    pitcher_matrix <- pitcher_matrix[, !all_na]
  }
  
  # Replace any remaining NAs with 0
  pitcher_matrix[is.na(pitcher_matrix)] <- 0
  
  # Scale
  if (verbose) cat("Scaling features...\n")
  start_scale <- Sys.time()
  pitcher_scaled <- scale(pitcher_matrix)
  if (verbose) cat(sprintf("✓ Scaled (%.1fs)\n", 
                           as.numeric(difftime(Sys.time(), start_scale, units = "secs"))))
  
  # Only remove zero-variance AFTER scaling
  zero_var <- apply(pitcher_scaled, 2, var, na.rm = TRUE) == 0
  zero_var[is.na(zero_var)] <- TRUE
  
  if (any(zero_var)) {
    if (verbose) cat(sprintf("  Removing %d zero-variance columns\n", sum(zero_var)))
    pitcher_scaled <- pitcher_scaled[, !zero_var]
  }
  
  if (verbose) cat(sprintf("✓ Final: %d features across %d pitchers\n", 
                           ncol(pitcher_scaled), nrow(pitcher_scaled)))
  
  # Distance computation
  n <- nrow(pitcher_scaled)
  total_comparisons <- (n * (n - 1)) / 2
  
  if (verbose) {
    cat(sprintf("\nComputing distance matrix...\n"))
    cat(sprintf("  Pitchers: %s\n", format(n, big.mark = ",")))
    cat(sprintf("  Pairwise comparisons: %s\n", format(total_comparisons, big.mark = ",")))
    cat(sprintf("  Estimated time: ~%.0f seconds\n", n^2 / 100000))
  }
  
  start_dist <- Sys.time()
  dist_matrix <- dist(pitcher_scaled, method = "euclidean")
  elapsed_dist <- as.numeric(difftime(Sys.time(), start_dist, units = "secs"))
  
  if (verbose) cat(sprintf("✓ Distance matrix computed (%.1fs)\n", elapsed_dist))
  
  # Clustering
  if (verbose) cat("Running hierarchical clustering (Ward's method)...\n")
  start_clust <- Sys.time()
  hclust_result <- hclust(dist_matrix, method = "ward.D2")
  elapsed_clust <- as.numeric(difftime(Sys.time(), start_clust, units = "secs"))
  
  if (verbose) cat(sprintf("✓ Clustering complete (%.1fs)\n", elapsed_clust))
  
  total_time <- as.numeric(difftime(Sys.time(), start_time_total, units = "secs"))
  if (verbose) {
    cat(sprintf("\n========== COMPLETE ==========\n"))
    cat(sprintf("Total time: %.1f seconds (%.1f minutes)\n\n", total_time, total_time/60))
  }
  
  return(list(
    hclust = hclust_result,
    ids = pitcher_ids,
    features = pitcher_scaled,
    dist = dist_matrix
  ))
}


results_step1 <- cluster_from_db(simple_db, min_total_pitches = 100, verbose = TRUE)

library(NbClust)

nb <- NbClust(results_step1$features,
              distance = "euclidean",
              min.nc = 2,
              max.nc = 30,
              method = "ward.D2",
              index = "silhouette")

nb$Best.nc

# Cut into clusters
optimal_k <- 10 
clusters <- cutree(results_step1$hclust, k = optimal_k)
results_step1$ids$cluster <- clusters

# View results
cat("\nCluster sizes:\n")
print(table(clusters))


library(dbscan)

# Find good epsilon
kNNdistplot(results_step1$features, k = 5)
abline(h = 30, col = "red")  # Adjust based on plot

# Run DBSCAN
dbscan_result <- dbscan(results_step1$features, eps = 30, minPts = 10)

cat(sprintf("DBSCAN found %d clusters\n", max(dbscan_result$cluster)))
cat(sprintf("Noise points: %d\n", sum(dbscan_result$cluster == 0)))

table(dbscan_result$cluster)

