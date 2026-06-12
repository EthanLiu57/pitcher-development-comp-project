library(DBI)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "~/Downloads/baseball_savant_2025.db")

# See what tables exist
dbListTables(con)

# See pitcher table
dbGetQuery(con, "SELECT * FROM pitchers")

# See first 10 pitches
dbGetQuery(con, "SELECT * FROM pitches LIMIT 10")

pitchers2025 <- dbGetQuery(con, "SELECT * FROM pitchers")
pitches2025 <- dbGetQuery(con, "SELECT * FROM pitches")

write.csv(pitches2025,  "/Users/liu13/Downloads/pitches2025.csv")

dbDisconnect(con)
