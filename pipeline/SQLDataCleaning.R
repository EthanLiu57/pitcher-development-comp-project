library(DBI)
library(RSQLite)
#Do for each year
con <- dbConnect(RSQLite::SQLite(), "~/Downloads/baseball_savant_2025.db")

columns <- dbGetQuery(con, "PRAGMA table_info(pitches)")
print(columns$name)


dbExecute(con, "
  CREATE TABLE pitches_clean AS
  SELECT 
    pitcher,
    player_name,
    game_year,
    pitch_type,
    zone,
    pitch_name,
    release_speed,
    release_pos_x,
    release_pos_z,
    pfx_x,
    pfx_z,
    plate_x,
    plate_z,
    vx0,
    vy0,
    vz0,
    ax,
    ay,
    az,
    effective_speed,
    release_spin_rate,
    release_extension,
    release_pos_y,
    spin_axis,
    age_pit,
    api_break_z_with_gravity,
    api_break_x_arm,
    api_break_x_batter_in,
    p_throws,
    type
  FROM pitches
")

pitches2025clean <- dbGetQuery(con, "SELECT * FROM pitches_clean")

dbDisconnect(con)
