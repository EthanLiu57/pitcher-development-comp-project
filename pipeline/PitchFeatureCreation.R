library(dplyr)

# Function to add all the advanced features
add_pitch_features <- function(df) {
  df %>%
    mutate(
      # VAA (Vertical Approach Angle) in degrees
      vaa = atan(vz0 / -vy0) * (180 / pi),
      
      # HAA (Horizontal Approach Angle) in degrees
      haa = atan(vx0 / -vy0) * (180 / pi),
      
      # Bauer Units (Spin Rate / Velocity)
      bauer_units = release_spin_rate / release_speed,
      
      # Induced Vertical Break (IVB) in inches
      induced_vert_break = pfx_z * 12,
      
      # Horizontal Break in inches
      horz_break = pfx_x * 12,
      
      # Total Movement (magnitude in inches)
      total_movement = sqrt((pfx_x * 12)^2 + (pfx_z * 12)^2),
      
      # Gyro Degree (0 = pure backspin, 90 = pure gyro)
      gyro_degree = abs(90 - abs(spin_axis - 180)),
      
      # Spin Efficiency (approximate - based on movement vs spin)
      # Higher efficiency = more movement per rpm
      spin_efficiency = case_when(
        is.na(release_spin_rate) | release_spin_rate == 0 ~ NA_real_,
        TRUE ~ abs(pfx_z * 12) / (release_spin_rate * 0.00222)
      ),
      
      # True Spin (spin that creates movement)
      true_spin = release_spin_rate * spin_efficiency,
      
      # Perceived Velocity (adjusted for extension)
      perceived_velocity = release_speed * (55 / (60.5 - release_extension)),
      
      # Release Speed in ft/s (for physics calculations)
      release_speed_fps = release_speed * 1.467,
      
      # Time to plate (approximate)
      time_to_plate = 55 / release_speed_fps,
      
      # Vertical break per second
      vert_break_rate = induced_vert_break / time_to_plate,
      
      # Movement direction angle (0 = pure backspin, 90 = glove side, 270 = arm side)
      movement_angle = atan2(pfx_x * 12, pfx_z * 12) * (180 / pi),
      
      # Stuff+ components (simplified - real Stuff+ is proprietary)
      # Vertical approach angle deviation from average
      vaa_plus = vaa - mean(vaa[pitch_type == pitch_type[1]], na.rm = TRUE),
      
      # Spin-to-velocity ratio percentile (within pitch type)
      bauer_percentile = percent_rank(bauer_units)
    ) %>%
    # Cap spin efficiency at realistic values (0-1)
    mutate(
      spin_efficiency = case_when(
        spin_efficiency > 1 ~ 1,
        spin_efficiency < 0 ~ 0,
        TRUE ~ spin_efficiency
      )
    )
}

# Apply to all years
pitches2025_enhanced <- add_pitch_features(pitches2025clean)
pitches2024_enhanced <- add_pitch_features(pitches2024clean)
pitches2023_enhanced <- add_pitch_features(pitches2023clean)
pitches2022_enhanced <- add_pitch_features(pitches2022clean)
pitches2021_enhanced <- add_pitch_features(pitches2021clean)

# Combine all years
all_pitches <- bind_rows(
  pitches2021_enhanced,
  pitches2022_enhanced,
  pitches2023_enhanced,
  pitches2024_enhanced,
  pitches2025_enhanced
)

# Check the results
summary(all_pitches %>% select(vaa, haa, bauer_units, spin_efficiency, induced_vert_break))


# Save to CSV for backup
write.csv(all_pitches, "~/Downloads/all_pitches_enhanced_2021_2025.csv", row.names = FALSE)

# Or save back to SQLite
con <- dbConnect(RSQLite::SQLite(), "~/Downloads/all_pitches_enhanced.db")
dbWriteTable(con, "pitches", all_pitches, overwrite = TRUE)
dbDisconnect(con)
