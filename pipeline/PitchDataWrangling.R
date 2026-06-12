library(dplyr)
library(tidyr)

# Calculate pitcher-level statistics for ALL metrics
pitcher_stats_long <- all_pitches %>%
  filter(!is.na(pitch_type)) %>%
  group_by(pitcher, player_name, game_year, pitch_type, p_throws, age_pit) %>%
  summarise(
    n_pitches = n(),
    
    # ALL metrics - means and variances
    velo_mean = mean(release_speed, na.rm = TRUE),
    velo_var = var(release_speed, na.rm = TRUE),
    
    spin_mean = mean(release_spin_rate, na.rm = TRUE),
    spin_var = var(release_spin_rate, na.rm = TRUE),
    
    bauer_mean = mean(bauer_units, na.rm = TRUE),
    bauer_var = var(bauer_units, na.rm = TRUE),
    
    spin_eff_mean = mean(spin_efficiency, na.rm = TRUE),
    spin_eff_var = var(spin_efficiency, na.rm = TRUE),
    
    ivb_mean = mean(induced_vert_break, na.rm = TRUE),
    ivb_var = var(induced_vert_break, na.rm = TRUE),
    
    horz_mean = mean(horz_break, na.rm = TRUE),
    horz_var = var(horz_break, na.rm = TRUE),
    
    total_movement_mean = mean(total_movement, na.rm = TRUE),
    total_movement_var = var(total_movement, na.rm = TRUE),
    
    vaa_mean = mean(vaa, na.rm = TRUE),
    vaa_var = var(vaa, na.rm = TRUE),
    
    haa_mean = mean(haa, na.rm = TRUE),
    haa_var = var(haa, na.rm = TRUE),
    
    extension_mean = mean(release_extension, na.rm = TRUE),
    extension_var = var(release_extension, na.rm = TRUE),
    
    release_height_mean = mean(release_pos_z, na.rm = TRUE),
    release_height_var = var(release_pos_z, na.rm = TRUE),
    
    release_side_mean = mean(release_pos_x, na.rm = TRUE),
    release_side_var = var(release_pos_x, na.rm = TRUE),
    
    release_y_mean = mean(release_pos_y, na.rm = TRUE),
    release_y_var = var(release_pos_y, na.rm = TRUE),
    
    perceived_velo_mean = mean(perceived_velocity, na.rm = TRUE),
    perceived_velo_var = var(perceived_velocity, na.rm = TRUE),
    
    gyro_mean = mean(gyro_degree, na.rm = TRUE),
    gyro_var = var(gyro_degree, na.rm = TRUE),
    
    true_spin_mean = mean(true_spin, na.rm = TRUE),
    true_spin_var = var(true_spin, na.rm = TRUE),
    
    movement_angle_mean = mean(movement_angle, na.rm = TRUE),
    movement_angle_var = var(movement_angle, na.rm = TRUE),
    
    vert_break_rate_mean = mean(vert_break_rate, na.rm = TRUE),
    vert_break_rate_var = var(vert_break_rate, na.rm = TRUE),
    
    effective_speed_mean = mean(effective_speed, na.rm = TRUE),
    effective_speed_var = var(effective_speed, na.rm = TRUE),
    
    spin_axis_mean = mean(spin_axis, na.rm = TRUE),
    spin_axis_var = var(spin_axis, na.rm = TRUE),
    
    # Acceleration components
    ax_mean = mean(ax, na.rm = TRUE),
    ax_var = var(ax, na.rm = TRUE),
    
    ay_mean = mean(ay, na.rm = TRUE),
    ay_var = var(ay, na.rm = TRUE),
    
    az_mean = mean(az, na.rm = TRUE),
    az_var = var(az, na.rm = TRUE),
    
    # Velocity components
    vx0_mean = mean(vx0, na.rm = TRUE),
    vx0_var = var(vx0, na.rm = TRUE),
    
    vy0_mean = mean(vy0, na.rm = TRUE),
    vy0_var = var(vy0, na.rm = TRUE),
    
    vz0_mean = mean(vz0, na.rm = TRUE),
    vz0_var = var(vz0, na.rm = TRUE),
    
    # Baseball Savant API breaks
    api_break_z_mean = mean(api_break_z_with_gravity, na.rm = TRUE),
    api_break_z_var = var(api_break_z_with_gravity, na.rm = TRUE),
    
    api_break_x_arm_mean = mean(api_break_x_arm, na.rm = TRUE),
    api_break_x_arm_var = var(api_break_x_arm, na.rm = TRUE),
    
    api_break_x_batter_mean = mean(api_break_x_batter_in, na.rm = TRUE),
    api_break_x_batter_var = var(api_break_x_batter_in, na.rm = TRUE),
    
    # pfx components
    pfx_x_mean = mean(pfx_x, na.rm = TRUE),
    pfx_x_var = var(pfx_x, na.rm = TRUE),
    
    pfx_z_mean = mean(pfx_z, na.rm = TRUE),
    pfx_z_var = var(pfx_z, na.rm = TRUE),
    
    # Plate location
    plate_x_mean = mean(plate_x, na.rm = TRUE),
    plate_x_var = var(plate_x, na.rm = TRUE),
    
    plate_z_mean = mean(plate_z, na.rm = TRUE),
    plate_z_var = var(plate_z, na.rm = TRUE),
    
    .groups = 'drop'
  )

# Updated function that works with pick()
apply_js_shrinkage <- function(mean_vals, var_vals, n_vals) {
  # Grand mean
  grand_mean <- mean(mean_vals, na.rm = TRUE)
  
  # Number of observations
  p <- length(mean_vals)
  
  # Pooled variance
  pooled_var <- sum(var_vals * (n_vals - 1), na.rm = TRUE) / 
    sum(n_vals - 1, na.rm = TRUE)
  
  # Squared distance from grand mean
  sq_dist <- sum((mean_vals - grand_mean)^2, na.rm = TRUE)
  
  # JS shrinkage factor (positive part)
  if (sq_dist > 0 && !is.na(pooled_var) && pooled_var > 0) {
    shrink_factor <- max(0, 1 - ((p - 2) * pooled_var) / sq_dist)
  } else {
    shrink_factor <- 0
  }
  
  # Apply shrinkage
  js_estimate <- grand_mean + shrink_factor * (mean_vals - grand_mean)
  
  return(js_estimate)
}

# Apply James-Stein to all metrics within each pitch type
pitcher_js_estimates <- pitcher_stats_long %>%
  group_by(pitch_type) %>%
  mutate(
    # Primary metrics
    velo_js = apply_js_shrinkage(velo_mean, velo_var, n_pitches),
    spin_js = apply_js_shrinkage(spin_mean, spin_var, n_pitches),
    bauer_js = apply_js_shrinkage(bauer_mean, bauer_var, n_pitches),
    spin_eff_js = apply_js_shrinkage(spin_eff_mean, spin_eff_var, n_pitches),
    
    # Movement
    ivb_js = apply_js_shrinkage(ivb_mean, ivb_var, n_pitches),
    horz_js = apply_js_shrinkage(horz_mean, horz_var, n_pitches),
    total_movement_js = apply_js_shrinkage(total_movement_mean, total_movement_var, n_pitches),
    
    # Angles
    vaa_js = apply_js_shrinkage(vaa_mean, vaa_var, n_pitches),
    haa_js = apply_js_shrinkage(haa_mean, haa_var, n_pitches),
    movement_angle_js = apply_js_shrinkage(movement_angle_mean, movement_angle_var, n_pitches),
    
    # Release
    extension_js = apply_js_shrinkage(extension_mean, extension_var, n_pitches),
    release_height_js = apply_js_shrinkage(release_height_mean, release_height_var, n_pitches),
    release_side_js = apply_js_shrinkage(release_side_mean, release_side_var, n_pitches),
    release_y_js = apply_js_shrinkage(release_y_mean, release_y_var, n_pitches),
    
    # Spin characteristics
    perceived_velo_js = apply_js_shrinkage(perceived_velo_mean, perceived_velo_var, n_pitches),
    gyro_js = apply_js_shrinkage(gyro_mean, gyro_var, n_pitches),
    true_spin_js = apply_js_shrinkage(true_spin_mean, true_spin_var, n_pitches),
    spin_axis_js = apply_js_shrinkage(spin_axis_mean, spin_axis_var, n_pitches),
    
    # Other metrics
    vert_break_rate_js = apply_js_shrinkage(vert_break_rate_mean, vert_break_rate_var, n_pitches),
    effective_speed_js = apply_js_shrinkage(effective_speed_mean, effective_speed_var, n_pitches),
    
    # Acceleration
    ax_js = apply_js_shrinkage(ax_mean, ax_var, n_pitches),
    ay_js = apply_js_shrinkage(ay_mean, ay_var, n_pitches),
    az_js = apply_js_shrinkage(az_mean, az_var, n_pitches),
    
    # Velocity components
    vx0_js = apply_js_shrinkage(vx0_mean, vx0_var, n_pitches),
    vy0_js = apply_js_shrinkage(vy0_mean, vy0_var, n_pitches),
    vz0_js = apply_js_shrinkage(vz0_mean, vz0_var, n_pitches),
    
    # API breaks
    api_break_z_js = apply_js_shrinkage(api_break_z_mean, api_break_z_var, n_pitches),
    api_break_x_arm_js = apply_js_shrinkage(api_break_x_arm_mean, api_break_x_arm_var, n_pitches),
    api_break_x_batter_js = apply_js_shrinkage(api_break_x_batter_mean, api_break_x_batter_var, n_pitches),
    
    # pfx
    pfx_x_js = apply_js_shrinkage(pfx_x_mean, pfx_x_var, n_pitches),
    pfx_z_js = apply_js_shrinkage(pfx_z_mean, pfx_z_var, n_pitches),
    
    # Plate location
    plate_x_js = apply_js_shrinkage(plate_x_mean, plate_x_var, n_pitches),
    plate_z_js = apply_js_shrinkage(plate_z_mean, plate_z_var, n_pitches)
  ) %>%
  ungroup() %>%
  # Keep only the JS estimates and identifiers
  select(pitcher, player_name, game_year, pitch_type, p_throws, age_pit, n_pitches,
         ends_with("_js"))


all_pitch_types <- unique(pitcher_js_estimates$pitch_type)

pitcher_year_combos <- pitcher_js_estimates %>%
  distinct(pitcher, player_name, game_year, p_throws, age_pit)

complete_grid <- pitcher_year_combos %>%
  crossing(pitch_type = all_pitch_types)

pitcher_complete <- complete_grid %>%
  left_join(pitcher_js_estimates, 
            by = c("pitcher", "player_name", "game_year", "pitch_type", "p_throws", "age_pit")) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

pitcher_wide <- pitcher_complete %>%
  pivot_wider(
    id_cols = c(pitcher, player_name, game_year, p_throws, age_pit),
    names_from = pitch_type,
    values_from = c(n_pitches, ends_with("_js")),
    names_sep = "_"
  )


