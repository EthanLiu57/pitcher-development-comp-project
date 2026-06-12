# PRIMARY VELOCITY METRICS
velocity_metrics <- c(
  "release_speed",      # Raw velocity
  "effective_speed",    # Adjusted for movement
  "perceived_velocity"  # Adjusted for extension
)

# SPIN METRICS
spin_metrics <- c(
  "release_spin_rate",  # Total spin
  "bauer_units",        # Spin/velocity ratio
  "spin_efficiency",    # Proportion creating movement
  "true_spin",          # Effective spin
  "gyro_degree",        # Gyroscopic component
  "spin_axis"           # Direction of spin axis
)

# MOVEMENT METRICS
movement_metrics <- c(
  "induced_vert_break", # Vertical movement (IVB)
  "horz_break",         # Horizontal movement
  "total_movement",     # Magnitude
  "movement_angle",     # Direction of movement
  "pfx_x",              # Raw horizontal break
  "pfx_z",              # Raw vertical break
  "api_break_z_with_gravity",  # Baseball Savant break
  "api_break_x_arm",           # Arm-side break
  "api_break_x_batter_in"      # Glove/arm side break
)

# APPROACH ANGLE METRICS
angle_metrics <- c(
  "vaa",                # Vertical approach angle
  "haa",                # Horizontal approach angle
  "vert_break_rate"     # Rate of vertical break
)

# RELEASE POINT METRICS
release_metrics <- c(
  "release_pos_x",      # Side-to-side release
  "release_pos_y",      # Front-to-back release
  "release_pos_z",      # Height of release
  "extension"           # Release extension
)

# PHYSICS METRICS (velocity/acceleration components)
physics_metrics <- c(
  "vx0", "vy0", "vz0",  # Velocity components
  "ax", "ay", "az"      # Acceleration components
)

# LOCATION METRICS (where pitches end up - might not want these for arsenal clustering)
location_metrics <- c(
  "plate_x",            # Horizontal location
  "plate_z"             # Vertical location
)

# AGE (demographic)
demographic <- c(
  "age_pit"
)

# CORE METRICS (Most important for pitch characteristics)
core_metrics <- c(
  # Velocity
  "release_speed",
  "perceived_velocity",
  
  # Spin
  "release_spin_rate",
  "bauer_units",
  "spin_efficiency",
  "spin_axis",
  
  # Movement
  "induced_vert_break",
  "horz_break",
  "total_movement",
  "movement_angle",
  
  # Angles
  "vaa",
  "haa",
  
  # Release
  "release_extension",
  "release_pos_z",  # Release height
  "release_pos_x"   # Release side
)

# EXTENDED METRICS 
extended_metrics <- c(
  core_metrics,
  
  # Additional spin characteristics
  "gyro_degree",
  "true_spin",
  
  # Movement details
  "vert_break_rate",
  
  # Physics
  "vx0", "vy0", "vz0"
)

# COMPREHENSIVE 
comprehensive_metrics <- c(
  # All velocity
  "release_speed",
  "effective_speed",
  "perceived_velocity",
  
  # All spin
  "release_spin_rate",
  "bauer_units",
  "spin_efficiency",
  "true_spin",
  "gyro_degree",
  "spin_axis",
  
  # All movement
  "induced_vert_break",
  "horz_break",
  "total_movement",
  "movement_angle",
  "pfx_x",
  "pfx_z",
  
  # Angles
  "vaa",
  "haa",
  "vert_break_rate",
  
  # Release
  "extension",
  "release_pos_x",
  "release_pos_y",
  "release_pos_z",
  
  # Physics
  "vx0", "vy0", "vz0",
  "ax", "ay", "az"
)
