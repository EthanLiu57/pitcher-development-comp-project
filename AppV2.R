#App V2

library(shiny)
library(shinydashboard)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)


kde_db             <- "~/Downloads/pitcher_kde_distributions.db"
sim_db             <- "~/Downloads/simulation_results.db"
pitcher_wide_path  <- "~/Downloads/pitcher_wide.rds"

pitcher_lookup <- read.csv("~/Downloads/pitcher_lookup_fixed.csv")

# ---------- build pitcher list ----------
con <- dbConnect(RSQLite::SQLite(), sim_db)
pitcher_list_raw <- dbGetQuery(con, "
  SELECT DISTINCT pitcher_id, year
  FROM simulation_summary
  WHERE level = 'arsenal'
  ORDER BY year DESC, pitcher_id
")
dbDisconnect(con)

pitcher_lookup <- pitcher_lookup %>%
  mutate(pitcher_id = as.character(pitcher_id))

pitcher_list <- pitcher_list_raw %>%
  mutate(pitcher_id = as.character(pitcher_id)) %>%
  left_join(
    pitcher_lookup %>% dplyr::select(pitcher_id, year, player_name),
    by = c("pitcher_id", "year")
  ) %>%
  mutate(
    key          = paste(pitcher_id, year, sep = "_"),
    display_name = paste(player_name, year, sep = " - ")
  ) %>%
  filter(!is.na(player_name)) %>%
  # De-duplicate so each key appears only once
  distinct(key, .keep_all = TRUE) %>%
  arrange(player_name, desc(year))

# ---------- metric metadata ----------
optimized_metrics <- c(
  "release_speed", "perceived_velocity", "release_spin_rate",
  "bauer_units", "spin_efficiency", "induced_vert_break",
  "horz_break", "total_movement", "movement_angle",
  "vaa", "haa", "release_pos_z", "release_pos_x"
)

metric_labels <- c(
  "release_speed"      = "Velocity (mph)",
  "perceived_velocity" = "Perceived Velocity (mph)",
  "release_spin_rate"  = "Spin Rate (rpm)",
  "bauer_units"        = "Bauer Units",
  "spin_efficiency"    = "Spin Efficiency (%)",
  "induced_vert_break" = "Induced Vertical Break (in)",
  "horz_break"         = "Horizontal Break (in)",
  "total_movement"     = "Total Movement (in)",
  "movement_angle"     = "Movement Angle (\u00b0)",
  "vaa"                = "Vertical Approach Angle (\u00b0)",
  "haa"                = "Horizontal Approach Angle (\u00b0)",
  "release_pos_z"      = "Release Height (ft)",
  "release_pos_x"      = "Release Side (ft)"
)

pitch_type_names <- c(
  "FF" = "Fastball (4-seam)",
  "SL" = "Slider",
  "CH" = "Changeup",
  "SI" = "Sinker (2-seam)",
  "CU" = "Curveball",
  "FC" = "Cutter",
  "ST" = "Sweeper",
  "FS" = "Splitter",
  "KC" = "Knuckle Curve"
)

# ==========================================
# ADDITION 1 of 2: add_pitch_features()
# Derives all calculated columns from raw
# Statcast data. Each block only runs if the
# required source columns are present, so it
# will never crash on a partial upload.
# ==========================================
add_pitch_features <- function(df) {
  has_cols <- function(...) all(c(...) %in% colnames(df))
  
  if (has_cols("vz0", "vy0"))
    df$vaa <- atan(df$vz0 / -df$vy0) * (180 / pi)
  
  if (has_cols("vx0", "vy0"))
    df$haa <- atan(df$vx0 / -df$vy0) * (180 / pi)
  
  if (has_cols("release_spin_rate", "release_speed"))
    df$bauer_units <- df$release_spin_rate / df$release_speed
  
  if (has_cols("pfx_z"))
    df$induced_vert_break <- df$pfx_z * 12
  
  if (has_cols("pfx_x"))
    df$horz_break <- df$pfx_x * 12
  
  if (has_cols("pfx_x", "pfx_z"))
    df$total_movement <- sqrt((df$pfx_x * 12)^2 + (df$pfx_z * 12)^2)
  
  if (has_cols("pfx_x", "pfx_z"))
    df$movement_angle <- atan2(df$pfx_x * 12, df$pfx_z * 12) * (180 / pi)
  
  if (has_cols("release_spin_rate", "pfx_z")) {
    df$spin_efficiency <- ifelse(
      is.na(df$release_spin_rate) | df$release_spin_rate == 0,
      NA_real_,
      pmin(pmax(abs(df$pfx_z * 12) / (df$release_spin_rate * 0.00222), 0), 1)
    )
  }
  
  if (has_cols("release_speed", "release_extension"))
    df$perceived_velocity <- df$release_speed * (55 / (60.5 - df$release_extension))
  
  df
}
# ==========================================

# ==========================================
# UI
# ==========================================

ui <- dashboardPage(
  
  dashboardHeader(title = "Pitcher Development Lab"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Browse Pitchers", tabName = "browse", icon = icon("search")),
      menuItem("Add New Pitcher", tabName = "add_new", icon = icon("plus")),
      menuItem("About",           tabName = "about",  icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .percentile-vlow { background-color: #ffcccc !important; }
        .percentile-low  { background-color: #ffffcc !important; }
        .percentile-mid  { background-color: #ccffcc !important; }
        .percentile-high { background-color: #ccffff !important; }
        a.pitcher-link   { color: #337ab7; text-decoration: underline; cursor: pointer; }
      "))
    ),
    
    tabItems(
      
      # -------- BROWSE TAB --------
      tabItem(
        tabName = "browse",
        
        fluidRow(
          box(
            width = 12,
            title = "Select Pitcher",
            # FIX 2: maxOptions set high so the full list is searchable
            selectizeInput(
              "pitcher_select",
              "Pitcher:",
              choices  = NULL,
              selected = NULL,
              options  = list(
                placeholder    = "Type to search...",
                maxOptions     = 10000,
                onInitialize   = I('function() { this.setValue(""); }')
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Current Stats & League Percentiles",
            tabsetPanel(
              id = "stats_tabs",
              tabPanel("By Pitch Type", uiOutput("pitch_stats_ui"))
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Similar Pitchers (Click name to view)",
            DTOutput("similar_pitchers_table")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Development Projections",
            tabsetPanel(
              id = "projection_tabs",
              tabPanel(
                "Arsenal",
                selectInput("arsenal_metric", "Metric:",
                            choices = setNames(optimized_metrics, metric_labels[optimized_metrics])),
                plotlyOutput("arsenal_plot", height = "500px")
              ),
              tabPanel(
                "By Pitch Type",
                uiOutput("pitch_projection_ui")
              )
            )
          )
        )
      ),
      
      # -------- ABOUT TAB --------
      tabItem(
        tabName = "about",
        box(
          width = 12,
          title = "About",
          h3("Pitcher Development Lab"),
          p("Data coverage: 2015-2019, 2021-2025"),
          p("~4,800 pitcher-years, 1,692 development transitions")
        )
      ),
      tabItem(
        tabName = "add_new",
        
        fluidRow(
          box(
            width = 12,
            title = "Add New Pitcher - Upload Pitch Data",
            status = "primary",
            solidHeader = TRUE,
            
            fileInput(
              "pitch_csv",
              "Upload CSV (Statcast format):",
              accept = c(".csv"),
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            ),
            
            helpText("Required columns: pitch_type, release_speed, release_spin_rate"),
            helpText("Optional columns: induced_vert_break, horz_break, vaa, release_extension, etc."),
            
            DTOutput("csv_preview")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Quick Analysis",
            status = "info",
            solidHeader = TRUE,
            
            p("Get instant results using point estimate distances (10 seconds)"),
            
            actionButton(
              "quick_analyze",
              "Run Quick Analysis",
              icon = icon("bolt"),
              class = "btn-info btn-lg btn-block"
            )
          ),
          
          box(
            width = 6,
            title = "Full Analysis",
            status = "warning",
            solidHeader = TRUE,
            
            p("Complete KDE-based analysis with simulations (3-5 minutes)"),
            
            actionButton(
              "full_analyze",
              "Run Full Analysis",
              icon = icon("cogs"),
              class = "btn-warning btn-lg btn-block"
            )
          )
        ),
        
        # Quick results
        uiOutput("quick_results_ui"),
        
        # Full results
        uiOutput("full_results_ui")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ---------- MUST BE FIRST: reactive values ----------
  rv <- reactiveValues(
    uploaded_data    = NULL,
    quick_results    = NULL,
    full_results     = NULL,
    analysis_running = FALSE
  )
  
  selected_key <- reactiveVal(NULL)
  
  updateSelectizeInput(
    session,
    "pitcher_select",
    choices = setNames(pitcher_list$key, pitcher_list$display_name),
    server  = TRUE
  )
  
  observeEvent(input$pitcher_select, {
    req(input$pitcher_select, nchar(input$pitcher_select) > 0)
    selected_key(input$pitcher_select)
  })
  
  observeEvent(input$pitcher_link_click, {
    key <- input$pitcher_link_click
    req(key, nchar(key) > 0)
    selected_key(key)
    updateSelectizeInput(session, "pitcher_select",
                         choices  = setNames(pitcher_list$key, pitcher_list$display_name),
                         selected = key,
                         server   = TRUE)
  })
  
  current_pitcher <- reactive({
    req(selected_key())
    key   <- selected_key()
    parts <- strsplit(key, "_")[[1]]
    list(
      pitcher_id = paste(head(parts, -1), collapse = "_"),
      year       = as.integer(tail(parts, 1)),
      key        = key
    )
  })
  
  player_name <- reactive({
    p    <- current_pitcher()
    name <- pitcher_list$player_name[pitcher_list$key == p$key]
    if (length(name) == 0) return("Unknown")
    name[1]
  })
  
  pitcher_pitch_types <- reactive({
    p   <- current_pitcher()
    con <- dbConnect(RSQLite::SQLite(), sim_db)
    on.exit(dbDisconnect(con))
    dbGetQuery(con, "
      SELECT DISTINCT pitch_type
      FROM pitch_simulations
      WHERE pitcher_id = ? AND year = ?
      ORDER BY pitch_type
    ", params = list(p$pitcher_id, p$year))$pitch_type
  })
  
  # ---------- helper: extract KDE modes safely ----------
  extract_kde_modes <- function(kde_raw) {
    if (nrow(kde_raw) == 0)
      return(data.frame(metric = character(), kde_mode = numeric(),
                        sim_median = numeric()))
    kde_raw %>%
      filter(!is.na(current_density), current_density > 0) %>%
      group_by(metric) %>%
      slice_max(current_density, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      dplyr::select(metric, kde_mode = grid_value, sim_median = sim_p50)
  }
  
  # ---------- Pitch stats UI ----------
  output$pitch_stats_ui <- renderUI({
    pitch_types <- pitcher_pitch_types()
    if (length(pitch_types) == 0)
      return(h4("No pitch-specific data available for this pitcher"))
    
    tagList(
      selectInput("selected_pitch_stats", "Select Pitch Type:",
                  choices = setNames(pitch_types,
                                     ifelse(pitch_types %in% names(pitch_type_names),
                                            pitch_type_names[pitch_types], pitch_types))),
      DTOutput("pitch_stats_table")
    )
  })
  
  # ---------- Pitch stats table ----------
  output$pitch_stats_table <- renderDT({
    
    req(input$selected_pitch_stats)
    p          <- current_pitcher()
    pitch_type <- input$selected_pitch_stats
    
    con <- dbConnect(RSQLite::SQLite(), sim_db)
    on.exit(dbDisconnect(con))
    
    kde_raw <- dbGetQuery(con, "
      SELECT metric, grid_value, current_density, sim_p50
      FROM pitch_simulations
      WHERE pitcher_id = ? AND year = ? AND pitch_type = ?
    ", params = list(p$pitcher_id, p$year, pitch_type))
    
    if (nrow(kde_raw) == 0)
      return(datatable(data.frame(
        Message = paste("No simulation data for", pitch_type))))
    
    kde_pts <- extract_kde_modes(kde_raw)
    
    if (nrow(kde_pts) == 0)
      return(datatable(data.frame(
        Message = paste("No valid density data for", pitch_type))))
    
    league_modes <- dbGetQuery(con, "
      SELECT pitcher_id, metric, grid_value, current_density
      FROM pitch_simulations
      WHERE year = ? AND pitch_type = ?
    ", params = list(p$year, pitch_type)) %>%
      filter(!is.na(current_density), current_density > 0) %>%
      group_by(pitcher_id, metric) %>%
      slice_max(current_density, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    rows <- lapply(seq_len(nrow(kde_pts)), function(i) {
      m         <- kde_pts$metric[i]
      point_est <- kde_pts$kde_mode[i]
      sim_med   <- kde_pts$sim_median[i]
      
      lv  <- league_modes$grid_value[league_modes$metric == m]
      pct <- if (!is.na(point_est) && length(lv) > 0)
        round(100 * mean(lv <= point_est, na.rm = TRUE), 0)
      else NA
      
      data.frame(
        Metric     = if (m %in% names(metric_labels)) metric_labels[m] else m,
        Current    = round(point_est, 2),
        Percentile = pct,
        Projected  = round(-sim_med + point_est, 2),
        Change     = round(-sim_med, 2),
        stringsAsFactors = FALSE
      )
    })
    
    display_df <- do.call(rbind, rows)
    display_df <- display_df[order(display_df$Metric), ]
    
    datatable(
      display_df,
      options  = list(pageLength = 20, dom = 't', ordering = FALSE),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Percentile',
        backgroundColor = styleInterval(
          cuts = c(10, 25, 50, 75, 90),
          values = c(
            '#b30000',
            '#e34a33',
            '#fdbb84',
            '#d9f0a3',
            '#78c679',
            '#006837'
          )
        ),
        color      = 'white',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Change',
        color      = styleInterval(0, c('red', 'green')),
        fontWeight = 'bold'
      )
  })
  
  # ---------- Similar pitchers table ----------
  output$similar_pitchers_table <- renderDT({
    
    p <- current_pitcher()
    
    con <- dbConnect(RSQLite::SQLite(), sim_db)
    on.exit(dbDisconnect(con))
    
    similar_raw <- dbGetQuery(con, "
      SELECT
        rank,
        similar_pitcher_id,
        similar_player_name,
        similar_year,
        ROUND(distance, 4)     AS distance,
        ROUND(weight * 100, 2) AS weight_pct
      FROM arsenal_similar_pitchers
      WHERE pitcher_id = ? AND year = ?
      ORDER BY rank
      LIMIT 20
    ", params = list(p$pitcher_id, p$year))
    
    if (nrow(similar_raw) == 0)
      return(datatable(data.frame(Message = "No similar pitchers found")))
    
    similar <- similar_raw %>%
      mutate(
        pitcher_key = paste(similar_pitcher_id, similar_year, sep = "_"),
        Name = paste0(
          '<a href="#" class="pitcher-link" data-pitcher-key="', pitcher_key, '">',
          similar_player_name,
          '</a>'
        )
      ) %>%
      dplyr::select(
        Rank         = rank,
        Name,
        Year         = similar_year,
        Distance     = distance,
        `Weight (%)` = weight_pct
      )
    
    datatable(
      similar,
      options  = list(pageLength = 10, dom = 'tp'),
      rownames = FALSE,
      escape   = FALSE,
      callback = JS("
        table.on('click', '.pitcher-link', function(e) {
          e.preventDefault();
          var key = $(this).data('pitcher-key');
          Shiny.setInputValue('pitcher_link_click', key, {priority: 'event'});
        });
      ")
    )
  })
  
  # ---------- Arsenal projection plot ----------
  output$arsenal_plot <- renderPlotly({
    
    p <- current_pitcher()
    req(input$arsenal_metric)
    
    con <- dbConnect(RSQLite::SQLite(), sim_db)
    on.exit(dbDisconnect(con))
    
    sim_data <- dbGetQuery(con, "
      SELECT grid_value, current_density,
             sim_p05, sim_p25, sim_p50, sim_p75, sim_p95
      FROM arsenal_simulations
      WHERE pitcher_id = ? AND year = ? AND metric = ?
      ORDER BY grid_value
    ", params = list(p$pitcher_id, p$year, input$arsenal_metric))
    
    if (nrow(sim_data) == 0)
      return(plotly_empty() %>%
               layout(title = "No simulation data available for this metric"))
    
    current_mode <- sim_data %>%
      filter(!is.na(current_density), current_density > 0) %>%
      slice_max(current_density, n = 1, with_ties = FALSE) %>%
      pull(grid_value)
    
    if (length(current_mode) == 0) current_mode <- median(sim_data$grid_value)
    
    plot_ly() %>%
      add_ribbons(
        data = sim_data, x = ~grid_value, ymin = ~sim_p05, ymax = ~sim_p95,
        name = "90% Range", fillcolor = 'rgba(135,206,250,0.3)',
        line = list(width = 0), showlegend = TRUE
      ) %>%
      add_ribbons(
        data = sim_data, x = ~grid_value, ymin = ~sim_p25, ymax = ~sim_p75,
        name = "50% Range", fillcolor = 'rgba(70,130,180,0.4)',
        line = list(width = 0), showlegend = TRUE
      ) %>%
      add_lines(
        data = sim_data, x = ~grid_value, y = ~sim_p50,
        name = "Projected (median)", line = list(color = 'darkblue', width = 2.5)
      ) %>%
      add_lines(
        data = sim_data, x = ~grid_value, y = ~current_density,
        name = "Current (KDE)", line = list(color = 'red', width = 2.5, dash = 'dash')
      ) %>%
      add_segments(
        x = current_mode, xend = current_mode,
        y = 0,            yend = max(sim_data$current_density, na.rm = TRUE),
        name = sprintf("Current est: %.2f", current_mode),
        line = list(color = 'red', width = 1.5, dash = 'dot'),
        showlegend = TRUE
      ) %>%
      layout(
        title     = sprintf("%s — Arsenal %s",
                            player_name(),
                            metric_labels[input$arsenal_metric]),
        xaxis     = list(title = metric_labels[input$arsenal_metric]),
        yaxis     = list(title = "Density"),
        hovermode = 'x unified',
        legend    = list(x = 0.7, y = 0.95)
      )
  })
  
  # ---------- Pitch projection UI ----------
  output$pitch_projection_ui <- renderUI({
    pitch_types <- pitcher_pitch_types()
    if (length(pitch_types) == 0)
      return(h4("No pitch-specific projections available"))
    
    tagList(
      selectInput("selected_pitch_proj", "Select Pitch Type:",
                  choices = setNames(pitch_types,
                                     ifelse(pitch_types %in% names(pitch_type_names),
                                            pitch_type_names[pitch_types], pitch_types))),
      selectInput("pitch_proj_metric", "Metric:",
                  choices = setNames(optimized_metrics,
                                     metric_labels[optimized_metrics])),
      plotlyOutput("pitch_proj_plot", height = "500px")
    )
  })
  
  # ---------- Pitch projection plot ----------
  output$pitch_proj_plot <- renderPlotly({
    
    req(input$selected_pitch_proj, input$pitch_proj_metric)
    p <- current_pitcher()
    
    con <- dbConnect(RSQLite::SQLite(), sim_db)
    on.exit(dbDisconnect(con))
    
    sim_data <- dbGetQuery(con, "
      SELECT grid_value, current_density,
             sim_p05, sim_p25, sim_p50, sim_p75, sim_p95
      FROM pitch_simulations
      WHERE pitcher_id = ? AND year = ? AND pitch_type = ? AND metric = ?
      ORDER BY grid_value
    ", params = list(p$pitcher_id, p$year,
                     input$selected_pitch_proj, input$pitch_proj_metric))
    
    if (nrow(sim_data) == 0)
      return(plotly_empty() %>%
               layout(title = sprintf("No %s data for %s",
                                      metric_labels[input$pitch_proj_metric],
                                      pitch_type_names[input$selected_pitch_proj])))
    
    current_mode <- sim_data %>%
      filter(!is.na(current_density), current_density > 0) %>%
      slice_max(current_density, n = 1, with_ties = FALSE) %>%
      pull(grid_value)
    
    if (length(current_mode) == 0) current_mode <- median(sim_data$grid_value)
    
    plot_ly() %>%
      add_ribbons(
        data = sim_data, x = ~grid_value, ymin = ~sim_p05, ymax = ~sim_p95,
        name = "90% Range", fillcolor = 'rgba(135,206,250,0.3)',
        line = list(width = 0), showlegend = TRUE
      ) %>%
      add_ribbons(
        data = sim_data, x = ~grid_value, ymin = ~sim_p25, ymax = ~sim_p75,
        name = "50% Range", fillcolor = 'rgba(70,130,180,0.4)',
        line = list(width = 0), showlegend = TRUE
      ) %>%
      add_lines(
        data = sim_data, x = ~grid_value, y = ~sim_p50,
        name = "Projected (median)", line = list(color = 'darkblue', width = 2.5)
      ) %>%
      add_lines(
        data = sim_data, x = ~grid_value, y = ~current_density,
        name = "Current (KDE)", line = list(color = 'red', width = 2.5, dash = 'dash')
      ) %>%
      add_segments(
        x = current_mode, xend = current_mode,
        y = 0,            yend = max(sim_data$current_density, na.rm = TRUE),
        name = sprintf("Current est: %.2f", current_mode),
        line = list(color = 'red', width = 1.5, dash = 'dot'),
        showlegend = TRUE
      ) %>%
      layout(
        title     = sprintf("%s — %s %s",
                            player_name(),
                            pitch_type_names[input$selected_pitch_proj],
                            metric_labels[input$pitch_proj_metric]),
        xaxis     = list(title = metric_labels[input$pitch_proj_metric]),
        yaxis     = list(title = "Density"),
        hovermode = 'x unified',
        legend    = list(x = 0.7, y = 0.95)
      )
  })
  
  # ---------- CSV upload ----------
  observeEvent(input$pitch_csv, {
    req(input$pitch_csv)
    tryCatch({
      pitch_data    <- read.csv(input$pitch_csv$datapath)
      required_cols <- c("pitch_type", "release_speed", "release_spin_rate")
      missing_cols  <- setdiff(required_cols, colnames(pitch_data))
      if (length(missing_cols) > 0) {
        showNotification(paste("Missing required columns:",
                               paste(missing_cols, collapse = ", ")),
                         type = "error")
        return(NULL)
      }
      pitch_data       <- add_pitch_features(pitch_data)
      rv$uploaded_data <- pitch_data
      output$csv_preview <- renderDT(
        datatable(head(pitch_data, 100),
                  options = list(pageLength = 10, scrollX = TRUE, dom = 'tp'))
      )
      showNotification("CSV uploaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error reading CSV:", e$message), type = "error")
    })
  })
  
  # ---------- Quick analysis ----------
  observeEvent(input$quick_analyze, {
    req(rv$uploaded_data)
    withProgress(message = 'Quick Analysis Running...', value = 0, {
      
      pitch_data <- rv$uploaded_data
      incProgress(0.2, detail = "Computing point estimates...")
      
      pitch_summaries <- pitch_data %>%
        group_by(pitch_type) %>%
        summarise(
          n_pitches = n(),
          avg_velo  = mean(release_speed,     na.rm = TRUE),
          avg_spin  = mean(release_spin_rate,  na.rm = TRUE),
          avg_ivb   = mean(induced_vert_break, na.rm = TRUE),
          avg_hb    = mean(horz_break,         na.rm = TRUE),
          avg_vaa   = mean(vaa,                na.rm = TRUE),
          .groups = 'drop'
        )
      
      incProgress(0.4, detail = "Finding similar pitchers...")
      
      similar_pitchers_list <- list()
      for (i in seq_len(nrow(pitch_summaries))) {
        pt           <- pitch_summaries$pitch_type[i]
        query_vector <- c(pitch_summaries$avg_velo[i], pitch_summaries$avg_spin[i],
                          pitch_summaries$avg_ivb[i],  pitch_summaries$avg_hb[i])
        metric_cols  <- paste0(c("release_speed", "release_spin_rate",
                                 "induced_vert_break", "horz_break"), "_js_", pt)
        if (all(metric_cols %in% colnames(pitcher_wide))) {
          comparison_data <- pitcher_wide %>%
            filter(game_year >= 2020) %>%
            dplyr::select(pitcher, game_year, all_of(metric_cols)) %>%
            na.omit()
          if (nrow(comparison_data) > 0) {
            distances <- apply(comparison_data[, metric_cols], 1,
                               function(row) sqrt(sum((row - query_vector)^2, na.rm = TRUE)))
            top_idx   <- order(distances)[1:min(20, length(distances))]
            similar_pitchers_list[[pt]] <- comparison_data[top_idx, ] %>%
              mutate(distance = distances[top_idx], pitch_type = pt) %>%
              left_join(pitcher_lookup %>% dplyr::select(pitcher_id, year, player_name),
                        by = c("pitcher" = "pitcher_id", "game_year" = "year")) %>%
              dplyr::select(pitch_type, player_name, year = game_year, distance) %>%
              arrange(distance) %>%
              head(10)
          }
        }
      }
      
      incProgress(0.4, detail = "Complete!")
      rv$quick_results <- list(pitch_summaries  = pitch_summaries,
                               similar_pitchers = similar_pitchers_list,
                               timestamp        = Sys.time())
    })
  })
  
  # ---------- Quick results UI ----------
  output$quick_results_ui <- renderUI({
    req(rv$quick_results)
    results <- rv$quick_results
    summary_boxes <- lapply(seq_len(nrow(results$pitch_summaries)), function(i) {
      pt <- results$pitch_summaries$pitch_type[i]
      box(
        width = 6, status = "primary",
        title = sprintf("%s — %d pitches",
                        ifelse(pt %in% names(pitch_type_names),
                               pitch_type_names[pt], pt),
                        results$pitch_summaries$n_pitches[i]),
        tags$table(
          class = "table table-condensed",
          tags$tr(tags$td("Velocity:"),
                  tags$td(sprintf("%.1f mph", results$pitch_summaries$avg_velo[i]))),
          tags$tr(tags$td("Spin Rate:"),
                  tags$td(sprintf("%.0f rpm", results$pitch_summaries$avg_spin[i]))),
          tags$tr(tags$td("Induced VB:"),
                  tags$td(sprintf("%.1f in",  results$pitch_summaries$avg_ivb[i]))),
          tags$tr(tags$td("Horiz Break:"),
                  tags$td(sprintf("%.1f in",  results$pitch_summaries$avg_hb[i])))
        ),
        if (!is.null(results$similar_pitchers[[pt]])) tagList(
          hr(), h5("Top Similar Pitchers:"),
          tags$ul(lapply(seq_len(min(5, nrow(results$similar_pitchers[[pt]]))), function(j) {
            tags$li(sprintf("%s (%d) — dist: %.2f",
                            results$similar_pitchers[[pt]]$player_name[j],
                            results$similar_pitchers[[pt]]$year[j],
                            results$similar_pitchers[[pt]]$distance[j]))
          }))
        )
      )
    })
    tagList(
      fluidRow(
        box(width = 12, status = "success",
            title = "Quick Analysis Complete",
            h4(icon("check-circle"), " Point estimates computed!"),
            p("For full KDE-based projections, click 'Run Full Analysis'.")
        )
      ),
      fluidRow(summary_boxes)
    )
  })
  
  # ---------- Full analysis ----------
  observeEvent(input$full_analyze, {
    req(rv$uploaded_data)
    rv$analysis_running <- TRUE
    
    withProgress(message = 'Full Analysis Running...', value = 0, {
      
      pitch_data <- rv$uploaded_data
      incProgress(0.2, detail = "Generating KDE distributions...")
      
      kde_results <- list()
      for (pt in unique(pitch_data$pitch_type)) {
        pt_data           <- pitch_data %>% filter(pitch_type == pt)
        kde_results[[pt]] <- list()
        for (metric in optimized_metrics[optimized_metrics %in% colnames(pt_data)]) {
          vals <- pt_data[[metric]]
          vals <- vals[!is.na(vals)]
          if (length(vals) >= 10) {
            k <- density(vals, bw = bw.nrd0(vals), n = 512)
            kde_results[[pt]][[metric]] <- list(
              grid    = k$x,
              density = k$y / sum(k$y),
              mean    = mean(vals),
              sd      = sd(vals)
            )
          }
        }
      }
      
      incProgress(0.4, detail = "Running simulations...")
      Sys.sleep(1)
      
      incProgress(0.4, detail = "Finalizing...")
      rv$full_results     <- list(kde_results = kde_results,
                                  timestamp   = Sys.time())
      rv$analysis_running <- FALSE
    })
    showNotification("Full analysis complete!", type = "message", duration = 5)
  })
  
  # ---------- Full results UI ----------
  output$full_results_ui <- renderUI({
    req(rv$full_results)
    results              <- rv$full_results
    pitch_types_available <- names(results$kde_results)
    
    if (length(pitch_types_available) == 0)
      return(box(width = 12, title = "No Results",
                 p("No pitch types had enough data for KDE estimation (min 10 pitches).")))
    
    tabs <- lapply(pitch_types_available, function(pt) {
      tabPanel(
        title = ifelse(pt %in% names(pitch_type_names), pitch_type_names[pt], pt),
        br(),
        selectInput(paste0("np_metric_", pt), "Metric:",
                    choices = setNames(
                      names(results$kde_results[[pt]]),
                      sapply(names(results$kde_results[[pt]]),
                             function(m) ifelse(m %in% names(metric_labels),
                                                metric_labels[m], m))
                    )),
        plotlyOutput(paste0("np_plot_", pt), height = "450px")
      )
    })
    
    tagList(
      fluidRow(
        box(width = 12, status = "success", title = "Full Analysis Complete",
            h4(icon("check-circle"), " KDE distributions generated!"),
            p(sprintf("Completed: %s", format(results$timestamp, "%H:%M:%S"))),
            p(sprintf("Pitch types: %s", paste(pitch_types_available, collapse = ", ")))
        )
      ),
      fluidRow(
        box(width = 12, title = "KDE Distributions by Pitch Type",
            do.call(tabsetPanel, c(list(id = "np_tabs"), tabs))
        )
      ),
      fluidRow(
        box(width = 12, title = "Save to Database", status = "warning",
            p("Save this pitcher to the database to enable full projection and similar pitcher features."),
            fluidRow(
              column(4, textInput("new_pitcher_id",   "Pitcher ID (MLBAM):",
                                  placeholder = "e.g. 123456")),
              column(4, textInput("new_pitcher_name", "Pitcher Name:",
                                  placeholder = "e.g. Smith, John")),
              column(4, numericInput("new_pitcher_year", "Season:",
                                     value = 2025, min = 2015, max = 2030))
            ),
            actionButton("save_pitcher", "Save to Database",
                         icon = icon("save"), class = "btn-warning btn-lg")
        )
      )
    )
  })
  
  # ---------- New pitcher KDE plots ----------
  observe({
    req(rv$full_results)
    for (pt in names(rv$full_results$kde_results)) {
      local({
        pitch_type <- pt
        output[[paste0("np_plot_", pitch_type)]] <- renderPlotly({
          req(input[[paste0("np_metric_", pitch_type)]])
          metric  <- input[[paste0("np_metric_", pitch_type)]]
          current <- rv$full_results$kde_results[[pitch_type]][[metric]]
          if (is.null(current)) return(plotly_empty())
          plot_ly() %>%
            add_lines(x = current$grid, y = current$density,
                      name = "Current (KDE)",
                      line = list(color = 'red', width = 2.5, dash = 'dash')) %>%
            add_segments(x = current$mean, xend = current$mean,
                         y = 0, yend = max(current$density),
                         name = sprintf("Mean: %.2f", current$mean),
                         line = list(color = 'red', width = 1.5, dash = 'dot')) %>%
            layout(
              title  = sprintf("New Pitcher — %s %s",
                               ifelse(pitch_type %in% names(pitch_type_names),
                                      pitch_type_names[pitch_type], pitch_type),
                               ifelse(metric %in% names(metric_labels),
                                      metric_labels[metric], metric)),
              xaxis  = list(title = ifelse(metric %in% names(metric_labels),
                                           metric_labels[metric], metric)),
              yaxis  = list(title = "Density"),
              legend = list(x = 0.7, y = 0.95)
            )
        })
      })
    }
  })
  
  # ---------- Save to database ----------
  observeEvent(input$save_pitcher, {
    req(rv$full_results, input$new_pitcher_id, input$new_pitcher_name,
        nchar(trimws(input$new_pitcher_id)) > 0,
        nchar(trimws(input$new_pitcher_name)) > 0)
    
    pid   <- trimws(input$new_pitcher_id)
    pname <- trimws(input$new_pitcher_name)
    pyear <- input$new_pitcher_year
    
    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), sim_db)
      on.exit(dbDisconnect(con))
      
      existing <- dbGetQuery(con,
                             "SELECT COUNT(*) as n FROM simulation_summary
         WHERE pitcher_id = ? AND year = ? AND level = 'arsenal'",
                             params = list(pid, pyear))$n
      
      if (existing > 0) {
        showNotification(
          sprintf("%s (%s) already exists in the database for %d.",
                  pname, pid, pyear),
          type = "warning", duration = 5)
        return()
      }
      
      kde_res      <- rv$full_results$kde_results
      summary_rows <- do.call(rbind, lapply(names(kde_res), function(pt) {
        do.call(rbind, lapply(names(kde_res[[pt]]), function(metric) {
          k <- kde_res[[pt]][[metric]]
          data.frame(pitcher_id   = pid,
                     year         = pyear,
                     level        = paste0("pitch_", pt),
                     metric       = metric,
                     current_mean = k$mean,
                     sim_mean     = k$mean,
                     delta_mean   = 0,
                     stringsAsFactors = FALSE)
        }))
      }))
      
      dbWriteTable(con, "simulation_summary", summary_rows, append = TRUE)
      
      new_row <- data.frame(pitcher_id  = pid,
                            year        = pyear,
                            player_name = pname,
                            stringsAsFactors = FALSE)
      pitcher_lookup <<- bind_rows(pitcher_lookup, new_row)
      
      new_key      <- paste(pid, pyear, sep = "_")
      pitcher_list <<- bind_rows(
        pitcher_list,
        data.frame(pitcher_id   = pid,
                   year         = pyear,
                   player_name  = pname,
                   key          = new_key,
                   display_name = paste(pname, pyear, sep = " - "),
                   stringsAsFactors = FALSE)
      ) %>% arrange(player_name, desc(year))
      
      updateSelectizeInput(session, "pitcher_select",
                           choices = setNames(pitcher_list$key,
                                              pitcher_list$display_name),
                           server  = TRUE)
      
      showNotification(
        sprintf("%s saved! You can now find them in the Browse tab.", pname),
        type = "message", duration = 6)
      
    }, error = function(e) {
      showNotification(paste("Save failed:", e$message), type = "error", duration = 8)
    })
  })
}

shinyApp(ui, server)

