#################################################################################################
# Shiny app by Samy Kut
# 11/2/25
# Title: Vehicle & Weather risk dashboard (2021-2023)
#################################################################################################

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(scales)
options(dplyr.summarise.inform = FALSE)

# ------------ 1) READ & PREP (from RDS, no heavy CSVs) ------------------------

req_files <- c(
  "accidents_2021.rds","accidents_2022.rds","accidents_2023.rds",
  "vehicle_2021.rds", "vehicle_2022.rds", "vehicle_2023.rds"
)
missing <- req_files[!file.exists(req_files)]
if (length(missing)) {
  stop(
    "Missing RDS files: ", paste(missing, collapse = ", "),
    ". Run your prep script to create them."
  )
}

# Load cleaned yearly RDS
acc21 <- readRDS("accidents_2021.rds")
acc22 <- readRDS("accidents_2022.rds")
acc23 <- readRDS("accidents_2023.rds")
veh21 <- readRDS("vehicle_2021.rds")
veh22 <- readRDS("vehicle_2022.rds")
veh23 <- readRDS("vehicle_2023.rds")

# Combine + join
acc <- bind_rows(acc21, acc22, acc23)
veh <- bind_rows(veh21, veh22, veh23)
veh_acc <- inner_join(veh, acc, by = c("year","STATE","ST_CASE"))

# Helper vectors for UI (drop NAs)
years_vec   <- sort(unique(acc$year))
weather_vec <- sort(na.omit(unique(acc$weatherlab)))
light_vec   <- sort(na.omit(unique(acc$lightlab)))
vehicle_vec <- sort(na.omit(unique(veh_acc$veh_typelab)))

# Fallbacks in case any vector is empty
if (!length(weather_vec)) weather_vec <- ""
if (!length(light_vec))   light_vec   <- ""
if (!length(vehicle_vec)) vehicle_vec <- ""

# ------------ 2) UI (navbarPage top-level) ------------------------------------

bar_color <- "#18BC9C"   # turquoise
accent    <- "#2C3E50"   # deep slate titles

ui <- navbarPage(
  title = "DriveSafe: Vehicle & Weather Risk Dashboard (FARS 2021–2023)",
  theme = shinytheme("flatly"),
  
  # Custom CSS in header
  header = tags$head(
    tags$style(HTML(sprintf("
      body {
        background: linear-gradient(135deg, #f7fbfc 0%%, #eef6f7 60%%, #e7f3f5 100%%);
      }
      .navbar-default { background-color: #2C3E50; border-color: #2C3E50; }
      .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a { color: #ECF0F1 !important; }

      .well, .panel, .panel-default, .form-control, .table {
        border-radius: 12px !important;
      }
      .panel, .well {
        background: #ffffffcc !important;
        box-shadow: 0 8px 18px rgba(0,0,0,0.06);
        border: 1px solid rgba(0,0,0,0.05);
      }
      h1, h2, h3, h4 { color: %s; font-weight: 600; }
      .help-block { color: #5f6c72; }
    ", accent)))
  ),
  
  tabPanel("By State",
           sidebarLayout(
             sidebarPanel(
               selectInput("year1", "Year", choices = years_vec, selected = max(years_vec)),
               selectInput("veh1",  "Vehicle Type", choices = c("All vehicle types", vehicle_vec)),
               selectInput("wx1",   "Weather", choices = c("All weather", weather_vec)),
               selectInput("lt1",   "Lighting", choices = c("All lighting", light_vec)),
               helpText("Bars show crashes that involve the chosen vehicle type (if selected), under the chosen weather/lighting (if selected).")
             ),
             mainPanel(
               h4(textOutput("title_state")),
               plotOutput("bar_state", height = 480),
               br(),
               h4("Top 10 states"),
               tableOutput("tbl_state")
             )
           )
  ),
  
  tabPanel("Vehicle × Conditions (Nationwide)",
           sidebarLayout(
             sidebarPanel(
               selectInput("year2", "Year", choices = years_vec, selected = max(years_vec)),
               selectInput("wx2",   "Weather", choices = weather_vec, selected = weather_vec[1]),
               selectInput("lt2",   "Lighting", choices = light_vec, selected = light_vec[1]),
               sliderInput("topN", "Show top N vehicle types (by involved crashes)", min = 5, max = 25, value = 10, step = 1)
             ),
             mainPanel(
               h4(textOutput("title_veh_cond")),
               plotOutput("bar_veh_cond", height = 480),
               br(),
               h4("Top types (table)"),
               tableOutput("tbl_veh_cond")
             )
           )
  ),
  
  tabPanel("About",
           fluidPage(
             h4("Notes"),
             tags$ul(
               tags$li("Data: NHTSA FARS 2021–2023."),
               tags$li("Each row in accidents = a fatal crash; FATALS = number of people who died in that crash."),
               tags$li("Vehicle 'involved crashes' counts crashes that include at least one vehicle of the selected type."),
               tags$li("Explore the relationship by filtering vehicle type WITH weather and lighting."),
               tags$li("Preprocessing moved to a separate script; the app loads compact RDS files for speed.")
             )
           )
  )
)

# ------------ 3) SERVER -------------------------------------------------------

server <- function(input, output, session) {
  
  # ---- BY STATE (filterable relation) ----
  by_state_data <- reactive({
    df <- veh_acc %>% filter(year == input$year1)
    
    if (input$veh1 != "All vehicle types") df <- df %>% filter(veh_typelab == input$veh1)
    if (input$wx1  != "All weather")       df <- df %>% filter(weatherlab == input$wx1)
    if (input$lt1  != "All lighting")      df <- df %>% filter(lightlab == input$lt1)
    
    df %>%
      group_by(statelab) %>%
      summarise(
        involved_crashes = n_distinct(ST_CASE),
        fatalities       = sum(FATALS, na.rm = TRUE),
        deaths_per_inv   = round(fatalities / pmax(involved_crashes, 1), 3),
        .groups = "drop"
      ) %>%
      arrange(desc(involved_crashes))
  })
  
  output$title_state <- renderText({
    paste0(
      "States — ", input$year1,
      if (input$veh1 != "All vehicle types") paste0(" • Vehicle: ", input$veh1) else "",
      if (input$wx1  != "All weather")       paste0(" • Weather: ", input$wx1) else "",
      if (input$lt1  != "All lighting")      paste0(" • Lighting: ", input$lt1) else ""
    )
  })
  
  output$bar_state <- renderPlot({
    df <- by_state_data()
    validate(need(nrow(df) > 0, "No data for this selection. Try different filters."))
    ggplot(df, aes(x = reorder(statelab, involved_crashes), y = involved_crashes)) +
      geom_col(fill = bar_color) +
      coord_flip() +
      labs(x = "State", y = "Involved crashes", title = NULL) +
      scale_y_continuous(labels = label_comma()) +
      theme_minimal(base_size = 14)
  })
  
  output$tbl_state <- renderTable({
    by_state_data() %>%
      head(10) %>%
      rename(
        State              = statelab,
        `Involved Crashes` = involved_crashes,
        Fatalities         = fatalities,
        `Deaths/Inv Crash` = deaths_per_inv
      )
  })
  
  # ---- VEHICLE × CONDITIONS (nationwide totals for chosen year/conditions) ----
  veh_cond_data <- reactive({
    veh_acc %>%
      filter(year == input$year2,
             weatherlab == input$wx2,
             lightlab   == input$lt2) %>%
      group_by(veh_typelab) %>%
      summarise(
        involved_crashes = n_distinct(ST_CASE),
        fatalities       = sum(FATALS, na.rm = TRUE),
        deaths_per_inv   = round(fatalities / pmax(involved_crashes, 1), 3),
        .groups = "drop"
      ) %>%
      arrange(desc(involved_crashes))
  })
  
  output$title_veh_cond <- renderText({
    paste0("Vehicle types under ", input$wx2, " • ", input$lt2, " — ", input$year2)
  })
  
  output$bar_veh_cond <- renderPlot({
    df <- veh_cond_data() %>% slice_head(n = input$topN)
    validate(need(nrow(df) > 0, "No data for this selection."))
    ggplot(df, aes(x = reorder(veh_typelab, involved_crashes), y = involved_crashes)) +
      geom_col(fill = bar_color) +
      coord_flip() +
      labs(x = "Vehicle type", y = "Involved crashes", title = NULL) +
      scale_y_continuous(labels = label_comma()) +
      theme_minimal(base_size = 14)
  })
  
  output$tbl_veh_cond <- renderTable({
    veh_cond_data() %>%
      slice_head(n = input$topN) %>%
      rename(
        `Vehicle Type`     = veh_typelab,
        `Involved Crashes` = involved_crashes,
        Fatalities         = fatalities,
        `Deaths/Inv Crash` = deaths_per_inv
      )
  })
}

# IMPORTANT: this must be the last line so the app returns a shiny.appobj
shinyApp(ui, server)

