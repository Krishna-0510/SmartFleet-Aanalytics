# =============================================================================
# SmartFleetAnalytics - Complete Fixed Dashboard
# File: app/app.R
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(highcharter)

# =============================================================================
# 1. DATA
# =============================================================================
set.seed(42)
n <- 5000

fleet_data <- tibble(
  is_delayed               = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)),
  delivery_person_age      = round(runif(n, 22, 60)),
  delivery_person_ratings  = round(runif(n, 3.5, 5), 1),
  distance_km              = round(runif(n, 1, 15), 1),
  order_hour               = sample(0:23, n, replace = TRUE),
  is_weekend               = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)),
  delivery_cost            = round(runif(n, 30, 150), 0),
  efficiency_score         = round(runif(n, 40, 100), 0)
) %>%
  mutate(
    age_group = case_when(
      delivery_person_age < 25 ~ "18-24",
      delivery_person_age < 35 ~ "25-34",
      delivery_person_age < 45 ~ "35-44",
      TRUE                     ~ "45+"
    ),
    rating_group = case_when(
      delivery_person_ratings >= 4.5 ~ "Excellent (4.5-5)",
      delivery_person_ratings >= 4.0 ~ "Good (4.0-4.4)",
      TRUE                           ~ "Needs Improvement (<4.0)"
    ),
    distance_category = case_when(
      distance_km < 3  ~ "Short (<3km)",
      distance_km < 7  ~ "Medium (3-7km)",
      TRUE             ~ "Long (>7km)"
    )
  )

total_deliveries <- nrow(fleet_data)
delay_rate       <- round(mean(fleet_data$is_delayed) * 100, 1)
avg_distance     <- round(mean(fleet_data$distance_km), 1)
avg_rating       <- round(mean(fleet_data$delivery_person_ratings), 1)
on_time_rate     <- 100 - delay_rate
avg_cost         <- round(mean(fleet_data$delivery_cost), 0)

# =============================================================================
# 2. CSS
# =============================================================================
custom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600;700&family=Space+Mono:wght@400;700&display=swap');

  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

  /* ============================================================
     CSS VARIABLES — swap all colors here by theme
  ============================================================ */
  :root {
    --bg-main:       #0f1923;
    --bg-card:       #111c27;
    --bg-sidebar:    #111c27;
    --border:        #1e2d3d;
    --text-primary:  #f1f5f9;
    --text-secondary:#cbd5e1;
    --text-muted:    #64748b;
    --text-label:    #94a3b8;
    --accent:        #38bdf8;
    --input-bg:      #0f1923;
    --input-text:    #e2e8f0;
    --table-row-bg:  transparent;
    --table-hover:   rgba(56,189,248,0.05);
    --scrollbar-bg:  #1e2d3d;
  }

  /* LIGHT THEME overrides — triggered by body.light-mode */
  body.light-mode {
    --bg-main:       #f0f4f8;
    --bg-card:       #ffffff;
    --bg-sidebar:    #1e293b;
    --border:        #e2e8f0;
    --text-primary:  #0f172a;
    --text-secondary:#1e293b;
    --text-muted:    #475569;
    --text-label:    #334155;
    --accent:        #0284c7;
    --input-bg:      #ffffff;
    --input-text:    #0f172a;
    --table-row-bg:  #ffffff;
    --table-hover:   rgba(2,132,199,0.06);
    --scrollbar-bg:  #cbd5e1;
  }

  html, body, .wrapper {
    font-family: 'DM Sans', sans-serif !important;
    background: var(--bg-main) !important;
    color: var(--text-primary) !important;
    overflow-x: hidden;
  }

  /* ── THEME TOGGLE BUTTON ── */
  #theme_toggle_btn {
    position: fixed;
    top: 10px;
    right: 16px;
    z-index: 9999;
    background: var(--bg-card);
    border: 1px solid var(--border);
    color: var(--text-primary);
    border-radius: 20px;
    padding: 5px 14px;
    font-size: 12px;
    font-family: 'DM Sans', sans-serif;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.2s ease;
    display: flex;
    align-items: center;
    gap: 6px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.2);
  }
  #theme_toggle_btn:hover {
    background: var(--accent);
    color: #fff;
    border-color: var(--accent);
  }

  /* ── HEADER — always dark so logo is always readable ── */
  .main-header,
  .main-header .navbar,
  .skin-black .main-header,
  .skin-black .main-header .navbar {
    background: #0f1923 !important;
    background-color: #0f1923 !important;
    border-bottom: 1px solid #1e2d3d !important;
    box-shadow: none !important;
  }
  body.light-mode .main-header,
  body.light-mode .main-header .navbar {
    background: #1e293b !important;
    background-color: #1e293b !important;
    border-bottom: 1px solid #334155 !important;
  }

  /* Logo — ALWAYS visible, hardcoded bright cyan, nothing can override */
  .main-header .logo,
  .main-header .logo span,
  .main-header a.logo,
  .main-header .logo *,
  a.logo span.logo-lg,
  a.logo span.logo-mini,
  .skin-black .main-header .logo,
  .skin-black .main-header a.logo {
    font-family: 'Space Mono', monospace !important;
    font-weight: 700 !important;
    font-size: 15px !important;
    letter-spacing: 2px !important;
    color: #38bdf8 !important;
    -webkit-text-fill-color: #38bdf8 !important;
    text-shadow: 0 0 12px rgba(56,189,248,0.5) !important;
    background: #0f1923 !important;
    background-color: #0f1923 !important;
    width: 260px !important;
    opacity: 1 !important;
    visibility: visible !important;
  }
  /* Light mode logo — still cyan but on lighter dark bg */
  body.light-mode .main-header .logo,
  body.light-mode .main-header a.logo,
  body.light-mode a.logo span.logo-lg {
    color: #38bdf8 !important;
    -webkit-text-fill-color: #38bdf8 !important;
    background: #1e293b !important;
    background-color: #1e293b !important;
  }

  /* Hamburger */
  .main-header .navbar .sidebar-toggle {
    color: var(--text-label) !important;
    background: transparent !important;
    border: none !important;
    padding: 15px 18px !important;
    font-size: 18px !important;
    float: left !important;
    position: relative !important;
    z-index: 1050 !important;
  }
  .main-header .navbar .sidebar-toggle:hover {
    color: var(--accent) !important;
    background: rgba(56,189,248,0.08) !important;
  }
  .main-header .navbar {
    margin-left: 260px !important;
  }
  .sidebar-collapse .main-header .navbar { margin-left: 50px !important; }
  .sidebar-collapse .content-wrapper     { margin-left: 50px !important; }
  .navbar-custom-menu .navbar-nav > li > a { color: var(--text-label) !important; }

  /* ── SIDEBAR — always dark regardless of theme ── */
  .main-sidebar {
    background: #111c27 !important;
    border-right: 1px solid #1e2d3d !important;
    width: 260px !important;
    box-shadow: none !important;
  }
  .sidebar { padding-top: 10px !important; }
  .sidebar-menu > li > a {
    color: #94a3b8 !important;
    font-size: 13px !important;
    font-weight: 500 !important;
    padding: 12px 20px !important;
    border-left: 3px solid transparent !important;
    transition: all 0.2s ease !important;
    display: flex !important;
    align-items: center !important;
    gap: 10px !important;
  }
  .sidebar-menu > li > a:hover {
    background: rgba(56,189,248,0.08) !important;
    color: #38bdf8 !important;
    border-left-color: #38bdf8 !important;
  }
  .sidebar-menu > li.active > a {
    background: rgba(56,189,248,0.12) !important;
    color: #38bdf8 !important;
    border-left-color: #38bdf8 !important;
    font-weight: 600 !important;
  }
  .sidebar-menu > li > a > i { width: 18px; font-size: 14px; }

  /* Sidebar filter box */
  .sidebar .box {
    background: rgba(56,189,248,0.04) !important;
    border: 1px solid #1e2d3d !important;
    border-radius: 10px !important;
    margin: 12px !important;
  }
  .sidebar .box-header  { border-bottom: 1px solid #1e2d3d !important; }
  .sidebar .box-title   { color: #38bdf8 !important; font-size: 12px !important; }
  .sidebar label        { color: #94a3b8 !important; font-size: 12px !important; font-weight: 500 !important; }
  .sidebar .form-control {
    background: #0f1923 !important;
    border: 1px solid #1e2d3d !important;
    color: #e2e8f0 !important;
    border-radius: 7px !important;
    font-size: 12px !important;
  }
  .sidebar .form-control:focus { border-color: #38bdf8 !important; box-shadow: none !important; }
  .sidebar .irs-bar, .sidebar .irs-bar-edge { background: #38bdf8 !important; border-color: #38bdf8 !important; }
  .sidebar .irs-slider  { background: #38bdf8 !important; border: 2px solid #0f1923 !important; }
  .sidebar .irs-from, .sidebar .irs-to, .sidebar .irs-single { background: #38bdf8 !important; border-radius: 4px !important; }

  /* ── CONTENT ── */
  .content-wrapper {
    background: var(--bg-main) !important;
    margin-left: 260px !important;
    padding: 24px !important;
    min-height: 100vh !important;
  }

  /* ── KPI CARDS ── */
  .kpi-card {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 12px;
    padding: 20px 22px;
    display: flex;
    align-items: center;
    gap: 16px;
    transition: border-color 0.2s, box-shadow 0.2s;
    margin-bottom: 20px;
    min-height: 100px;
  }
  .kpi-card:hover {
    border-color: var(--accent);
    box-shadow: 0 0 0 1px rgba(56,189,248,0.15), 0 8px 24px rgba(0,0,0,0.15);
  }
  .kpi-icon {
    width: 48px; height: 48px;
    border-radius: 10px;
    display: flex; align-items: center; justify-content: center;
    font-size: 20px;
    flex-shrink: 0;
  }
  .kpi-icon.blue  { background: rgba(56,189,248,0.12);  color: #38bdf8; }
  .kpi-icon.green { background: rgba(52,211,153,0.12);  color: #34d399; }
  .kpi-icon.amber { background: rgba(251,191,36,0.12);  color: #fbbf24; }
  .kpi-icon.rose  { background: rgba(251,113,133,0.12); color: #fb7185; }
  .kpi-icon.purple{ background: rgba(167,139,250,0.12); color: #a78bfa; }
  .kpi-text { flex: 1; min-width: 0; }
  .kpi-value {
    font-family: 'Space Mono', monospace;
    font-size: 24px;
    font-weight: 700;
    color: var(--text-primary);
    line-height: 1.2;
    white-space: nowrap;
  }
  .kpi-label {
    font-size: 12px;
    color: var(--text-muted);
    font-weight: 500;
    margin-top: 3px;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  /* ── BOXES ── */
  .box {
    background: var(--bg-card) !important;
    border: 1px solid var(--border) !important;
    border-radius: 12px !important;
    box-shadow: none !important;
    margin-bottom: 20px !important;
  }
  .box .box-header {
    background: transparent !important;
    border-bottom: 1px solid var(--border) !important;
    padding: 14px 18px !important;
    border-radius: 12px 12px 0 0 !important;
  }
  .box .box-title {
    font-size: 13px !important;
    font-weight: 600 !important;
    color: var(--text-secondary) !important;
    letter-spacing: 0.3px !important;
  }
  .box .box-body { padding: 16px !important; background: transparent !important; }
  .box.box-primary > .box-header { border-top: 2px solid #38bdf8 !important; }
  .box.box-info    > .box-header { border-top: 2px solid #818cf8 !important; }
  .box.box-success > .box-header { border-top: 2px solid #34d399 !important; }
  .box.box-warning > .box-header { border-top: 2px solid #fbbf24 !important; }
  .box.box-danger  > .box-header { border-top: 2px solid #fb7185 !important; }

  /* ── BUTTONS ── */
  .btn-primary {
    background: var(--accent) !important;
    border: none !important;
    color: #ffffff !important;
    font-weight: 700 !important;
    border-radius: 8px !important;
    padding: 10px 28px !important;
    font-size: 13px !important;
    letter-spacing: 0.5px !important;
    transition: all 0.2s ease !important;
  }
  .btn-primary:hover { opacity: 0.85 !important; transform: translateY(-1px) !important; }

  /* ── DATATABLES ── */
  .dataTables_wrapper { color: var(--text-label) !important; }
  table.dataTable       { background: var(--table-row-bg) !important; color: var(--text-secondary) !important; }
  table.dataTable thead th {
    background: var(--bg-main) !important;
    color: var(--text-muted) !important;
    border-bottom: 1px solid var(--border) !important;
    font-size: 11px !important;
    text-transform: uppercase !important;
    letter-spacing: 0.5px !important;
    font-weight: 600 !important;
  }
  table.dataTable tbody tr  { background: var(--table-row-bg) !important; border-bottom: 1px solid var(--border) !important; }
  table.dataTable tbody td  { color: var(--text-secondary) !important; }
  table.dataTable tbody tr:hover { background: var(--table-hover) !important; }
  .dataTables_filter input, .dataTables_length select {
    background: var(--input-bg) !important;
    border: 1px solid var(--border) !important;
    color: var(--input-text) !important;
    border-radius: 6px !important;
    padding: 4px 8px !important;
  }
  .dataTables_info, .dataTables_paginate { color: var(--text-muted) !important; font-size: 12px !important; }
  .paginate_button       { color: var(--text-muted) !important; border-radius: 5px !important; }
  .paginate_button.current { background: var(--accent) !important; color: #fff !important; border: none !important; }

  /* ── FORM CONTROLS ── */
  .form-control {
    background: var(--input-bg) !important;
    border: 1px solid var(--border) !important;
    color: var(--input-text) !important;
    border-radius: 8px !important;
  }
  .form-control:focus { border-color: var(--accent) !important; box-shadow: none !important; }
  /* Main content labels (NOT sidebar) */
  .content-wrapper label { color: var(--text-label) !important; font-size: 13px !important; font-weight: 500 !important; }

  /* ── TABS ── */
  .nav-tabs { border-bottom: 1px solid var(--border) !important; }
  .nav-tabs > li > a { color: var(--text-muted) !important; border: none !important; border-radius: 0 !important; padding: 8px 16px !important; font-size: 13px !important; }
  .nav-tabs > li > a:hover { background: transparent !important; color: var(--accent) !important; border-bottom: 2px solid var(--accent) !important; }
  .nav-tabs > li.active > a { color: var(--accent) !important; border-bottom: 2px solid var(--accent) !important; background: transparent !important; }

  /* ── PAGE TITLE ── */
  .content-header h1 { color: var(--text-primary) !important; font-size: 18px !important; font-weight: 600 !important; }
  .breadcrumb { background: transparent !important; }
  .breadcrumb > li, .breadcrumb > li a { color: var(--text-muted) !important; font-size: 12px !important; }

  /* ── SCROLLBAR ── */
  ::-webkit-scrollbar { width: 6px; height: 6px; }
  ::-webkit-scrollbar-track { background: var(--bg-main); }
  ::-webkit-scrollbar-thumb { background: var(--scrollbar-bg); border-radius: 3px; }
  ::-webkit-scrollbar-thumb:hover { background: var(--accent); }

  /* ── SELECT DROPDOWNS (shinydashboard selectInput) ── */
  .selectize-input {
    background: var(--input-bg) !important;
    border: 1px solid var(--border) !important;
    color: var(--input-text) !important;
    border-radius: 7px !important;
  }
  .selectize-dropdown {
    background: var(--bg-card) !important;
    border: 1px solid var(--border) !important;
    color: var(--text-secondary) !important;
  }
  .selectize-dropdown .option:hover,
  .selectize-dropdown .active { background: var(--accent) !important; color: #fff !important; }

  /* ── IRS SLIDER (main content) ── */
  .content-wrapper .irs-single,
  .content-wrapper .irs-from,
  .content-wrapper .irs-to    { background: var(--accent) !important; color: #fff !important; }
  .content-wrapper .irs-bar   { background: var(--accent) !important; }
  .content-wrapper .irs-line  { background: var(--border) !important; }
  .content-wrapper .irs-min,
  .content-wrapper .irs-max   { color: var(--text-muted) !important; background: transparent !important; }

  /* ── NUMERIC INPUT arrows ── */
  .content-wrapper input[type=number] {
    background: var(--input-bg) !important;
    color: var(--input-text) !important;
    border: 1px solid var(--border) !important;
    border-radius: 8px !important;
  }
"

# =============================================================================
# 3. HELPER: kpi card HTML
# =============================================================================
kpi_card <- function(value, label, icon_class, color_class) {
  div(class = "kpi-card",
    div(class = paste("kpi-icon", color_class), icon(icon_class)),
    div(class = "kpi-text",
      div(class = "kpi-value", value),
      div(class = "kpi-label", label)
    )
  )
}

# =============================================================================
# 4. UI
# =============================================================================
ui <- dashboardPage(
  title = "SmartFleet Analytics",
  skin = "black",

  dashboardHeader(
    title = tags$a(
      href = "#",
      style = "color: #38bdf8 !important; -webkit-text-fill-color: #38bdf8 !important;
               font-family: 'Space Mono', monospace !important; font-weight: 700 !important;
               font-size: 15px !important; letter-spacing: 2px !important;
               text-decoration: none !important; display: block;
               text-shadow: 0 0 10px rgba(56,189,248,0.4);",
      "⚡ SMARTFLEET"
    ),
    titleWidth = 260,
    tags$li(class = "dropdown",
      tags$style(HTML(custom_css))
    ),
    tags$li(class = "dropdown",
      tags$button(
        id = "theme_toggle_btn",
        onclick = "
          var body = document.body;
          if (body.classList.contains('light-mode')) {
            body.classList.remove('light-mode');
            this.innerHTML = '☀️ Light Mode';
          } else {
            body.classList.add('light-mode');
            this.innerHTML = '🌙 Dark Mode';
          }
        ",
        "☀️ Light Mode"
      )
    )
  ),

  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard",   tabName = "dashboard", icon = icon("gauge-high"),  selected = TRUE),
      menuItem("Drivers",     tabName = "drivers",   icon = icon("users")),
      menuItem("Routes",      tabName = "routes",    icon = icon("route")),
      menuItem("Predict",     tabName = "predict",   icon = icon("brain")),
      menuItem("Data",        tabName = "data",      icon = icon("table"))
    ),
    br(),
    box(
      title = "Filters", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, collapsed = FALSE, width = 12,
      selectInput("age_filter",    "Age Group:", choices = c("All", "18-24","25-34","35-44","45+"), selected = "All"),
      selectInput("rating_filter", "Rating:",    choices = c("All", "Excellent (4.5-5)","Good (4.0-4.4)","Needs Improvement (<4.0)"), selected = "All"),
      sliderInput("hour_filter",   "Hours:", min = 0, max = 23, value = c(0, 23)),
      prettySwitch("weekend_filter", "Weekends only", status = "primary", fill = TRUE)
    )
  ),

  dashboardBody(
    tabItems(

      # ── DASHBOARD ──────────────────────────────────────────────────────────
      tabItem(tabName = "dashboard",
        fluidRow(
          column(width = 3, uiOutput("kpi_deliveries")),
          column(width = 3, uiOutput("kpi_ontime")),
          column(width = 3, uiOutput("kpi_rating")),
          column(width = 3, uiOutput("kpi_distance"))
        ),
        fluidRow(
          column(width = 8,
            box(title = "Hourly Performance", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = NULL,
                plotlyOutput("hourly_chart", height = "380px"))
          ),
          column(width = 4,
            box(title = "Delay by Distance", status = "info", solidHeader = TRUE,
                width = NULL,
                plotlyOutput("distance_metrics", height = "380px"))
          )
        ),
        fluidRow(
          column(width = 6,
            box(title = "Performance Heatmap", status = "warning", solidHeader = TRUE,
                width = NULL,
                plotlyOutput("driver_heatmap", height = "340px"))
          ),
          column(width = 6,
            box(title = "Delay Trend", status = "success", solidHeader = TRUE,
                width = NULL,
                highchartOutput("trend_chart", height = "340px"))
          )
        )
      ),

      # ── DRIVERS ────────────────────────────────────────────────────────────
      tabItem(tabName = "drivers",
        fluidRow(
          column(width = 12,
            box(title = "Driver Performance Summary", status = "primary", solidHeader = TRUE,
                width = NULL,
                DTOutput("driver_table"))
          )
        ),
        fluidRow(
          column(width = 6,
            box(title = "Rating Distribution", status = "info", solidHeader = TRUE,
                width = NULL,
                plotlyOutput("rating_dist", height = "360px"))
          ),
          column(width = 6,
            box(title = "Age Group Analysis", status = "warning", solidHeader = TRUE,
                width = NULL,
                plotlyOutput("age_performance", height = "360px"))
          )
        )
      ),

      # ── ROUTES ─────────────────────────────────────────────────────────────
      tabItem(tabName = "routes",
        fluidRow(
          column(width = 12,
            box(title = "Distance vs Delay Rate", status = "success", solidHeader = TRUE,
                width = NULL,
                plotlyOutput("distance_delay", height = "420px"))
          )
        ),
        fluidRow(
          column(width = 6,
            box(title = "Order Volume by Hour", status = "danger", solidHeader = TRUE,
                width = NULL,
                plotlyOutput("peak_hours", height = "340px"))
          ),
          column(width = 6,
            box(title = "Volume by Distance Category", status = "info", solidHeader = TRUE,
                width = NULL,
                plotlyOutput("volume_distance", height = "340px"))
          )
        )
      ),

      # ── PREDICT ────────────────────────────────────────────────────────────
      tabItem(tabName = "predict",
        fluidRow(
          column(width = 12,
            box(title = "Delay Risk Predictor", status = "primary", solidHeader = TRUE,
                width = NULL,
                fluidRow(
                  column(3, numericInput("pred_age",      "Driver Age",      value = 30,  min = 18, max = 70)),
                  column(3, numericInput("pred_rating",   "Driver Rating",   value = 4.2, min = 1,  max = 5,  step = 0.1)),
                  column(3, numericInput("pred_distance", "Distance (km)",   value = 5,   min = 0.5,max = 30)),
                  column(3, numericInput("pred_hour",     "Order Hour (0-23)",value = 14,  min = 0,  max = 23))
                ),
                br(),
                fluidRow(
                  column(12, align = "center",
                    actionButton("predict_btn", "Run Prediction", class = "btn-primary"),
                    br(), br(),
                    uiOutput("prediction_box")
                  )
                )
            )
          )
        ),
        fluidRow(
          column(width = 12,
            box(title = "Factor Importance", status = "info", solidHeader = TRUE,
                width = NULL,
                plotlyOutput("factor_importance", height = "360px"))
          )
        )
      ),

      # ── DATA ───────────────────────────────────────────────────────────────
      tabItem(tabName = "data",
        fluidRow(
          column(width = 12,
            box(title = "Raw Data Explorer (first 200 rows)", status = "primary", solidHeader = TRUE,
                width = NULL,
                DTOutput("data_table"))
          )
        )
      )
    )
  )
)

# =============================================================================
# 5. SERVER
# =============================================================================
server <- function(input, output, session) {

  # ── Reactive filter ────────────────────────────────────────────────────────
  filtered <- reactive({
    d <- fleet_data
    if (input$age_filter    != "All") d <- d %>% filter(age_group    == input$age_filter)
    if (input$rating_filter != "All") d <- d %>% filter(rating_group == input$rating_filter)
    d <- d %>% filter(order_hour >= input$hour_filter[1], order_hour <= input$hour_filter[2])
    if (input$weekend_filter) d <- d %>% filter(is_weekend == TRUE)
    d
  })

  # ── KPI cards ─────────────────────────────────────────────────────────────
  output$kpi_deliveries <- renderUI({
    kpi_card(format(nrow(filtered()), big.mark = ","), "Total Deliveries", "truck",      "blue")
  })
  output$kpi_ontime <- renderUI({
    rate <- round((1 - mean(filtered()$is_delayed)) * 100, 1)
    kpi_card(paste0(rate, "%"), "On-Time Rate", "circle-check", "green")
  })
  output$kpi_rating <- renderUI({
    kpi_card(paste0(round(mean(filtered()$delivery_person_ratings), 1), " ★"), "Avg Rating", "star", "amber")
  })
  output$kpi_distance <- renderUI({
    kpi_card(paste0(round(mean(filtered()$distance_km), 1), " km"), "Avg Distance", "road", "rose")
  })

  # ── Plot theme helper ──────────────────────────────────────────────────────
  dark_layout <- function(title_text = "") {
    list(
      title      = list(text = title_text, font = list(family = "DM Sans", size = 14, color = "#cbd5e1")),
      plot_bgcolor  = "transparent",
      paper_bgcolor = "transparent",
      font       = list(family = "DM Sans", size = 12, color = "#94a3b8"),
      xaxis      = list(gridcolor = "#1e2d3d", zerolinecolor = "#1e2d3d", color = "#64748b"),
      yaxis      = list(gridcolor = "#1e2d3d", zerolinecolor = "#1e2d3d", color = "#64748b"),
      margin     = list(l = 55, r = 20, t = 40, b = 50),
      legend     = list(font = list(color = "#94a3b8"), bgcolor = "transparent")
    )
  }

  # ── Hourly chart ──────────────────────────────────────────────────────────
  output$hourly_chart <- renderPlotly({
    d <- filtered() %>%
      group_by(order_hour) %>%
      summarise(delay_rate = mean(is_delayed) * 100,
                avg_distance = mean(distance_km), .groups = "drop")

    ly <- dark_layout()
    plot_ly() %>%
      add_trace(data = d, x = ~order_hour, y = ~delay_rate,
                type = "scatter", mode = "lines+markers", name = "Delay Rate (%)",
                line   = list(color = "#fb7185", width = 3),
                marker = list(size = 7, color = "#fb7185", line = list(color = "#0f1923", width = 2)),
                hovertemplate = "<b>%{x}:00</b><br>Delay: %{y:.1f}%<extra></extra>") %>%
      add_trace(data = d, x = ~order_hour, y = ~avg_distance,
                type = "scatter", mode = "lines+markers", name = "Avg Distance (km)",
                line   = list(color = "#38bdf8", width = 3, dash = "dot"),
                marker = list(size = 7, color = "#38bdf8", line = list(color = "#0f1923", width = 2)),
                yaxis  = "y2",
                hovertemplate = "<b>%{x}:00</b><br>Distance: %{y:.1f} km<extra></extra>") %>%
      layout(
        xaxis      = modifyList(ly$xaxis, list(title = "Hour of Day", dtick = 2)),
        yaxis      = modifyList(ly$yaxis, list(title = "Delay Rate (%)", range = c(0, 100))),
        yaxis2     = list(title = "Avg Distance (km)", overlaying = "y", side = "right",
                          gridcolor = "transparent", color = "#64748b"),
        hovermode  = "x unified",
        plot_bgcolor  = ly$plot_bgcolor,
        paper_bgcolor = ly$paper_bgcolor,
        font   = ly$font,
        margin = ly$margin,
        legend = ly$legend,
        showlegend = TRUE
      )
  })

  # ── Distance metrics ──────────────────────────────────────────────────────
  output$distance_metrics <- renderPlotly({
    d <- filtered() %>%
      group_by(distance_category) %>%
      summarise(delay_rate = mean(is_delayed) * 100, .groups = "drop")

    ly <- dark_layout()
    plot_ly(d, x = ~distance_category, y = ~delay_rate, type = "bar",
            marker = list(color = c("#38bdf8","#818cf8","#34d399"),
                          line  = list(color = "#0f1923", width = 1.5)),
            text = ~paste0(round(delay_rate, 1), "%"), textposition = "outside",
            textfont = list(color = "#cbd5e1", size = 12),
            hovertemplate = "<b>%{x}</b><br>Delay: %{y:.1f}%<extra></extra>") %>%
      layout(
        xaxis = modifyList(ly$xaxis, list(title = "")),
        yaxis = modifyList(ly$yaxis, list(title = "Delay Rate (%)")),
        plot_bgcolor  = ly$plot_bgcolor,
        paper_bgcolor = ly$paper_bgcolor,
        font   = ly$font,
        margin = ly$margin,
        showlegend = FALSE
      )
  })

  # ── Heatmap ───────────────────────────────────────────────────────────────
  output$driver_heatmap <- renderPlotly({
    d <- filtered() %>%
      group_by(age_group, rating_group) %>%
      summarise(delay_rate = mean(is_delayed) * 100, .groups = "drop")

    ly <- dark_layout()
    plot_ly(d, x = ~age_group, y = ~rating_group, z = ~delay_rate,
            type = "heatmap", colorscale = list(c(0,"#0f3460"), c(0.5,"#38bdf8"), c(1,"#fb7185")),
            hovertemplate = "Age: <b>%{x}</b><br>Rating: <b>%{y}</b><br>Delay: %{z:.1f}%<extra></extra>") %>%
      layout(
        xaxis = modifyList(ly$xaxis, list(title = "Age Group")),
        yaxis = modifyList(ly$yaxis, list(title = "")),
        plot_bgcolor  = ly$plot_bgcolor,
        paper_bgcolor = ly$paper_bgcolor,
        font   = ly$font,
        margin = list(l = 200, r = 20, t = 30, b = 50)
      )
  })

  # ── Trend chart ───────────────────────────────────────────────────────────
  output$trend_chart <- renderHighchart({
    d <- filtered() %>%
      group_by(order_hour) %>%
      summarise(delay_rate = mean(is_delayed) * 100, .groups = "drop")

    highchart() %>%
      hc_chart(backgroundColor = "transparent",
               style = list(fontFamily = "DM Sans")) %>%
      hc_title(text = "", style = list(color = "#cbd5e1")) %>%
      hc_xAxis(categories = paste0(d$order_hour, ":00"),
               labels = list(style = list(color = "#64748b")),
               gridLineColor = "#1e2d3d", lineColor = "#1e2d3d") %>%
      hc_yAxis(title = list(text = "Delay Rate (%)", style = list(color = "#64748b")),
               labels = list(style = list(color = "#64748b")),
               gridLineColor = "#1e2d3d") %>%
      hc_add_series(data = round(d$delay_rate, 1), name = "Delay Rate",
                    color = "#34d399", lineWidth = 3) %>%
      hc_plotOptions(line = list(marker = list(enabled = TRUE, radius = 4,
                                               fillColor = "#34d399",
                                               lineColor = "#0f1923", lineWidth = 2))) %>%
      hc_tooltip(backgroundColor = "#111c27", borderColor = "#1e2d3d",
                 style = list(color = "#e2e8f0")) %>%
      hc_legend(itemStyle = list(color = "#94a3b8"))
  })

  # ── Driver table ──────────────────────────────────────────────────────────
  output$driver_table <- renderDT({
    d <- filtered() %>%
      group_by(`Age Group` = age_group, `Rating Group` = rating_group) %>%
      summarise(
        Deliveries   = n(),
        `Delay %`    = paste0(round(mean(is_delayed) * 100, 1), "%"),
        `Avg Rating` = round(mean(delivery_person_ratings), 2),
        `Avg Dist`   = paste0(round(mean(distance_km), 1), " km"),
        `Avg Cost`   = paste0("₹", round(mean(delivery_cost), 0)),
        .groups = "drop"
      )
    datatable(d, options = list(pageLength = 10, dom = "frtip",
                                 initComplete = JS("function(s,d,n){$(d.nTable()).css('color','#cbd5e1');}")),
              class = "display", rownames = FALSE)
  })

  # ── Rating dist ───────────────────────────────────────────────────────────
  output$rating_dist <- renderPlotly({
    d <- filtered() %>% count(rating_group)
    ly <- dark_layout()
    plot_ly(d, labels = ~rating_group, values = ~n, type = "pie",
            marker = list(colors = c("#34d399","#38bdf8","#fb7185"),
                          line   = list(color = "#0f1923", width = 2)),
            textposition = "inside", textinfo = "percent+label",
            hovertemplate = "<b>%{label}</b><br>Count: %{value}<extra></extra>") %>%
      layout(plot_bgcolor = ly$plot_bgcolor, paper_bgcolor = ly$paper_bgcolor,
             font = ly$font, margin = list(t = 20), showlegend = TRUE, legend = ly$legend)
  })

  # ── Age performance ───────────────────────────────────────────────────────
  output$age_performance <- renderPlotly({
    d <- filtered() %>%
      group_by(age_group) %>%
      summarise(delay_rate  = mean(is_delayed) * 100,
                avg_rating  = mean(delivery_person_ratings), .groups = "drop")

    ly <- dark_layout()
    plot_ly() %>%
      add_trace(data = d, x = ~age_group, y = ~delay_rate, type = "bar", name = "Delay %",
                marker = list(color = "#fb7185", line = list(color = "#0f1923", width = 1.5)),
                hovertemplate = "<b>%{x}</b><br>Delay: %{y:.1f}%<extra></extra>") %>%
      add_trace(data = d, x = ~age_group, y = ~avg_rating * 20,
                type = "scatter", mode = "lines+markers", name = "Rating (×20)",
                line   = list(color = "#38bdf8", width = 3),
                marker = list(size = 9, color = "#38bdf8", line = list(color = "#0f1923", width = 2)),
                yaxis  = "y2",
                hovertemplate = "<b>%{x}</b><br>Rating: %{y:.2f}<extra></extra>") %>%
      layout(
        xaxis = modifyList(ly$xaxis, list(title = "Age Group")),
        yaxis = modifyList(ly$yaxis, list(title = "Delay Rate (%)")),
        yaxis2 = list(title = "Avg Rating", overlaying = "y", side = "right",
                      gridcolor = "transparent", color = "#64748b"),
        plot_bgcolor  = ly$plot_bgcolor,
        paper_bgcolor = ly$paper_bgcolor,
        font   = ly$font,
        margin = ly$margin,
        legend = ly$legend
      )
  })

  # ── Distance delay ────────────────────────────────────────────────────────
  output$distance_delay <- renderPlotly({
    d <- filtered() %>%
      mutate(dist_bucket = cut(distance_km, breaks = seq(0, 16, 2), include.lowest = TRUE)) %>%
      group_by(dist_bucket) %>%
      summarise(delay_rate = mean(is_delayed) * 100, count = n(), .groups = "drop") %>%
      filter(!is.na(dist_bucket))

    ly <- dark_layout()
    plot_ly(d, x = ~dist_bucket, y = ~delay_rate, type = "scatter", mode = "lines+markers",
            marker = list(size = ~sqrt(count) * 2.5, color = ~delay_rate,
                          colorscale = list(c(0,"#38bdf8"), c(1,"#fb7185")),
                          showscale = TRUE,
                          line = list(color = "#0f1923", width = 1.5),
                          colorbar = list(title = "Delay %", tickfont = list(color = "#94a3b8"),
                                          titlefont = list(color = "#94a3b8"))),
            line = list(color = "#38bdf8", width = 2.5),
            hovertemplate = "<b>%{x}</b><br>Delay: %{y:.1f}%<extra></extra>") %>%
      layout(
        xaxis = modifyList(ly$xaxis, list(title = "Distance Range (km)")),
        yaxis = modifyList(ly$yaxis, list(title = "Delay Rate (%)")),
        plot_bgcolor  = ly$plot_bgcolor,
        paper_bgcolor = ly$paper_bgcolor,
        font   = ly$font,
        margin = list(l = 65, r = 80, t = 30, b = 60)
      )
  })

  # ── Peak hours ────────────────────────────────────────────────────────────
  output$peak_hours <- renderPlotly({
    d <- filtered() %>% count(order_hour)
    ly <- dark_layout()
    plot_ly(d, x = ~order_hour, y = ~n, type = "bar",
            marker = list(color = ~n, colorscale = list(c(0,"#1e3a5f"), c(1,"#38bdf8")),
                          showscale = FALSE, line = list(color = "#0f1923", width = 1)),
            hovertemplate = "<b>%{x}:00</b><br>Orders: %{y}<extra></extra>") %>%
      layout(
        xaxis = modifyList(ly$xaxis, list(title = "Hour of Day", dtick = 2)),
        yaxis = modifyList(ly$yaxis, list(title = "Orders")),
        plot_bgcolor  = ly$plot_bgcolor,
        paper_bgcolor = ly$paper_bgcolor,
        font = ly$font, margin = ly$margin, showlegend = FALSE
      )
  })

  # ── Volume by distance ────────────────────────────────────────────────────
  output$volume_distance <- renderPlotly({
    d <- filtered() %>% count(distance_category)
    ly <- dark_layout()
    plot_ly(d, x = ~distance_category, y = ~n, type = "bar",
            marker = list(color = c("#38bdf8","#818cf8","#34d399"),
                          line  = list(color = "#0f1923", width = 1.5)),
            text = ~n, textposition = "outside", textfont = list(color = "#cbd5e1"),
            hovertemplate = "<b>%{x}</b><br>Orders: %{y}<extra></extra>") %>%
      layout(
        xaxis = modifyList(ly$xaxis, list(title = "")),
        yaxis = modifyList(ly$yaxis, list(title = "Orders")),
        plot_bgcolor  = ly$plot_bgcolor,
        paper_bgcolor = ly$paper_bgcolor,
        font = ly$font, margin = ly$margin, showlegend = FALSE
      )
  })

  # ── Prediction ────────────────────────────────────────────────────────────
  observeEvent(input$predict_btn, {
    risk_score <- 0
    if (input$pred_rating   < 4.0)               risk_score <- risk_score + 35
    if (input$pred_distance > 7)                 risk_score <- risk_score + 28
    if (input$pred_hour %in% c(8:10, 17:20))     risk_score <- risk_score + 22
    if (input$pred_age      < 25)                risk_score <- risk_score + 10
    if (input$pred_age      > 50)                risk_score <- risk_score + 5

    level  <- if (risk_score >= 50) "HIGH RISK" else if (risk_score >= 25) "MEDIUM RISK" else "LOW RISK"
    color  <- if (risk_score >= 50) "#fb7185"  else if (risk_score >= 25) "#fbbf24"     else "#34d399"
    icon_s <- if (risk_score >= 50) "⚠️"       else if (risk_score >= 25) "🔶"          else "✅"
    msg    <- if (risk_score >= 50) "High delay probability — consider route adjustment or different driver"
              else if (risk_score >= 25) "Moderate risk — monitor closely"
              else "Likely on-time delivery"

    output$prediction_box <- renderUI({
      div(style = paste0("text-align:center; padding:24px; background:#111c27;
                          border:1px solid ", color, "30; border-radius:12px; margin-top:10px;"),
        div(style = "font-size:14px; color:#64748b; margin-bottom:8px; text-transform:uppercase; letter-spacing:1px;",
            "Prediction Result"),
        div(style = paste0("font-family:'Space Mono',monospace; font-size:32px; font-weight:700; color:", color, "; margin-bottom:8px;"),
            paste(icon_s, level)),
        div(style = "font-size:13px; color:#94a3b8;", msg),
        div(style = paste0("margin-top:14px; font-size:12px; color:", color, ";"),
            paste0("Risk Score: ", risk_score, " / 100"))
      )
    })
  })

  # ── Factor importance ─────────────────────────────────────────────────────
  output$factor_importance <- renderPlotly({
    d <- data.frame(
      Factor = c("Driver Rating", "Distance", "Peak Hours", "Driver Age", "Weekend"),
      Score  = c(35, 28, 22, 10, 5)
    ) %>% arrange(Score)

    ly <- dark_layout()
    plot_ly(d, y = ~reorder(Factor, Score), x = ~Score, type = "bar", orientation = "h",
            marker = list(color = ~Score,
                          colorscale = list(c(0,"#1e3a5f"), c(1,"#38bdf8")),
                          showscale = FALSE,
                          line = list(color = "#0f1923", width = 1.5)),
            text = ~Score, textposition = "outside", textfont = list(color = "#cbd5e1"),
            hovertemplate = "<b>%{y}</b><br>Score: %{x}<extra></extra>") %>%
      layout(
        xaxis = modifyList(ly$xaxis, list(title = "Importance Score", range = c(0, 45))),
        yaxis = modifyList(ly$yaxis, list(title = "")),
        plot_bgcolor  = ly$plot_bgcolor,
        paper_bgcolor = ly$paper_bgcolor,
        font   = ly$font,
        margin = list(l = 140, r = 60, t = 20, b = 50),
        showlegend = FALSE
      )
  })

  # ── Data table ────────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    datatable(
      filtered() %>% head(200) %>%
        select(Age = delivery_person_age, Rating = delivery_person_ratings,
               Distance = distance_km, Hour = order_hour,
               Weekend = is_weekend, Delayed = is_delayed,
               Cost = delivery_cost, Efficiency = efficiency_score,
               `Age Group` = age_group, `Rating Group` = rating_group),
      options  = list(pageLength = 25, scrollX = TRUE, dom = "frtip"),
      class    = "display",
      rownames = FALSE
    )
  })
}

# =============================================================================
# 6. RUN
# =============================================================================
shinyApp(ui = ui, server = server)