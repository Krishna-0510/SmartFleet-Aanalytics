# =============================================================================
# SmartFleetAnalytics - Configuration Settings
# File: config/settings.R
# =============================================================================

# Paths
PATHS <- list(
  raw_data = "data/raw/",
  proc_data = "data/processed/",
  dashboard_data = "data/dashboard/",
  models = "models/",
  logs = "logs/"
)

# App theme colors
COLORS <- list(
  primary = "#2c3e50",
  success = "#27ae60",
  warning = "#f39c12",
  danger = "#e74c3c",
  info = "#3498db",
  delay_high = "#e74c3c",
  delay_medium = "#f39c12",
  delay_low = "#27ae60"
)

# Model parameters
MODEL_PARAMS <- list(
  delay_threshold_mins = 35,
  cost_per_km = 8.5,
  revenue_per_order = 120
)

# Date range for dashboard
DATE_RANGE <- list(
  start = "2023-01-01",
  end = "2024-06-30"
)