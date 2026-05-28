# ============================================================================
# SmartFleetAnalytics - Dashboard Data Preparation Script
# ============================================================================
# Purpose: Aggregate and shape all feature datasets into dashboard-ready files
# Input:   All processed .rds files from 03_feature_engineering.R
#          Models from 04_model_training.R
# Output:  data/processed/dashboard_data.rds   — master dashboard object
#          data/processed/dashboard_data.csv   — flat export
#          data/processed/kpi_summary.rds      — top-level KPI cards
#          data/processed/time_series_data.rds — charts over time
#          data/processed/ml_ready.rds         — live prediction input template
# ============================================================================

library(tidyverse)
library(lubridate)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("🎨 DASHBOARD DATA PREPARATION\n")
cat(strrep("=", 70), "\n\n", sep = "")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# 1. LOAD ALL FEATURE DATASETS
# ============================================================================

cat("📂 Loading processed feature datasets...\n\n")

load_rds_or_csv <- function(stem) {
  rds_path <- paste0("data/processed/", stem, ".rds")
  csv_path <- paste0("data/processed/", stem, ".csv")
  if (file.exists(rds_path)) {
    readRDS(rds_path)
  } else if (file.exists(csv_path)) {
    read.csv(csv_path, stringsAsFactors = FALSE)
  } else {
    stop(paste("File not found:", stem, "— run 03_feature_engineering.R first."))
  }
}

data_features     <- load_rds_or_csv("data_features")
partner_features  <- load_rds_or_csv("partner_features")
category_features <- load_rds_or_csv("category_features")
daily_features    <- load_rds_or_csv("daily_features")
hourly_features   <- load_rds_or_csv("hourly_features")
weekly_features   <- load_rds_or_csv("weekly_features")
ml_features       <- load_rds_or_csv("ml_features")

cat("✅ Loaded 7 feature datasets\n\n")

# Load model metadata if available
model_meta <- tryCatch(
  readRDS("model/model_metadata.rds"),
  error = function(e) NULL
)
if (!is.null(model_meta)) cat("✅ Loaded model metadata\n\n") else
  cat("⚠️  model_metadata.rds not found — model metrics will be placeholder\n\n")

# ============================================================================
# 2. TOP-LEVEL KPI SUMMARY
# ============================================================================

cat("📊 Building KPI summary cards...\n\n")

kpi_summary <- list(

  # Volume
  total_orders        = nrow(data_features),
  total_revenue       = round(sum(data_features$order_value), 2),
  total_deliveries    = nrow(data_features),

  # Delivery performance
  on_time_rate        = round(mean(data_features$on_time_delivery) * 100, 1),
  avg_delivery_time   = round(mean(data_features$actual_delivery_mins), 1),
  avg_delay_mins      = round(mean(data_features$delay_mins), 1),
  delayed_orders      = sum(data_features$is_delayed),
  delay_rate          = round(mean(data_features$is_delayed) * 100, 1),

  # Financial
  avg_order_value     = round(mean(data_features$order_value), 2),
  total_profit        = round(sum(data_features$net_profit), 2),
  avg_profit_margin   = round(mean(data_features$profit_margin), 1),
  avg_cost_per_km     = round(mean(data_features$cost_per_km), 2),

  # Quality
  avg_rating          = round(mean(data_features$rating), 2),
  avg_efficiency      = round(mean(data_features$efficiency_score), 1),
  avg_satisfaction    = round(mean(data_features$satisfaction_index), 1),

  # Partners & Categories
  total_partners      = n_distinct(data_features$delivery_partner),
  total_categories    = n_distinct(data_features$store_category),

  # Date range
  date_from           = min(data_features$order_date),
  date_to             = max(data_features$order_date),

  # Model accuracy (from metadata or placeholder)
  model_accuracy      = if (!is.null(model_meta))
                          round(model_meta$delay_model$accuracy * 100, 1)
                        else NA_real_
)

cat("✅ KPI summary built\n")
cat(sprintf("   Total orders      : %s\n",   format(kpi_summary$total_orders, big.mark = ",")))
cat(sprintf("   Total revenue     : ₹%s\n",  format(round(kpi_summary$total_revenue), big.mark = ",")))
cat(sprintf("   On-time rate      : %.1f%%\n", kpi_summary$on_time_rate))
cat(sprintf("   Avg delivery time : %.1f min\n", kpi_summary$avg_delivery_time))
cat(sprintf("   Avg rating        : %.2f / 5\n\n", kpi_summary$avg_rating))

saveRDS(kpi_summary, "data/processed/kpi_summary.rds")
cat("✅ Saved: data/processed/kpi_summary.rds\n\n")

# ============================================================================
# 3. TIME SERIES DATA  (daily + weekly + hourly)
# ============================================================================

cat("📈 Building time series datasets...\n\n")

# --- 3a. Daily trend (ensure date column is Date type) ---
daily_trend <- daily_features %>%
  mutate(
    date          = as.Date(date),
    weekday       = wday(date, label = TRUE, abbr = FALSE),
    month_label   = format(date, "%b %Y"),
    rolling_avg_orders  = zoo::rollmean(total_orders,  k = 7, fill = NA, align = "right"),
    rolling_avg_revenue = zoo::rollmean(total_revenue, k = 7, fill = NA, align = "right")
  ) %>%
  arrange(date)

# --- 3b. Hourly heatmap (avg across all days) ---
hourly_heatmap <- hourly_features %>%
  group_by(order_hour) %>%
  summarise(
    avg_orders        = round(mean(orders), 1),
    avg_delivery_time = round(mean(avg_delivery_time), 1),
    avg_on_time_pct   = round(mean(on_time_pct), 1),
    avg_revenue       = round(mean(revenue), 2),
    avg_delayed       = round(mean(delayed), 1),
    .groups = "drop"
  ) %>%
  mutate(
    hour_label = sprintf("%02d:00", order_hour),
    peak_flag  = avg_orders >= quantile(avg_orders, 0.75)
  )

# --- 3c. Day-of-week performance ---
dow_performance <- data_features %>%
  mutate(day_of_week = factor(day_of_week,
    levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>%
  group_by(day_of_week) %>%
  summarise(
    total_orders      = n(),
    avg_delivery_time = round(mean(actual_delivery_mins), 1),
    on_time_rate      = round(mean(on_time_delivery) * 100, 1),
    avg_revenue       = round(mean(order_value), 2),
    avg_rating        = round(mean(rating), 2),
    delay_rate        = round(mean(is_delayed) * 100, 1),
    .groups = "drop"
  )

# --- 3d. Monthly trend ---
monthly_trend <- data_features %>%
  mutate(month = floor_date(as.Date(order_date), "month")) %>%
  group_by(month) %>%
  summarise(
    total_orders      = n(),
    total_revenue     = round(sum(order_value), 2),
    total_profit      = round(sum(net_profit), 2),
    on_time_rate      = round(mean(on_time_delivery) * 100, 1),
    avg_delivery_time = round(mean(actual_delivery_mins), 1),
    avg_rating        = round(mean(rating), 2),
    .groups = "drop"
  )

time_series_data <- list(
  daily   = daily_trend,
  hourly  = hourly_heatmap,
  dow     = dow_performance,
  monthly = monthly_trend,
  weekly  = weekly_features
)

saveRDS(time_series_data, "data/processed/time_series_data.rds")
cat("✅ Saved: data/processed/time_series_data.rds\n")
cat(sprintf("   Daily   : %d days\n",   nrow(daily_trend)))
cat(sprintf("   Hourly  : %d hours\n",  nrow(hourly_heatmap)))
cat(sprintf("   DoW     : %d days\n",   nrow(dow_performance)))
cat(sprintf("   Monthly : %d months\n", nrow(monthly_trend)))
cat(sprintf("   Weekly  : %d weeks\n\n", nrow(weekly_features)))

# ============================================================================
# 4. PARTNER ANALYTICS (dashboard tab)
# ============================================================================

cat("👥 Building partner analytics...\n\n")

partner_dashboard <- partner_features %>%
  mutate(
    # Rank partners by quality
    rank = rank(-quality_score, ties.method = "first"),

    # Sparkline data label (for tooltip)
    label = sprintf(
      "%s | Tier: %s | OTR: %.1f%% | Rating: %.2f",
      delivery_partner, partner_tier, on_time_rate, avg_rating
    )
  ) %>%
  arrange(rank)

# Per-partner order history (for drilldown)
partner_history <- data_features %>%
  mutate(order_date = as.Date(order_date)) %>%
  group_by(delivery_partner, order_date) %>%
  summarise(
    daily_orders      = n(),
    daily_revenue     = round(sum(order_value), 2),
    daily_on_time_pct = round(mean(on_time_delivery) * 100, 1),
    daily_avg_delay   = round(mean(delay_mins), 1),
    .groups = "drop"
  )

saveRDS(list(summary = partner_dashboard, history = partner_history),
        "data/processed/partner_dashboard.rds")
cat(sprintf("✅ Saved: partner_dashboard.rds (%d partners)\n\n", nrow(partner_dashboard)))

# ============================================================================
# 5. CATEGORY ANALYTICS (dashboard tab)
# ============================================================================

cat("🏪 Building category analytics...\n\n")

category_dashboard <- category_features %>%
  mutate(
    revenue_share = round(total_revenue / sum(total_revenue) * 100, 1),
    order_share   = round(total_orders  / sum(total_orders)  * 100, 1)
  ) %>%
  arrange(desc(total_revenue))

# Category × traffic breakdown
category_traffic <- data_features %>%
  group_by(store_category, traffic_level) %>%
  summarise(
    orders            = n(),
    avg_delivery_time = round(mean(actual_delivery_mins), 1),
    on_time_pct       = round(mean(on_time_delivery) * 100, 1),
    .groups = "drop"
  )

saveRDS(list(summary = category_dashboard, traffic = category_traffic),
        "data/processed/category_dashboard.rds")
cat(sprintf("✅ Saved: category_dashboard.rds (%d categories)\n\n",
            nrow(category_dashboard)))

# ============================================================================
# 6. ROUTE / DISTANCE ANALYTICS
# ============================================================================

cat("🛣️  Building route analytics...\n\n")

# Distance bucket analysis
route_analysis <- data_features %>%
  mutate(
    distance_bucket = cut(distance_km,
      breaks = c(0, 2, 4, 6, 8, 10, Inf),
      labels = c("0-2 km","2-4 km","4-6 km","6-8 km","8-10 km","10+ km"),
      right  = FALSE
    )
  ) %>%
  group_by(distance_bucket) %>%
  summarise(
    orders            = n(),
    avg_delivery_time = round(mean(actual_delivery_mins), 1),
    avg_cost_per_km   = round(mean(cost_per_km), 2),
    on_time_rate      = round(mean(on_time_delivery) * 100, 1),
    avg_efficiency    = round(mean(efficiency_score), 1),
    delay_rate        = round(mean(is_delayed) * 100, 1),
    .groups = "drop"
  )

# Traffic × delivery time matrix
traffic_matrix <- data_features %>%
  group_by(traffic_level, time_period) %>%
  summarise(
    avg_delivery_time = round(mean(actual_delivery_mins), 1),
    avg_delay_mins    = round(mean(delay_mins), 1),
    on_time_rate      = round(mean(on_time_delivery) * 100, 1),
    orders            = n(),
    .groups = "drop"
  )

saveRDS(list(distance = route_analysis, traffic_matrix = traffic_matrix),
        "data/processed/route_dashboard.rds")
cat("✅ Saved: data/processed/route_dashboard.rds\n\n")

# ============================================================================
# 7. ML PREDICTION INPUT TEMPLATE  (used by Shiny predict tab)
# ============================================================================

cat("🤖 Building ML prediction template...\n\n")

# Feature ranges / levels for UI sliders and dropdowns
ml_ready <- list(
  # Numeric ranges
  distance_range    = range(data_features$distance_km,   na.rm = TRUE),
  value_range       = range(data_features$order_value,   na.rm = TRUE),
  items_range       = range(data_features$num_items,     na.rm = TRUE),
  hour_range        = range(data_features$order_hour,    na.rm = TRUE),

  # Medians as defaults
  distance_median   = median(data_features$distance_km),
  value_median      = median(data_features$order_value),
  items_median      = median(data_features$num_items),

  # Factor levels
  traffic_levels    = c("Low", "Medium", "High"),
  category_levels   = sort(unique(data_features$store_category)),
  day_levels        = c("Monday","Tuesday","Wednesday","Thursday",
                        "Friday","Saturday","Sunday"),
  time_period_levels = c("Early Morning","Morning","Afternoon","Evening","Night"),

  # Historical delay stats (for benchmark display)
  delay_stats = data_features %>%
    group_by(traffic_level) %>%
    summarise(
      avg_delay  = round(mean(delay_mins), 1),
      p75_delay  = round(quantile(delay_mins, 0.75), 1),
      delay_rate = round(mean(is_delayed) * 100, 1),
      .groups    = "drop"
    )
)

saveRDS(ml_ready, "data/processed/ml_ready.rds")
cat("✅ Saved: data/processed/ml_ready.rds\n\n")

# ============================================================================
# 8. MASTER DASHBOARD DATA OBJECT
# ============================================================================

cat("📦 Assembling master dashboard_data object...\n\n")

dashboard_data <- list(
  # Meta
  prepared_at      = Sys.time(),
  date_range       = c(min(data_features$order_date),
                       max(data_features$order_date)),

  # Core
  kpi              = kpi_summary,
  orders           = data_features,

  # Time series
  time_series      = time_series_data,

  # Partner
  partners         = list(
    summary  = partner_dashboard,
    history  = partner_history
  ),

  # Category
  categories       = list(
    summary  = category_dashboard,
    traffic  = category_traffic
  ),

  # Route
  routes           = list(
    distance       = route_analysis,
    traffic_matrix = traffic_matrix
  ),

  # ML
  ml               = ml_ready,
  model_meta       = model_meta
)

saveRDS(dashboard_data, "data/processed/dashboard_data.rds")

# Flat CSV export (orders only — the rest are nested lists)
write.csv(data_features, "data/processed/dashboard_data.csv", row.names = FALSE)

cat("✅ Saved: data/processed/dashboard_data.rds  (master object)\n")
cat("✅ Saved: data/processed/dashboard_data.csv  (flat orders export)\n\n")

# ============================================================================
# 9. SUMMARY
# ============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("✅ DASHBOARD PREP COMPLETE!\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("📁 FILES CREATED:\n")
cat("   data/processed/dashboard_data.rds   — master dashboard object\n")
cat("   data/processed/dashboard_data.csv   — flat orders export\n")
cat("   data/processed/kpi_summary.rds      — top-level KPI cards\n")
cat("   data/processed/time_series_data.rds — daily/hourly/weekly charts\n")
cat("   data/processed/partner_dashboard.rds— partner tab data\n")
cat("   data/processed/category_dashboard.rds- category tab data\n")
cat("   data/processed/route_dashboard.rds  — route tab data\n")
cat("   data/processed/ml_ready.rds         — prediction input template\n\n")

cat("📊 DASHBOARD READY WITH:\n")
cat(sprintf("   %-25s %s\n", "Total orders:",     format(kpi_summary$total_orders, big.mark=",")))
cat(sprintf("   %-25s ₹%s\n", "Total revenue:",   format(round(kpi_summary$total_revenue), big.mark=",")))
cat(sprintf("   %-25s %.1f%%\n", "On-time rate:", kpi_summary$on_time_rate))
cat(sprintf("   %-25s %d\n", "Partners:",         kpi_summary$total_partners))
cat(sprintf("   %-25s %d\n", "Categories:",       kpi_summary$total_categories))
cat(sprintf("   %-25s %s to %s\n", "Date range:",
    as.character(kpi_summary$date_from),
    as.character(kpi_summary$date_to)))
cat("\n")
cat("🚀 Next Step: Open app/app.R and run the Shiny dashboard!\n\n")