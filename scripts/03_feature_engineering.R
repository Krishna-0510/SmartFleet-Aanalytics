# ============================================================================
# SmartFleetAnalytics - Feature Engineering Script (CALCULATES DISTANCE)
# ============================================================================

library(tidyverse)
library(lubridate)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("🔧 FEATURE ENGINEERING AND BUSINESS METRICS...\n")
cat(strrep("=", 70), "\n\n", sep = "")

# ============================================================================
# 1. LOAD CLEANED DATA
# ============================================================================

cat("📂 Loading cleaned data...\n\n")

df_orders <- readRDS("data/processed/clean_orders_base.rds")
cat("✅ Loaded clean_orders_base.rds\n")
cat("   Records:", nrow(df_orders), "\n")
cat("   Columns:", ncol(df_orders), "\n\n")

# Load ML data if exists
df_ml <- if(file.exists("data/processed/clean_ml_features.rds")) {
  readRDS("data/processed/clean_ml_features.rds")
  cat("✅ Loaded ML features\n\n")
} else { 
  cat("⚠️  ML features not found\n\n")
  NULL
}

# ============================================================================
# 2. HELPER FUNCTION: HAVERSINE DISTANCE
# ============================================================================

haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  
  # Earth radius in km (6371)
  distance <- 6371 * c
  return(distance)
}

cat("📏 Using Haversine formula for distance calculation\n\n")

# ============================================================================
# 3. CREATE BASE FEATURES
# ============================================================================

cat("📋 Creating individual order features...\n\n")

data_features <- df_orders %>%
  mutate(
    # Calculate distance using Haversine formula
    distance_km = haversine_distance(
      restaurant_latitude, restaurant_longitude,
      delivery_location_latitude, delivery_location_longitude
    ),
    
    # Date features
    order_date = as.Date(order_date),
    year = year(order_date),
    month = month(order_date, label = TRUE),
    quarter = quarter(order_date),
    day_of_week = wday(order_date, label = TRUE),
    is_weekend = day_of_week %in% c("Sat", "Sun"),
    
    # Time features
    order_hour = case_when(
      !is.na(time_ordered) ~ hour(ymd_hms(paste(order_date, time_ordered), quiet = TRUE)),
      TRUE ~ NA_real_
    ),
    
    time_period = case_when(
      order_hour < 6 ~ "Late Night",
      order_hour < 12 ~ "Morning", 
      order_hour < 17 ~ "Afternoon",
      order_hour < 21 ~ "Evening",
      TRUE ~ "Night"
    ),
    
    # Clean categorical columns
    weather_conditions = str_to_lower(str_trim(weather_conditions)),
    road_traffic_density = str_to_lower(str_trim(road_traffic_density)),
    type_of_vehicle = str_to_lower(str_trim(type_of_vehicle)),
    
    # Create numeric traffic level (for ML)
    traffic_level_num = case_when(
      road_traffic_density == "low" ~ 1,
      road_traffic_density == "medium" ~ 2,
      road_traffic_density == "high" ~ 3,
      TRUE ~ 2
    ),
    
    # Weather impact score
    weather_impact = case_when(
      weather_conditions %in% c("rainy", "storm") ~ 80,
      weather_conditions %in% c("foggy", "cloudy") ~ 50,
      TRUE ~ 20
    ),
    
    # Delivery performance metrics
    actual_delivery_mins = delivery_duration_mins,
    
    # Delay calculation (assuming >35 mins is delayed)
    is_delayed = delivery_duration_mins > 35,
    delay_mins = if_else(is_delayed, delivery_duration_mins - 35, 0),
    on_time_delivery = !is_delayed,
    
    # Delivery speed categories
    delivery_speed = case_when(
      delivery_duration_mins <= 20 ~ "Very Fast",
      delivery_duration_mins <= 35 ~ "Fast",
      delivery_duration_mins <= 50 ~ "Normal",
      delivery_duration_mins <= 65 ~ "Slow",
      TRUE ~ "Very Slow"
    ),
    
    # Cost metrics (estimated)
    delivery_cost = distance_km * 8.5,  # ₹8.5 per km
    order_value = delivery_cost * 1.3,  # 30% markup
    profit_margin = (order_value - delivery_cost) / order_value * 100,
    cost_per_km = delivery_cost / distance_km,
    
    # Performance scores
    traffic_impact = case_when(
      road_traffic_density == "high" ~ 80,
      road_traffic_density == "medium" ~ 50,
      TRUE ~ 20
    ),
    
    distance_impact = round((distance_km / max(distance_km, na.rm = TRUE)) * 100, 0),
    
    partner_reliability = delivery_person_ratings * 20,
    
    delivery_risk = round(
      (traffic_impact * 0.3) +
      (distance_impact * 0.3) +
      ((100 - partner_reliability) * 0.2) +
      (if_else(is_weekend, 10, 0))
    ),
    
    efficiency_score = round(
      pmax(pmin(((50 - delay_mins) / 50) * 100, 100), 0)
    ),
    
    satisfaction_index = round((delivery_person_ratings / 5) * 100, 2),
    
    # Rider categories
    rider_experience = case_when(
      delivery_person_age < 25 ~ "Junior",
      delivery_person_age < 35 ~ "Mid",
      delivery_person_age < 50 ~ "Senior",
      TRUE ~ "Veteran"
    ),
    
    rating_category = case_when(
      delivery_person_ratings >= 4.5 ~ "Excellent",
      delivery_person_ratings >= 4.0 ~ "Good",
      delivery_person_ratings >= 3.0 ~ "Average",
      TRUE ~ "Poor"
    )
  )

# Merge ML data if available
if(!is.null(df_ml) && nrow(df_ml) == nrow(data_features)) {
  data_features <- data_features %>%
    bind_cols(df_ml %>% select(-any_of(names(data_features))))
  cat("   ✅ Merged ML features\n")
}

cat("✅ Created individual order features\n")
cat("   Distance range:", round(min(data_features$distance_km, na.rm=TRUE), 2), "to", 
    round(max(data_features$distance_km, na.rm=TRUE), 2), "km\n")
cat("   Avg delivery time:", round(mean(data_features$actual_delivery_mins, na.rm=TRUE), 1), "mins\n")
cat("   Delay rate:", round(mean(data_features$is_delayed, na.rm=TRUE) * 100, 1), "%\n\n")

# ============================================================================
# 4. PARTNER-LEVEL AGGREGATED FEATURES
# ============================================================================

cat("👥 Creating partner-level aggregated features...\n\n")

partner_features <- data_features %>%
  group_by(delivery_person_id) %>%
  summarise(
    total_deliveries = n(),
    avg_delivery_time = mean(actual_delivery_mins, na.rm = TRUE),
    avg_delay_mins = mean(delay_mins, na.rm = TRUE),
    on_time_rate = mean(on_time_delivery, na.rm = TRUE) * 100,
    avg_rating = mean(delivery_person_ratings, na.rm = TRUE),
    avg_distance = mean(distance_km, na.rm = TRUE),
    avg_delivery_cost = mean(delivery_cost, na.rm = TRUE),
    total_revenue = sum(order_value, na.rm = TRUE),
    total_profit = sum(order_value - delivery_cost, na.rm = TRUE),
    avg_efficiency = mean(efficiency_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    partner_tier = case_when(
      on_time_rate >= 90 & avg_rating >= 4.5 ~ "Gold",
      on_time_rate >= 80 & avg_rating >= 4.0 ~ "Silver",
      TRUE ~ "Bronze"
    ),
    quality_score = round(
      (on_time_rate * 0.4) + ((avg_rating / 5) * 100 * 0.4) + (avg_efficiency * 0.2)
    )
  ) %>%
  arrange(desc(quality_score))

cat("✅ Created partner-level features for", nrow(partner_features), "partners\n\n")

# ============================================================================
# 5. WEATHER & TRAFFIC AGGREGATED FEATURES
# ============================================================================

cat("🌤️ Creating weather and traffic aggregates...\n\n")

weather_features <- data_features %>%
  group_by(weather_conditions) %>%
  summarise(
    total_orders = n(),
    avg_delay_mins = mean(delay_mins, na.rm = TRUE),
    delay_rate = mean(is_delayed, na.rm = TRUE) * 100,
    avg_delivery_time = mean(actual_delivery_mins, na.rm = TRUE),
    avg_rating = mean(delivery_person_ratings, na.rm = TRUE),
    .groups = "drop"
  )

traffic_features <- data_features %>%
  group_by(road_traffic_density) %>%
  summarise(
    total_orders = n(),
    avg_delay_mins = mean(delay_mins, na.rm = TRUE),
    delay_rate = mean(is_delayed, na.rm = TRUE) * 100,
    avg_delivery_time = mean(actual_delivery_mins, na.rm = TRUE),
    .groups = "drop"
  )

cat("✅ Created weather and traffic features\n\n")

# ============================================================================
# 6. TIME-BASED AGGREGATED FEATURES
# ============================================================================

cat("⏰ Creating time-based aggregated features...\n\n")

daily_features <- data_features %>%
  group_by(order_date) %>%
  summarise(
    total_orders = n(),
    on_time_rate = mean(on_time_delivery, na.rm = TRUE) * 100,
    avg_delivery_time = mean(actual_delivery_mins, na.rm = TRUE),
    avg_delay_mins = mean(delay_mins, na.rm = TRUE),
    avg_rating = mean(delivery_person_ratings, na.rm = TRUE),
    total_profit = sum(order_value - delivery_cost, na.rm = TRUE),
    delay_rate = mean(is_delayed, na.rm = TRUE) * 100,
    avg_efficiency = mean(efficiency_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(order_date)

hourly_features <- data_features %>%
  filter(!is.na(order_hour)) %>%
  group_by(order_hour) %>%
  summarise(
    total_orders = n(),
    avg_delivery_time = mean(actual_delivery_mins, na.rm = TRUE),
    on_time_pct = mean(on_time_delivery, na.rm = TRUE) * 100,
    delay_rate = mean(is_delayed, na.rm = TRUE) * 100,
    avg_rating = mean(delivery_person_ratings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(order_hour)

weekly_features <- data_features %>%
  mutate(week_start = floor_date(order_date, "week")) %>%
  group_by(week_start) %>%
  summarise(
    total_orders = n(),
    on_time_rate = mean(on_time_delivery, na.rm = TRUE) * 100,
    avg_delivery_time = mean(actual_delivery_mins, na.rm = TRUE),
    total_profit = sum(order_value - delivery_cost, na.rm = TRUE),
    .groups = "drop"
  )

cat("✅ Created time-based features\n\n")

# ============================================================================
# 7. ML MODEL FEATURES
# ============================================================================

cat("🤖 Creating ML model features...\n\n")

ml_features <- data_features %>%
  select(
    # Target variables
    is_delayed,
    delay_mins,
    on_time_delivery,
    
    # Core features
    delivery_person_id,
    delivery_person_age,
    delivery_person_ratings,
    distance_km,
    weather_conditions,
    road_traffic_density,
    type_of_vehicle,
    
    # Time features
    order_hour,
    day_of_week,
    is_weekend,
    month,
    quarter,
    
    # Engineered features
    traffic_level_num,
    weather_impact,
    traffic_impact,
    distance_impact,
    delivery_risk,
    efficiency_score,
    satisfaction_index,
    
    # Business metrics
    delivery_cost,
    profit_margin,
    cost_per_km
  ) %>%
  filter(complete.cases(.))

cat("✅ Created ML model features:", nrow(ml_features), "records x", ncol(ml_features), "cols\n\n")

# ============================================================================
# 8. SAVE ALL FEATURE SETS
# ============================================================================

cat("💾 Saving all feature datasets...\n\n")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

saveRDS(data_features, "data/processed/data_features.rds")
write.csv(data_features, "data/processed/data_features.csv", row.names = FALSE)
cat("✅ data_features.rds & .csv\n")

saveRDS(partner_features, "data/processed/partner_features.rds")
write.csv(partner_features, "data/processed/partner_features.csv", row.names = FALSE)
cat("✅ partner_features.rds & .csv\n")

saveRDS(weather_features, "data/processed/weather_features.rds")
write.csv(weather_features, "data/processed/weather_features.csv", row.names = FALSE)
cat("✅ weather_features.rds & .csv\n")

saveRDS(traffic_features, "data/processed/traffic_features.rds")
write.csv(traffic_features, "data/processed/traffic_features.csv", row.names = FALSE)
cat("✅ traffic_features.rds & .csv\n")

saveRDS(daily_features, "data/processed/daily_features.rds")
write.csv(daily_features, "data/processed/daily_features.csv", row.names = FALSE)
cat("✅ daily_features.rds & .csv\n")

saveRDS(hourly_features, "data/processed/hourly_features.rds")
write.csv(hourly_features, "data/processed/hourly_features.csv", row.names = FALSE)
cat("✅ hourly_features.rds & .csv\n")

saveRDS(weekly_features, "data/processed/weekly_features.rds")
write.csv(weekly_features, "data/processed/weekly_features.csv", row.names = FALSE)
cat("✅ weekly_features.rds & .csv\n")

saveRDS(ml_features, "data/processed/ml_features.rds")
write.csv(ml_features, "data/processed/ml_features.csv", row.names = FALSE)
cat("✅ ml_features.rds & .csv\n\n")

# ============================================================================
# 9. SUMMARY
# ============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("✅ FEATURE ENGINEERING COMPLETE!\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("📊 FEATURE ENGINEERING SUMMARY:\n\n")
cat("   Individual Order Features:", ncol(data_features), "columns,", nrow(data_features), "rows\n")
cat("   Partner Aggregated Features:", nrow(partner_features), "partners\n")
cat("   Weather Features:", nrow(weather_features), "conditions\n")
cat("   Traffic Features:", nrow(traffic_features), "levels\n")
cat("   Daily Features:", nrow(daily_features), "days\n")
cat("   Hourly Features:", nrow(hourly_features), "hours\n")
cat("   Weekly Features:", nrow(weekly_features), "weeks\n")
cat("   ML Model Features:", nrow(ml_features), "records x", ncol(ml_features), "cols\n\n")

cat("📁 Files saved in data/processed/ (16 files)\n\n")
cat("🚀 Next Step: Run 04_model_training.R\n\n")