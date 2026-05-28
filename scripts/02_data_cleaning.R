# ==============================================================================
# Script ID: 02_data_cleaning.R
# Purpose: Clean, handle missing values, and standardize column types across datasets
# ==============================================================================

# Source global settings (create config folder first if needed)
if(file.exists("config/settings.R")) {
  source("config/settings.R")
} else {
  # Fallback if settings.R doesn't exist yet
  PATHS <- list(
    raw_data = "data/raw/",
    proc_data = "data/processed/"
  )
  cat("⚠️  config/settings.R not found, using default paths\n")
}

# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(janitor)
library(readxl)
library(openxlsx)

cat("\n--- Starting Step 2: Data Cleaning & Standardization ---\n")

# Ensure directories exist
if(!dir.exists(PATHS$proc_data)) dir.create(PATHS$proc_data, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Load Step 1 Raw Datasets
# ------------------------------------------------------------------------------
cat("📥 Loading raw datasets for cleaning...\n")

df_raw_orders    <- read_csv(paste0(PATHS$raw_data, "Porter_Data_Set.csv"), show_col_types = FALSE)
df_ml_raw        <- read_csv(paste0(PATHS$raw_data, "processed_data.csv"), show_col_types = FALSE)
df_cleaned_porter <- read_excel(paste0(PATHS$raw_data, "cleaned_dataset_porter.xlsx"))
df_case_study    <- read_excel(paste0(PATHS$raw_data, "Porter_Case_Study_Results.xlsx"))

# ------------------------------------------------------------------------------
# 2. Clean Dataset 1: Raw Orders (Porter_Data_Set.csv)
# ------------------------------------------------------------------------------
cat("🧹 Cleaning Main Orders Dataset...\n")

# Check if time columns exist
if("time_ordered" %in% names(df_raw_orders) && "time_ordered_picked" %in% names(df_raw_orders)) {
  df_orders_clean <- df_raw_orders %>%
    janitor::clean_names() %>%
    distinct() %>%
    mutate(
      order_date = as.Date(order_date, format = "%Y-%m-%d"),
      # Create datetime from date and time strings (only if columns exist)
      order_datetime = ymd_hms(paste(order_date, time_ordered, sep = " "), quiet = TRUE),
      pickup_datetime = ymd_hms(paste(order_date, time_ordered_picked, sep = " "), quiet = TRUE)
    ) %>%
    # Calculate delivery duration in minutes
    mutate(
      delivery_duration_mins = as.numeric(difftime(pickup_datetime, order_datetime, units = "mins")),
      delivery_duration_mins = if_else(is.na(delivery_duration_mins) | delivery_duration_mins < 0, 
                                       NA_real_, delivery_duration_mins)
    )
} else {
  # Fallback if time columns don't exist
  cat("   ⚠️  Time columns not found, skipping datetime parsing\n")
  df_orders_clean <- df_raw_orders %>%
    janitor::clean_names() %>%
    distinct() %>%
    mutate(
      order_date = as.Date(order_date, format = "%Y-%m-%d"),
      delivery_duration_mins = NA_real_
    )
}

# Continue cleaning common columns
df_orders_clean <- df_orders_clean %>%
  mutate(
    # Clean categorical columns
    weather_conditions = str_to_lower(str_trim(weather_conditions)),
    weather_conditions = case_when(
      grepl("sun|clear", weather_conditions) ~ "clear",
      grepl("cloud", weather_conditions) ~ "cloudy",
      grepl("rain|storm", weather_conditions) ~ "rainy",
      grepl("fog|mist", weather_conditions) ~ "foggy",
      TRUE ~ "other"
    ),
    road_traffic_density = str_to_lower(str_trim(road_traffic_density)),
    road_traffic_density = case_when(
      grepl("low", road_traffic_density) ~ "low",
      grepl("medium", road_traffic_density) ~ "medium",
      grepl("high", road_traffic_density) ~ "high",
      grepl("jam", road_traffic_density) ~ "gridlock",
      TRUE ~ "unknown"
    ),
    type_of_vehicle = str_to_lower(str_trim(type_of_vehicle)),
    # Clean numeric columns
    delivery_person_age = if_else(delivery_person_age < 18 | delivery_person_age > 70, 
                                  NA_real_, delivery_person_age),
    delivery_person_ratings = if_else(delivery_person_ratings < 1 | delivery_person_ratings > 5,
                                      NA_real_, delivery_person_ratings)
  ) %>%
  # Impute missing values
  mutate(
    delivery_person_age = if_else(is.na(delivery_person_age), 
                                  median(delivery_person_age, na.rm = TRUE), 
                                  delivery_person_age),
    delivery_person_ratings = if_else(is.na(delivery_person_ratings),
                                      median(delivery_person_ratings, na.rm = TRUE),
                                      delivery_person_ratings)
  ) %>%
  # Remove rows with critical missing data
  filter(!is.na(delivery_person_id), !is.na(order_date))

cat(sprintf("   ✅ Cleaned: %d rows x %d cols\n", nrow(df_orders_clean), ncol(df_orders_clean)))

# ------------------------------------------------------------------------------
# 3. Clean Dataset 2: ML Raw Features (processed_data.csv)
# ------------------------------------------------------------------------------
cat("🧹 Standardizing ML Feature Dataset...\n")

df_ml_clean <- df_ml_raw %>%
  janitor::clean_names() %>%
  distinct() %>%
  mutate(
    is_delayed = as.factor(is_delayed),
    # Cap extreme values
    delay_mins = if_else(delay_mins < 0, 0, delay_mins),
    delay_mins = if_else(delay_mins > 120, 120, delay_mins),
    distance_km = if_else(distance_km < 0.1 | distance_km > 50, 
                          median(distance_km, na.rm = TRUE), 
                          distance_km),
    traffic_level = pmin(pmax(traffic_level, 0), 10),
    actual_time_mins = if_else(actual_time_mins < 1 | actual_time_mins > 180,
                               median(actual_time_mins, na.rm = TRUE),
                               actual_time_mins)
  ) %>%
  filter(!is.na(distance_km), !is.na(actual_time_mins))

cat(sprintf("   ✅ Cleaned: %d rows x %d cols\n", nrow(df_ml_clean), ncol(df_ml_clean)))

# ------------------------------------------------------------------------------
# 4. Clean Dataset 3: Cleaned Porter (cleaned_dataset_porter.xlsx)
# ------------------------------------------------------------------------------
cat("🧹 Cleaning Cleaned Porter Dataset...\n")

# Check if required columns exist
if("actual_time_mins" %in% names(df_cleaned_porter) && "distance_km" %in% names(df_cleaned_porter)) {
  df_cleaned_clean <- df_cleaned_porter %>%
    janitor::clean_names() %>%
    distinct() %>%
    mutate(
      # Flag slow deliveries
      is_slow_delivery = if_else(actual_time_mins > 45, 1, 0),
      # Calculate speed
      speed_kmh = distance_km / (actual_time_mins / 60),
      speed_kmh = if_else(speed_kmh > 60, 60, speed_kmh),
      # Time category (if order_hour exists)
      time_category = if("order_hour" %in% names(.)) {
        case_when(
          order_hour < 6 ~ "late_night",
          order_hour < 12 ~ "morning",
          order_hour < 17 ~ "afternoon",
          order_hour < 21 ~ "evening",
          TRUE ~ "night"
        )
      } else {
        NA_character_
      }
    ) %>%
    filter(!is.na(distance_km), !is.na(actual_time_mins))
} else {
  cat("   ⚠️  Required columns missing, using basic cleaning\n")
  df_cleaned_clean <- df_cleaned_porter %>%
    janitor::clean_names() %>%
    distinct()
}

cat(sprintf("   ✅ Cleaned: %d rows x %d cols\n", nrow(df_cleaned_clean), ncol(df_cleaned_clean)))

# ------------------------------------------------------------------------------
# 5. Clean Dataset 4: Case Study Results
# ------------------------------------------------------------------------------
cat("🧹 Aligning Case Study Engine Datasets...\n")

# Check if required columns exist
if("delivery_cost_inr" %in% names(df_case_study) && "efficiency_score" %in% names(df_case_study)) {
  df_case_clean <- df_case_study %>%
    janitor::clean_names() %>%
    distinct() %>%
    mutate(
      delivery_cost_inr = if_else(delivery_cost_inr < 10 | delivery_cost_inr > 500,
                                  median(delivery_cost_inr, na.rm = TRUE),
                                  delivery_cost_inr),
      efficiency_score = pmin(pmax(efficiency_score, 0), 100),
      efficiency_tier = case_when(
        efficiency_score >= 80 ~ "gold",
        efficiency_score >= 60 ~ "silver",
        efficiency_score >= 40 ~ "bronze",
        TRUE ~ "needs_improvement"
      )
    ) %>%
    filter(!is.na(delivery_cost_inr), !is.na(efficiency_score))
} else {
  cat("   ⚠️  Cost/Efficiency columns not found, using basic cleaning\n")
  df_case_clean <- df_case_study %>%
    janitor::clean_names() %>%
    distinct()
}

cat(sprintf("   ✅ Cleaned: %d rows x %d cols\n", nrow(df_case_clean), ncol(df_case_clean)))

# ------------------------------------------------------------------------------
# 6. Export Sanitized Pipeline Outputs
# ------------------------------------------------------------------------------
cat("\n💾 Exporting standardized data configurations to intermediate storage...\n")

# Save as CSV
write_csv(df_orders_clean, paste0(PATHS$proc_data, "clean_orders_base.csv"), na = "")
write_csv(df_ml_clean,     paste0(PATHS$proc_data, "clean_ml_features.csv"), na = "")
write_csv(df_cleaned_clean, paste0(PATHS$proc_data, "clean_cleaned_porter.csv"), na = "")
write_csv(df_case_clean,   paste0(PATHS$proc_data, "clean_case_study.csv"), na = "")

# Save as RDS for faster loading
saveRDS(df_orders_clean, paste0(PATHS$proc_data, "clean_orders_base.rds"))
saveRDS(df_ml_clean,     paste0(PATHS$proc_data, "clean_ml_features.rds"))
saveRDS(df_cleaned_clean, paste0(PATHS$proc_data, "clean_cleaned_porter.rds"))
saveRDS(df_case_clean,   paste0(PATHS$proc_data, "clean_case_study.rds"))

# Create master clean file for backward compatibility
saveRDS(df_orders_clean, paste0(PATHS$proc_data, "data_clean.rds"))

# Also save merged version for feature engineering
master_clean <- list(
  orders = df_orders_clean,
  ml = df_ml_clean,
  cleaned = df_cleaned_clean,
  case = df_case_clean
)
saveRDS(master_clean, paste0(PATHS$proc_data, "master_clean.rds"))

cat("\n", strrep("=", 60), "\n")
cat("🎉 Step 2 Complete! Clean records generated:\n")
cat(strrep("=", 60), "\n")
cat(sprintf("   📦 Clean Base Orders    : %d rows x %d cols\n", nrow(df_orders_clean), ncol(df_orders_clean)))
cat(sprintf("   📦 Clean ML Features    : %d rows x %d cols\n", nrow(df_ml_clean), ncol(df_ml_clean)))
cat(sprintf("   📦 Clean Porter Data    : %d rows x %d cols\n", nrow(df_cleaned_clean), ncol(df_cleaned_clean)))
cat(sprintf("   📦 Clean Case Study     : %d rows x %d cols\n", nrow(df_case_clean), ncol(df_case_clean)))
cat("\n👉 Next Step: Run scripts/03_feature_engineering.R\n")
cat(strrep("=", 60), "\n")