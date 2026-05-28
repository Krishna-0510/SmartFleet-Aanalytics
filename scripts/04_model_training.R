# ============================================================================
# SmartFleetAnalytics - Model Training Script (FIXED FOR YOUR DATA)
# ============================================================================
# Purpose: Train delay prediction model + cost & efficiency models
# Input:   ml_features.rds from 03_feature_engineering.R
# Output:  model/delay_prediction_model.rds
#          model/cost_optimization_model.rds
#          model/route_efficiency_model.rds
#          model/model_metadata.rds
# ============================================================================

library(tidyverse)
library(randomForest)
library(caret)
library(lubridate)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("🤖 MODEL TRAINING - SMARTFLEETANALYTICS\n")
cat(strrep("=", 70), "\n\n", sep = "")

dir.create("model", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# 1. LOAD ML FEATURES
# ============================================================================

cat("📂 Loading ML features...\n\n")

if (file.exists("data/processed/ml_features.rds")) {
  ml_features <- readRDS("data/processed/ml_features.rds")
  cat("✅ Loaded ml_features.rds\n")
} else if (file.exists("data/processed/ml_features.csv")) {
  ml_features <- read.csv("data/processed/ml_features.csv", stringsAsFactors = FALSE)
  cat("✅ Loaded ml_features.csv\n")
} else {
  stop("ml_features not found. Please run 03_feature_engineering.R first.")
}

cat("   Records:", nrow(ml_features), "\n")
cat("   Columns:", ncol(ml_features), "\n\n")

# ============================================================================
# 2. PREPARE FEATURES FOR MODELING (USING YOUR ACTUAL COLUMNS)
# ============================================================================

cat("🔧 Preparing features for modeling...\n\n")

# Check available columns
cat("   Available columns:", paste(names(ml_features), collapse=", "), "\n\n")

# Use only columns that exist in your data
model_data <- ml_features %>%
  mutate(
    # Convert target to factor
    is_delayed = as.factor(is_delayed),
    
    # Convert categorical columns to factors (using your actual column names)
    day_of_week = as.factor(day_of_week),
    is_weekend = as.factor(is_weekend),
    weather_conditions = as.factor(weather_conditions),
    road_traffic_density = as.factor(road_traffic_density),
    type_of_vehicle = as.factor(type_of_vehicle),
    
    # Convert month to factor if exists
    month = if("month" %in% names(.)) as.factor(month) else NULL,
    
    # Convert quarter to factor if exists
    quarter = if("quarter" %in% names(.)) as.factor(quarter) else NULL
  )

# Select only available columns for modeling
available_predictors <- c(
  "delivery_person_age", "delivery_person_ratings", "distance_km",
  "order_hour", "traffic_level_num", "weather_impact", "traffic_impact",
  "distance_impact", "cost_per_km", "day_of_week", "is_weekend",
  "weather_conditions", "road_traffic_density", "type_of_vehicle"
)

# Keep only columns that exist
existing_predictors <- available_predictors[available_predictors %in% names(model_data)]

model_data <- model_data %>%
  select(all_of(c("is_delayed", existing_predictors))) %>%
  drop_na()

cat("   Clean model records:", nrow(model_data), "\n")
cat("   Features used:", ncol(model_data) - 1, "\n")
cat("   Predictors:", paste(existing_predictors, collapse=", "), "\n\n")

# Class balance check
delay_tbl <- table(model_data$is_delayed)
cat("   Class balance:\n")
cat("     OnTime :", delay_tbl[1], sprintf("(%.1f%%)", delay_tbl[1] / sum(delay_tbl) * 100), "\n")
cat("     Delayed:", delay_tbl[2], sprintf("(%.1f%%)", delay_tbl[2] / sum(delay_tbl) * 100), "\n\n")

# ============================================================================
# 3. TRAIN / TEST SPLIT
# ============================================================================

cat("✂️  Splitting data (80% train / 20% test)...\n\n")

set.seed(42)
train_idx <- createDataPartition(model_data$is_delayed, p = 0.80, list = FALSE)
train_data <- model_data[ train_idx, ]
test_data  <- model_data[-train_idx, ]

cat("   Train set:", nrow(train_data), "records\n")
cat("   Test  set:", nrow(test_data),  "records\n\n")

# ============================================================================
# 4. MODEL 1 — DELAY PREDICTION (CLASSIFICATION)
# ============================================================================

cat(strrep("-", 60), "\n")
cat("🎯 MODEL 1: Delay Prediction (Random Forest Classifier)\n")
cat(strrep("-", 60), "\n\n")

set.seed(42)
delay_model <- randomForest(
  is_delayed ~ .,
  data       = train_data,
  ntree      = 100,  # Reduced for speed (was 300)
  mtry       = floor(sqrt(ncol(train_data) - 1)),
  importance = TRUE
)

# Evaluate on test set
delay_preds <- predict(delay_model, test_data)
delay_cm    <- confusionMatrix(delay_preds, test_data$is_delayed, positive = "TRUE")

cat("📊 Delay Prediction — Test Set Results:\n")
cat(sprintf("   Accuracy  : %.2f%%\n", delay_cm$overall["Accuracy"]  * 100))
cat(sprintf("   Sensitivity: %.2f%%\n", delay_cm$byClass["Sensitivity"] * 100))
cat(sprintf("   Specificity: %.2f%%\n", delay_cm$byClass["Specificity"] * 100))
cat(sprintf("   Kappa      : %.3f\n\n",  delay_cm$overall["Kappa"]))

# Top-10 important features
imp_delay <- importance(delay_model, type = 2) %>%
  as.data.frame() %>%
  rownames_to_column("Feature") %>%
  rename(Importance = MeanDecreaseGini) %>%
  arrange(desc(Importance)) %>%
  slice_head(n = 10)

cat("🔍 Top 10 Important Features (Delay Model):\n")
imp_delay %>%
  mutate(Bar = strrep("█", round(Importance / max(Importance) * 20))) %>%
  { cat(sprintf("   %-30s %s\n", .$Feature, .$Bar)); . } %>%
  invisible()

cat("\n")
saveRDS(delay_model, "model/delay_prediction_model.rds")
cat("✅ Saved: model/delay_prediction_model.rds\n\n")

# ============================================================================
# 5. MODEL 2 — COST OPTIMIZATION (REGRESSION)
# ============================================================================

cat(strrep("-", 60), "\n")
cat("💰 MODEL 2: Cost Optimization (Random Forest Regressor)\n")
cat(strrep("-", 60), "\n\n")

# Load data_features for regression models
if (file.exists("data/processed/data_features.rds")) {
  df_feat <- readRDS("data/processed/data_features.rds")
  cat("✅ Loaded data_features.rds\n")
} else if (file.exists("data/processed/data_features.csv")) {
  df_feat <- read.csv("data/processed/data_features.csv", stringsAsFactors = FALSE)
  cat("✅ Loaded data_features.csv\n")
} else {
  cat("⚠️  data_features not found, skipping cost model\n")
  cost_rmse <- NA
  cost_r2 <- NA
}

if (exists("df_feat")) {
  # Use available columns for cost model
  cost_data <- df_feat %>%
    mutate(
      road_traffic_density = as.factor(road_traffic_density),
      is_weekend = as.factor(is_weekend)
    ) %>%
    select(
      profit_margin,        # target
      distance_km,
      order_value,
      delivery_person_ratings,
      road_traffic_density,
      order_hour,
      is_weekend,
      delivery_risk,
      efficiency_score
    ) %>%
    drop_na()
  
  if(nrow(cost_data) > 0) {
    set.seed(42)
    cost_idx    <- createDataPartition(cost_data$profit_margin, p = 0.80, list = FALSE)
    cost_train  <- cost_data[ cost_idx, ]
    cost_test   <- cost_data[-cost_idx, ]
    
    cost_model  <- randomForest(
      profit_margin ~ .,
      data      = cost_train,
      ntree     = 100,
      importance = TRUE
    )
    
    cost_preds <- predict(cost_model, cost_test)
    cost_rmse  <- sqrt(mean((cost_preds - cost_test$profit_margin)^2))
    cost_r2    <- cor(cost_preds, cost_test$profit_margin)^2
    
    cat(sprintf("   RMSE : %.3f\n",  cost_rmse))
    cat(sprintf("   R²   : %.3f\n\n", cost_r2))
    
    saveRDS(cost_model, "model/cost_optimization_model.rds")
    cat("✅ Saved: model/cost_optimization_model.rds\n\n")
  } else {
    cat("   Not enough data for cost model\n\n")
  }
}

# ============================================================================
# 6. MODEL 3 — ROUTE EFFICIENCY (REGRESSION)
# ============================================================================

cat(strrep("-", 60), "\n")
cat("🛣️  MODEL 3: Route Efficiency (Random Forest Regressor)\n")
cat(strrep("-", 60), "\n\n")

if (exists("df_feat")) {
  route_data <- df_feat %>%
    mutate(
      road_traffic_density = as.factor(road_traffic_density),
      is_weekend = as.factor(is_weekend)
    ) %>%
    select(
      efficiency_score,    # target
      distance_km,
      cost_per_km,
      road_traffic_density,
      order_hour,
      is_weekend,
      delivery_risk,
      traffic_impact,
      distance_impact
    ) %>%
    drop_na()
  
  if(nrow(route_data) > 0) {
    set.seed(42)
    route_idx   <- createDataPartition(route_data$efficiency_score, p = 0.80, list = FALSE)
    route_train <- route_data[ route_idx, ]
    route_test  <- route_data[-route_idx, ]
    
    route_model <- randomForest(
      efficiency_score ~ .,
      data       = route_train,
      ntree      = 100,
      importance = TRUE
    )
    
    route_preds <- predict(route_model, route_test)
    route_rmse  <- sqrt(mean((route_preds - route_test$efficiency_score)^2))
    route_r2    <- cor(route_preds, route_test$efficiency_score)^2
    
    cat(sprintf("   RMSE : %.3f\n",  route_rmse))
    cat(sprintf("   R²   : %.3f\n\n", route_r2))
    
    saveRDS(route_model, "model/route_efficiency_model.rds")
    cat("✅ Saved: model/route_efficiency_model.rds\n\n")
  } else {
    cat("   Not enough data for route model\n\n")
  }
}

# ============================================================================
# 7. SAVE MODEL METADATA (used by the Shiny dashboard)
# ============================================================================

cat("📦 Saving model metadata...\n\n")

model_metadata <- list(
  created_at = Sys.time(),
  r_version  = as.character(getRversion()),
  data_shape = list(rows = nrow(model_data), cols = ncol(model_data)),
  
  delay_model = list(
    type         = "randomForest (classification)",
    target       = "is_delayed",
    n_trees      = delay_model$ntree,
    accuracy     = unname(delay_cm$overall["Accuracy"]),
    sensitivity  = unname(delay_cm$byClass["Sensitivity"]),
    specificity  = unname(delay_cm$byClass["Specificity"]),
    kappa        = unname(delay_cm$overall["Kappa"]),
    top_features = imp_delay$Feature[1:5]
  ),
  
  cost_model = if(exists("cost_rmse")) list(
    type   = "randomForest (regression)",
    target = "profit_margin",
    rmse   = cost_rmse,
    r2     = cost_r2
  ) else list(available = FALSE),
  
  route_model = if(exists("route_rmse")) list(
    type   = "randomForest (regression)",
    target = "efficiency_score",
    rmse   = route_rmse,
    r2     = route_r2
  ) else list(available = FALSE),
  
  feature_levels = list(
    traffic_density = c("low", "medium", "high", "gridlock", "unknown"),
    day_of_week = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
    weather = c("clear", "cloudy", "rainy", "foggy", "other"),
    vehicle_type = c("motorcycle", "scooter", "car", "truck", "other")
  )
)

saveRDS(model_metadata, "model/model_metadata.rds")
cat("✅ Saved: model/model_metadata.rds\n\n")

# ============================================================================
# 8. SUMMARY
# ============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("✅ MODEL TRAINING COMPLETE!\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("📁 FILES CREATED:\n")
cat("   model/delay_prediction_model.rds  — classifier (on-time vs delayed)\n")
if(file.exists("model/cost_optimization_model.rds")) {
  cat("   model/cost_optimization_model.rds — regressor (profit margin)\n")
}
if(file.exists("model/route_efficiency_model.rds")) {
  cat("   model/route_efficiency_model.rds  — regressor (efficiency score)\n")
}
cat("   model/model_metadata.rds          — metrics & feature levels\n\n")

cat("📊 PERFORMANCE SUMMARY:\n")
cat(sprintf("   Delay model accuracy : %.2f%%\n", delay_cm$overall["Accuracy"] * 100))
if(exists("cost_r2")) cat(sprintf("   Cost model R²        : %.3f\n", cost_r2))
if(exists("route_r2")) cat(sprintf("   Route model R²       : %.3f\n", route_r2))

cat("\n🚀 Next Step: Run 05_dashboard_prep.R\n\n")