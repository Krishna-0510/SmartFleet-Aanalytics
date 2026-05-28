# =============================================================================
# SmartFleetAnalytics - Data Loading Script
# File: scripts/01_data_loading.R
# Purpose: Load all raw CSV & Excel files into R environment
# Run AFTER: generate_sample_data.R
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────
required_packages <- c("dplyr", "readr", "openxlsx", "lubridate", "stringr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}
cat("✅ Packages loaded\n")

# ── 1. Set Base Path ──────────────────────────────────────────────────────────
# Auto-detect project root (works from any subfolder)
base_path <- tryCatch({
  here::here()
}, error = function(e) {
  getwd()
})

raw_path <- file.path(base_path, "data", "raw")
cat(sprintf("📂 Loading from: %s\n", raw_path))

# ── 2. Load CSVs ──────────────────────────────────────────────────────────────

# ─── 2a. Porter_Data_Set.csv ──────────────────────────────────────────────────
cat("\n📥 Loading Porter_Data_Set.csv...\n")
porter_raw <- tryCatch({
  read_csv(
    file.path(raw_path, "Porter_Data_Set.csv"),
    col_types = cols(
      ID                          = col_character(),
      Delivery_person_ID          = col_character(),
      Delivery_person_Age         = col_double(),
      Delivery_person_Ratings     = col_double(),
      Restaurant_latitude         = col_double(),
      Restaurant_longitude        = col_double(),
      Delivery_location_latitude  = col_double(),
      Delivery_location_longitude = col_double(),
      Order_Date                  = col_character(),
      Time_Ordered                = col_character(),
      Time_Order_Picked           = col_character(),
      Weather_conditions          = col_character(),
      Road_traffic_density        = col_character(),
      Type_of_vehicle             = col_character()
    ),
    show_col_types = FALSE
  )
}, error = function(e) {
  stop(sprintf("❌ Failed to load Porter_Data_Set.csv: %s", e$message))
})
cat(sprintf("   ✅ Loaded %d rows x %d cols\n", nrow(porter_raw), ncol(porter_raw)))

# ─── 2b. processed_data.csv ───────────────────────────────────────────────────
cat("\n📥 Loading processed_data.csv...\n")
processed_data <- tryCatch({
  read_csv(
    file.path(raw_path, "processed_data.csv"),
    col_types = cols(
      Traffic_Level        = col_double(),
      Distance_km          = col_double(),
      Weather_Index        = col_double(),
      Rider_Age            = col_double(),
      Multiple_Deliveries  = col_double(),
      Hour_of_Day          = col_double(),
      Is_Festival          = col_double(),
      Actual_Time_mins     = col_double(),
      Estimated_mins       = col_double(),
      Delay_mins           = col_double(),
      Is_Delayed           = col_double()
    ),
    show_col_types = FALSE
  )
}, error = function(e) {
  stop(sprintf("❌ Failed to load processed_data.csv: %s", e$message))
})
cat(sprintf("   ✅ Loaded %d rows x %d cols\n", nrow(processed_data), ncol(processed_data)))

# ── 3. Load Excel Files ───────────────────────────────────────────────────────

# ─── 3a. cleaned_dataset_porter.xlsx ─────────────────────────────────────────
cat("\n📥 Loading cleaned_dataset_porter.xlsx...\n")
cleaned_porter <- tryCatch({
  read.xlsx(
    file.path(raw_path, "cleaned_dataset_porter.xlsx"),
    sheet      = "cleaned_data",
    detectDates = TRUE
  )
}, error = function(e) {
  stop(sprintf("❌ Failed to load cleaned_dataset_porter.xlsx: %s", e$message))
})
cat(sprintf("   ✅ Loaded %d rows x %d cols\n", nrow(cleaned_porter), ncol(cleaned_porter)))

# ─── 3b. Porter_Case_Study_Results.xlsx ──────────────────────────────────────
cat("\n📥 Loading Porter_Case_Study_Results.xlsx...\n")
case_study <- tryCatch({
  read.xlsx(
    file.path(raw_path, "Porter_Case_Study_Results.xlsx"),
    sheet       = "case_study_results",
    detectDates = TRUE
  )
}, error = function(e) {
  stop(sprintf("❌ Failed to load Porter_Case_Study_Results.xlsx: %s", e$message))
})
cat(sprintf("   ✅ Loaded %d rows x %d cols\n", nrow(case_study), ncol(case_study)))

# ── 4. Quick Validation ───────────────────────────────────────────────────────
cat("\n🔍 Running validation checks...\n")

validate_dataset <- function(df, name, required_cols) {
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    cat(sprintf("   ⚠️  %s: Missing columns: %s\n", name, paste(missing_cols, collapse = ", ")))
  } else {
    cat(sprintf("   ✅ %s: All required columns present\n", name))
  }
  na_pct <- round(sum(is.na(df)) / (nrow(df) * ncol(df)) * 100, 2)
  cat(sprintf("   📊 %s: %.2f%% missing values\n", name, na_pct))
}

validate_dataset(porter_raw,     "Porter_Data_Set",          c("ID","Order_Date","Weather_conditions","Road_traffic_density"))
validate_dataset(processed_data, "processed_data",           c("Traffic_Level","Distance_km","Delay_mins","Is_Delayed"))
validate_dataset(cleaned_porter, "cleaned_dataset_porter",   c("Order_ID","Distance_km","Actual_Time_mins"))
validate_dataset(case_study,     "Case_Study_Results",       c("Order_ID","Delay_mins","Delivery_Cost_INR","Efficiency_Score"))

# ── 5. Print Column Summaries ─────────────────────────────────────────────────
cat("\n📋 DATASET SUMMARIES\n")
cat(strrep("-", 50), "\n")

print_summary <- function(df, name) {
  cat(sprintf("\n🗂️  %s\n", name))
  cat(sprintf("   Rows    : %d\n", nrow(df)))
  cat(sprintf("   Columns : %d\n", ncol(df)))
  cat(sprintf("   Cols    : %s\n", paste(colnames(df), collapse = ", ")))
}

print_summary(porter_raw,     "Porter_Data_Set.csv")
print_summary(processed_data, "processed_data.csv")
print_summary(cleaned_porter, "cleaned_dataset_porter.xlsx")
print_summary(case_study,     "Porter_Case_Study_Results.xlsx")

# ── 6. Save to RDS for Next Script ───────────────────────────────────────────
cat("\n💾 Saving loaded data to data/processed/...\n")

processed_dir <- file.path(base_path, "data", "processed")
if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

saveRDS(porter_raw,     file.path(processed_dir, "porter_raw.rds"))
saveRDS(processed_data, file.path(processed_dir, "processed_data.rds"))
saveRDS(cleaned_porter, file.path(processed_dir, "cleaned_porter.rds"))
saveRDS(case_study,     file.path(processed_dir, "case_study.rds"))

cat("   ✅ porter_raw.rds saved\n")
cat("   ✅ processed_data.rds saved\n")
cat("   ✅ cleaned_porter.rds saved\n")
cat("   ✅ case_study.rds saved\n")

# ── 7. Final Summary ──────────────────────────────────────────────────────────
cat("\n", strrep("=", 60), "\n")
cat("🎉 DATA LOADING COMPLETE\n")
cat(strrep("=", 60), "\n")
cat(sprintf("📦 porter_raw     : %d rows\n", nrow(porter_raw)))
cat(sprintf("📦 processed_data : %d rows\n", nrow(processed_data)))
cat(sprintf("📦 cleaned_porter : %d rows\n", nrow(cleaned_porter)))
cat(sprintf("📦 case_study     : %d rows\n", nrow(case_study)))
cat("\n👉 Next Step: Run scripts/02_data_cleaning.R\n")
cat(strrep("=", 60), "\n")