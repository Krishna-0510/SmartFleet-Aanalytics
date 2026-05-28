# =============================================================================
# SmartFleetAnalytics - Sample Data Generator
# File: generate_sample_data.R
# Purpose: Generate realistic sample data files matching the real data structure
# Run this FIRST before any other script
# =============================================================================

# ── 0. Install & Load Required Packages ───────────────────────────────────────
required_packages <- c("dplyr", "lubridate", "openxlsx", "readr", "stringr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

cat("✅ Packages loaded\n")

# ── 1. Setup ──────────────────────────────────────────────────────────────────
set.seed(42)
n <- 5000  # Number of delivery records

# Create output directories
dirs <- c("data/raw", "data/processed", "model", "scripts", "app", "docs", "config")
for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}
cat("✅ Directories created\n")

# ── 2. Helper Vectors ─────────────────────────────────────────────────────────
restaurant_names <- c(
  "Burger Palace", "Pizza Hub", "Spice Garden", "Sushi Spot", "Taco Town",
  "Noodle House", "Curry Corner", "Sandwich Stop", "Salad Bowl", "BBQ Junction",
  "Wrap Zone", "Rice Bucket", "Dosa Point", "Biryani Express", "Roll Master"
)

city_zones <- c("North", "South", "East", "West", "Central")

vehicle_types <- c("bike", "scooter", "bicycle")

weather_conditions <- c("sunny", "cloudy", "rainy", "windy", "foggy")

road_traffic_densities <- c("low", "medium", "high", "jam")

festival_names <- c("None", "Diwali", "Holi", "Navratri", "Christmas", "Eid", "New Year")

# ── 3. Generate Date-Time Variables ───────────────────────────────────────────
start_date <- as.POSIXct("2023-01-01 08:00:00")
end_date   <- as.POSIXct("2024-06-30 23:00:00")

order_times <- sort(sample(
  seq(start_date, end_date, by = "min"),
  size = n,
  replace = FALSE
))

order_date        <- as.Date(order_times)
order_hour        <- hour(order_times)
order_day_of_week <- wday(order_times, label = TRUE, abbr = FALSE)
order_month       <- month(order_times, label = TRUE, abbr = FALSE)

# ── 4. Generate Core Business Variables ───────────────────────────────────────

# Distance (km) — log-normal distribution to mimic real delivery distances
distance_km <- round(rlnorm(n, meanlog = 1.8, sdlog = 0.5), 2)
distance_km <- pmax(pmin(distance_km, 25), 1)  # Clamp between 1 and 25 km

# Traffic density — weighted sampling
traffic      <- sample(road_traffic_densities, n, replace = TRUE,
                       prob = c(0.20, 0.40, 0.30, 0.10))
traffic_num  <- case_when(
  traffic == "low"    ~ 1,
  traffic == "medium" ~ 2,
  traffic == "high"   ~ 3,
  traffic == "jam"    ~ 4
)

# Weather
weather      <- sample(weather_conditions, n, replace = TRUE,
                       prob = c(0.45, 0.25, 0.15, 0.10, 0.05))
weather_num  <- case_when(
  weather == "sunny"  ~ 1,
  weather == "cloudy" ~ 2,
  weather == "windy"  ~ 3,
  weather == "rainy"  ~ 4,
  weather == "foggy"  ~ 5
)

# Rider age and ratings
rider_age    <- round(rnorm(n, mean = 28, sd = 5))
rider_age    <- pmax(pmin(rider_age, 50), 18)

rider_rating <- round(runif(n, min = 3.0, max = 5.0), 1)

# Multiple deliveries flag
multiple_deliveries <- sample(0:1, n, replace = TRUE, prob = c(0.65, 0.35))

# Vehicle type
vehicle      <- sample(vehicle_types, n, replace = TRUE,
                       prob = c(0.55, 0.35, 0.10))

# Restaurant / delivery details
restaurant   <- sample(restaurant_names, n, replace = TRUE)
city_zone    <- sample(city_zones, n, replace = TRUE)
festival     <- sample(festival_names, n, replace = TRUE,
                       prob = c(0.78, 0.05, 0.04, 0.04, 0.03, 0.03, 0.03))

# ── 5. Calculate Delivery Time (realistic model) ───────────────────────────────
# Base time: 5 min + 2 min/km
base_time <- 5 + 2 * distance_km

# Adjustments
traffic_penalty  <- (traffic_num - 1) * 3.5
weather_penalty  <- (weather_num - 1) * 1.5
multiple_penalty <- multiple_deliveries * 5
festival_penalty <- ifelse(festival != "None", 7, 0)
peak_penalty     <- ifelse(order_hour %in% c(12, 13, 19, 20, 21), 4, 0)
rider_penalty    <- pmax(0, (30 - rider_age) * 0.2)  # Younger riders slightly slower

# Random noise
noise <- rnorm(n, mean = 0, sd = 3)

time_taken_mins <- round(
  base_time + traffic_penalty + weather_penalty +
    multiple_penalty + festival_penalty + peak_penalty +
    rider_penalty + noise,
  1
)
time_taken_mins <- pmax(time_taken_mins, 5)  # Minimum 5 minutes

# Estimated delivery time (Porter quote ≈ actual + small bias)
estimated_mins <- round(time_taken_mins * runif(n, 0.85, 1.20), 1)

# Delay (actual - estimated)
delay_mins <- round(time_taken_mins - estimated_mins, 1)
is_delayed  <- as.integer(delay_mins > 0)

# ── 6. Cost Calculation ────────────────────────────────────────────────────────
base_cost       <- 20 + distance_km * 5
surge_cost      <- ifelse(traffic %in% c("high", "jam"), base_cost * 0.15, 0)
festival_cost   <- ifelse(festival != "None", base_cost * 0.10, 0)
delivery_cost   <- round(base_cost + surge_cost + festival_cost + rnorm(n, 0, 5), 2)
delivery_cost   <- pmax(delivery_cost, 15)

# ── 7. Build Data Frames ───────────────────────────────────────────────────────

# ─── 7a. Porter_Data_Set.csv (raw orders, 14 columns) ─────────────────────────
porter_raw <- data.frame(
  ID                         = sprintf("ORD%06d", 1:n),
  Delivery_person_ID         = sprintf("DP%04d", sample(1:500, n, replace = TRUE)),
  Delivery_person_Age        = rider_age,
  Delivery_person_Ratings    = rider_rating,
  Restaurant_latitude        = round(runif(n, 12.85, 13.15), 6),
  Restaurant_longitude       = round(runif(n, 77.45, 77.75), 6),
  Delivery_location_latitude = round(runif(n, 12.80, 13.20), 6),
  Delivery_location_longitude= round(runif(n, 77.40, 77.80), 6),
  Order_Date                 = format(order_date, "%d-%m-%Y"),
  Time_Ordered               = format(order_times, "%H:%M:%S"),
  Time_Order_Picked          = format(order_times + minutes(round(runif(n,2,8))), "%H:%M:%S"),
  Weather_conditions         = weather,
  Road_traffic_density       = traffic,
  Type_of_vehicle            = vehicle,
  stringsAsFactors           = FALSE
)

# ─── 7b. processed_data.csv (ML training: Traffic + Distance → Delay) ─────────
processed_data <- data.frame(
  Traffic_Level    = traffic_num,
  Distance_km      = distance_km,
  Weather_Index    = weather_num,
  Rider_Age        = rider_age,
  Multiple_Deliveries = multiple_deliveries,
  Hour_of_Day      = order_hour,
  Is_Festival      = as.integer(festival != "None"),
  Actual_Time_mins = time_taken_mins,
  Estimated_mins   = estimated_mins,
  Delay_mins       = delay_mins,
  Is_Delayed       = is_delayed,
  stringsAsFactors = FALSE
)

# ─── 7c. cleaned_dataset_porter.xlsx (Clean orders with time features) ─────────
cleaned_porter <- data.frame(
  Order_ID            = porter_raw$ID,
  Delivery_Person_ID  = porter_raw$Delivery_person_ID,
  Age                 = rider_age,
  Rating              = rider_rating,
  Order_Date          = order_date,
  Order_Hour          = order_hour,
  Day_of_Week         = as.character(order_day_of_week),
  Month               = as.character(order_month),
  Is_Weekend          = as.integer(order_day_of_week %in% c("Saturday", "Sunday")),
  Is_Peak_Hour        = as.integer(order_hour %in% c(12, 13, 19, 20, 21)),
  Weather             = weather,
  Traffic             = traffic,
  Vehicle             = vehicle,
  Distance_km         = distance_km,
  Restaurant_Name     = restaurant,
  City_Zone           = city_zone,
  Festival            = festival,
  Multiple_Deliveries = multiple_deliveries,
  Actual_Time_mins    = time_taken_mins,
  Estimated_mins      = estimated_mins,
  stringsAsFactors    = FALSE
)

# ─── 7d. Porter_Case_Study_Results.xlsx (Enhanced with delivery metrics) ───────
case_study <- cleaned_porter
case_study$Delay_mins         <- delay_mins
case_study$Is_Delayed         <- is_delayed
case_study$Delivery_Cost_INR  <- delivery_cost
case_study$Efficiency_Score   <- round(
  pmin(100, pmax(0, 100 - (delay_mins / estimated_mins) * 50 + rider_rating * 5)), 1
)
case_study$On_Time_Pct        <- ifelse(is_delayed == 0, 1, 0)
case_study$Cost_per_km        <- round(delivery_cost / distance_km, 2)

# ── 8. Write Files ─────────────────────────────────────────────────────────────

# CSVs
write_csv(porter_raw,    "data/raw/Porter_Data_Set.csv")
write_csv(processed_data,"data/raw/processed_data.csv")
cat("✅ Porter_Data_Set.csv written\n")
cat("✅ processed_data.csv written\n")

# Excel files
wb1 <- createWorkbook()
addWorksheet(wb1, "cleaned_data")
writeData(wb1, "cleaned_data", cleaned_porter)
addStyle(wb1, "cleaned_data",
         style = createStyle(fontName = "Calibri", fontSize = 10, textDecoration = "bold",
                             fgFill = "#4472C4", fontColour = "white"),
         rows = 1, cols = 1:ncol(cleaned_porter), gridExpand = TRUE)
saveWorkbook(wb1, "data/raw/cleaned_dataset_porter.xlsx", overwrite = TRUE)
cat("✅ cleaned_dataset_porter.xlsx written\n")

wb2 <- createWorkbook()
addWorksheet(wb2, "case_study_results")
writeData(wb2, "case_study_results", case_study)
addStyle(wb2, "case_study_results",
         style = createStyle(fontName = "Calibri", fontSize = 10, textDecoration = "bold",
                             fgFill = "#ED7D31", fontColour = "white"),
         rows = 1, cols = 1:ncol(case_study), gridExpand = TRUE)
saveWorkbook(wb2, "data/raw/Porter_Case_Study_Results.xlsx", overwrite = TRUE)
cat("✅ Porter_Case_Study_Results.xlsx written\n")

# ── 9. Summary Report ─────────────────────────────────────────────────────────
cat("\n", strrep("=", 60), "\n")
cat("🎉 SAMPLE DATA GENERATION COMPLETE\n")
cat(strrep("=", 60), "\n")
cat(sprintf("📦 Total Records   : %d\n", n))
cat(sprintf("📅 Date Range      : %s to %s\n",
            min(order_date), max(order_date)))
cat(sprintf("⏱️  Avg Delivery    : %.1f minutes\n", mean(time_taken_mins)))
cat(sprintf("🚦 Delay Rate      : %.1f%%\n", mean(is_delayed) * 100))
cat(sprintf("💰 Avg Cost (INR)  : ₹%.2f\n", mean(delivery_cost)))
cat(sprintf("📏 Avg Distance    : %.2f km\n", mean(distance_km)))
cat("\n📁 Files written to data/raw/:\n")
cat("   ├── Porter_Data_Set.csv\n")
cat("   ├── processed_data.csv\n")
cat("   ├── cleaned_dataset_porter.xlsx\n")
cat("   └── Porter_Case_Study_Results.xlsx\n")
cat("\n👉 Next Step: Run scripts/01_data_loading.R\n")
cat(strrep("=", 60), "\n")