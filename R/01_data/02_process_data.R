# Project: Syrian Wheat Supply Response
# Script : 02_process_data.R
# Purpose: Processing data into analysis-ready datasets
# Author : Shoaib Mohsen

# Loading needed packages

library(dplyr)

# Fixing variable types

raw_data$year <- as.integer(raw_data$year)

# Creating the full_data dataset

full_data <- raw_data

# Deriving needed variables

## Calculating real prices

full_data$real_wheat_price <- (full_data$wheat_price/full_data$gdp_deflator)*100
full_data$real_cotton_price <- (full_data$cotton_price/full_data$gdp_deflator)*100
full_data$real_barley_price <- (full_data$barley_price/full_data$gdp_deflator)*100

## Creating logarithmic transformations

full_data$ln_real_cotton_price <- log(full_data$real_cotton_price)
full_data$ln_real_barley_price <- log(full_data$real_barley_price)
full_data$ln_real_wheat_price <- log(full_data$real_wheat_price)

full_data$ln_production <- log(full_data$production)
full_data$ln_area <- log(full_data$area)
full_data$ln_yield <- log(full_data$yield)
full_data$ln_asi <- log(full_data$asi)
full_data$ln_political_stability <- log(full_data$political_stability)
full_data$ln_gov_effectiveness <- log(full_data$government_effectiveness)

# Inspecting processed data

cat("\n\n Processed Data Inspection \n\n")

str(full_data)

summary(full_data)

head(full_data)

# Checking for missing values

if (sum(colSums(is.na(full_data))) > 0){
  stop("Missing data detected in processed data.")
}

# Verifying

if(any(full_data == Inf) | any(full_data == -Inf)){
  stop("Infinite data detected in processed data.")
}

# Creating the model data

model_data <- select(
  full_data,
  c(year,
    ln_area,
    ln_production,
    ln_yield,
    ln_asi,
    ln_gov_effectiveness,
    ln_political_stability,
    ln_real_wheat_price,
    ln_real_cotton_price,
    ln_real_barley_price))

# Saving processed data

write_csv(full_data, "data/processed/full_data.csv")
write_csv(model_data, "data/processed/model_data.csv")
