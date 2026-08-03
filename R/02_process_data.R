# Project: Syrian Wheat Supply Response
# Script : 02_process_data.R
# Purpose: Processing data into an analysis-ready dataset
# Author : Shoaib Mohsen

# loading needed packages
library("readr")

# importing the and inspecting the data
source("R/01_import_data.R")

# Fixing variable types
raw_data$year <- as.integer(raw_data$year)

# creating the full_data dataset
full_data <- raw_data

# Creating derived variables

## Calculating real prices

full_data$ln_real_cotton_price <- log((full_data$cotton_price/full_data$gdp_deflator)*100)
full_data$ln_real_barley_price <- log((full_data$barley_price/full_data$gdp_deflator)*100)
full_data$ln_real_wheat_price <- log((full_data$wheat_price/full_data$gdp_deflator)*100)

## Creating logarithmic transformations

full_data$ln_production <- log(full_data$production)
full_data$ln_area <- log(full_data$area)
full_data$ln_yield <- log(full_data$yield)
full_data$ln_asi <- log(full_data$asi)
full_data$ln_political_stability <- log(full_data$political_stability)

# Verifying
any(full_data == Inf)
any(full_data == -Inf)
any(is.na(full_data))
lapply(full_data, function(x) any(is.nan(x)))

# Inspect processed data

str(full_data)

summary(full_data)

head(full_data)

# Check for missing values

colSums(is.na(full_data))

# Verify data dimensions

dim(full_data)

# Verify time range

range(full_data$year)

# creating the model data
model_data <- select(
  full_data,
  c(year,
    ln_area,
    ln_production,
    ln_yield,
    ln_asi,
    ln_political_stability,
    ln_real_wheat_price,
    ln_real_cotton_price,
    ln_real_barley_price))

# Saving processed data
write_csv(full_data, "data/processed/full_data.csv")
write_csv(model_data, "data/processed/model_data.csv")
