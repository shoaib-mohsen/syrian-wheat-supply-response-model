# Project: Syrian Wheat Supply Response
# Script : 02_process_data.R
# Purpose: Processing data into an analysis-ready dataset
# Author : Shoaib Mohsen

# loading needed packages
library("readr")

# importing the and inspecting the data
source("R/01_import_data.R")

# Fixing variable types
data$year <- as.integer(data$year)

# Creating derived variables

## Calculating real prices

data$ln_real_cotton_price <- log((data$cotton_price/data$gdp_deflator)*100)
data$ln_real_barley_price <- log((data$barley_price/data$gdp_deflator)*100)
data$ln_real_wheat_price <- log((data$wheat_price/data$gdp_deflator)*100)

## Creating logarithmic transformations

data$ln_production <- log(data$production)
data$ln_area <- log(data$area)
data$ln_yield <- log(data$yield)
data$ln_asi <- log(data$asi)
data$ln_political_stability <- log(data$political_stability)

# Verifying
any(data == Inf)
any(data == -Inf)
any(is.na(data))
lapply(data, function(x) any(is.nan(x)))

# Inspect processed data

str(data)

summary(data)

head(data)

# Check for missing values

colSums(is.na(data))

# Verify data dimensions

dim(data)

# Verify time range

range(data$year)

# Saving processed data
write_csv(data, "data/processed/data.csv")
