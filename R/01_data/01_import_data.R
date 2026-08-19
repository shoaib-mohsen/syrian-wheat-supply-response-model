# Project: Syrian Wheat Supply Response
# Script : 01_import_data.R
# Purpose: Import raw data and perform basic validation
# Author : Shoaib Mohsen

# # loading needed packages

library(readxl)

# Importing raw dataset

raw_data <- read_excel("data/raw/data.xlsx", sheet = "1")

# Renaming variables

cat("\n\n WARNING: DATA COLUMNS MUST FOLLOW THIS ORDER : \n\n")
cat("\n\n Year - Production - Area - Yield - Wheat Price - Agriculture Stress Index - GDP Deflator - Government Effectiveness - Political Stability - Cotton Price - Barley Price \n\n")

colnames(raw_data) <- c(
  "year",
  "production",
  "area",
  "yield",
  "wheat_price",
  "asi", 
  "gdp_deflator", 
  "government_effectiveness",
  "political_stability",
  "cotton_price",
  "barley_price"
  )

# Inspect imported data

cat("\n\n Raw Data Inspection \n\n")

str(raw_data)

summary(raw_data)

head(raw_data)

# Check for missing values

if (sum(colSums(is.na(raw_data))) > 0){
  stop("Missing data detected in raw data.")
}

# Check duplicates

if (anyDuplicated(raw_data$year) > 0) {
  stop("Duplicate years detected in raw data.")
}

# Verify data dimensions

if (dim(raw_data)[1] < 10) {
  stop("Too few observations.")
}

# Verify time range

if (!is.numeric(raw_data$year)) {
  stop("Year variable is not numeric.")
}

# Verify non-negativity

if (any(raw_data <= 0)) {
  stop("Non-positive values detected in raw data.")
}