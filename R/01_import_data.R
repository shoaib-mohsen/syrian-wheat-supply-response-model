# Project: Syrian Wheat Supply Response
# Script : 01_import_data.R
# Purpose: Import raw data and perform basic validation
# Author : Shoaib Mohsen

# # loading needed packages

library("readxl")

# Importing raw dataset

raw_data <- read_xlsx("data/raw/data.xlsx")

# Renaming variables

colnames(raw_data) <- c(
  "year",
  "production",
  "area",
  "yield",
  "wheat_price",
  "asi", 
  "gdp_deflator", 
  "political_stability",
  "cotton_price",
  "barley_price")

# Inspect imported data

str(raw_data)

summary(raw_data)

head(raw_data)

# Check for missing values

colSums(is.na(raw_data))

# Check duplicates

any(duplicated(raw_data$year))

# Verify data dimensions

dim(raw_data)

# Verify time range

range(raw_data$year)