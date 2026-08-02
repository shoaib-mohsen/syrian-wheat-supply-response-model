# Project: Syrian Wheat Supply Response
# Script : 01_import_data.R
# Purpose: Import raw data and perform basic validation
# Author : Shoaib Mohsen

# Loading 'readxl' package

library("readxl")

# Importing raw dataset

data <- read_xlsx("data/raw/data.xlsx")

# Renaming variables

colnames(data) <- c(
  "year",
  "production",
  "area",
  "yield",
  "wheat_price",
  "asi", 
  "inflation", 
  "political_stability",
  "cotton_price",
  "barley_price")

# Inspect imported data

str(data)

summary(data)

head(data)

# Check for missing values

colSums(is.na(data))

# Check duplicates

any(duplicated(data$year))

# Verify data dimensions

dim(data)

# Verify time range

range(data$year)