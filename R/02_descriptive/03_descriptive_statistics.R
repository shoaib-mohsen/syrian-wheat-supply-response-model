# Project: Syrian Wheat Supply Response
# Script : 03_descriptive_statistics.R
# Purpose: Generating descriptive statistics
# Author : Shoaib Mohsen

# Loading needed packages & setting options

library(dplyr)
library(tidyr)
library(moments)
library(flextable)
library(officer)

options(scipen = 999)

# Running needed scripts

source("R/functions.R")

# Generating a summary table for the dataset

summary_table <- get_summary_table(full_data)

print(summary_table, width = Inf)

# Extracting high correlations between variables in both raw and model data

raw_high_corrs <- get_high_corrs(raw_data)

print(raw_high_corrs)

model_high_corrs <- get_high_corrs(model_data)

print(model_high_corrs)

# Splitting the data set by conflict start year 2011

conflict_data <- filter(full_data, year >= 2011)

pre_conflict_data <- filter(full_data, year < 2011)

# Comparing the two periods

pre_conflict_summary <- get_summary_table(pre_conflict_data)

print(pre_conflict_summary, width = Inf)

conflict_summary <- get_summary_table(conflict_data)

print(conflict_summary, width = Inf)

# Exporting tables to Word documents

print(get_doc(summary_table, caption = "Table 1. Descriptive Statistics"), target = "output/01_area/tables/01_summary_table.docx")

print(get_doc(raw_high_corrs, caption = "Table 2. Raw Data High Correlations"), target = "output/01_area/tables/02_raw_data_high_correlations_table.docx")

print(get_doc(model_high_corrs, caption = "Table 3. Model Data High Correlations"), target = "output/01_area/tables/03_model_data_high_correlations_table.docx")

print(get_doc(pre_conflict_summary, caption = "Table 4. Pre Conflict Period Summary Statistics"), target = "output/01_area/tables/04_pre_conflict_data_summary_table.docx")

print(get_doc(conflict_summary, caption = "Table 5. Conflict Period Summary Statistics"), target = "output/01_area/tables/05_conflict_data_summary_table.docx")

