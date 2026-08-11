# Project: Syrian Wheat Supply Response
# Script : 06_diagnostics.R
# Purpose: Running diagnostic tests
# Author : Shoaib Mohsen

source("R/functions.R")

library(lmtest)
library(tseries)
library(strucchange)

# Extract the underlying model object

m <- baseline_model$best_model

baseline_lm_obj <- to_lm(baseline_model$best_model, fix_names = TRUE)

n <- nobs(baseline_lm_obj)
k <- length(coef(baseline_lm_obj))

alpha <- 0.05

model <- baseline_model$best_model

used_rows <- as.numeric(rownames(model$model))

years <- used_rows

baseline_diagnostics <- get_diagnostics(m,baseline_lm_obj,years) 