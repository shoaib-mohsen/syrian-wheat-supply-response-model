# Project: Syrian Wheat Supply Response
# Script : 05_ardl_model.R
# Purpose: Building the model
# Author : Shoaib Mohsen

# Research Question: How does wheat price policy affect wheat supply
# through farmers' land-allocation decisions in Syria
# during the period 2002 to 2023?

# Wheat price → wheat area → wheat production.

# BASIC MODEL:

# Starting from the Nerlove model: Expected wheat price → wheat area

# Expected wheat price = a linear combination of former prices

# Due to sample size limitations: up to 2 years before
# (determined by the available data)

# Expected productivity → wheat area

# Using yield as a proxy for expected productivity

# Expected productivity = a linear combination of former yields

# Due to sample size limitations: up to 2 years before
# (determined by the available data)

# Farmers maximize profit while facing different sources of risk.

# Institutional risk: lagged political stability is used as a proxy
# for the expected institutional environment/risk. Moreover, it highly
# reflects the conflict happened after 2011

# Climate risk: lagged Agriculture Stress Index is used as a proxy
# for expected climate conditions, assuming perfect climate forecasts
# do not exist

# Due to sample size limitations: up to 2 years before
# (determined by the available data)

# EXTENSIONS:

# The model is extended by adding competing crops' prices, since
# the main incentive for acreage allocation may be relative
# profitability rather than absolute profitability.
# Cotton and barley are the only fully available data that can
# be considered competing crops to wheat from the supply side.

# Loading the required packages and scripts

library(ARDL)
library(lmtest)
library(tibble)
library(MuMIn)
library(dplyr)

source("R/04_stationarity_tests.R")

# Initializing data needed for the model

ardl_data <- data %>%
  select(
    year,
    ln_area,
    ln_real_wheat_price,
    ln_real_barley_price,
    ln_real_cotton_price,
    ln_yield,
    ln_political_stability,
    ln_asi,
    ln_production
  )

ardl_ts <- ts(ardl_data, start = 2002, frequency = 1)

# Estimating the primary model

primary_formula <- ln_area ~
  ln_real_wheat_price +
  ln_yield +
  ln_political_stability

primary_model <- auto_ardl(
  primary_formula,
  data = ardl_ts,
  max_order = c(2, 2, 2, 2),
  selection = "AICc",
  grid = TRUE
)

summary(primary_model$best_model)

# The resulting model does not align with the assumed timing,
# as contemporaneous explanatory variables contain information
# realized after land-allocation decisions are made.
# Therefore, the explanatory variables are pre-lagged to impose
# a timing structure in which explanatory information predates
# acreage decisions.

# Generating pre-lagged variables

vars_to_lag <- c(
  "ln_real_wheat_price",
  "ln_real_barley_price",
  "ln_real_cotton_price",
  "ln_yield",
  "ln_asi",
  "ln_political_stability"
)

lagged_vars <- stats::lag(ardl_ts[, vars_to_lag], k = -1)

colnames(lagged_vars) <- paste0("lagged_", colnames(lagged_vars))

ardl_ts <- cbind(ardl_ts, lagged_vars)

colnames(ardl_ts) <- gsub(
  "ardl_ts\\.|lagged_vars\\.",
  "",
  colnames(ardl_ts)
)

# Estimating the basic model

basic_formula <- ln_area ~
  lagged_ln_real_wheat_price +
  lagged_ln_yield +
  lagged_ln_asi +
  lagged_ln_political_stability

basic_model <- auto_ardl(
  basic_formula,
  data = ardl_ts,
  max_order = c(1, 1, 1, 1, 1),
  selection = "AICc",
  grid = TRUE
)

summary(basic_model$best_model)

baseline_formula <- ln_area ~
  lagged_ln_real_wheat_price +
  lagged_ln_yield +
  lagged_ln_political_stability

baseline_model <- auto_ardl(
  baseline_formula,
  data = ardl_ts,
  max_order = c(1, 1, 1, 1),
  selection = "AICc",
  grid = TRUE
)

summary(baseline_model$best_model)

AICc(baseline_model$best_model) <= AICc(basic_model$best_model)

# Given the AICc results for both models:
#
# - baseline_model AICc = -35.10106
# - basic_model AICc = -30.506393
#
# and the insignificance of lagged_ln_asi
#
# - p-value = 0.90478
#
# the baseline model (lagged_ln_asi excluded) is preferred to
# the basic model (lagged_ln_asi included) according to AICc.
# The exclusion of lagged_ln_asi is therefore retained for the
# preferred specification, subject to subsequent diagnostic tests.

# Extending the model

full_extended_formula <- ln_area ~
  lagged_ln_real_wheat_price +
  lagged_ln_real_cotton_price +
  lagged_ln_real_barley_price +
  lagged_ln_yield +
  lagged_ln_political_stability

full_extended_model <- auto_ardl(
  full_extended_formula,
  data = ardl_ts,
  max_order = c(1, 1, 1, 1, 1, 1),
  selection = "AICc",
  grid = TRUE
)

summary(full_extended_model$best_model)

AICc(baseline_model$best_model) <= AICc(full_extended_model$best_model)

# Based on the AICc results:
#
# - baseline_model AICc = -35.10106
# - full_model AICc = -31.87276
#
# and the economically unexpected coefficient sign of
# lagged_ln_real_barley_price, as well as the insignificance
# of lagged_ln_real_cotton_price,
#
# the full extension is not preferred. However, further investigation
# is needed to uncover the underlying reasons

barley_extended_formula <- ln_area ~
  lagged_ln_real_wheat_price +
  lagged_ln_real_barley_price +
  lagged_ln_yield +
  lagged_ln_political_stability

barley_extended_model <- auto_ardl(
  barley_extended_formula,
  data = ardl_ts,
  max_order = c(1, 1, 1, 1, 1),
  selection = "AICc",
  grid = TRUE
)

summary(barley_extended_model$best_model)

AICc(baseline_model$best_model) <= AICc(barley_extended_model$best_model)

cotton_extended_formula <- ln_area ~
  lagged_ln_real_wheat_price +
  lagged_ln_real_cotton_price +
  lagged_ln_yield +
  lagged_ln_political_stability

cotton_extended_model <- auto_ardl(
  cotton_extended_formula,
  data = ardl_ts,
  max_order = c(1, 1, 1, 1, 1),
  selection = "AICc",
  grid = TRUE
)

summary(cotton_extended_model$best_model)

AICc(baseline_model$best_model) <= AICc(cotton_extended_model$best_model)

# Based on the AICc results:
#
# - baseline_model AICc = -35.10106
# - cotton_extended_model AICc = -31.85127
# - barley_extended_model AICc = -33.85368
#
# Extending the model is not preferred according to AICc

models <- list(
  baseline_model = baseline_model$best_model,
  basic_model = basic_model$best_model,
  full_extended_model = full_extended_model$best_model,
  cotton_extended_model = cotton_extended_model$best_model,
  barley_extended_model = barley_extended_model$best_model
)

orders <- list(
  baseline_model = baseline_model$best_order,
  basic_model = basic_model$best_order,
  full_extended_model = full_extended_model$best_order,
  cotton_extended_model = cotton_extended_model$best_order,
  barley_extended_model = barley_extended_model$best_order
)


ardl_table <- tibble(
  model_names = names(models),
  AICc = sapply(models, AICc),
  N = sapply(models, nobs),
  K = sapply(models, \(m) length(coef(m))),
  BIC = sapply(models, BIC),
  AIC = sapply(models, AIC),
  Adj_R2 = sapply(models, \(m) summary(m)$adj.r.squared),
  Residual_SE = sapply(models, sigma),
  models = models,
  orders = orders
)

print(ardl_table)

# The baseline specification is preferred according to AICc,
# which applies a finite-sample correction to the AIC.
#
# Although the baseline does not have the lowest BIC or AIC,
# the specifications preferred by those criteria include
# lagged_ln_real_barley_price, whose estimated coefficient has
# an unexpected sign relative to the economic hypothesis.
#
# This provides a theoretical and empirical reason for examining
# the competing-crop specifications carefully, but does not by
# itself invalidate them.
#
# Economic interpretation, multicollinearity, residual diagnostics,
# parameter stability, cointegration, and robustness will be examined
# before the final model is committed.


# Bounds test for cointegration

# Although ln_area exhibits a downward trend on visual inspection,
# institutional and political conditions following the conflict, which
# is reflected in the political stability index, is expected to account 
# for part of the observed trend. Consequently, a deterministic trend is 
# not imposed in the baseline model. However, The robustness analysis will
# examine the model's sensitivity to the treatment of the trend term

bounds_test <- bounds_f_test(
  baseline_model$best_model,
  case = 3
)

bounds_test

multipliers(baseline_model$best_model)

# Bounds test suggests that a long-run equilibrium  
# relationship exists between baseline model variables 

recm <- recm(baseline_model$best_model, case = 3)

summary(recm)

uecm <- uecm(baseline_model$best_model, case = 3)

summary(uecm)
