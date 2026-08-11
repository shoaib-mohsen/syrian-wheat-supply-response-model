# Project: Syrian Wheat Supply Response
# Script : 07_robustness_checks.R
# Purpose: Running Robustness Checks
# Author : Shoaib Mohsen

source("R/functions.R")
source("R/05_ardl_model.R")

rob_data <- as.data.frame(ardl_ts)

# Model 1 (excluding 2019)

rob_data_1 <- rob_data[rob_data$year != 2019, ]

  
  rob_formula_1 <- ln_area ~
    lagged_ln_real_wheat_price +
    lagged_ln_yield +
    lagged_ln_political_stability

  
  rob_model_1 <- auto_ardl(
    rob_formula_1,
    data = rob_data_1,
    max_order = c(1, 0, 0, 0),
    selection = "AICc",
    grid = TRUE
  )

rob1_lm_obj <- to_lm(rob_model_1$best_model, fix_names = TRUE)

# Model 2 (excluding 2022)

rob_data_2 <- rob_data[rob_data$year != 2022, ]

rob_formula_2 <- ln_area ~
  lagged_ln_real_wheat_price +
  lagged_ln_yield +
  lagged_ln_political_stability

rob_model_2 <- auto_ardl(
  rob_formula_2,
  data = rob_data_2,
  max_order = c(1, 0, 0, 0),
  selection = "AICc",
  grid = TRUE
)

rob2_lm_obj <- to_lm(rob_model_2$best_model, fix_names = TRUE)

# Robust Check 3 (excluding both 2019 and 2022)

rob_data_3 <- rob_data[!(rob_data$year %in% c(2019,2022)), ]

rob_formula_3 <- ln_area ~
  lagged_ln_real_wheat_price +
  lagged_ln_yield +
  lagged_ln_political_stability

rob_model_3 <- auto_ardl(
  rob_formula_3,
  data = rob_data_3,
  max_order = c(1, 0, 0, 0),
  selection = "AICc",
  grid = TRUE
)

rob3_lm_obj <- to_lm(rob_model_3$best_model, fix_names = TRUE)

# Robust Check 4 (introducing a dummy for 2019)

rob_data_4 <- rob_data
rob_data_4$d2019 = ifelse(rob_data_4$year == 2019, 1, 0)

rob_formula_4 <- ln_area ~
  lagged_ln_real_wheat_price +
  lagged_ln_yield +
  lagged_ln_political_stability |
  d2019

rob_model_4 <- auto_ardl(
  rob_formula_4,
  data = rob_data_4,
  max_order = c(1, 0, 0, 0),
  selection = "AICc",
  grid = TRUE
)

rob4_lm_obj <- to_lm(rob_model_4$best_model, fix_names = TRUE)

# Robust Check 5 (introducing a dummy for 2022)

rob_data_5 <- rob_data

rob_data_5$d2022 = ifelse(rob_data_5$year == 2022, 1, 0)

rob_formula_5 <- ln_area ~
  lagged_ln_real_wheat_price +
  lagged_ln_yield +
  lagged_ln_political_stability |
  d2022

rob_model_5 <- auto_ardl(
  rob_formula_5,
  data = rob_data_5,
  max_order = c(1, 0, 0, 0),
  selection = "AICc",
  grid = TRUE
)

rob5_lm_obj <- to_lm(rob_model_5$best_model, fix_names = TRUE)

# Robust Check 6 (introducing a dummy for 2019 and  a dummy for 2022)

rob_data_6 <- rob_data

rob_data_6$d2022 = ifelse(rob_data_6$year == 2022, 1, 0)
rob_data_6$d2019 = ifelse(rob_data_6$year == 2019, 1, 0)

rob_formula_6 <- ln_area ~
  lagged_ln_real_wheat_price +
  lagged_ln_yield +
  lagged_ln_political_stability |
  d2019 + d2022

rob_model_6 <- auto_ardl(
  rob_formula_6,
  data = rob_data_6,
  max_order = c(1, 0, 0, 0),
  selection = "AICc",
  grid = TRUE
)

rob6_lm_obj <- to_lm(rob_model_6$best_model, fix_names = TRUE)

#----

m <- baseline_model$best_model

baseline_lm_obj <- to_lm(baseline_model$best_model, fix_names = TRUE)

#----

# Generating the robustness summary

model_summary <- bind_rows(
  build_summary_row("baseline", baseline_model, baseline_lm_obj, rob_data),
  build_summary_row("rob1_drop2019", rob_model_1, rob1_lm_obj, rob_data_1),
  build_summary_row("rob2_drop2022", rob_model_2, rob2_lm_obj, rob_data_2),
  build_summary_row("rob3_drop_both", rob_model_3, rob3_lm_obj, rob_data_3),
  build_summary_row("rob4_dummy2019", rob_model_4, rob4_lm_obj, rob_data_4),
  build_summary_row("rob5_dummy2022", rob_model_5, rob5_lm_obj, rob_data_5),
  build_summary_row("rob6_dummy_both", rob_model_6, rob6_lm_obj, rob_data_6)
)

model_summary
