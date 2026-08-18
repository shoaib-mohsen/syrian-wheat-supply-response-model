# ==============================================================================
# Project: Syrian Wheat Supply Response Model
# Script : 00_master.R
# Purpose: Run the complete empirical research pipeline
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Project setup
# ------------------------------------------------------------------------------

source("R/functions.R")


# ------------------------------------------------------------------------------
# 1. Data preparation
# ------------------------------------------------------------------------------

source("R/01_data/01_import_data.R")
source("R/01_data/02_process_data.R")


# ------------------------------------------------------------------------------
# 2. Descriptive analysis
# ------------------------------------------------------------------------------

source("R/02_descriptive/03_descriptive_statistics.R")


# ------------------------------------------------------------------------------
# 3. Stationarity
# ------------------------------------------------------------------------------

source("R/03_stationarity/04_stationarity_tests.R")


# ------------------------------------------------------------------------------
# 4. Area response model
# ------------------------------------------------------------------------------

source("R/04_area_response/05_area_model.R")
source("R/04_area_response/06_area_diagnostics.R")
source("R/04_area_response/07_area_robustness.R")


# ------------------------------------------------------------------------------
# 5. Yield response model
# ------------------------------------------------------------------------------

source("R/05_yield_response/08_yield_model.R")
source("R/05_yield_response/09_yield_diagnostics.R")
source("R/05_yield_response/10_yield_robustness.R")


# ------------------------------------------------------------------------------
# 6. Production analysis
# ------------------------------------------------------------------------------

source("R/06_production/11_production_aggregation.R")
source("R/06_production/12_production_decomposition.R")
source("R/06_production/13_production_scenarios.R")


# ------------------------------------------------------------------------------
# 7. Figures
# ------------------------------------------------------------------------------

source("R/07_figures/14_area_figures.R")
source("R/07_figures/15_yield_figures.R")
source("R/07_figures/16_production_figures.R")


# ------------------------------------------------------------------------------
# 8. Tables
# ------------------------------------------------------------------------------

source("R/08_tables/17_area_tables.R")
source("R/08_tables/18_yield_tables.R")
source("R/08_tables/19_production_tables.R")


# ------------------------------------------------------------------------------
# End of research pipeline
# ------------------------------------------------------------------------------

message("Research pipeline completed successfully.")