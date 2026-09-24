# Project: Syrian Wheat Supply Response
# Script : 04_stationarity_tests.R
# Purpose: checking stationarity for model vatiables 
# Author : Shoaib Mohsen

# Loading the required scripts and packages

library(dplyr)
library(tseries)
library(urca)
library(flextable)
library(officer)
library(lmtest)
library(sandwich)

# Plotting the model variables for (trend/intercept/both) determination

spec <- list(ln_area ="",
              ln_production="",
              ln_yield="",
              ln_asi="",
              ln_political_stability="",
              ln_real_wheat_price="",
              ln_real_cotton_price="",
              ln_real_barley_price="")

varnames <- names(spec)

full_table <- data.frame(
  Variable = varnames,
  ADF = NA_character_,
  PP = NA_character_,
  KPSS = NA_character_,
  ERS = NA_character_,
  row.names = varnames,
  stringsAsFactors = FALSE
)

adf_table <- data.frame(
  Variable = varnames,
  Integration_Order = NA_character_,
  Tau_Stat = NA_real_,
  Tau_CV = NA_real_,
  Tau_Type = NA_character_,
  Phi_Stat = NA_real_,
  Phi_CV = NA_real_,
  Phi_Type = NA_character_,
  Lags_Selected = NA_integer_,
  Effective_N = NA_integer_,
  row.names = varnames,
  stringsAsFactors = FALSE
)

pp_table <- data.frame(
  Variable = varnames,
  Integration_Order = NA_character_,
  Z_Stat=NA,
  Z_Critical_Value=NA,
  Model_Type = NA_character_,
  Beta_Stat=NA,
  Lags_Selected = NA_integer_,
  Effective_N = NA_integer_,
  row.names = varnames,
  stringsAsFactors = FALSE
)

kpss_table <- data.frame(
  Variable = varnames,
  Integration_Order = NA_character_,
  KPSS_Stat=NA,
  KPSS_Critical_Value=NA,
  Model_Type = NA_character_,
  Lags_Selected = NA_integer_,
  Effective_N = NA_integer_,
  row.names = varnames,
  stringsAsFactors = FALSE
)

ers_table <- data.frame(
  Variable = varnames,
  Integration_Order = NA_character_,
  ERS_Stat=NA,
  ERS_Critical_Value=NA,
  Model_Type = NA_character_,
  Lags_Selected = NA_integer_,
  Effective_N = NA_integer_,
  row.names = varnames,
  stringsAsFactors = FALSE
)

spec$ln_real_barley_price <- "INTERCEPT"

for(x in varnames){
  adf_table[x,2:10] <- get_adf_result(full_data[[x]])$summary
  pp_table[x,2:8] <- get_pp_result(full_data[[x]])$summary
  kpss_table[x,2:7] <- get_kpss_result(full_data[[x]])$summary
  ers_table[x,2:7] <- get_ers_result(full_data[[x]])$summary
  
  #if(spec[[x]] == "S_B"){
  #  full_table[x,"ZA"] <- get_za_result(data[[x]],spec[[x]])
  # }
}

for(x in varnames){
  full_table[x,"ADF"] <- adf_table[x,2]
  full_table[x,"PP"] <- pp_table[x,2]
  full_table[x,"KPSS"] <- kpss_table[x,2]
  full_table[x,"ERS"] <- ers_table[x,2]
  #if(spec[[x]] == "S_B"){
  #  full_table[x,"ZA"] <- get_za_result(data[[x]],spec[[x]])
 # }
}

