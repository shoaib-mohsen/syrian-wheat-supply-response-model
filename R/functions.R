# Project: Syrian Wheat Supply Response
# Script : functions.R
# Purpose: all needed functions
# Author : Shoaib Mohsen

library(lmtest)
library(sandwich)

# Converting R table to Word file 
get_doc <- function(tb, caption){
  
  # Creating a formatted flex table from the input
  tb <- flextable::flextable(tb)
  
  tb <- colformat_double(
    tb,
    digits = 3
  )
  
  tb <- autofit(tb)
  tb <- theme_booktabs(tb)
  
  # Creating a Word file
  doc <- read_docx()
  
  # Adding a table title 
  body_add_par(doc,
               caption,
               style = "table title")
  
  # Inserting the table into the Word file
  doc <- body_add_flextable(doc, value = tb)
  
  # Returning
  return(doc)
}

# Computing a correlation matrix and returning variable pairs with an absolute correlation of at least 0.8
get_high_corrs <- function(data) {
  
  # Computing the correlation matrix
  cor_matrix <- cor(data %>% select(where(is.numeric)), use = "complete.obs")
  
  # Removing duplicate correlations and the diagonal
  cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
  
  # Extracting highly correlated pairs
  high_corrs <- as.data.frame(as.table(cor_matrix)) %>%
    drop_na(Freq) %>%
    rename(Correlation = Freq) %>%
    filter(abs(Correlation) >= 0.8) %>%
    mutate(Abs_Corr = abs(Correlation)) %>%
    arrange(desc(Abs_Corr)) %>%
    select(-Abs_Corr)
  
  return(high_corrs)
}

# Generating a table of descriptive statistics
get_summary_table <- function(data) {
  data %>%
    select(where(is.numeric)) %>%
    summarise(across(everything(), list(
      Mean            = ~ mean(.x, na.rm = TRUE),
      Median          = ~ median(.x, na.rm = TRUE),
      SD              = ~ sd(.x, na.rm = TRUE),
      IQR             = ~ IQR(.x, na.rm = TRUE),
      Skewness        = ~ moments::skewness(.x, na.rm = TRUE),
      Excess_Kurtosis = ~ moments::kurtosis(.x, na.rm = TRUE) - 3
    ), .names = "{.col}__{.fn}")) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("Variable", "Metric"),
      names_sep = "__"
    ) %>%
    pivot_wider(names_from = Metric, values_from = value)
}



get_adf_result <- function(data, alpha = 0.05) {
  
  # Initialize output variables
  integration_order <- NULL
  tau_stat <- NA; tau_cv <- NA; tau_type <- NA_character_
  phi_stat <- NA; phi_cv <- NA; phi_type <- NA_character_
  lags_selected <- NA; effective_n <- NA
  test <- NULL
  
  for(i in 0:3){
    if(i==3){
      integration_order <- ">I(2)"
      tau_stat   <- NA; tau_cv <- NA; tau_type <- NA_character_
      phi_stat   <- NA; phi_cv <- NA; phi_type <- NA_character_
      lags_selected <- NA; effective_n <- NA
      test <- NULL
      break
    } else if(i==2){
      var <- diff(data,differences =2)
      io <- "I(2)"
    } else if(i==1){
      var <- diff(data)
      io <- "I(1)"
    } else{
      var <- data
      io <- "I(0)"
    }
    
    if(i==0){
    test <- ur.df(var, type = "trend", selectlags = "AIC", lags=2)
    
    phi_type           = "phi3"
    tau_type           = "tau3"
    lags_selected <- test@lags
    effective_n <- length(test@res)
    
    tau_stat <- test@teststat[1, "tau3"]
    tau_cv   <- test@cval["tau3", paste0(as.character(alpha * 100), "pct")]
    phi_stat <- test@teststat[1, "phi3"]
    phi_cv   <- test@cval["phi3", paste0(as.character(alpha * 100), "pct")]
    
    if (tau_stat <= tau_cv) {
      integration_order <- paste0("Trend-Stationary, ",as.character(io))
      if (phi_stat > phi_cv) break
    }}
  
    if (phi_stat > phi_cv  & i==0) {
      next 
    }
      
      test <- ur.df(var, type = "drift", selectlags = "AIC", lags=2)
      
      phi_type           = "phi1"
      tau_type           = "tau2"
      lags_selected <- test@lags
      effective_n <- length(test@res)
      
      tau_stat <- test@teststat[1, "tau2"]
      tau_cv   <- test@cval["tau2", paste0(as.character(alpha * 100), "pct")]
      phi_stat <- test@teststat[1, "phi1"]
      phi_cv   <- test@cval["phi1", paste0(as.character(alpha * 100), "pct")]
      
      if (tau_stat <= tau_cv) {
        integration_order <- paste0("Drift-Stationary, ",as.character(io))
        if (phi_stat > phi_cv) break
      }
      
      if (phi_stat > phi_cv) {
        next
      } else {
        
        test <- ur.df(var, type = "none", selectlags = "AIC", lags=2)
        
        phi_type           = "NA_character_"
        tau_type           = "tau1"
        lags_selected <- test@lags
        effective_n <- length(test@res)
        
        tau_stat <- test@teststat[1, "tau1"]
        tau_cv   <- test@cval["tau1", paste0(as.character(alpha * 100), "pct")]
        phi_stat <- NA
        phi_cv <- NA
        
        if (tau_stat <= tau_cv) {
          integration_order <- paste0("Zero-Mean-Stationary, ",as.character(io))
          break
        } else {
         next
        }
        
      }
      
      
    

    
  }
  
  
  
  list(
    # --- 1. FINAL CONCLUSION ---
    summary = list(
    integration_order,
    # --- 2. FINAL DECIDING STATISTICS ---
    tau_stat,
    tau_cv,
    tau_type,                # Specifies WHICH tau (tau3, tau2, tau1)
    phi_stat,             # (Will be NA for Step 3)
    phi_cv,
    phi_type,                # (Will be NA for Step 3)
    
    # --- 3. ESTIMATION DETAILS (Crucial for Papers) ---
    lags_selected,       # The exact lag length chosen by AIC
    effective_n# Effective sample size after differencing/lagging
    ),               # The significance level used (e.g., 0.05)
    
    # --- 4. TRACEABILITY & REPLICATION --- 
   details = test          # The raw urca object for residual testing
  )
  
}


get_pp_result <- function(data, alpha = 0.05) {
  
  # Initialize output variables
  integration_order <- NULL
  z_stat <- NA; z_cv <- NA; model_type <- NA_character_
  beta_stat <- NA
  lags_selected <- NA; effective_n <- NA
  test <- NULL
  
  for(i in 0:3){
    if(i==3){
      integration_order <- ">I(2)"
      z_stat   <- NA; z_cv <- NA; model_type <- NA_character_
      beta_stat   <- NA;
      lags_selected <- NA; effective_n <- NA
      test <- NULL
      break
    } else if(i==2){
      var <- diff(data, differences = 2)
      io <- "I(2)"
    } else if(i==1){
      var <- diff(data)
      io <- "I(1)"
    } else{
      var <- data
      io <- "I(0)"
    }
    
    if(i==0){
      
    full_test <- ur.pp(var, type = "Z-tau", model = "trend", use.lag = 2)
    test <- summary(full_test)
    
    model_type           = "trend"
    lags_selected <- full_test@lag
    effective_n <- length(full_test@res)
    
    z_stat <- test@teststat
    z_cv   <- test@cval["critical values", paste0(as.character(alpha * 100), "pct")]
    beta_stat <- test@testreg$coefficients["trend","Pr(>|t|)"]
    
    if (z_stat <= z_cv) {
      integration_order <- paste0("Trend-Stationary, ",as.character(io))
      if(beta_stat < alpha) break
    } }
    
    if (beta_stat < alpha & i==0) {
      next 
    } 
      
      full_test <- ur.pp(var, type = "Z-tau", model = "constant", use.lag = 2)
      test <- summary(full_test)
      
      model_type <- "constant"
      lags_selected <- full_test@lag
      effective_n <- length(full_test@res)
      
      z_stat <- test@teststat
      z_cv   <- test@cval["critical values", paste0(as.character(alpha * 100), "pct")]
      beta_stat <- test@testreg$coefficients["(Intercept)","Pr(>|t|)"]
      
      if (z_stat <= z_cv) {
        integration_order <- paste0("Constant-Stationary, ",as.character(io))
       if(beta_stat < alpha) break
      }
      if(beta_stat < alpha) {
        next
      } else{
        # Applying pp.test{aTSA} for Zero-Mean Stationarity check which it is not provided in ur.pp{urca}
        test <- aTSA::pp.test(var, type = "Z_tau")
        
        model_type <- "none"
        lags_selected <- test[1,"lag"]
        effective_n <- "Not Directly Provided"
        
        z_stat <- test[1,"Z_tau"]
        z_cv   <- "Not Directly Provided"
       
        if(test[1,"p.value"] < 0.05){
          integration_order <- paste0("Zero-Mean-Stationary (using p.value from pp.test{aTSA} Type 1), ",as.character(io))
          break
        }
      }
      
 
  }
  
  
  
  list(
    # --- 1. FINAL CONCLUSION ---
    summary = list(
      integration_order,
      # --- 2. FINAL DECIDING STATISTICS ---
      z_stat,
      z_cv,
      model_type,                # Specifies WHICH tau (tau3, tau2, tau1)
      beta_stat,             # (Will be NA for Step 3) 
      
      # --- 3. ESTIMATION DETAILS (Crucial for Papers) ---
      lags_selected,       # The exact lag length chosen by AIC
      effective_n# Effective sample size after differencing/lagging
    ),               # The significance level used (e.g., 0.05)
    
    # --- 4. TRACEABILITY & REPLICATION --- 
    details = test          # The raw urca object for residual testing
  )
  
}

get_kpss_result <- function(data, alpha = 0.05) {
  
  # Initialize output variables
  integration_order <- NULL
  kpss_stat <- NA; kpss_cv <- NA; model_type <- NA_character_
  lags_selected <- NA; effective_n <- NA
  test <- NULL
  
  for(i in 0:3){
    if(i==3){
      integration_order <- ">I(2)"
      kpss_stat   <- NA; kpss_cv <- NA; model_type <- NA_character_
      lags_selected <- NA; effective_n <- NA
      test <- NULL
      break
    } else if(i==2){
      var <- diff(data, differences = 2)
      io <- "I(2)"
    } else if(i==1){
      var <- diff(data)
      io <- "I(1)"
    } else{
      var <- data
      io <- "I(0)"
    }
    
    if(i==0){
      
      test <- ur.kpss(var, type = "tau", use.lag=2)
      
      model_type           = "tau (Trend + Constant)"
      lags_selected <- test@lag
      effective_n <- length(test@y)
      
      kpss_stat <- test@teststat
      kpss_cv   <- test@cval["critical values", paste0(as.character(alpha * 100), "pct")]
      
      if (kpss_stat <= kpss_cv) {
        integration_order <- paste0("Trend-Stationary, ",as.character(io))
        trend <- 1:length(var)
        kpss_trend_lm <- lm(var ~ trend)
        lm_sum <- summary(kpss_trend_lm)
        if(lm_sum$coefficients["trend","Pr(>|t|)"] <= alpha) break
      } }
    
    test <- ur.kpss(var, type = "mu", use.lag = 2)
    
    model_type <- "mu (Constant only)"
    lags_selected <- test@lag
    effective_n <- length(test@y)
    
    kpss_stat <- test@teststat
    kpss_cv   <- test@cval["critical values", paste0(as.character(alpha * 100), "pct")]
    
    
    if (kpss_stat <= kpss_cv) {
      integration_order <- paste0("Constant-Stationary, ",as.character(io))
      break
    }
 
    
  }
  
  
  
  list(
    # --- 1. FINAL CONCLUSION ---
    summary = list(
      integration_order,
      # --- 2. FINAL DECIDING STATISTICS ---
      kpss_stat,
      kpss_cv,
      model_type,                # Specifies WHICH tau (tau3, tau2, tau1)
      
      # --- 3. ESTIMATION DETAILS (Crucial for Papers) ---
      lags_selected,       # The exact lag length chosen by AIC
      effective_n# Effective sample size after differencing/lagging
    ),               # The significance level used (e.g., 0.05)
    
    # --- 4. TRACEABILITY & REPLICATION --- 
    details = test          # The raw urca object for residual testing
  )
  
}

get_ers_result <- function(data, alpha = 0.05) {
  
  # Initialize output variables
  integration_order <- NULL
  ers_stat <- NA; ers_cv <- NA; model_type <- NA_character_
  lags_selected <- NA; effective_n <- NA
  test <- NULL
  
  for(i in 0:3){
    if(i==3){
      integration_order <- ">I(2)"
      ers_stat   <- NA; ers_cv <- NA; model_type <- NA_character_
      lags_selected <- NA; effective_n <- NA
      test <- NULL
      break
    } else if(i==2){
      var <- diff(data, differences = 2)
      io <- "I(2)"
    } else if(i==1){
      var <- diff(data)
      io <- "I(1)"
    } else{
      var <- data
      io <- "I(0)"
    }
    
    if(i==0){
      
      test <- ur.ers(var, type = "DF-GLS", model="trend", lag.max=2)
      
      model_type           = "trend"
      lags_selected <- test@lag
      effective_n <- length(test@y)
      
      ers_stat <- test@teststat
      ers_cv   <- test@cval["critical values", paste0(as.character(alpha * 100), "pct")]
      
      if (ers_stat <= ers_cv) {
        integration_order <- paste0("Trend-Stationary, ",as.character(io))
      break
      } }
    
    test <- ur.ers(var,  type = "DF-GLS", model="constant", lag.max=2)
    
    model_type <- "constant"
    lags_selected <- test@lag
    effective_n <- length(test@y)
    
    ers_stat <- test@teststat
    ers_cv   <- test@cval["critical values", paste0(as.character(alpha * 100), "pct")]
    
    
    if (ers_stat <= ers_cv) {
      integration_order <- paste0("Constant-Stationary, ",as.character(io))
     break
    }
    
    
  }
  
  
  
  list(
    # --- 1. FINAL CONCLUSION ---
    summary = list(
      integration_order,
      # --- 2. FINAL DECIDING STATISTICS ---
      ers_stat,
      ers_cv,
      model_type,                # Specifies WHICH tau (tau3, tau2, tau1)
      
      # --- 3. ESTIMATION DETAILS (Crucial for Papers) ---
      lags_selected,       # The exact lag length chosen by AIC
      effective_n# Effective sample size after differencing/lagging
    ),               # The significance level used (e.g., 0.05)
    
    # --- 4. TRACEABILITY & REPLICATION --- 
    details = test          # The raw urca object for residual testing
  )
  
}

