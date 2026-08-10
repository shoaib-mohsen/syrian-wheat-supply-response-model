library(lmtest)
library(tseries)
library(strucchange)

# Extract the underlying model object
m <- baseline_model$best_model

# 1. Serial Correlation: Breusch-Godfrey Test
# Null Hypothesis: No serial correlation up to order p

bg_test <- bgtest(m, order = 2)

#--- Breusch-Godfrey Serial Correlation Test ---

print(bg_test)

# 2. Heteroskedasticity: Breusch-Pagan Test
# Null Hypothesis: Homoskedastic errors (constant variance)

bp_test <- bptest(m)

#--- Breusch-Pagan Heteroskedasticity Test ---

print(bp_test)

# 3. Functional Form: Ramsey RESET Test
# Null Hypothesis: Correct functional form (no omitted non-linearities)

reset_test <- resettest(m, power = 2:3, type = "fitted")

#"--- Ramsey RESET Test ---"

print(reset_test)

# 4. Residual Normality: Jarque-Bera Test & Shapiro-Wilk Test
# Null Hypothesis: Residuals are normally distributed

jb_test <- jarque.bera.test(residuals(m))

#--- Jarque-Bera Normality Test ---

print(jb_test)

sw_test <- shapiro.test(residuals(m))

#--- Shapiro-Wilk Normality Test ---

print(sw_test)

# 5. Structural Stability: CUSUM & MOSUM
# Null Hypothesis: statistical model's parameters are stable and constant over time

baseline_lm_obj <- to_lm(baseline_model$best_model, fix_names = TRUE)

cusum_test <- efp(
  formula(baseline_lm_obj), 
  data = baseline_lm_obj$model, 
  type = "OLS-CUSUM"
)

#--- CUSUM Structural Stability Test ---

sctest(cusum_test)

mosum_test <- efp(
  formula(baseline_lm_obj),
  data = baseline_lm_obj$model,
  type = "OLS-MOSUM"
)

#--- MOSUM Structural Stability Test ---
 
sctest(mosum_test)

# 6. Influential Data Points: studentized residuals, leverage, Cook's distance, and DFBETAs tests
# Null Hypothesis: a data point is typical, has no unusual influence.

n <- nobs(baseline_lm_obj)
k <- length(coef(baseline_lm_obj))

alpha <- 0.05

studentized_resid <- rstudent(baseline_lm_obj)

studentized_table <- data.frame(
  Observation = seq_along(studentized_resid),
  Year = tail(ardl_data$year, length(studentized_resid)),
  Studentized_Residual = studentized_resid
)

#--- Studentized unusual Residuals (Bonferroni-Corrected) ---

df_val <- df.residual(baseline_lm_obj) - 1 

bonferroni_crit <- qt(1 - alpha / (2 * n), df = df_val)

studentized_table$Potentially_Unusual <-
  abs(studentized_table$Studentized_Residual) > bonferroni_crit

print(
  studentized_table[
    studentized_table$Potentially_Unusual,
  ]
)

leverage <- hatvalues(baseline_lm_obj)

leverage_cutoff_2 <- 2 * k / n
leverage_cutoff_3 <- 3 * k / n

leverage_table <- data.frame(
  Observation = seq_along(leverage),
  Year = tail(ardl_data$year, length(studentized_resid)),
  Leverage = leverage
)

#--- Leverage Diagnostics ---

print(leverage_table)

cat(
  "Leverage cutoff (2k/n):",
  round(leverage_cutoff_2, 4),
  "\n"
)

cat(
  "Leverage cutoff (3k/n):",
  round(leverage_cutoff_3, 4),
  "\n"
)

leverage_table$High_Leverage <-
  leverage_table$Leverage > leverage_cutoff_2

print(
  leverage_table[
    leverage_table$High_Leverage,
  ]
)

cooks_d <- cooks.distance(baseline_lm_obj)

cooks_cutoff <- 4 / n

cooks_table <- data.frame(
  Observation = seq_along(cooks_d),
  Year = tail(ardl_data$year, length(studentized_resid)),
  Cooks_Distance = cooks_d
)

#--- Cook's Distance ---

print(cooks_table)

cat(
  "Cook's distance cutoff (4/n):",
  round(cooks_cutoff, 4),
  "\n"
)

cooks_table$Potentially_Influential <-
  cooks_table$Cooks_Distance > cooks_cutoff

print(
  cooks_table[
    cooks_table$Potentially_Influential,
  ]
)

dfbetas_values <- dfbetas(baseline_lm_obj)

dfbeta_cutoff <- 2 / sqrt(n)

#--- DFBETAs ---

print(dfbetas_values)

cat(
  "DFBETA cutoff:",
  round(dfbeta_cutoff, 4),
  "\n"
)

# Identify observations affecting at least one coefficient

dfbeta_flag <- apply(
  abs(dfbetas_values),
  1,
  function(x) any(x > dfbeta_cutoff)
)

dfbeta_table <- data.frame(
  Observation = seq_len(n),
  Year = tail(ardl_data$year, length(studentized_resid)),
  dfbetas_values,
  Potentially_Influential = dfbeta_flag
)

print(
  dfbeta_table[
    dfbeta_table$Potentially_Influential,
  ]
)

dffits_values <- dffits(baseline_lm_obj)

dffits_cutoff <- 2 * sqrt(k / n)

dffits_table <- data.frame(
  Observation = seq_along(dffits_values),
  Year = tail(ardl_data$year, length(studentized_resid)),
  DFFITS = dffits_values
)

#--- DFFITS ---

print(dffits_table)

cat(
  "DFFITS cutoff:",
  round(dffits_cutoff, 4),
  "\n"
)

dffits_table$Potentially_Influential <-
  abs(dffits_table$DFFITS) > dffits_cutoff

print(
  dffits_table[
    dffits_table$Potentially_Influential,
  ]
)


# Combined Influence Summary

influence_summary <- data.frame(
  Year = tail(ardl_data$year, length(studentized_resid)),
  Studentized_Residual = studentized_resid,
  Leverage = leverage,
  Cooks_Distance = cooks_d,
  DFFITS = dffits_values,
  DFBETA_Flag = dfbeta_flag
)

influence_summary$Residual_Flag <-
  abs(influence_summary$Studentized_Residual) > bonferroni_crit

influence_summary$Leverage_Flag <-
  influence_summary$Leverage > leverage_cutoff_2

influence_summary$Cook_Flag <-
  influence_summary$Cooks_Distance > cooks_cutoff

influence_summary$DFFITS_Flag <-
  abs(influence_summary$DFFITS) > dffits_cutoff

influence_summary$Number_of_Flags <-
  rowSums(
    influence_summary[
      ,
      c(
        "Residual_Flag",
        "Leverage_Flag",
        "Cook_Flag",
        "DFFITS_Flag",
        "DFBETA_Flag"
      )
    ]
  )

#--- Combined Influence Diagnostics ---

print(influence_summary)

# Observations flagged by at least two diagnostics

print(
  influence_summary[
    influence_summary$Number_of_Flags >= 2,
  ]
)

