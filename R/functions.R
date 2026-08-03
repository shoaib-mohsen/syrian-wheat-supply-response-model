# Project: Syrian Wheat Supply Response
# Script : functions.R
# Purpose: all needed functions
# Author : Shoaib Mohsen

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