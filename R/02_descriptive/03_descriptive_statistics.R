# ==============================================================================
# Project : Syrian Wheat Supply Response
# Script  : 03_descriptive_statistics.R
# Purpose : Produce descriptive statistics tables and descriptive figures
# Author  : Shoaib Mohsen
#
# Expects in the environment:
#   Data frames : full_data, raw_data, model_data
#   Functions   : get_summary_table(), get_high_corrs(), get_doc(),
#                 detect_outliers()
#
# Writes to output/00_general/:
#   tables/  : 01-07 (.docx)
#   figures/ : 00-13 (.png)
# ==============================================================================


# 1. Setup ---------------------------------------------------------------------

# Print numbers in fixed notation instead of scientific notation
options(scipen = 999)


# 2. Summary statistics tables -------------------------------------------------

# Summary statistics for the full sample

summary_table <- get_summary_table(full_data)

print(summary_table, width = Inf)

# High pairwise correlations in the raw data and in the model data

raw_high_corrs <- get_high_corrs(raw_data)

print(raw_high_corrs)

model_high_corrs <- get_high_corrs(model_data)

print(model_high_corrs)

# Split the sample into pre-2011 (year < 2011) and post-2011 (year >= 2011)

post_2011_data <- filter(full_data, year >= 2011)

pre_2011_data <- filter(full_data, year < 2011)

# Summary statistics for each subsample

pre_2011_summary <- get_summary_table(pre_2011_data)

print(pre_2011_summary, width = Inf)

post_2011_summary <- get_summary_table(post_2011_data)

print(post_2011_summary, width = Inf)

# Export the tables to Word documents

print(get_doc(summary_table, caption = "Table 1. Descriptive Statistics"), target = "output/00_general/tables/01_summary_table.docx")

print(get_doc(raw_high_corrs, caption = "Table 2. Raw Data High Correlations"), target = "output/00_general/tables/02_raw_data_high_correlations_table.docx")

print(get_doc(model_high_corrs, caption = "Table 3. Model Data High Correlations"), target = "output/00_general/tables/03_model_data_high_correlations_table.docx")

print(get_doc(pre_2011_summary, caption = "Table 4. Pre 2011 Summary Statistics"), target = "output/00_general/tables/04_pre_2011_data_summary_table.docx")

print(get_doc(post_2011_summary, caption = "Table 5. Post 2011 Summary Statistics"), target = "output/00_general/tables/05_post_2011_data_summary_table.docx")


# 3. Time-series figures -------------------------------------------------------

# Figure 01: wheat production over time

p1 <- ggplot(data = full_data, aes(x = year, y = production)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "darkblue") +
  scale_y_continuous(
    name = "Production (Million Ton)") +
  theme_minimal()

ggsave("output/00_general/figures/01_production.png",
       plot = p1, width = 7, height = 5, units = "in", dpi = 300)

# Figure 02: wheat yield over time

p2 <- ggplot(data = full_data, aes(x = year, y = yield)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "darkgreen") +
  scale_y_continuous(
    name = "Yield (Tons/hectare)") +
  theme_minimal()

ggsave("output/00_general/figures/02_yield.png",
       plot = p2, width = 7, height = 5, units = "in", dpi = 300)

# Figure 03: harvested area over time (axis rescaled to millions of hectares)

p3 <- ggplot(data = full_data, aes(x = year, y = area)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "#BA8E23") +
  scale_y_continuous(
    labels = label_number(scale = 1e-6),
    name = "Area (Million Hectares)") +
  theme_minimal()

ggsave("output/00_general/figures/03_area.png",
       plot = p3, width = 7, height = 5, units = "in", dpi = 300)

# Figure 04: Agricultural Stress Index (ASI) over time

p4 <- ggplot(data = full_data, aes(x = year, y = asi)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "darkgreen") +
  scale_y_continuous(
    name = "Agricultural Stress Index") +
  theme_minimal()

ggsave("output/00_general/figures/04_ASI.png",
       plot = p4, width = 7, height = 5, units = "in", dpi = 300)

# Figure 05: Government Effectiveness Index over time

p5 <- ggplot(data = full_data, aes(x = year, y = government_effectiveness)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "darkgreen") +
  scale_y_continuous(
    name = "Government Effectiveness Index %") +
  theme_minimal()

ggsave("output/00_general/figures/05_Government_Effectiveness.png",
       plot = p5, width = 7, height = 5, units = "in", dpi = 300)

# Figure 06: real wheat price over time

p6 <- ggplot(data = full_data, aes(x = year, y = real_wheat_price)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "darkblue") +
  scale_y_continuous(
    name = "Real Wheat Price") +
  theme_minimal()

ggsave("output/00_general/figures/06_Real_Wheat_Price.png",
       plot = p6, width = 7, height = 5, units = "in", dpi = 300)

# Figure 07: Political Stability Index over time

p7 <- ggplot(data = full_data, aes(x = year, y = political_stability)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "#BA8E23") +
  scale_y_continuous(
    name = "Political Stability Index %") +
  theme_minimal()

ggsave("output/00_general/figures/07_Political_Stability.png",
       plot = p7, width = 7, height = 5, units = "in", dpi = 300)

# Figure 08: ratio of real wheat price to real cotton price over time
p8 <- ggplot(data = full_data, aes(x = year, y = real_wheat_price / real_cotton_price)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "darkblue") +
  scale_y_continuous(
    name = "Real wheat price to real cotton price") +
  theme_minimal()

ggsave("output/00_general/figures/08_Wheat-to-Cotton_Price_Ratio.png",
       plot = p8, width = 7, height = 5, units = "in", dpi = 300)

# Figure 09: ratio of real wheat price to real barley price over time
p9 <- ggplot(data = full_data, aes(x = year, y = real_wheat_price / real_barley_price)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "darkblue") +
  scale_y_continuous(
    name = "Real wheat price to real barley price") +
  theme_minimal()

ggsave("output/00_general/figures/09_Wheat-to-Barley_Price_Ratio.png",
       plot = p9, width = 7, height = 5, units = "in", dpi = 300)

# Figure 00: 3x3 grid of the nine time-series plots above, tagged A-I
p0 <- (p3 | p1 | p2) / (p5 | p6 | p4) / (p9 | p7 | p8) + plot_annotation(tag_levels = "A")

ggsave("output/00_general/figures/00_combined_trends.png", p0, width = 14, height = 10, dpi = 300)


# 4. Outlier checks ------------------------------------------------------------

# Figure 10: one boxplot per variable, free y-axis per facet, outliers in red
p10 <- full_data %>%
  select(year, area, yield, production, asi, government_effectiveness,
         political_stability, real_wheat_price, real_barley_price, real_cotton_price) %>%
  pivot_longer(-year, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(outlier.color = "red") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggsave("output/00_general/figures/10_Boxplots.png",
       plot = p10, width = 7, height = 5, units = "in", dpi = 300)

# Outliers in the real price variables (levels), one row per variable
price_vars <- c("real_wheat_price", "real_cotton_price", "real_barley_price")
outlier_summary <- purrr::map_dfr(price_vars, detect_outliers, data = full_data)
print(outlier_summary)

print(get_doc(outlier_summary, caption = "Table 6. Processed Data Outliers"), target = "output/00_general/tables/06_processed_data_outliers.docx")

# Same outlier check on the log-transformed price variables
ln_price_vars <- c("ln_real_wheat_price", "ln_real_cotton_price", "ln_real_barley_price")
ln_outlier_summary <- purrr::map_dfr(ln_price_vars, detect_outliers, data = full_data)
print(ln_outlier_summary)

print(get_doc(ln_outlier_summary, caption = "Table 7. Model Data Outliers"), target = "output/00_general/tables/07_model_data_outliers.docx")


# 5. Correlation and relationship plots ----------------------------------------

# Correlation matrix of the model data, reshaped to long format for plotting
cor_matrix <- cor(model_data, use = "pairwise.complete.obs")
cor_long <- melt(cor_matrix)

# Figure 11: correlation heatmap with coefficients printed in each tile
p11 <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

ggsave("output/00_general/figures/11_Heatmap.png",
       plot = p11, width = 7, height = 5, units = "in", dpi = 300)

# Figure 12: ASI vs yield scatter, point size = government effectiveness

p12 <- ggplot(full_data, aes(x = asi, y = yield, size = government_effectiveness)) +
  geom_point(alpha = 0.65, color = "darkblue", shape = 21, fill = "steelblue") +
  scale_size_continuous(range = c(3, 14), name = "Government\nEffectiveness (%)") +
  labs(x = "Agricultural Stress Index (%)", y = "Yield (Tons/hectare)") +
  theme_minimal()

ggsave("output/00_general/figures/12_ASI_Yield_GovEffectiveness.png",
       plot = p12, width = 7, height = 5, units = "in", dpi = 300)


# 6. Pre-2011 vs post-2011 mean comparison -------------------------------------

# Stack the pre-2011 and post-2011 summary tables into one data frame,
# adding a `period` column that labels which subsample each row comes from

comparison_df <- bind_rows(
  pre_2011_summary %>% mutate(period = "Pre-2011"),
  post_2011_summary %>% mutate(period = "Post-2011")
)

# Variable groups, used to order the facets in Figure 13
variable_groups <- list(
  core    = c("area", "production", "yield"),
  indexes = c("asi", "political_stability", "government_effectiveness"),
  prices  = c("real_wheat_price", "real_barley_price", "real_cotton_price")
)

# Keep the nine variables, order them by group, and set the period order

comparison_df <- comparison_df %>%  filter(
  Variable %in% c("production", "area", "yield", "real_wheat_price",
                  "political_stability", "government_effectiveness",
                  "asi", "real_barley_price", "real_cotton_price"))  %>%  mutate(
                    Variable = factor(Variable, levels = unlist(variable_groups, use.names = FALSE))) %>% mutate(
                      period = factor(period, levels = c("Pre-2011", "Post-2011")))

# Figure 13: bar chart of pre- vs post-2011 means, one facet per variable
p13 <- ggplot(comparison_df, aes(x = period, y = Mean, fill = period)) +
  geom_col( width = 0.7) +
  scale_fill_manual(
    values = c( "Post-2011" = "#DC2626", "Pre-2011" = "#475569"),
    labels = c("pre-2011" = "Pre-2011 Period", "post-2011" = "Post-2011 Period")
  ) +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(
    y = "Mean",
    fill = "Timeframe"
  )

ggsave("output/00_general/figures/13_Pre_Post_2011_Mean_Comparison.png",
       plot = p13, width = 8, height = 5, units = "in", dpi = 300)
