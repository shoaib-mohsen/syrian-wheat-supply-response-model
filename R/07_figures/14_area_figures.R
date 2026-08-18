# Project: Syrian Wheat Supply Response
# Script : 08_figures.R
# Purpose: Generating Figures
# Author : Shoaib Mohsen

library(ggplot2)
library(scales)

real_price <- (raw_data$wheat_price / raw_data$gdp_deflator) * 100

# ------------------------------------------------------------------
# Figure 0a: Wheat Area
# ------------------------------------------------------------------

figure_data_area <- data.frame(
  year = raw_data$year,
  area = raw_data$area
)

p0a <- ggplot(figure_data_area, aes(x = year)) +
  geom_line(aes(y = area, color = "Wheat Area", linetype = "Wheat Area"), linewidth = 0.8) +
  geom_point(aes(y = area, color = "Wheat Area"), size = 2) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6),
    name = "Figure 0a. Wheat Area (million hectares), 2002-2023"
  ) +
  scale_color_manual(name = "", values = c("Wheat Area" = "black")) +
  scale_linetype_manual(name = "", values = c("Wheat Area" = "solid")) +
  labs(title = "Wheat Area", x = "Year") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p0a

ggsave("output/01_area/figures/00_area.png", plot = p0a, width = 8, height = 5, dpi = 300)
ggsave("output/01_area/figures/00_area.pdf", plot = p0a, width = 8, height = 5)

# ------------------------------------------------------------------
# Figure 0b: Real Wheat Price
# ------------------------------------------------------------------

figure_data_price <- data.frame(
  year = raw_data$year,
  real_price = real_price
)

p0b <- ggplot(figure_data_price, aes(x = year)) +
  geom_line(aes(y = real_price, color = "Real Wheat Price", linetype = "Real Wheat Price"), linewidth = 0.8) +
  geom_point(aes(y = real_price, color = "Real Wheat Price"), size = 2) +
  scale_y_continuous(
    labels = label_number(scale = 1e-3),
    name = "Real Wheat Price"
  ) +
  scale_color_manual(name = "", values = c("Real Wheat Price" = "#609c4f")) +
  scale_linetype_manual(name = "", values = c("Real Wheat Price" = "solid")) +
  labs(title = "Figure 0b. Real Wheat Procurement Price (per Kilogram), 2002-2023", x = "Year") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p0b

ggsave("output/01_area/figures/01_price.png", plot = p0b, width = 8, height = 5, dpi = 300)
ggsave("output/01_area/figures/01_price.pdf", plot = p0b, width = 8, height = 5)

# ------------------------------------------------------------------
# Figure 0c: Political Stability
# ------------------------------------------------------------------

figure_data_ps_single <- data.frame(
  year = raw_data$year,
  political_stability = raw_data$political_stability
)

p0c <- ggplot(figure_data_ps_single, aes(x = year)) +
  geom_line(aes(y = political_stability, color = "Political Stability", linetype = "Political Stability"), linewidth = 0.8) +
  geom_point(aes(y = political_stability, color = "Political Stability"), size = 2) +
  scale_y_continuous(
    name = "Political Stability"
  ) +
  scale_color_manual(name = "", values = c("Political Stability" = "#609c4f")) +
  scale_linetype_manual(name = "", values = c("Political Stability" = "solid")) +
  labs(title = "Figure 0c. Political Stability, 2002-2023", x = "Year") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p0c

ggsave("output/01_area/figures/02_political_stability.png", plot = p0c, width = 8, height = 5, dpi = 300)
ggsave("output/01_area/figures/02_political_stability.pdf", plot = p0c, width = 8, height = 5)

# ------------------------------------------------------------------
# Figure 0d: Wheat Yield
# ------------------------------------------------------------------

figure_data_yield_single <- data.frame(
  year = raw_data$year,
  yield = raw_data$yield
)

p0d <- ggplot(figure_data_yield_single, aes(x = year)) +
  geom_line(aes(y = yield, color = "Wheat Yield", linetype = "Wheat Yield"), linewidth = 0.8) +
  geom_point(aes(y = yield, color = "Wheat Yield"), size = 2) +
  scale_y_continuous(
    name = "Wheat Yield"
  ) +
  scale_color_manual(name = "", values = c("Wheat Yield" = "#609c4f")) +
  scale_linetype_manual(name = "", values = c("Wheat Yield" = "solid")) +
  labs(title = "Figure 0d. Wheat Yield, 2002-2023", x = "Year") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p0d

ggsave("output/01_area/figures/03_yield.png", plot = p0d, width = 8, height = 5, dpi = 300)
ggsave("output/01_area/figures/03_yield.pdf", plot = p0d, width = 8, height = 5)

# ------------------------------------------------------------------
# Figure 1: Crop Area vs. Lagged Wheat Price
# ------------------------------------------------------------------

figure_data <- data.frame(
  year = raw_data$year[-1],
  area = raw_data$area[-1],
  lagged_price = real_price[-length(real_price)]
)

p1 <- ggplot(figure_data, aes(x = year)) +
  geom_line(aes(y = area*10, color = "Wheat Area", linetype = "Wheat Area"), linewidth = 0.8) +
  geom_point(aes(y = area*10, color = "Wheat Area"), size = 2) +
  geom_line(aes(y = lagged_price*1000, color = "Lagged Price", linetype = "Lagged Price"), linewidth = 0.8) +
  geom_point(aes(y = lagged_price*1000, color = "Lagged Price"), size = 2) +
  scale_y_continuous(
    labels = label_number(scale = 1e-7 ),
    name = "Wheat Area (million hectares)",
    sec.axis = sec_axis(transform = ~ . / 1, name = "Lagged Real Wheat Price (t-1)",labels =label_number(scale = 1e-6 ))
  ) +
  scale_color_manual(name = "", values = c("Wheat Area" = "black", "Lagged Price" = "#609c4f")) +
  scale_linetype_manual(name = "", values = c("Wheat Area" = "solid", "Lagged Price" = "dashed")) +
  labs(title = "Figure 1. Wheat Area and Lagged Real Procurement Price, 2003–2023", x = "Year") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p1

ggsave("output/01_area/figures/04_area_vs_lagged_price.png", plot = p1, width = 8, height = 5, dpi = 300)
ggsave("output/01_area/figures/04_area_vs_lagged_price.pdf", plot = p1, width = 8, height = 5)

# ------------------------------------------------------------------
# Figure 2: Crop Area vs. Lagged Political Stability
# ------------------------------------------------------------------

political_stability <- raw_data$political_stability
lagged_political_stability <- political_stability[1:(length(real_price) - 1)]

figure_data_ps <- data.frame(
  year = raw_data$year[2:length(raw_data$year)],
  area = raw_data$area[2:length(raw_data$area)],
  lagged_stability = lagged_political_stability
)

p2 <- ggplot(figure_data_ps, aes(x = year)) +
  geom_line(aes(y = area*100, color = "Wheat Area", linetype = "Wheat Area"), linewidth = 0.8) +
  geom_point(aes(y = area*100, color = "Wheat Area"), size = 2) +
  geom_line(aes(y = lagged_stability*10000000/3, color = "Political Stability", linetype = "Political Stability"), linewidth = 0.8) +
  geom_point(aes(y = lagged_stability*10000000/3, color = "Political Stability"), size = 2) +
  scale_y_continuous(
    labels = label_number(scale = 1e-8 ),
    name = "Wheat Area (million hectares)",
    sec.axis = sec_axis(transform = ~ . / 1, name = "Lagged Political Stability (t-1)",labels =label_number(scale = 1e-6/3 ))
  ) +
  scale_color_manual(name = "", values = c("Wheat Area" = "black", "Political Stability" = "#609c4f")) +
  scale_linetype_manual(name = "", values = c("Wheat Area" = "solid", "Political Stability" = "dashed")) +
  labs(title = "Figure 2. Wheat Area and Lagged Political Stability, 2003–2023", x = "Year") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p2

ggsave("output/01_area/figures/05_area_vs_lagged_political_stability.png", plot = p2, width = 8, height = 5, dpi = 300)
ggsave("output/01_area/figures/05_area_vs_lagged_political_stability.pdf", plot = p2, width = 8, height = 5)

# ------------------------------------------------------------------
# Figure 3: Crop Area vs. Lagged Yield
# ------------------------------------------------------------------

yield <- raw_data$yield
lagged_yield <- yield[1:(length(real_price) - 1)]

figure_data_yield <- data.frame(
  year = raw_data$year[2:length(raw_data$year)],
  area = raw_data$area[2:length(raw_data$area)],
  lagged_yield = lagged_yield
)

p3 <- ggplot(figure_data_yield, aes(x = year)) +
  geom_line(aes(y = area, color = "Wheat Area", linetype = "Wheat Area"), linewidth = 0.8) +
  geom_point(aes(y = area, color = "Wheat Area"), size = 2) +
  geom_line(aes(y = lagged_yield*1000000/2, color = "Wheat Yield", linetype = "Wheat Yield"), linewidth = 0.8) +
  geom_point(aes(y = lagged_yield*1000000/2, color = "Wheat Yield"), size = 2) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6 ),
    name = "Wheat Area (million hectares)",
    sec.axis = sec_axis(transform = ~ . / 1, name = "Lagged Wheat Yield (t-1)",labels =label_number(scale = 2e-6))
  ) +
  scale_color_manual(name = "", values = c("Wheat Area" = "black", "Wheat Yield" = "#609c4f")) +
  scale_linetype_manual(name = "", values = c("Wheat Area" = "solid", "Wheat Yield" = "dashed")) +
  labs(title = "Figure 3. Wheat Area and Lagged Wheat Yield, 2003–2023", x = "Year") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p3

ggsave("output/01_area/figures/06_area_vs_lagged_yield.png", plot = p3, width = 8, height = 5, dpi = 300)
ggsave("output/01_area/figures/06_area_vs_lagged_yield.pdf", plot = p3, width = 8, height = 5)

# ------------------------------------------------------------------
# Figure 4: Crop Area vs. Fitted Area
# ------------------------------------------------------------------

fitted_values <- exp(baseline_model$best_model$fitted.values)

figure_data_fitted <- data.frame(
  year = raw_data$year[2:length(raw_data$year)],
  area = raw_data$area[2:length(raw_data$area)],
  fitted_area = fitted_values
)


p4 <- ggplot(figure_data_fitted, aes(x = year)) +
  geom_line(aes(y = area, color = "Wheat Area", linetype = "Wheat Area"), linewidth = 0.8) +
  geom_point(aes(y = area, color = "Wheat Area"), size = 2) +
  geom_line(aes(y = fitted_area, color = "Wheat Fitted Area", linetype = "Wheat Fitted Area"), linewidth = 0.8) +
  geom_point(aes(y = fitted_area, color = "Wheat Fitted Area"), size = 2) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6 ),
    name = "Wheat Area (million hectares)") +
  scale_color_manual(name = "", values = c("Wheat Area" = "black", "Wheat Fitted Area" = "#609c4f")) +
  scale_linetype_manual(name = "", values = c("Wheat Area" = "solid", "Wheat Fitted Area" = "dashed")) +
  labs(title = "Figure 4. Wheat Area and Wheat Fitted Area, 2003–2023", x = "Year") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p4

ggsave("output/01_area/figures/07_area_vs_fitted_area.png", plot = p4, width = 8, height = 5, dpi = 300)
ggsave("output/01_area/figures/07_area_vs_fitted_area.pdf", plot = p4, width = 8, height = 5)