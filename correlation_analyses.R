
# Libraries ---------------------------------------------------------------
pacman::p_load(tidyverse,
               lubridate,
               zoo,
               feasts,
               imputeTS,
               tsibble,
               slider,
               plotly,
               patchwork,
               gridExtra,
               surveillance,
               RColorBrewer,
               forecast,
               fable,
               knitr,
               kableExtra,
               webshot2,
               htmlwidgets)

# Import cleaned and standardized series ----------------------------------
wwscan_flu_city_agg_with_county_info_ts_standard <- readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/FluWW/Standardized_Time_Series/wwscan_flu_city_agg_with_county_info_ts_standard.RDS")
county_flu_ac_season_norm_ts_standard <- readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/FluWW/Standardized_Time_Series/county_flu_ac_season_norm_ts_standard.RDS")
CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts_interpolated_standard <- readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/FluWW/Standardized_Time_Series/CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts_interpolated_standard.RDS") 
# Compute Correlations ----------------------------------------------------
#ACF
# wwscan_flu_city_agg_with_county_info_ts_standard|> ACF(y=Influenza_A_gc_g_dry_weight_pop_wt_imputed_scaled)|> autoplot()
#CCF
wwscan_chc_joined <- wwscan_flu_city_agg_with_county_info_ts_standard |> select(year_week, county_fips, Influenza_A_gc_g_dry_weight_pop_wt_imputed_scaled) |> inner_join(county_flu_ac_season_norm_ts_standard, by = c("year_week", "county_fips"))
nwss_chc_joined <- CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts_interpolated_standard|> select(year_week, county_fips, pcr_target_flowpop_lin_sum_interpolated_scaled)|> inner_join(county_flu_ac_season_norm_ts_standard, by = c("year_week", "county_fips"))
wwscan_chc_joined_CCF<- wwscan_chc_joined |> group_by_key()|> CCF(Influenza_A_gc_g_dry_weight_pop_wt_imputed_scaled, conf_flu_standard, lag_max = 52, type="correlation")
nwss_chc_joined_CCF <- nwss_chc_joined|> group_by_key() |> CCF(pcr_target_flowpop_lin_sum_interpolated_scaled, conf_flu_standard, lag_max = 52, type = "correlation")


# Plot cross-correlation estimates by county ---------------------------------------
wwscan_chc_joined_CCF_plot <- wwscan_chc_joined_CCF|> as_tibble() |> mutate(
  lag_numeric = as.numeric(lag),
  county_fips = as.character(county_fips)
)

counties_wwscan_chc <- unique(wwscan_chc_joined_CCF_plot$county_fips)
n_per_page <- 6
n_pages <- ceiling(length(counties_wwscan_chc) / n_per_page)

pdf("ccf_by_county_paginated_wwscan.pdf", width = 14, height = 11)

for (page in 1:n_pages) {
  start_idx <- (page - 1) * n_per_page + 1
  end_idx <- min(page * n_per_page, length(counties_wwscan_chc))
  counties_page <- counties_wwscan_chc[start_idx:end_idx]
  
  data_page <- wwscan_chc_joined_CCF_plot %>%
    filter(county_fips %in% counties_page)
  
  p_page <- ggplot(data_page, aes(x = lag_numeric, y = ccf)) +
    geom_segment(aes(xend = lag_numeric, yend = 0), 
                 linewidth = 0.6, color = "steelblue", alpha = 0.7) +
    geom_point(size = 2, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.3) +
    geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", 
               color = "red", linewidth = 0.4, alpha = 0.5) +
    facet_wrap(~county_fips, scales = "free_y", ncol = 2) +
    labs(
      title = paste("WWSCAN-CHC CCF by County — Page", page, "of", n_pages),
      x = "Lag (weeks)",
      y = "Cross-Correlation"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      strip.text = element_text(face = "bold", size = 10),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
    )
  
  print(p_page)
}

dev.off()
 
nwss_chc_joined_CCF_plot <- nwss_chc_joined_CCF|> as_tibble() |> mutate(
  lag_numeric = as.numeric(lag),
  county_fips = as.character(county_fips)
)
counties_nwss_chc <- unique(nwss_chc_joined_CCF_plot$county_fips)
n_pages <- ceiling(length(counties_nwss_chc) / n_per_page)
pdf("ccf_by_county_paginated_nwss.pdf", width = 14, height = 11)

for (page in 1:n_pages) {
  start_idx <- (page - 1) * n_per_page + 1
  end_idx <- min(page * n_per_page, length(counties_nwss_chc))
  counties_page <- counties_nwss_chc[start_idx:end_idx]
  
  data_page <- nwss_chc_joined_CCF_plot %>%
    filter(county_fips %in% counties_page)
  
  p_page <- ggplot(data_page, aes(x = lag_numeric, y = ccf)) +
    geom_segment(aes(xend = lag_numeric, yend = 0), 
                 linewidth = 0.6, color = "steelblue", alpha = 0.7) +
    geom_point(size = 2, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.3) +
    geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", 
               color = "red", linewidth = 0.4, alpha = 0.5) +
    facet_wrap(~county_fips, scales = "free_y", ncol = 2) +
    labs(
      title = paste("NWSS-CHC CCF by County — Page", page, "of", n_pages),
      x = "Lag (weeks)",
      y = "Cross-Correlation"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      strip.text = element_text(face = "bold", size = 10),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
    )
  
  print(p_page)
}

dev.off()

# Stratify by NCHS --------------------------------------------------------
NCHS_classifications <- read_csv("2023 NCHS classifications.csv")
NCHS_classifications$`2023 Code` <-  as.factor(NCHS_classifications$`2023 Code`)
NCHS_classifications <- NCHS_classifications|> rename(county_fips=Location)

#join to ccf datasets

wwscan_chc_joined_CCF_plot_urb <- left_join(wwscan_chc_joined_CCF_plot,NCHS_classifications, by="county_fips")
nwss_chc_joined_CCF_plot_urb <- left_join(nwss_chc_joined_CCF_plot, NCHS_classifications, by="county_fips")

counties_ordered_wwscan <- wwscan_chc_joined_CCF_plot_urb %>%
  distinct(county_fips, `2023 Code`, FullGeoName) %>%
  arrange(`2023 Code`, FullGeoName) %>%
  pull(county_fips)
max_ccf_per_county_wwscan <- wwscan_chc_joined_CCF_plot_urb %>%
  group_by(county_fips) %>%
  slice_max(abs(ccf), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    lag_direction = case_when(
      lag_numeric < 0 ~ paste0(abs(lag_numeric), "w lead"),
      lag_numeric > 0 ~ paste0(lag_numeric, "w lag"),
      lag_numeric == 0 ~ "0w (sync)"
    ),
    label = paste0(lag_direction, "\n", round(ccf, 3))
  ) %>%
  select(county_fips, lag_numeric, ccf, `2023 Code`, FullGeoName, lag_direction, label)
n_per_page <- 6
n_pages_wwscan <- ceiling(length(counties_ordered_wwscan) / n_per_page)
n_pages_nwss <- ceiling(length(counties_ordered_nwss) / n_per_page)

counties_ordered_nwss <-nwss_chc_joined_CCF_plot_urb  %>%
  distinct(county_fips, `2023 Code`, FullGeoName) %>%
  arrange(`2023 Code`, FullGeoName) %>%
  pull(county_fips)

max_ccf_per_county_nwss<- nwss_chc_joined_CCF_plot_urb %>%
  group_by(county_fips) %>%
  slice_max(abs(ccf), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    lag_direction = case_when(
      lag_numeric < 0 ~ paste0(abs(lag_numeric), "w lead"),
      lag_numeric > 0 ~ paste0(lag_numeric, "w lag"),
      lag_numeric == 0 ~ "0w (sync)"
    ),
    label = paste0(lag_direction, "\n", round(ccf, 3))
  ) %>%
  select(county_fips, lag_numeric, ccf, `2023 Code`, FullGeoName, lag_direction, label)
pdf("ccf_by_county_nchs_paginated_annotated_wwscan.pdf", width = 14, height = 12)

for (page in 1:n_pages_wwscan) {
  start_idx <- (page - 1) * n_per_page + 1
  end_idx <- min(page * n_per_page, length(counties_ordered_wwscan))
  counties_page <- counties_ordered_wwscan[start_idx:end_idx]
  
  data_page <- wwscan_chc_joined_CCF_plot_urb %>%
    filter(county_fips %in% counties_page)
  
  max_ccf_page <- max_ccf_per_county_wwscan %>%
    filter(county_fips %in% counties_page)
  
  p_page <- ggplot(data_page, aes(x = lag_numeric, y = ccf)) +
    geom_segment(aes(xend = lag_numeric, yend = 0), 
                 linewidth = 0.6, color = "steelblue", alpha = 0.7) +
    geom_point(size = 2, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.3) +
    geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", 
               color = "red", linewidth = 0.4, alpha = 0.5) +
    geom_point(data = max_ccf_page, size = 4, color = "darkred", shape = 21, stroke = 1.5) +
    # Add text directly on the point instead of above
    geom_text(data = max_ccf_page, 
              aes(label = label),
              vjust = 1.2, hjust = 0.5, size = 2.2, fontface = "bold", color = "darkred",
              nudge_y = 0) +
    facet_wrap(~county_fips + `2023 Code`, scales = "free_y", ncol = 2,
               labeller = labeller(county_fips = function(x) paste("County:", x),
                                   `2023 Code` = function(x) paste("NCHS:", x))) +
    # Expand plot area to accommodate labels
    coord_cartesian(clip = "off") +
    labs(
      title = paste("CCF by County and NCHS Code — Page", page, "of", n_pages_wwscan),
      x = "Lag (weeks)",
      y = "Cross-Correlation",
      caption = "Lead = wastewater precedes cases; Lag = wastewater follows cases"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      strip.text = element_text(face = "bold", size = 10),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt")
    )
  
  print(p_page)
}

dev.off()


pdf("ccf_by_county_nchs_paginated_annotated_nwss.pdf", width = 14, height = 12)

for (page in 1:n_pages_nwss) {
  start_idx <- (page - 1) * n_per_page + 1
  end_idx <- min(page * n_per_page, length(counties_ordered_nwss))
  counties_page <- counties_ordered_nwss[start_idx:end_idx]
  
  data_page <- nwss_chc_joined_CCF_plot_urb %>%
    filter(county_fips %in% counties_page)
  
  max_ccf_page <- max_ccf_per_county_nwss %>%
    filter(county_fips %in% counties_page)
  
  p_page <- ggplot(data_page, aes(x = lag_numeric, y = ccf)) +
    geom_segment(aes(xend = lag_numeric, yend = 0), 
                 linewidth = 0.6, color = "steelblue", alpha = 0.7) +
    geom_point(size = 2, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.3) +
    geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", 
               color = "red", linewidth = 0.4, alpha = 0.5) +
    geom_point(data = max_ccf_page, size = 4, color = "darkred", shape = 21, stroke = 1.5) +
    # Add text directly on the point instead of above
    geom_text(data = max_ccf_page, 
              aes(label = label),
              vjust = 1.2, hjust = 0.5, size = 2.2, fontface = "bold", color = "darkred",
              nudge_y = 0) +
    facet_wrap(~county_fips + `2023 Code`, scales = "free_y", ncol = 2,
               labeller = labeller(county_fips = function(x) paste("County:", x),
                                   `2023 Code` = function(x) paste("NCHS:", x))) +
    # Expand plot area to accommodate labels
    coord_cartesian(clip = "off") +
    labs(
      title = paste("CCF by County and NCHS Code — Page", page, "of", n_pages_nwss),
      x = "Lag (weeks)",
      y = "Cross-Correlation",
      caption = "Lead = wastewater precedes cases; Lag = wastewater follows cases"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      strip.text = element_text(face = "bold", size = 10),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt")
    )
  
  print(p_page)
}

dev.off()



# Histograms --------------------------------------------------------------


# Combine datasets with a source identifier
combined_data <- bind_rows(
  max_ccf_per_county_wwscan %>% mutate(source = "WWSCAN"),
  max_ccf_per_county_nwss %>% mutate(source = "NWSS")
)

# Create faceted histogram
p_combined_hists <- ggplot(combined_data, 
                           aes(x = lag_numeric, fill = `2023 Code`)) +
  geom_histogram(binwidth = 2, color = "black", linewidth = 0.3, alpha = 0.8) +
  facet_wrap(~source, ncol = 2) +
  labs(
    title = "Distribution of Maximum CCF Lags by County",
    x = "Lag (weeks)",
    y = "Frequency (Number of Counties)",
    subtitle = "Negative = wastewater leads; Positive = wastewater lags",
    fill = "NCHS Code"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 11)
  )

# Save
ggsave("Figures/ccf_max_lag_histograms.pdf", p_combined_hists, 
       width = 12, height = 5, dpi = 300)

print(p_combined_hists)


# Summary Tables ----------------------------------------------------------

# WWSCAN summary table
wwscan_lag_summary <- max_ccf_per_county_wwscan %>%
  group_by(`2023 Code`) %>%
  summarise(
    N_Counties = n(),
    Mean_Lag = round(mean(lag_numeric, na.rm = TRUE), 2),
    Median_Lag = median(lag_numeric, na.rm = TRUE),
    SD_Lag = round(sd(lag_numeric, na.rm = TRUE), 2),
    Min_Lag = min(lag_numeric, na.rm = TRUE),
    Max_Lag = max(lag_numeric, na.rm = TRUE),
    Lead_Count = sum(lag_numeric < 0, na.rm = TRUE),
    Sync_Count = sum(lag_numeric == 0, na.rm = TRUE),
    Lag_Count = sum(lag_numeric > 0, na.rm = TRUE),
    Mean_CCF = round(mean(ccf, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  mutate(
    Lead_Pct = round(100 * Lead_Count / N_Counties, 1),
    Sync_Pct = round(100 * Sync_Count / N_Counties, 1),
    Lag_Pct = round(100 * Lag_Count / N_Counties, 1)
  ) %>%
  select(`2023 Code`, N_Counties, Mean_Lag, Median_Lag, SD_Lag, Min_Lag, Max_Lag,
         Lead_Count, Lead_Pct, Sync_Count, Sync_Pct, Lag_Count, Lag_Pct, Mean_CCF)

# NWSS summary table
nwss_lag_summary <- max_ccf_per_county_nwss %>%
  group_by(`2023 Code`) %>%
  summarise(
    N_Counties = n(),
    Mean_Lag = round(mean(lag_numeric, na.rm = TRUE), 2),
    Median_Lag = median(lag_numeric, na.rm = TRUE),
    SD_Lag = round(sd(lag_numeric, na.rm = TRUE), 2),
    Min_Lag = min(lag_numeric, na.rm = TRUE),
    Max_Lag = max(lag_numeric, na.rm = TRUE),
    Lead_Count = sum(lag_numeric < 0, na.rm = TRUE),
    Sync_Count = sum(lag_numeric == 0, na.rm = TRUE),
    Lag_Count = sum(lag_numeric > 0, na.rm = TRUE),
    Mean_CCF = round(mean(ccf, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  mutate(
    Lead_Pct = round(100 * Lead_Count / N_Counties, 1),
    Sync_Pct = round(100 * Sync_Count / N_Counties, 1),
    Lag_Pct = round(100 * Lag_Count / N_Counties, 1)
  ) %>%
  select(`2023 Code`, N_Counties, Mean_Lag, Median_Lag, SD_Lag, Min_Lag, Max_Lag,
         Lead_Count, Lead_Pct, Sync_Count, Sync_Pct, Lag_Count, Lag_Pct, Mean_CCF)
comparison_table <- bind_rows(
  wwscan_lag_summary %>% mutate(Dataset = "WWSCAN"),
  nwss_lag_summary %>% mutate(Dataset = "NWSS")
) %>%
  select(Dataset, `2023 Code`, everything())

kable(wwscan_lag_summary, 
      caption = "WWSCAN Maximum CCF Lag Summary by NCHS Code",
      format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))

kable(nwss_lag_summary, 
      caption = "NWSS Maximum CCF Lag Summary by NCHS Code",
      format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))

# Save as formatted PDF
# library(kableExtra)

# pdf("Figures/ccf_lag_summary_tables.pdf", width = 14, height = 8)

# Create the comparison table with both datasets
comparison_table <- bind_rows(
  wwscan_lag_summary %>% mutate(Dataset = "WWSCAN"),
  nwss_lag_summary %>% mutate(Dataset = "NWSS")
) %>%
  select(Dataset, `2023 Code`, everything())

# # Create styled HTML table
# html_table <- kable(comparison_table, 
#                     caption = "WWSCAN vs NWSS: Maximum CCF Lag Comparison by NCHS Code",
#                     format = "html") %>%
#   kable_styling(full_width = FALSE, 
#                 bootstrap_options = c("striped", "hover"))
# 
# # Save as temporary HTML file
# temp_html <- tempfile(fileext = ".html")
# write(as.character(html_table), temp_html)
# 
# # Convert HTML to PDF using webshot2
# webshot2::webshot(temp_html, 
#                   output = "Figures/ccf_lag_summary_tables.pdf",
#                   vwidth = 1400,   # width in pixels (14 inches × 100)
#                   vheight = 800)   # height in pixels (8 inches × 100)
