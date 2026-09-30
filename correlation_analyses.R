
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
               fable)

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


