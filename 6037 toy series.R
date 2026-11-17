pacman::p_load(readr,
               tidyverse,
               surveillance,
               lubridate,
               zoo,
               feasts,
               imputeTS,
               tsibble)
county_flu_ac_season_norm_06037 <- readRDS(file = "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS") |> filter(county_fips=="06037")
# Import inpatient only CHC data
county_week_conf_flu_inpatient_imputed_06037 <- read_csv("county_week_conf_flu_inpatient_imputed.csv") |> mutate(year_week=yearweek(year_week_dt)) |> filter(county_fips=="06037")


# Compute Inpatient and Outpatient incidence ------------------------------
inpatient_06037 <- county_week_conf_flu_inpatient_imputed_06037 |>
  select(county_fips, year_week, year_week_dt, conf_flu_inpatient = conf_flu)

incidence_06037 <- county_flu_ac_season_norm_06037 |>
  mutate(year_week = yearweek(year_week_dt)) |>   
  left_join(inpatient_06037, by = c("county_fips", "year_week")) |>
  mutate(
    inpatient_incidence  = conf_flu_inpatient / season_all_cause,
    outpatient_incidence = (conf_flu_adj - conf_flu_inpatient) / season_all_cause
  ) |>
  select(county_fips, year_week, inpatient_incidence, outpatient_incidence)



# Wastewater data for 06037 -----------------------------------------------

CDC_Wastewater_Data_for_Influenza_A_20260910_clean_06037 <- readRDS("CDC_Wastewater_Data_for_Influenza_A_20260910_clean.RDS")|> filter(county_fips=="06037")

cdc_wastewater_weekly_06037 <- CDC_Wastewater_Data_for_Influenza_A_20260910_clean_06037 |>
  mutate(year_week = yearweek(sample_collect_date)) |>
  group_by(county_fips, year_week) |>
  summarise(
    cdc_flu_a_wastewater = if (all(is.na(pcr_target_flowpop_lin))) NA_real_ else sum(pcr_target_flowpop_lin, na.rm = TRUE),
    .groups = "drop"
  )


wwscan_flu_city_agg_with_county_info_06037 <- readr::read_csv(
  "wwscan_flu_city_agg_with_county_info.csv",
  col_types = cols(
    collection_date = col_date(format = "%m/%d/%Y"),
    FIPS = col_character()
  ),
  col_select = c(
    "collection_date",
    "Influenza_A_gc_g_dry_weight_pop_wt",
    "lat",
    "lon",
    "FIPS"
  )
)|>   mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))|> filter(FIPS=="06037")|> rename(county_fips=FIPS)

wwscan_weekly_06037 <- wwscan_flu_city_agg_with_county_info_06037 |>
  mutate(year_week = yearweek(collection_date)) |>
  group_by(county_fips, year_week) |>
  summarise(
    wwscan_flu_a_gc_g_dry_weight = if (all(is.na(Influenza_A_gc_g_dry_weight_pop_wt))) {
      NA_real_
    } else {
      sum(Influenza_A_gc_g_dry_weight_pop_wt, na.rm = TRUE)
    },
    wwscan_lat = mean(lat, na.rm = TRUE),
    wwscan_lon = mean(lon, na.rm = TRUE),
    .groups = "drop"
  )
combined_data_06037 <- incidence_06037 |>
  full_join(cdc_wastewater_weekly_06037, by = c("county_fips", "year_week")) |>
  full_join(wwscan_weekly_06037,         by = c("county_fips", "year_week")) |>
  arrange(year_week)

# 5. Write to CSV --------------------------------------------------------
write_csv(combined_data_06037, "combined_data_06037.csv", na = "")
