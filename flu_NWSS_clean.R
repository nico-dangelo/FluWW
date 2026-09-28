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
               ggforce)

# Import NWSS Wastewater Data ---------------------------------------------


CDC_Wastewater_Data_for_Influenza_A_20260910_clean <- readRDS("CDC_Wastewater_Data_for_Influenza_A_20260910_clean.RDS")

# Import CHC data for comparison ------------------------------------------
county_flu_ac_season_norm <- readRDS(file = "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS")|> mutate(year_week=yearweek(year_week_dt))

#detect entries with multiple county FIPS codes
CDC_Wastewater_Data_for_Influenza_A_20260910_clean |> summarise(across(county_fips, ~
                                                                         sum(str_detect(unique(
                                                                           .
                                                                         ), ","))))
#separate multiple fips rows
CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi <- CDC_Wastewater_Data_for_Influenza_A_20260910_clean |> separate_rows(county_fips, sep =
                                                                                                                                  ",") |> mutate(county_fips = str_trim(county_fips))
# common counties
common_FIPS <- intersect(
  CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi$county_fips,
  county_flu_ac_season_norm$county_fips
)
#Make year_weeks from sample collection dates
CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw <- CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi |> mutate(year_week=yearweek(sample_collect_date))


# Check missingness in population normalized concentration variable -------
CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw|> group_by(county_fips)|>  summarise(n_na=sum(is.na(pcr_target_flowpop_lin))) |> print(n=Inf)


# Aggregate to weekly scale -----------------------------------------------
CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate<- CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw |> group_by(county_fips, year_week) |> summarise(pcr_target_flowpop_lin_sum=sum(pcr_target_flowpop_lin,na.rm=F), .groups = "drop")

# Make tsibble object -----------------------------------------------------
CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts <- tsibble(CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate, key=county_fips, index = year_week)

# Check duplicates gaps in aggregated series -----------------------------------------
# duplicates(CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts)
CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts_gaps_plot<- CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts|> count_gaps(.full = FALSE)|> filter(.n>5) |> ggplot(aes(x=county_fips, colour=county_fips))+
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme(legend.position = "bottom")
ggplotly(CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts_gaps_plot)


# Drop counties with inadequate data --------------------------------------

counties_keep_NWSS<- CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts|> as_tibble() |>
  filter(!is.na(pcr_target_flowpop_lin_sum)) |>
  group_by(county_fips) |>
  tally() |>
  filter(n > 15) |>
  pull(county_fips)


# Interpolation -----------------------------------------------------------
CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts_interpolated <- CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts |> filter(county_fips %in% counties_keep_NWSS) |> fill_gaps(.full = FALSE)|>  group_by_key()|> mutate(pcr_target_flowpop_lin_sum_interpolated=na_interpolation(pcr_target_flowpop_lin_sum))

#Plot missingness comparisons by county

plot_data_NWSS <- CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi_yw_aggregate_ts_interpolated |>
  as_tibble() |>
  select(year_week, county_fips, 
         original = pcr_target_flowpop_lin_sum,
         imputed = pcr_target_flowpop_lin_sum_interpolated) |>
  pivot_longer(cols = c(original, imputed), names_to = "type", values_to = "value")


# Settings: 2 columns, N rows per page 
n_cols <- 2
n_rows <- 4  # counties per page = n_cols * n_rows = 8


n_counties <- n_distinct(plot_data_NWSS$county_fips)
n_pages <- ceiling(n_counties / (n_cols * n_rows))

# Build the paginated plot template
p <- ggplot(plot_data_NWSS, aes(x = year_week, y = value, color = type)) +
  geom_line(linewidth = 0.8) +
  facet_wrap_paginate(~county_fips, scales = "free_y",
                      ncol = n_cols, nrow = n_rows, page = 1) +
  labs(title = "NWSS Influenza A: Original vs Interpolated Data by County",
       x = "Year-Week", y = "Influenza A (flow-population normalized PCR concentration) Weekly sum",
       color = "Data Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save all pages into a single multi-page PDF
pdf("Figures/nwss_influenza_interpolations_by_county.pdf", width = 12, height = 10)
for (i in seq_len(n_pages)) {
  print(p + facet_wrap_paginate(~county_fips, scales = "free_y",
                                ncol = n_cols, nrow = n_rows, page = i))
}
dev.off()



#Urbanicity data
NCHS_classifications <- read_csv("2023 NCHS classifications.csv")
NCHS_classifications$`2023 Code` <-  as.factor(NCHS_classifications$`2023 Code`)

NCHS_classifications_common_fips <- NCHS_classifications |> filter(Location %in% common_FIPS)|> rename(county_fips=Location)
