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
wwscan_flu_city_agg_with_county_info <- readr::read_csv(
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
)
CDC_Wastewater_Data_for_Influenza_A_20260910_clean <- readRDS("CDC_Wastewater_Data_for_Influenza_A_20260910_clean.RDS")

# common counties
common_FIPS <- intersect(
  wwscan_flu_city_agg_with_county_info$FIPS,
  county_flu_ac_season_norm$county_fips
)
NCHS_classifications <- read_csv("2023 NCHS classifications.csv")
NCHS_classifications$`2023 Code` <-  as.factor(NCHS_classifications$`2023 Code`)
NCHS_classifications_common_fips <- NCHS_classifications |> filter(Location %in% common_FIPS)

wwscan_flu_city_agg_with_county_info_clean<-wwscan_flu_city_agg_with_county_info |> 
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
#compute year_week for wastewater data to match flu data
wwscan_flu_city_agg_with_county_info_clean_yw <- wwscan_flu_city_agg_with_county_info_clean |> mutate(year_week =
                                                                                                        yearweek(collection_date, week_start = 1)) |> rename(county_fips = FIPS) |>relocate(year_week, .after =
                                                                                                                                                                              collection_date)
#keep only common counties
wwscan_flu_city_agg_with_county_info_clean_yw_common <- wwscan_flu_city_agg_with_county_info_clean_yw |> filter(county_fips %in% common_FIPS)
county_flu_ac_season_norm_common <- county_flu_ac_season_norm |> filter(county_fips %in% common_FIPS)

# Make tsibbles
#Aggregate to weekly to avoid duplicates
ß <- wwscan_flu_city_agg_with_county_info_clean_yw_common |>
  group_by(year_week, county_fips) |>
  summarise(
    Influenza_A_gc_g_dry_weight_pop_wt_sum = sum(Influenza_A_gc_g_dry_weight_pop_wt, na.rm = F),
    lat = first(lat),
    lon = first(lon),
    collection_date = first(collection_date),
    .groups = "drop"
  ) |>
  as_tsibble(index = year_week, key = county_fips)


wwscan_flu_city_agg_with_county_info_clean_ts <- tsibble(wwscan_flu_city_agg_with_county_info_clean_yw_common_aggregate, key=county_fips, index=year_week)

#check implicit gaps before interpolation
wwscan_flu_city_agg_with_county_info_clean_ts_gaps_plot <- wwscan_flu_city_agg_with_county_info_clean_ts |> count_gaps() |>  arrange(county_fips)|> ggplot(aes(x=county_fips, colour=county_fips))+
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme(legend.position = "bottom")
ggplotly(wwscan_flu_city_agg_with_county_info_clean_ts_gaps_plot)

#Linear interpolation on missing data
wwscan_flu_city_agg_with_county_info_clean_ts_imputed <- wwscan_flu_city_agg_with_county_info_clean_ts |> fill_gaps(.full = FALSE)|>  group_by_key(county_fips)|> na_interpolation() |> ungroup()|> as_tsibble(key=county_fips, index=year_week)
#plot missingness comparisons by county
# Prepare data for plotting: pivot longer to compare original vs imputed
plot_data <- wwscan_flu_city_agg_with_county_info_clean_ts |>
  as_tibble() |>
  mutate(original = Influenza_A_gc_g_dry_weight_pop_wt_sum) |>
  select(year_week, county_fips, original) |>
  bind_cols(
    wwscan_flu_city_agg_with_county_info_clean_ts_imputed |>
      as_tibble() |>
      select(Influenza_A_gc_g_dry_weight_pop_wt_sum) |>
      rename(imputed = Influenza_A_gc_g_dry_weight_pop_wt_sum)
  ) |>
  pivot_longer(cols = c(original, imputed), names_to = "type", values_to = "value")

# Settings: 2 columns, N rows per page 
n_cols <- 2
n_rows <- 4  # counties per page = n_cols * n_rows = 8


n_counties <- n_distinct(plot_data$county_fips)
n_pages <- ceiling(n_counties / (n_cols * n_rows))

# Build the paginated plot template
p <- ggplot(plot_data, aes(x = year_week, y = value, color = type)) +
  geom_line(linewidth = 0.8) +
  facet_wrap_paginate(~county_fips, scales = "free_y",
                      ncol = n_cols, nrow = n_rows, page = 1) +
  labs(title = "Influenza A: Original vs Interpolated Data by County",
       x = "Year-Week", y = "Influenza A (gc/g dry weight, population weighted)",
       color = "Data Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save all pages into a single multi-page PDF
pdf("influenza_interpolations_by_county.pdf", width = 12, height = 10)
for (i in seq_len(n_pages)) {
  print(p + facet_wrap_paginate(~county_fips, scales = "free_y",
                                ncol = n_cols, nrow = n_rows, page = i))
}
dev.off()

#Remove counties with inadequate data/excessive interpolation
counties_to_keep <- wwscan_flu_city_agg_with_county_info_clean_ts |>
  as_tibble() |>
  filter(!is.na(Influenza_A_gc_g_dry_weight_pop_wt_sum)) |>
  group_by(county_fips) |>
  tally() |>
  filter(n >= 52) |>
  pull(county_fips)

wwscan_flu_city_agg_with_county_info_clean_ts_imputed <- wwscan_flu_city_agg_with_county_info_clean_ts_imputed |>
  filter(county_fips %in% counties_to_keep)
#Check implicit gaps in imputed data that remain
wwscan_flu_city_agg_with_county_info_clean_ts_imputed_gaps_plot <- count_gaps(wwscan_flu_city_agg_with_county_info_clean_ts_imputed)|> arrange(county_fips)|> ggplot(aes(x=county_fips, colour=county_fips))+
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme(legend.position = "bottom")
ggplotly(wwscan_flu_city_agg_with_county_info_clean_ts_imputed_gaps_plot)
