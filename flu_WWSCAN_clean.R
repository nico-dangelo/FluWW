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
# Import CHC data for comparison ------------------------------------------
county_flu_ac_season_norm <- readRDS(file = "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS")|> mutate(year_week=yearweek(year_week_dt))


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
wwscan_flu_city_agg_with_county_info_clean_yw_common_aggregate <- wwscan_flu_city_agg_with_county_info_clean_yw_common |>
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
#Remove counties with inadequate data/avoid excessive interpolation
counties_to_keep <- wwscan_flu_city_agg_with_county_info_clean_ts |>
  as_tibble() |>
  filter(!is.na(Influenza_A_gc_g_dry_weight_pop_wt_sum)) |>
  group_by(county_fips) |>
  tally() |>
  filter(n > 15) |>
  pull(county_fips)

#Linear interpolation on missing data
wwscan_flu_city_agg_with_county_info_clean_ts_imputed <- wwscan_flu_city_agg_with_county_info_clean_ts |> filter(county_fips %in% counties_to_keep)|> fill_gaps(.full = FALSE)|>  group_by_key()|> mutate(Influenza_A_gc_g_dry_weight_pop_wt_imputed = na_interpolation(Influenza_A_gc_g_dry_weight_pop_wt_sum)) 
#plot missingness comparisons by county
# Prepare data for plotting: pivot longer to compare original vs imputed
plot_data <- wwscan_flu_city_agg_with_county_info_clean_ts_imputed |>
  as_tibble() |>
  select(year_week, county_fips, 
         original = Influenza_A_gc_g_dry_weight_pop_wt_sum,
         imputed = Influenza_A_gc_g_dry_weight_pop_wt_imputed) |>
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



# Standardization ---------------------------------------------------------

wwscan_flu_city_agg_with_county_info_ts_standard <- wwscan_flu_city_agg_with_county_info_clean_ts_imputed |> group_by_key()|> mutate(Influenza_A_gc_g_dry_weight_pop_wt_imputed_scaled = 
  scale(Influenza_A_gc_g_dry_weight_pop_wt_imputed)[, 1]) |> ungroup()


# Plot standardized data --------------------------------------------------
plot_data_scaled <- wwscan_flu_city_agg_with_county_info_ts_standard |>
  as_tibble() |>
  select(year_week, county_fips, 
         scaled = Influenza_A_gc_g_dry_weight_pop_wt_imputed_scaled) |>
  pivot_longer(cols = c(scaled), 
               names_to = "type", 
               values_to = "value")

# Get unique counties for pagination
counties <- unique(plot_data_scaled$county_fips)
counties_per_page <- 6  # 3 rows x 2 columns
n_pages <- ceiling(length(counties) / counties_per_page)

# Create paginated plots
pdf("Figures/influenza_scaled_by_county.pdf", width = 12, height = 14)

for (page in 1:n_pages) {
  # Subset counties for this page
  start_idx <- (page - 1) * counties_per_page + 1
  end_idx <- min(page * counties_per_page, length(counties))
  counties_page <- counties[start_idx:end_idx]
  
  # Create plot for this page
  p <- plot_data_scaled |>
    filter(county_fips %in% counties_page) |>
    ggplot(aes(x = year_week, y = value, color = type)) +
    geom_line() +
    facet_wrap(~county_fips, scales = "free_y", ncol = 2) +
    labs(title = paste("Influenza A in WW: Scaled by County (Page", page, "of", n_pages, ")"),
         x = "Year-Week",
         y = "Value",
         color = "Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  print(p)
}

dev.off()

