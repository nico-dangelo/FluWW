

# Libraries ---------------------------------------------------------------
library(tidyverse)
# library(spiralize)
library(zoo)
library(magrittr)
library(lubridate)
library(slider)
library(sp)
library(sf)
library(RColorBrewer)
library(TSstudio)
library(DescTools)
library(patchwork)
library(plotly)
library(knitr)
library(gridExtra)
# Data import and variable casting ----------------------------------------
##WWSCAN only
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
#CDC NWSS -- includes some WWSCAN
CDC_Wastewater_Data_for_Influenza_A_20260910 <- read_csv("CDC_Wastewater_Data_for_Influenza_A_20260910.csv")
#clean inappropriate commas from numeric variables
CDC_Wastewater_Data_for_Influenza_A_20260910_clean <- CDC_Wastewater_Data_for_Influenza_A_20260910 |> mutate_at(vars(flow_rate), function(x) {
  gsub(",", "", x)
})
# Import CHC data for comparison ------------------------------------------
county_flu_ac_season_norm <- readRDS(file = "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS")


#Check fips encoding and counts
length(unique(wwscan_flu_city_agg_with_county_info$FIPS))
length(unique(county_flu_ac_season_norm$county_fips))
# common counties
common_FIPS <- intersect(
  wwscan_flu_city_agg_with_county_info$FIPS,
  county_flu_ac_season_norm$county_fips
)
length(common_FIPS)

# Import NCHS Urbanicity data ---------------------------------------------

NCHS_classifications <- read_csv("2023 NCHS classifications.csv")
NCHS_classifications$`2023 Code` <-  as.factor(NCHS_classifications$`2023 Code`)
#Check overall distribution first
table(NCHS_classifications$`2023 Code`)|> prop.table()*100

# Only use relevant fips codes
NCHS_classifications_common_fips <- NCHS_classifications |> filter(Location %in% common_FIPS)
table(NCHS_classifications_common_fips$`2023 Code`) |> prop.table() * 100 
# Subset example data -- Palm Beach County --------------------------------
# flu_PBC <- wwscan_flu_city_agg_with_county_info %>% filter(FIPS==12099)
# plot(flu_PBC$collection_date,flu_PBC$Influenza_A_gc_g_dry_weight_pop_wt)
#
#
# # Handle NAs and NaNs --------------------------------------------------------------
# #Replace NaN with NA
# #make value of most recent non-NA data
# flu_PBC_clean <- flu_PBC
# for(i in which(is.na(flu_PBC$Influenza_A_gc_g_dry_weight_pop_wt))) {
#   flu_PBC_clean$Influenza_A_gc_g_dry_weight_pop_wt[[i]] = flu_PBC_clean$Influenza_A_gc_g_dry_weight_pop_wt[[i-1]]
# }

wwscan_flu_city_agg_with_county_info |> mutate_all( ~ ifelse(is.nan(.), NA, .)) |> group_by(FIPS) |> summarise(n =
                                                                                                                 n(), n_NA = sum(is.na(Influenza_A_gc_g_dry_weight_pop_wt)))
wwscan_flu_city_agg_with_county_info_clean <- wwscan_flu_city_agg_with_county_info |> group_by(FIPS)  |> na.locf() |> ungroup()
wwscan_flu_city_agg_with_county_info_clean |> summarize(n_NA = sum(is.na(Influenza_A_gc_g_dry_weight_pop_wt)))
#
# county_flu_ac_season_norm <- readRDS(file = "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS")
# library(zoo)
# library(tidyverse)
# county_flu_zoo <- county_flu_ac_season_norm %>%
#   group_by(county_fips) %>%
#   nest() %>%
#   mutate(
#     zoo_obj = map(data, ~ zoo(
#       x = .x$conf_flu,
#       order.by = as.Date(.x$year_week_dt, format = "%G-%V-%u")
#     ))
#   ) %>%
#   select(county_fips, zoo_obj)
# autoplot.zoo(county_flu_zoo)
# 



#compute year_week for wastewater data to match flu data
wwscan_flu_city_agg_with_county_info_clean_yw <- wwscan_flu_city_agg_with_county_info_clean |> mutate(year_week =
                                                                                                        strftime(collection_date, format = "%G-%V")) |> rename(county_fips = FIPS) |> relocate(year_week, .after =
                                                                                                                                                                                                 collection_date)

#Align CHC and WW series
wwscan_flu_city_agg_with_county_info_clean_yw_common <- wwscan_flu_city_agg_with_county_info_clean_yw |> filter(county_fips %in% common_FIPS)
county_flu_ac_season_norm_common <- county_flu_ac_season_norm |> filter(county_fips %in% common_FIPS)
county_flu_ac_season_norm_common |> group_by(county_fips) |> summarize(n_NA =
                                                                         sum(is.na(conf_flu))) |> print(n = Inf) |> ungroup()
#compute sum of observations in a week
#compute seven day rolling mean of ww concentrations
wwscan_flu_city_agg_with_county_info_clean_yw_common_roll <- wwscan_flu_city_agg_with_county_info_clean_yw_common |> group_by(county_fips, year_week) |> summarize(
  sum_week = sum(Influenza_A_gc_g_dry_weight_pop_wt, na.rm = T),
  roll_mean_week = mean(Influenza_A_gc_g_dry_weight_pop_wt, na.rm = T),
  .groups = "drop"
)
# wwscan_flu_city_agg_with_county_info_clean_yw_common_roll<- wwscan_flu_city_agg_with_county_info_clean_yw_common %>% group_by(county_fips) %>% arrange(year_week_dt)  %>% mutate(rolling_mean = slide_index_dbl(
#   Influenza_A_gc_g_dry_weight_pop_wt,
#   .i = year_week_dt,
#   .f = mean,
#   .before = days(7)
# )) %>%
#   ungroup()
# ggplot(wwscan_flu_city_agg_with_county_info_clean_yw_common_roll|> filter(county_fips %in% common_FIPS[20:30]), aes(x=year_week_dt, y=roll_mean_week, group=county_fips))  + geom_line(aes(color=county_fips))

# match on county_fips and year_week
wwscan_flu_city_agg_with_county_info_clean_yw_common_roll %>% group_by(county_fips) %>% summarise(
  n = n(),
  n_NA = sum(is.na(roll_mean_week) | is.na(sum_week)),
  n_comp = sum(!is.na(roll_mean_week) &
                 !is.na(sum_week))
) |> print(n = Inf)

wwscan_flu_ac_season_norm_county_week_matched <- inner_join(
  wwscan_flu_city_agg_with_county_info_clean_yw_common_roll,
  county_flu_ac_season_norm_common,
  by = c("county_fips", "year_week")
) |> select(!season_fit)



wwscan_flu_ac_season_norm_county_week_matched |> group_by(county_fips) |> summarise(
  N = n(),
  n_NA_cfn = sum(is.na(conf_flu)),
  n_comp = sum(
    !is.na(conf_flu) &
      !is.na(roll_mean_week) &
      !is.na(sum_week)
  )
) |>  print(n = Inf) |> ungroup()

# Keep only series with >=10 week observations
wwscan_flu_ac_season_norm_county_week_matched_keep <- wwscan_flu_ac_season_norm_county_week_matched |> group_by(county_fips) |> filter(sum(!is.na(sum_week)) >= 10,
                                                                                                                                       sum(!is.na(conf_flu)) >= 10,
                                                                                                                                       sum(!is.na(roll_mean_week)) >= 10) |> ungroup()

# #  wwscan_flu_ac_season_norm_county_week_matched_keep <- wwscan_flu_ac_season_norm_county_week_matched |> group_by(county_fips)|> filter(n()>10)|> na.omit() |> distinct() |> ungroup()
# #
# # #  # Compute granger causality on aligned series --------------------------
# #
# #
# # wwscan_flu_ac_season_norm_county_week_matched_split<- wwscan_flu_ac_season_norm_county_week_matched_keep %>% split(.$county_fips_grp)
#
#
# #Make into time series objects
#
#  wwscan_flu_ac_season_norm_county_week_ts <- wwscan_flu_ac_season_norm_county_week_matched_split |> lapply(function(X){
#    mutate(X, conf_flu_norm_ts = zoo(conf_flu_norm, order.by = year_week),
#           roll_mean_week_ts = zoo(roll_mean_week, order.by = year_week))})
#
#  # Apply granger test
#  # CHC flu predicted by mean ww
#  granger_CHC_WW_res<- wwscan_flu_ac_season_norm_county_week_ts|> lapply(function(x){if(nrow(x)>=10){granger_df=data.frame(conf_flu_norm=as.numeric(coredata(x$conf_flu_norm_ts)), roll_mean_week=as.numeric(coredata(x$roll_mean_week_ts))); lmtest::grangertest(conf_flu_norm ~ roll_mean_week, order=2, data=granger_df)}})
#  # Mean WW predicted by CHC flu
#  granger_WW_CHC_res<- wwscan_flu_ac_season_norm_county_week_ts|> lapply(function(x){if(nrow(x)>=10){granger_df=data.frame(conf_flu_norm=as.numeric(coredata(x$conf_flu_norm_ts)), roll_mean_week=as.numeric(coredata(x$roll_mean_week_ts))); lmtest::grangertest(roll_mean_week~conf_flu_norm, order=2, data=granger_df)}})
#
#
# # Compute Dynamic Time Warp -----------------------------------------------
# library(dtw)
#  #lists of series pairs
# dtw_res<- lapply(wwscan_flu_ac_season_norm_county_week_ts, function(X){dtw(X$conf_flu_norm_ts, X$roll_mean_week_ts)$distance})
# # Matrices of multivariate series
# # dtw_mat_res<-
# # Apply Cluster Permutation test ------------------------------------------
# library(permutes)
# #separate linear models
# cplm_res<- wwscan_flu_ac_season_norm_county_week_ts|> lapply(function(X){clusterperm.lm(conf_flu_norm_ts ~ roll_mean_week_ts, data=X)})
# #mixed effect linear model
# # wwscan_flu_ac_season_norm_county_week_ts |> lapply(function(X){clusterperm.lmer(conf_flu_norm_ts~roll_mean_week_ts +(1|county_fips), data=X)})
# # Compute Transfer Entropy ------------------------------------------------
# library(RTransferEntropy)
# te_res<- wwscan_flu_ac_season_norm_county_week_ts|> lapply(function(X){transfer_entropy(X$conf_flu_norm_ts,X$roll_mean_week_ts )})
#  # Summary tables ----------------------------------------------------------
#
#
# granger_CHC_WW_res |> knitr::kable()
# granger_WW_CHC_res|> knitr::kable()
# dtw_res |> knitr::kable()
# te_res |> knitr::kable()


# Function to convert regression summary to data frame
# summary_to_df <- function(summary_obj) {
#   coefs <- summary_obj$coefficients
#   df <- data.frame(
#     term = rownames(coefs),
#     estimate = coefs[, "Estimate"],
#     std.error = coefs[, "Std. Error"],
#     statistic = coefs[, "t value"],
#     p.value = coefs[, "Pr(>|t|)"]
#   )
#   return(df)
# }


# Line and seasonality plots ----------------------------------------------

# wwscan_flu_ac_season_norm_county_week_matched_keep |> ggplot(aes(x=year_week, y=log(roll_mean_week), colour = county_fips_grp, group=county_fips_grp)) + geom_line()

# Add seasonal, summer, and  indicators for seasonal/period analysis ----------------------------

ww_chc_county_season <- wwscan_flu_ac_season_norm_county_week_matched_keep |> mutate(
  year = as.numeric(substr(year_week, 1, 4)),
  week =
    as.numeric(substr(year_week, 6, 7)),
  summer =
    ifelse(month(year_week_dt) >= 6 & month(year_week_dt) <= 9, T, F)
)
# ww_chc_county_season |> filter(season=="2023-2024") |>  ggplot(aes(x=factor(year_week, levels=unique(year_week)), y=log(roll_mean_week), group = county_fips, color=county_fips)) + geom_line()
# ww_chc_county_season |> filter(season=="2023-2024") |>  ggplot(aes(x=factor(year_week, levels=unique(year_week)), y=log(sum_week), group = county_fips, color=county_fips)) + geom_line()

# Standardization ---------------------------------------------------------

# WW and CHC data ---------------------------------------------------------


#Compute summer means (group by county_fips, year)
summer_means <- ww_chc_county_season |> 
  filter(summer) |>
  group_by(county_fips, year) |> 
  summarise(
    summer_mean_ww_mean_week = mean(roll_mean_week, na.rm = TRUE),
    summer_mean_ww_sum = mean(sum_week, na.rm = TRUE),
    summer_mean_chc_count = mean(conf_flu, na.rm = TRUE),
    summer_mean_chc_norm = mean(conf_flu_norm, na.rm = TRUE),
    .groups = "drop")

# Join back to full dataset and compute differences
ww_chc_county_season_summer_mean_diff <- ww_chc_county_season |>
  left_join(summer_means, by = c("county_fips", "year")) |>
  mutate(
    summer_mean_diff_ww_mean_week = roll_mean_week - summer_mean_ww_mean_week,
    summer_mean_diff_ww_sum = sum_week - summer_mean_ww_sum,
    summer_mean_diff_chc_count = conf_flu - summer_mean_chc_count,
    summer_mean_diff_chc_norm = conf_flu_norm - summer_mean_chc_norm
  )

# Standardize (compute SD across full year, not just summer)
ww_chc_county_season_standard <- ww_chc_county_season_summer_mean_diff |> 
  group_by(county_fips) |> 
  mutate(
    st_dev_ww_mean_week = sd(roll_mean_week, na.rm = TRUE),
    st_dev_ww_sum_week = sd(sum_week, na.rm = TRUE),
    st_dev_chc_count = sd(conf_flu, na.rm = TRUE),
    st_dev_chc_norm = sd(conf_flu_norm, na.rm = TRUE),
    roll_mean_week_std = summer_mean_diff_ww_mean_week / st_dev_ww_mean_week,
    sum_week_std = summer_mean_diff_ww_sum / st_dev_ww_sum_week,
    conf_flu_count_std = summer_mean_diff_chc_count / st_dev_chc_count,
    conf_flu_norm_std = summer_mean_diff_chc_norm / st_dev_chc_norm
  ) |> 
  ungroup()

ww_chc_county_season_standard |> group_by(county_fips) |>  summarise(across(
  c(conf_flu_count_std, conf_flu_norm_std, roll_mean_week_std),
  list(min = min, max = max),
  na.rm = T
))

#Check missingness
ww_chc_county_season_standard |>
  summarise(sum_week_std_na = sum(is.na(sum_week_std)),
            conf_flu_count_std_na = sum(is.na(conf_flu_count_std)), 
            )
ww_chc_county_season_standard |>
  group_by(county_fips) |>
  summarise(across(
    c(conf_flu_count_std, conf_flu_norm_std, roll_mean_week_std),
    ~sum(!is.na(.x)),  # Count non-NA values
    .names = "{.col}_n_obs"
  ))|> print(n=Inf)
# ww_chc_county_season_standard|> is.na(
# )
ww_chc_county_season_standard |>
  group_by(county_fips) |>
  summarise(
    sd_sum_week = sd(sum_week, na.rm = TRUE),
    sd_conf_flu = sd(conf_flu, na.rm = TRUE),
    min_sum_week = min(sum_week, na.rm = TRUE),
    max_sum_week = max(sum_week, na.rm = TRUE),
    min_conf_flu = min(conf_flu, na.rm = TRUE),
    max_conf_flu = max(conf_flu, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(sd_sum_week < 0.01 | sd_conf_flu < 0.01 |
           is.na(sd_sum_week) | is.na(sd_conf_flu))

ww_chc_county_season_standard |>
  group_by(county_fips) |>
  summarise(
    diff_sum_na = sum(is.na(summer_mean_diff_ww_sum)),
    diff_sum_n = n(),
    diff_conf_na = sum(is.na(summer_mean_diff_chc_count)),
    diff_conf_n = n(),
    .groups = "drop"
  ) |>
  filter(diff_sum_na == diff_sum_n | diff_conf_na == diff_conf_n)



# ww_chc_county_season_standard |>
#   filter(if_any(c(sum_week_std, conf_flu_count_std, roll_mean_week_std), \x(is.na(x) | is.nan(x))))

ww_chc_county_season_standard |>
  mutate(
    complete = !if_any(c(sum_week_std, conf_flu_count_std, roll_mean_week_std), \(x) is.na(x) | is.nan(x))
  ) |>
  group_by(county_fips, complete) |>
  summarise(n = n(),, n_false=sum(!complete), .groups = "drop")


# CDC and Verily data -----------------------------------------------------


# Line plots of standardized series by county -----------------------------
# ww_chc_county_season_standard |> ggplot(aes(x = year_week, y = roll_mean_week_std, group =
#                                               county_fips, colour = county_fips)) + geom_line()

# Create a plot for each county
counties <- unique(ww_chc_county_season_standard$county_fips)
plots <- lapply(counties, function(county) {
  ww_chc_county_season_standard |>
    filter(county_fips == county) |>
    pivot_longer(cols = c(sum_week_std, conf_flu_count_std),
                 names_to = "variable",
                 values_to = "value") |>
    ggplot(aes(x = year_week_dt, y = value, colour = variable, group = variable)) +
    geom_line() +
    labs(title = paste("County:", county),
         x = "Year Week",
         y = "Standardized Value",
         colour = "Variable") +
    theme_minimal()
})

# Display all plots
plots




#Check missingness
# ww_chc_county_season_standard |>
#   summarise(sum_week_std_na = sum(is.na(sum_week_std)),
#             conf_flu_count_std_na = sum(is.na(conf_flu_count_std)))

# combined_plot <- wrap_plots(plots, ncol = 2)

pdf(file = "Figures/county_plots.pdf", width = 30, height = 10)

for(i in seq(1, length(plots), by = 2)) {  
  end_idx <- min(i + 3, length(plots))
  grid.arrange(grobs = plots[i:end_idx], ncol = 2)
}

dev.off()
# Investigate problematic series for 05009, 06039, 06049, 19045, 19139, 23001, 27049, 36109, 37195, 47157, 48097, 51179, 51630, 54069, 55073
counties_check <- counties_check <- c(
  "05009", "06039", "06049", "19045", "19139", "23001", 
  "27049", "36109", "37195", "47157", "48097", "51179", 
  "51630", "54069", "55073"
)

ww_chc_county_season_standard|> filter(county_fips %in% counties_check)|> View()


# counties <- unique(ww_chc_county_season_standard$county_fips)
plots_nonstandard <- lapply(counties, function(county) {
  ww_chc_county_season_standard |>
    filter(county_fips == county) |>
    pivot_longer(cols = c(sum_week, conf_flu),
                 names_to = "variable",
                 values_to = "value") |>
    ggplot(aes(x = year_week_dt, y = log(value), colour = variable, group = variable)) +
    geom_line() +
    labs(title = paste("County:", county),
         x = "Year Week",
         y = "log(Value) (Nonstandardized)",
         colour = "Variable") +
    theme_minimal()
})

plots_nonstandard

# ww_chc_county_season_standard |> filter(year<2023)|> View()
# # Pearson, spearman, kendall's Tau correlation between WW and CHC ---------------------------------
# #7-day mean WW
# correlations_season_mean_pearson <- ww_chc_county_season |> group_by(county_fips, season) |> filter(season >=
#                                                                                                       2022) |> summarise(
#                                                                                                         sigma_cfn = sd(conf_flu_norm),
#                                                                                                         sigma_ww_mean = sd(roll_mean_week),
#                                                                                                         r = cor(
#                                                                                                           conf_flu_norm,
#                                                                                                           roll_mean_week,
#                                                                                                           use = "complete.obs",
#                                                                                                           method = "pearson"
#                                                                                                         ),
#                                                                                                         .groups = "drop"
#                                                                                                       )
# correlations_season_mean_spearman <- ww_chc_county_season |> group_by(county_fips, season) |> filter(season >=
#                                                                                                        2022) |> summarise(
#                                                                                                          rho = cor(
#                                                                                                            conf_flu_norm,
#                                                                                                            roll_mean_week,
#                                                                                                            use = "complete.obs",
#                                                                                                            method = "spearman"
#                                                                                                          ),
#                                                                                                          .groups = "drop"
#                                                                                                        )
# correlations_season_mean_kendall <- ww_chc_county_season |> group_by(county_fips, season) |> filter(season >=
#                                                                                                       2022) |> summarise(tau_a = DescTools::KendallTauA(conf_flu_norm, roll_mean_week),
#                                                                                                                          .groups = "drop")
# # #Weekly sum WW
# pearson_WW_sum_CHC <- wwscan_flu_ac_season_norm_county_week_matched_keep |> group_by(county_fips)|> summarize(r_mean=cor.test(conf_flu_norm,sum_week, method="pearson")$estimate) |> ungroup()
# correlations_season_sum_pearson <- ww_chc_county_season |> group_by(county_fips, season) |>  filter(season>=2022) |>  summarise(r=cor(conf_flu_norm, sum_week, use="complete.obs", method = "pearson"), .groups = "drop")
# correlations_season_sum_spearman <- ww_chc_county_season |> group_by(county_fips, season) |>  filter(season>=2022) |> summarise(rho=cor(conf_flu_norm, sum_week, use="complete.obs", method = "spearman"), .groups = "drop")

# CCF plots ---------------------------------------------------------------
# wwscan_flu_ac_season_norm_county_week_matched_keep |> group_by(county_fips) |> group_map(~ccf(.$conf_flu_norm, .$sum_week))
# wwscan_flu_ac_season_norm_county_week_matched_keep|> group_by(county_fips) |> group_map(~ccf(.$conf_flu_norm, .$roll_mean_week))
# Choropleth plots of correlations by season ---------------------------------------

# US_county_shape <- st_read(
#   "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Shapefiles/cb_2020_us_county_5m/cb_2020_us_county_5m.shp"
# )
# US_county_shape_correlations_season_mean_pearson <-   left_join(US_county_shape,
#                                                                 correlations_season_mean_pearson,
#                                                                 by = c("GEOID" = "county_fips"))
# US_county_shape_correlations_season_mean_pearson |> filter(season >= 2022, !(STATE_NAME %in% c("Hawaii", "Alaska", "Puerto Rico"))) |> ggplot() + geom_sf(aes(fill = r)) +
#   scale_fill_gradient2(
#     low = "red",
#     mid = "white",
#     high = "blue",
#     midpoint = 0,
#     name = "Correlation (r)",
#     limits = c(-1, 1)
#   ) +
#   facet_grid( ~ season) +
#   labs(title = "County-Level Correlation: Wastewater Flu Signal vs. CHC Flu Claims", subtitle = "Pearson r by County FIPS Code and season") +
#   theme_minimal() +
#   theme(
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )

# Time series plots for both Flu variables --------------------------------
# Remove anomalous county for scaling (37067) and restrict season
ww_chc_county_season_long <- ww_chc_county_season |> filter(county_fips !=
                                                              "37067", season >= 2022) |> pivot_longer(
                                                                cols = c(conf_flu_norm, roll_mean_week),
                                                                names_to = "series_name",
                                                                values_to = "value"
                                                              )
#log scale
ww_chc_county_season_2022_2024_log_line <- ww_chc_county_season_long |>
  ggplot(aes(
    x = week,
    y = log(value),
    group = county_fips,
    colour = county_fips,
    linetype = series_name
  )) +
  geom_line(data = ww_chc_county_season_long |> filter(series_name == "conf_flu_norm")) +
  geom_point(data = ww_chc_county_season_long |> filter(series_name == "roll_mean_week"))  + theme_minimal()
#free/linear scale
# ww_chc_county_season_long |>
#   ggplot(aes(x = week, y = value, group = county_fips,
#              colour = county_fips, linetype = series_name)) +
#   geom_line(data = ww_chc_county_season_long |> filter(series_name == "conf_flu_norm")) +
#   geom_point(data = ww_chc_county_season_long |> filter(series_name == "roll_mean_week"))  + theme_minimal() + facet_free(~variable, scales="free_y")
# Heatmaps ----------------------------------------------------------------

# Heatmap for conf_flu_norm
# h_cfn<-  ww_chc_county_season_long |>
#    filter(series_name == "conf_flu_norm") |>
#    ggplot(aes(x = year_week, y = county_fips, fill = value)) +
#    geom_tile() + scale_fill_viridis_c()+
#    # scale_fill_viridis_c(trans = "log1p") +
#    theme_minimal() +
#    labs(title = "Confirmed Flu Normalized", x = "Year-Week", y = "County FIPS")
#  ggplotly(h_cfn)
#  # Heatmap for roll_mean_week
# h_rmw<-  ww_chc_county_season_long |>
#    filter(series_name == "roll_mean_week") |>
#    ggplot(aes(x = year_week, y = county_fips, fill = value)) +
#    geom_tile() + scale_fill_viridis_c() +
#    # scale_fill_viridis_c(trans = "log1p") +
#    theme_minimal() +
#    labs(title = "Weekly Mean", x = "Year-Week", y = "County FIPS")
# # ggplotly(h_rmw + h_cfn)
# Combined heatmap
# ww_chc_county_season_long |>
#   ggplot(aes(x = year_week, y = fct_rev(fct_inorder(paste(county_fips, series_name, sep = " - "))),
#              fill = value)) +
#   geom_tile() +
#   scale_fill_viridis_c(trans = "log") +
#   theme_minimal() +
#   labs(title = "Wastewater and Flu Metrics by County",
#        x = "Year-Week",
#        y = "County FIPS - Series",
#        fill = "Value (log scale)")

# Separate plots for fips representing grouped counties -------------------

ww_chc_county_season_long |> filter(county_fips_grp != county_fips) |>
  ggplot(aes(
    x = week,
    y = log(value),
    colour = county_fips_grp,
    linetype = series_name
  )) +
  geom_line(data = ww_chc_county_season_long |> filter(series_name == "conf_flu_norm", county_fips_grp != county_fips)) +
  geom_point(data = ww_chc_county_season_long |> filter(series_name == "roll_mean_week", county_fips_grp != county_fips))  + theme_minimal()
# Cross scatter plots -----------------------------------------------------
# ww_chc_county_season |> ggplot(aes(x=conf_flu_norm, y=roll_mean_week, color = year_week)) + geom_point() +facet_grid(~county_fips)


# Correlogram -------------------------------------------------------------

corr_ww_chc_mean_season <- ww_chc_county_season |> filter(county_fips !=
                                                            "37067") |>
  group_by(county_fips) |>
  summarise(correlation = cor(roll_mean_week, conf_flu_norm, use = "complete.obs")) |>
  ggplot(aes(
    x = reorder(county_fips, correlation),
    y = correlation,
    fill = correlation
  )) +
  geom_col() +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    limits = c(-1, 1)
  ) +
  coord_flip() +
  labs(title = "Correlation between Roll Mean Week and Conf Flu Norm by County", x = "County FIPS", y = "Correlation")

ggplotly(corr_ww_chc_mean_season)

corr_ww_chc_mean_2022_2024_season <- ww_chc_county_season |> filter(season >=
                                                                      2022, county_fips != "37067") |>
  group_by(county_fips, season) |>
  summarise(
    correlation = cor(roll_mean_week, conf_flu_norm, use = "complete.obs"),
    .groups = "drop"
  ) |>
  ggplot(aes(
    x = reorder(county_fips, correlation),
    y = correlation,
    fill = correlation
  )) +
  geom_col() +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    limits = c(-1, 1)
  ) +
  coord_flip() +
  facet_wrap( ~ season) +
  labs(title = "Correlation between Roll Mean Week and Conf Flu Norm by County", x = "County FIPS", y = "Correlation")
ggplotly(corr_ww_chc_mean_2022_2024_season)


# Compare wastewater sources ----------------------------------------------
CDC_Wastewater_Data_for_Influenza_A_20260910_clean$sample_collect_date |> range()
CDC_Wastewater_Data_for_Influenza_A_20260910_clean |> select(source) |> table()
CDC_Wastewater_Data_for_Influenza_A_20260910_clean |> select(county_fips) |> table()
#detect entries with multiple county FIPS codes
CDC_Wastewater_Data_for_Influenza_A_20260910_clean |> summarise(across(county_fips, ~
                                                                         sum(str_detect(unique(
                                                                           .
                                                                         ), ","))))
#separate multiple fips rows
CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi <- CDC_Wastewater_Data_for_Influenza_A_20260910_clean |> separate_rows(county_fips, sep =
                                                                                                                                  ",") |> mutate(county_fips = str_trim(county_fips))
# Match counties to WWSCAN and CHC data
intersect(
  county_flu_ac_season_norm_common$county_fips,
  CDC_Wastewater_Data_for_Influenza_A_20260910_clean_multi$county_fips
) |> length()
