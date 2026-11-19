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
               ggforce, update = F)

# Import CHC data and normalize------------------------------------------
county_week_flu_v3_imputed <- readr::read_csv("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_week_flu_v3_imputed.csv")

# Clean data and fix formats -----------------------------------------------
county_week_flu_v3_imputed_clean <- county_week_flu_v3_imputed
county_week_flu_v3_imputed_clean$year_week_dt <- as_date(county_week_flu_v3_imputed$year_week_dt, format =
                                                           "%G-%V-%u")
# # Normalization -----------------------------------------------------------
# #Import all cause mortality data
county_week_ac_v3_imputed <- readr::read_csv("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_week_ac_v3_imputed.csv")
#merge ac with flu data by county and year_week

df <- left_join(
  county_week_flu_v3_imputed_clean,
  county_week_ac_v3_imputed,
  by = c("county_fips", "year_week")
) |>
  rename(all_cause_weekly = all_cause_wtd) |>
  mutate(year = sub("-.*", "", year_week)) |>
  group_by(county_fips, year) |>
  # get mean of all cause weekly by county and year
  mutate(
    all_cause_weekly_mean = mean(all_cause_weekly, na.rm = TRUE)
  ) |>
  ungroup()
# calculate weekly factor and scale up cases accordingly

df <- df  |> mutate(all_cause_weekly_mean_factor = all_cause_weekly/all_cause_weekly_mean)
df <- df |> mutate(conf_flu_adjusted = conf_flu/ all_cause_weekly_mean_factor)
df <- df |> mutate(conf_flu_adjusted_norm=conf_flu_adjusted/all_cause_weekly)
# county_flu_ac_season_norm <- readRDS(file = "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS")

# #Correct date format
df_yw <- mutate(df, year_week=yearweek(year_week_dt))
# #make tsibble
ts <- tsibble(df_yw, index = year_week, key=county_fips)
# counties_to_keep_CHC <- ts |>
#   as_tibble() |>
#   group_by(county_fips) |>
#   filter(any(season == "2023-2024" & season_all_cause >= 5000 & conf_flu > 10, na.rm = TRUE)) |>
#   ungroup() |>
#   distinct(county_fips) |>
#   pull(county_fips)

# Import inpatient only CHC data
df_inpatient <- read_csv("county_week_conf_flu_inpatient_imputed.csv") |> mutate(year=sub("-.*", "", year_week))

# Compute Inpatient and Outpatient Incidence ------------------------------
df_inpatient_all_cause <- left_join(df_inpatient,county_week_ac_v3_imputed, by=c("county_fips", "year_week"))|> rename(conf_flu_inpatient=conf_flu, all_cause_weekly=all_cause_wtd)
df_inpatient_all_cause_mean <- df_inpatient_all_cause |> group_by(county_fips, year) |>
  # get mean of all cause weekly by county and year
  mutate(
    all_cause_weekly_mean = mean(all_cause_weekly, na.rm = TRUE)
  ) |>
  ungroup()
df_inpatient_all_cause_mean<- df_inpatient_all_cause |> mutate(all_cause_weekly_mean_factor = all_cause_weekly/all_cause_weekly_mean, 
                                                               conf_flu_inpatient_adjusted=conf_flu_inpatient/all_cause_weekly_mean_factor, 
                                                               conf_flu_inpatient_adjusted_norm=conf_flu_inpatient_adjusted/all_cause_weekly)
df_inpatient_outpatient <- left_join(df, df_inpatient_all_cause_mean, by=c("county_fips", "year_week")) |> mutate(conf_flu_outpatient=conf_flu - conf_flu_inpatient, 
                                                                 conf_flu_outpatient_adjusted=conf_flu_outpatient/conf_flu_outpatient / all_cause_weekly_mean_factor,
                                                                 conf_flu_outpatient_adjusted_norm = conf_flu_outpatient_adjusted / all_cause_weekly.x) #check this


# Repeat as Seasonal scale ------------------------------------------------
library(dplyr)

df_inpatient_outpatient_seeason <- df_inpatient_outpatient %>%
  mutate(
    month = as.integer(format(year_week_dt.x, "%m")),
    yr = as.integer(format(year_week_dt.x, "%Y")),
    # season starts in October of year n and ends in April of year n+1
    season_start = ifelse(month >= 10, yr, yr - 1),
    season = ifelse(month >= 5 & month <= 9, NA_character_,
                    paste0(season_start, "-", season_start + 1))
  ) %>%
  # within mean of all-cause weekly, per season
  group_by(county_fips, season) %>%
  mutate(all_cause_county_season_mean = mean(all_cause_weekly.x, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    # outpatient = total - inpatient (done before adjustment)
    conf_flu_outpatient = conf_flu - conf_flu_inpatient,
    
    # step (i): weekly factor relative to the cross-county season mean
    weekly_means_factor = all_cause_weekly.x / all_cause_county_season_mean,
    conf_flu_adjusted = conf_flu / weekly_means_factor,
    conf_flu_inp_adjusted = conf_flu_inpatient / weekly_means_factor,
    conf_flu_outp_adjusted = conf_flu_outpatient / weekly_means_factor,
    
    # step (ii): normalize by county all-cause weekly
    conf_flu_adjusted_norm = conf_flu_adjusted / all_cause_weekly.x,
    conf_flu_inp_adjusted_norm = conf_flu_inp_adjusted / all_cause_weekly.x,
    conf_flu_outp_adjusted_norm = conf_flu_outp_adjusted / all_cause_weekly.x
  )


# n_dropped_chc= n_distinct(county_flu_ac_season_norm$county_fips)- n_distinct(counties_to_keep_CHC)
# county_flu_ac_season_norm_ts <- county_flu_ac_season_norm_ts |> filter(county_fips %in% counties_to_keep_CHC)
# county_week_conf_flu_inpatient_imputed_ts <- county_week_conf_flu_inpatient_imputed_ts |> filter(county_fips %in% counties_to_keep_CHC)
# saveRDS(county_flu_ac_season_norm_ts, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/FluWW/Imputed_Time_Series/county_flu_ac_season_norm_ts.RDS")
# saveRDS(county_week_conf_flu_inpatient_imputed_ts, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/FluWW/Imputed_Time_Series/county_week_conf_flu_inpatient_imputed_ts.RDS")
# Standardization ---------------------------------------------------------
county_flu_ac_season_norm_ts_standard <- county_flu_ac_season_norm_ts |>
  mutate(month = month(year_week)) |>
  group_by(county_fips) |>
  mutate(
    epi_mean   = mean(conf_flu_norm[month(year_week) %in% c(10,11,12,1,2,3,4)], na.rm = TRUE),
    county_sd  = sd(conf_flu_norm, na.rm = TRUE),
    conf_flu_standard = (conf_flu_norm - epi_mean) / county_sd
  ) |>
  ungroup()
saveRDS(county_flu_ac_season_norm_ts_standard, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/FluWW/Standardized_Time_Series/county_flu_ac_season_norm_ts_norm_standard.RDS")


county_week_conf_flu_inpatient_imputed_ts_standard <- county_week_conf_flu_inpatient_imputed_ts |>  mutate(month = month(year_week)) |>
  group_by(county_fips) |>
  mutate(
    epi_mean   = mean(conf_flu[month(year_week)%in% c(10,11,12,1,2,3,4)], na.rm = TRUE),
    county_sd  = sd(conf_flu, na.rm = TRUE),
    conf_flu_standard = (conf_flu - epi_mean) / county_sd
  ) |>
  ungroup()
saveRDS(county_week_conf_flu_inpatient_imputed_ts_standard, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/FluWW/Standardized_Time_Series/county_week_conf_flu_inpatient_imputed_ts_standard.RDS")
# Inpatient share of the CHC normalized flu series, bounded to [0, 1]
inpatient_all_cause_ts <- county_flu_ac_season_norm_ts |>
  as_tibble() |>
  select(county_fips, year_week, season_all_cause, conf_flu_norm) |>
  left_join(
    county_week_conf_flu_inpatient_imputed_ts |>
      as_tibble() |>
      select(county_fips, year_week, inpatient_conf_flu = conf_flu),
    by = c("county_fips", "year_week")
  ) |>
  mutate(
    inpatient_prop      = if_else(conf_flu_norm > 0,
                                  pmin(inpatient_conf_flu / conf_flu_norm, 1),
                                  NA_real_),
    inpatient_all_cause = season_all_cause * inpatient_prop
  )

saveRDS(inpatient_all_cause_ts,
        file = "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/FluWW/Imputed_Time_Series/inpatient_all_cause_ts.RDS")

inpatient_all_cause_ts_standard <- inpatient_all_cause_ts |>
  group_by(county_fips) |>
  mutate(
    epi_mean  = mean(inpatient_all_cause[month(year_week) %in% c(10,11,12,1,2,3,4)], na.rm = TRUE),
    county_sd = sd(inpatient_all_cause, na.rm = TRUE),
    inpatient_all_cause_standard = (inpatient_all_cause - epi_mean) / county_sd
  ) |>
  ungroup()
#plot standardized series
plot_data_scaled_chc <- county_flu_ac_season_norm_ts_standard |>
  as_tibble() |>
  select(year_week, county_fips, 
         original = conf_flu,
         scaled = conf_flu_standard) |>
  pivot_longer(cols = c(original, scaled), names_to = "type", values_to = "value")

counties <- unique(plot_data_scaled_chc$county_fips)
counties_per_page <- 6  # 3 rows x 2 columns
n_pages <- ceiling(length(counties) / counties_per_page)
plot_data_scaled_inpatient_chc <- county_week_conf_flu_inpatient_imputed_ts_standard |>
  as_tibble() |>
  select(year_week, county_fips, 
         original = conf_flu,
         scaled = conf_flu_standard) |>
  pivot_longer(cols = c(original, scaled), names_to = "type", values_to = "value")
counties_inpatient <- unique(plot_data_scaled_inpatient_chc$county_fips)
counties_per_page_inpatient <- 6  # 3 rows x 2 columns
n_pages_inpatient <- ceiling(length(counties_inpatient) / counties_per_page_inpatient)
# Create paginated plots
pdf("Figures/chc_influenza_standardized_by_county.pdf", width = 12, height = 14)

for (page in 1:n_pages) {
  # Subset counties for this page
  start_idx <- (page - 1) * counties_per_page + 1
  end_idx <- min(page * counties_per_page, length(counties))
  counties_page <- counties[start_idx:end_idx]
  
  # Create plot for this page
  p <- plot_data_scaled_chc |>
    filter(county_fips %in% counties_page) |>
    ggplot(aes(x = year_week, y = value)) +
    geom_line(color="hotpink3") +
    facet_wrap(~county_fips, scales = "free_y", ncol = 2) +
    labs(title = paste("Influenza Claims: Scaled by County (Page", page, "of", n_pages, ")"),
         x = "Year-Week",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  print(p)
}

dev.off()

normalized_long <- bind_rows(
  county_flu_ac_season_norm_ts_standard |>
    as_tibble() |>
    transmute(year_week, county_fips, series = "CHC normalized", value = conf_flu_standard),
  county_week_conf_flu_inpatient_imputed_ts_standard |>
    as_tibble() |>
    transmute(year_week, county_fips, series = "Inpatient normalized", value = conf_flu_standard)
)

counties_norm <- sort(unique(normalized_long$county_fips))
counties_per_page <- 6  # 3 rows x 2 columns
n_pages_norm <- ceiling(length(counties_norm) / counties_per_page)

pdf("Figures/chc_and_inpatient_influenza_epi_normalized_by_county.pdf", width = 12, height = 14)
for (page in 1:n_pages_norm) {
  start_idx <- (page - 1) * counties_per_page + 1
  end_idx   <- min(page * counties_per_page, length(counties_norm))
  counties_page <- counties_norm[start_idx:end_idx]
  
  p <- normalized_long |>
    filter(county_fips %in% counties_page) |>
    ggplot(aes(x = year_week, y = value, color = series)) +
    geom_line(na.rm = TRUE) +
    facet_wrap(~county_fips, scales = "free_y", ncol = 2) +
    scale_color_manual(values = c("CHC normalized" = "hotpink3",
                                  "Inpatient normalized" = "steelblue")) +
    labs(title = paste("Influenza: Epidemic-Period Normalized by County (Page", page, "of", n_pages_norm, ")"),
         x = "Year-Week",
         y = "Normalized Value",
         color = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  print(p)
}
dev.off()



pdf("Figures/chc_influenza_inpatient_standardized_by_county.pdf", width = 12, height = 14)
for (page in 1:n_pages_inpatient) {
  # Subset counties for this page
  start_idx <- (page - 1) * counties_per_page_inpatient + 1
  end_idx <- min(page * counties_per_page_inpatient, length(counties_inpatient))
  counties_page <- counties_inpatient[start_idx:end_idx]
  
  # Create plot for this page
  p <- plot_data_scaled_inpatient_chc |>
    filter(county_fips %in% counties_page) |>
    ggplot(aes(x = year_week, y = value)) +
    geom_line(color="hotpink3") +
    facet_wrap(~county_fips, scales = "free_y", ncol = 2) +
    labs(title = paste("Influenza Claims: Scaled by County (Page", page, "of", n_pages_inpatient, ")"),
         x = "Year-Week",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  print(p)
}
dev.off()

# Stack all three standardized series for joint plotting
all_three_long <- bind_rows(
  county_flu_ac_season_norm_ts_standard |>
    as_tibble() |>
    transmute(year_week, county_fips, series = "CHC normalized", value = conf_flu_standard),
  county_week_conf_flu_inpatient_imputed_ts_standard |>
    as_tibble() |>
    transmute(year_week, county_fips, series = "Inpatient normalized", value = conf_flu_standard),
  inpatient_all_cause_ts_standard |>
    transmute(year_week, county_fips, series = "Inpatient all-cause", value = inpatient_all_cause_standard)
) |>
  mutate(series = factor(series, levels = c("CHC normalized", "Inpatient normalized", "Inpatient all-cause")))

counties_all <- sort(unique(all_three_long$county_fips))
counties_per_page <- 6  # 3 rows x 2 columns
n_pages_all <- ceiling(length(counties_all) / counties_per_page)

pdf("Figures/chc_inpatient_and_all_cause_epi_standardized_by_county.pdf", width = 12, height = 14)
for (page in 1:n_pages_all) {
  start_idx <- (page - 1) * counties_per_page + 1
  end_idx   <- min(page * counties_per_page, length(counties_all))
  counties_page <- counties_all[start_idx:end_idx]
  
  p <- all_three_long |>
    filter(county_fips %in% counties_page) |>
    ggplot(aes(x = year_week, y = value, color = series)) +
    geom_line(na.rm = TRUE) +
    facet_wrap(~county_fips, scales = "free_y", ncol = 2) +
    scale_color_manual(values = c("CHC normalized" = "hotpink3",
                                  "Inpatient normalized" = "steelblue",
                                  "Inpatient all-cause" = "darkgreen")) +
    labs(title = paste("Influenza and All-Cause: Epidemic-Period Standardized by County (Page", page, "of", n_pages_all, ")"),
         x = "Year-Week",
         y = "Standardized Value",
         color = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  print(p)
}
dev.off()
# Faceted plots by Urbanization -------------------------------------------
# common counties

NCHS_classifications <- read_csv("2023 NCHS classifications.csv")
NCHS_classifications$`2023 Code` <-  as.factor(NCHS_classifications$`2023 Code`)
NCHS_classifications <- NCHS_classifications|> rename(county_fips=Location)

#Join NCHS level to standardized series
county_flu_ac_season_norm_ts_standard_urb <- left_join(county_flu_ac_season_norm_ts_standard, NCHS_classifications, by="county_fips")

# Prepare data for plotting: standardized series grouped by NCHS code
plot_data_nchs_chc <- county_flu_ac_season_norm_ts_standard_urb |> 
  as_tibble() |>
  select(year_week, county_fips, `2023 Code`,
         value = conf_flu_standard)
# Get unique NCHS codes and sort them
nchs_codes <- unique(plot_data_nchs_chc$`2023 Code`) |> sort()
# Create paginated plots grouped by NCHS code
pdf("Figures/chc_influenza_standardized_by_nchs_code.pdf", width = 14, height = 11)
for (nchs_code in nchs_codes) {
  # Filter data for this NCHS code
  data_nchs <- plot_data_nchs_chc |>
    filter(`2023 Code` == nchs_code)
  
  # Get counties for this NCHS code
  counties_nchs <- unique(data_nchs$county_fips)
  n_counties <- length(counties_nchs)
  counties_per_page <- 6  # 3 rows x 2 columns
  n_pages <- ceiling(n_counties / counties_per_page)
  
  # Create plots for each page within this NCHS code
  for (page in 1:n_pages) {
    start_idx <- (page - 1) * counties_per_page + 1
    end_idx <- min(page * counties_per_page, n_counties)
    counties_page <- counties_nchs[start_idx:end_idx]
    
    # Create plot
    p <- data_nchs |>
      filter(county_fips %in% counties_page) |>
      ggplot(aes(x = year_week, y = value)) +
      geom_line(color = "hotpink3") +
      facet_wrap(~county_fips, scales = "free_y", ncol = 2) +
      labs(title = paste(nchs_code, "- Standardized Influenza A (Page", page, "of", n_pages, ")"),
           x = "Year-Week",
           y = "Standardized Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 12, face = "bold"))
    
    print(p)
  }
}

dev.off()

