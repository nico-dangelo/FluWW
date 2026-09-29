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

# Import CHC data ------------------------------------------
county_flu_ac_season_norm <- readRDS(file = "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS")|> mutate(year_week=yearweek(year_week_dt))

#Correct date format
county_flu_ac_season_norm_yw <- mutate(county_flu_ac_season_norm, year_week=yearweek(year_week))
#make tsibble
county_flu_ac_season_norm_ts <- tsibble(county_flu_ac_season_norm_yw, index = year_week, key=county_fips)
saveRDS(county_flu_ac_season_norm_ts, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/FluWW/Imputed_Time_Series/county_flu_ac_season_norm_ts.RDS")
# Standardization ---------------------------------------------------------
county_flu_ac_season_norm_ts_standard <- county_flu_ac_season_norm_ts|> mutate(conf_flu_standard=scale(conf_flu)[,1])
saveRDS(county_flu_ac_season_norm_ts_standard, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/FluWW/Standardized_Time_Series/county_flu_ac_season_norm_ts_standard.RDS")
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

# Create paginated plots
pdf("Figures/chc_influenza_scaled_by_county.pdf", width = 12, height = 14)

for (page in 1:n_pages) {
  # Subset counties for this page
  start_idx <- (page - 1) * counties_per_page + 1
  end_idx <- min(page * counties_per_page, length(counties))
  counties_page <- counties[start_idx:end_idx]
  
  # Create plot for this page
  p <- plot_data_scaled_chc |>
    filter(county_fips %in% counties_page) |>
    ggplot(aes(x = year_week, y = value, color = type)) +
    geom_line() +
    facet_wrap(~county_fips, scales = "free_y", ncol = 2) +
    labs(title = paste("Influenza Claims: Scaled by County (Page", page, "of", n_pages, ")"),
         x = "Year-Week",
         y = "Value",
         color = "Type") +
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
      geom_line(color = "steelblue") +
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

