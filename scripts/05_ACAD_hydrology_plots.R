#------------------------------------------
# Plotting hydrology data collected in ACAD SENT sites and Great Meadow
#------------------------------------------

library(tidyverse)
library(patchwork)
# devtools::install_github("katemmiller/climateNETN")
library(climateNETN)

theme_wet <- function(){
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "#696969", fill = "white",
                                        linewidth = 0.4),
        plot.background = element_blank(),
        strip.background = element_rect(color = "#696969", fill = "grey90", linewidth = 0.4),
        legend.key = element_blank(),
        axis.line.x = element_line(color = "#696969", linewidth = 0.4),
        axis.line.y = element_line(color = "#696969", linewidth = 0.4),
        axis.ticks = element_line(color = "#696969", size = 0.4))
}

wl_gilm <- read.csv("./data/ACAD_data/well_prec_data_2013-2025.csv")
wl_gilm$year_fac <- as.factor(wl_gilm$Year)
wl_gilm <- wl_gilm |> mutate(timestamp2 = ifelse(hr == 0,
                                                 format(as.POSIXct(paste0(timestamp, " 00:00:00"),
                                                                   format = "%Y-%m-%d %H:%M:%S",
                                                                   tz = "America/New_York"),
                                                        "%Y-%m-%d %H:%M:%S"),
                                                 format(as.POSIXct(timestamp,
                                                                   format = "%Y-%m-%d %H:%M:%S",
                                                                   tz = "America/New_York"),
                                                        "%Y-%m-%d %H:%M:%S")),
                             Date = as.Date(Date, format = "%Y-%m-%d")) |>
  select(timestamp = timestamp2, Date:year_fac)

head(wl_gilm)
write.csv(wl_gilm, "./data/ACAD_data/SEN_water_level_precip_data_2013-2025.csv", row.names = F)

gilm_sum <- wl_gilm |> filter(Year >= 2016) |>
  summarize(num_samps = sum(!is.na(GILM_WL)),
            median_wl = median(GILM_WL, na.rm = T),
            min_wl = min(GILM_WL, na.rm = T),
            max_wl = max(GILM_WL, na.rm = T),
            lower95 = quantile(GILM_WL, 0.025, na.rm = T),
            upper95 = quantile(GILM_WL, 0.975, na.rm = T),
            lower50 = quantile(GILM_WL, 0.25, na.rm = T),
            upper50 = quantile(GILM_WL, 0.75, na.rm = T),
    .by = c(doy)
  )

head(gilm_sum)

wl_grme1 <- read.csv("./data/ACAD_data/great_meadow_well_data_2025_20260304.csv")

wl_grme <- wl_grme1 |>
  mutate(year_fac = as.factor(year),
         Date = as.Date(date, format = "%Y-%m-%d"),
         timestamp = ifelse(hr == 0,
                             format(as.POSIXct(paste0(timestamp, " 00:00:00"),
                                               format = "%Y-%m-%d %H:%M:%S",
                                               tz = "America/New_York"),
                                    "%Y-%m-%d %H:%M:%S"),
                             format(as.POSIXct(timestamp,
                                               format = "%Y-%m-%d %H:%M:%S",
                                               tz = "America/New_York"),
                                    "%Y-%m-%d %H:%M:%S"))) |>
  select(timestamp, date, doy, Year = year, precip_cm = precip.cm, plot.num,
         water.depth, lag.precip, hr, doy_h, year_fac)  |>
  filter(Year >= 2016) |> filter(doy > 134 & doy < 275) |>
  filter(water.depth <200 & water.depth > -200) |> # some rogue data points in there
  filter(!(water.depth < -120 & doy == 159 & Year == 2016 & plot.num == 4)) |>
  filter(!(water.depth < -115 & doy == 215 & Year == 2017 & plot.num == 6))

write.csv(wl_grme, "./data/ACAD_data/GRME_water_level_precip_data_2016-2025.csv", row.names = F)

grme_sum <- wl_grme |>
  filter(!(water.depth < -120 & doy == 159 & Year == 2016)) |>
  summarize(num_samps = sum(!is.na(water.depth)),
            median_wl = median(water.depth, na.rm = T),
            min_wl = min(water.depth, na.rm = T),
            max_wl = max(water.depth, na.rm = T),
            lower95 = quantile(water.depth, 0.025, na.rm = T),
            upper95 = quantile(water.depth, 0.975, na.rm = T),
            lower50 = quantile(water.depth, 0.25, na.rm = T),
            upper50 = quantile(water.depth, 0.75, na.rm = T),
            .by = c(doy, plot.num)
  )

d100 = "#E4F0F8"
d95 = "#B8D8ED"
d50 = "#7FB9DD"
med = "#1378b5"

p_grme <-
ggplot(data = grme_sum |> filter(plot.num == 1)) + theme_wet() +
  geom_ribbon(aes(ymin = min_wl, ymax = max_wl, x = doy,
                  color = "Historic range", fill = "Historic range")) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95, x = doy,
                  color = "Hist. 95% range", fill = "Hist. 95% range")) +
  geom_ribbon(aes(ymin = lower50, ymax = upper50, x = doy,
                  color = "Hist. 50% range", fill = "Hist. 50% range")) +
  geom_line(aes(x = doy, y = median_wl, color = "Median water level",
                fill = "Median water level"), linewidth = 1) +
  scale_color_manual(values = c("Historic range" = d100,
                                "Hist. 95% range" = d95,
                                "Hist. 50% range" = d50,
                                "Median water level" = med), name = "Daily Distributions") +
  scale_fill_manual(values = c("Historic range" = d100,
                               "Hist. 95% range" = d95,
                               "Hist. 50% range" = d50,
                               "Median water level" = med), name = "Daily Distributions") +
  labs(y = "Water Level (cm)", x = NULL, title = "Great Meadow") +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90, 120), limits = c(-65, 125)) +
  scale_x_continuous(breaks = c(135, 166, 196, 227, 258, 288),
                     labels = c("May-15", "Jun-15", "Jul-15",
                                "Aug-15", "Sep-15", "Oct-15"),
                     guide = guide_axis(minor.ticks = T)) +
  geom_hline(yintercept = 0, color = 'black') +
  theme(title = element_text(size = 9), legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 2, byrow = T),
         color = guide_legend(nrow = 2, byrow = T))


p_gilm <-
ggplot(data = gilm_sum) + theme_wet() +
  geom_ribbon(aes(ymin = min_wl, ymax = max_wl, x = doy,
                  color = "Historic range", fill = "Historic range")) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95, x = doy,
                  color = "Hist. 95% range", fill = "Hist. 95% range")) +
  geom_ribbon(aes(ymin = lower50, ymax = upper50, x = doy,
                  color = "Hist. 50% range", fill = "Hist. 50% range")) +
  geom_line(aes(x = doy, y = median_wl, color = "Median water level",
                fill = "Median water level"), linewidth = 1) +
  scale_color_manual(values = c("Historic range" = d100,
                                "Hist. 95% range" = d95,
                                "Hist. 50% range" = d50,
                                "Median water level" = med), name = "Daily Distributions") +
  scale_fill_manual(values = c("Historic range" = d100,
                                "Hist. 95% range" = d95,
                                "Hist. 50% range" = d50,
                                "Median water level" = med), name = "Daily Distributions") +
  labs(y = "Water Level (cm)", x = NULL, title = "Gilmore Meadow") +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90, 120), limits = c(-65, 125)) +
  scale_x_continuous(breaks = c(135, 166, 196, 227, 258, 288),
                     labels = c("May-15", "Jun-15", "Jul-15",
                                "Aug-15", "Sep-15", "Oct-15"),
                     guide = guide_axis(minor.ticks = T)) +
  geom_hline(yintercept = 0, color = 'black') +
  theme(title = element_text(size = 9), legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 2, byrow = T),
         color = guide_legend(nrow = 2, byrow = T))

plot_bands <- function(df, y, ptitle){
  df$wl_col <- df[,y]

  df_sum <-
  df |> filter(Year >= 2016) |>
    summarize(num_samps = sum(!is.na(wl_col)),
              median_wl = median(wl_col, na.rm = T),
              min_wl = min(wl_col, na.rm = T),
              max_wl = max(wl_col, na.rm = T),
              lower95 = quantile(wl_col, 0.025, na.rm = T),
              upper95 = quantile(wl_col, 0.975, na.rm = T),
              lower50 = quantile(wl_col, 0.25, na.rm = T),
              upper50 = quantile(wl_col, 0.75, na.rm = T),
              .by = c(doy))

  p <- suppressWarnings(
  ggplot(data = df_sum) + theme_wet() +
    geom_ribbon(aes(ymin = min_wl, ymax = max_wl, x = doy,
                    color = "Historic range", fill = "Historic range")) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, x = doy,
                    color = "Hist. 95% range", fill = "Hist. 95% range")) +
    geom_ribbon(aes(ymin = lower50, ymax = upper50, x = doy,
                    color = "Hist. 50% range", fill = "Hist. 50% range")) +
    geom_line(aes(x = doy, y = median_wl, color = "Median water level",
                  fill = "Median water level"), linewidth = 1) +
    scale_color_manual(values = c("Historic range" = d100,
                                  "Hist. 95% range" = d95,
                                  "Hist. 50% range" = d50,
                                  "Median water level" = med), name = "Daily Distributions") +
    scale_fill_manual(values = c("Historic range" = d100,
                                 "Hist. 95% range" = d95,
                                 "Hist. 50% range" = d50,
                                 "Median water level" = med), name = "Daily Distributions") +
    labs(y = "Water Level (cm)", x = NULL, title = ptitle) +
    # scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90, 120), limits = c(-65, 125)) +
    scale_x_continuous(breaks = c(135, 166, 196, 227, 258, 288),
                       labels = c("May-15", "Jun-15", "Jul-15",
                                  "Aug-15", "Sep-15", "Oct-15"),
                       guide = guide_axis(minor.ticks = T)) +
    geom_hline(yintercept = 0, color = 'black') +
    theme(title = element_text(size = 9), legend.position = 'bottom') +
    guides(fill = guide_legend(nrow = 2, byrow = T),
           color = guide_legend(nrow = 2, byrow = T))
  )
  return(p)
  }

plot_bands(df = wl_gilm, y = "BIGH_WL", ptitle = "Big Heath")
plot_bands(df = wl_gilm, y = "DUCK_WL", ptitle = "Duck Pond")
plot_bands(df = wl_gilm, y = "GILM_WL", ptitle = "Gilmore Meadow")
plot_bands(df = wl_gilm, y = "HEBR_WL", ptitle = "Heath Brook")
plot_bands(df = wl_gilm, y = "HODG_WL", ptitle = "Hodgdon Swamp")
plot_bands(df = wl_gilm, y = "LIHU_WL", ptitle = "Little Hunter")
plot_bands(df = wl_gilm, y = "NEMI_WL", ptitle = "New Mills Meadow - NW")
plot_bands(df = wl_gilm, y = "WMTN_WL", ptitle = "Western Mtn. Swamp")

p_grme + p_gilm + plot_layout(axes = "collect", guides = 'collect') & theme(legend.position = 'bottom')

ggsave("./results/Great_vs_Gilmore_water_level_distributions.png", width = 10, height = 6)

# Add rug for when drought conditions
drgt <- getClimDrought(park = "ACAD", years = 2020:2025)

drgt <- drgt |>
  filter(County == "Hancock County") |>
  select(DSCI, ValidStart)

new_drgt <- data.frame(Date = seq.Date(min(drgt$ValidStart), max(drgt$ValidStart), 1))

new_drgt2 <- left_join(new_drgt, drgt, by = c("Date" = "ValidStart")) |>
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         month = format(Date, "%d"),
         doy = as.numeric(format(Date, "%j")),
         year = as.numeric(format(Date, "%Y")),
         year_fac = as.factor(year)) |>
  fill(DSCI, .direction = 'down') |>
  mutate(drgt = ifelse(DSCI > 0, 1, NA_real_)) |>
  filter(!is.na(drgt)) |>
  filter(doy >= 135 & doy <= 274)

wl_gilm2 <- left_join(wl_gilm, new_drgt2, by = c("doy", "Year" = "year", "year_fac", "Date"))

# facet by year
ggplot(wl_gilm2 |> filter(Year > 2019) |> droplevels(),
       aes(x = doy_h, y = GILM_WL, group = year_fac)) +
  geom_line(linewidth = 0.75, aes(color = "Gilmore Meadow")) + theme_wet() +
  geom_line(data = wl_grme |> filter(Year > 2019) |> droplevels() |> filter(plot.num == 1),
            linewidth = 0.75,
            aes(x = doy_h, y = water.depth, group = year_fac, color = "Great Meadow")) +
  labs(y = "Water Level (cm)", x = NULL) +
  scale_color_manual(values = c("Gilmore Meadow" = "#0A60D1", "Great Meadow" = "#FFBB14")) +
  scale_x_continuous(breaks = c(135, 166, 196, 227, 258),
                     limits = c(135, 275),
                     labels = c("May-15", "Jun-15", "Jul-15", "Aug-15", "Sep-15")) +
  geom_hline(yintercept = 0) +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  geom_rug(data = new_drgt2 |> filter(year > 2019), stat = 'identity',
           aes(x = doy, y = drgt, group = year_fac),
           color = "dimgrey", linewidth = 1.5) +
  facet_wrap(~year_fac, ncol = 2)

head(wl_gilm2)

# Calc. growing season summary stats
grme_wide <- wl_grme |>
  filter(Year >= 2016) |> filter(doy > 134 & doy < 275) |>
  filter(water.depth <200 & water.depth > -200) |> # some rogue data points in there
  filter(!(water.depth < -120 & doy == 159 & Year == 2016 & plot.num == 4)) |>
  filter(!(water.depth < -120 & doy == 159 & Year == 2016 & plot.num == 3)) |>
  filter(!(water.depth < -115 & doy == 215 & Year == 2017 & plot.num == 6)) |>
  mutate(plot_name = paste0("GRME_", sprintf("%02d", plot.num))) |>
  select(-plot.num) |>
  pivot_wider(names_from = plot_name, values_from = water.depth)

head(grme_wide)
head(wl_gilm)

calc_WL_stats <- function(df, col_match = "GRME"){

df$timestamp <- as.POSIXct(ifelse(df$hr == 0, paste0(df$timestamp, " 00:00:00"), df$timestamp),
                           format = "%Y-%m-%d %H:%M:%S")

well_prp <- df |> mutate(month = lubridate::month(timestamp),
                         mon = months(timestamp, abbreviate = T)) |>
  filter(doy > 134 & doy < 275) |> droplevels()

# May 1 DOY= 121; May 15 = 135; Oct.1 = 274
well_prp_long <- well_prp |> pivot_longer(cols = c(contains(col_match)),
                                          names_to = "site", values_to = "water_level_cm")

well_prp_long2 <- well_prp_long |> group_by(Year, site) |>
  mutate(lag_WL = dplyr::lag(water_level_cm, n = 1),
         change_WL = water_level_cm-lag_WL)

# Calculate growing season stats
well_gs_stats <- well_prp_long2 |> group_by(Year, site) |>
  summarise(WL_mean = mean(water_level_cm, na.rm = TRUE),
            WL_sd = sd(water_level_cm, na.rm = TRUE),
            WL_min = suppressWarnings(min(water_level_cm, na.rm = TRUE)),
            WL_max = suppressWarnings(max(water_level_cm, na.rm = TRUE)),
            max_inc = suppressWarnings(max(change_WL, na.rm = TRUE)),
            max_dec = suppressWarnings(min(change_WL, na.rm = TRUE)),
            prop_GS_comp = length(which(!is.na(water_level_cm)))/n()*100,
            .groups = "drop")

# Calculate change in WL from average Jun to average September
well_gs_month <- well_prp_long2 |> group_by(Year, mon, site) |>
  summarise(WL_mean = mean(water_level_cm, na.rm = TRUE),
            .groups = 'drop') |>
  filter(mon %in% c("Jun","Sep")) |> droplevels() |>
  #spread(mon, WL_mean) |>
  pivot_wider(names_from = mon, values_from = WL_mean) |>
  mutate(GS_change = Sep - Jun)


well_gs_prop1 <- well_prp_long2 |> mutate(over_0 = ifelse(water_level_cm >= 0 & !is.na(water_level_cm), 1, 0),
                                          bet_0_neg30 = ifelse(water_level_cm <= 0 & water_level_cm >= -30 &
                                                                 !is.na(water_level_cm), 1, 0),
                                          under_neg30 = ifelse(water_level_cm< -30 & !is.na(water_level_cm), 1, 0),
                                          num_logs = ifelse(!is.na(water_level_cm) & !is.na(water_level_cm), 1, NA))

well_gs_prop <- well_gs_prop1 |> group_by(Year, site) |>
  summarise(prop_over_0cm = (sum(over_0, na.rm = TRUE)/
                               sum(num_logs, na.rm = TRUE))*100,
            prop_bet_0_neg30cm = (sum(bet_0_neg30, na.rm = TRUE)/
                                    sum(num_logs, na.rm = TRUE))*100,
            prop_under_neg30cm = (sum(under_neg30, na.rm = TRUE)/
                                    sum(num_logs, na.rm = TRUE))*100,
            .groups = 'drop')

gs_WL_stats <- list(well_gs_stats, well_gs_month[,c("Year","site","GS_change")], well_gs_prop) |>
  reduce(left_join, by = c("Year", "site"))

# Missing water level data from 2017, change to NA
metrics <- c("WL_mean","WL_sd","WL_min","WL_max", "max_inc","max_dec", "prop_GS_comp",
             "GS_change", "prop_over_0cm","prop_bet_0_neg30cm","prop_under_neg30cm" )

#gs_WL_stats[gs_WL_stats$site=="DUCK_WL" & gs_WL_stats$Year == 2017, metrics] <- NA
# Logger failed in DUCK in 2017

prop_complete_check <- gs_WL_stats[is.na(gs_WL_stats$prop_GS_comp) | gs_WL_stats$prop_GS_comp < 90,]

if(nrow(prop_complete_check) > 0) {
  message(paste0("Warning: The following ", nrow(prop_complete_check),
                 " sites have water level measurements for less than 90% of growing season: ",
                 "\n", "site    ", "year ", "%_complete", "\n",
                 paste(prop_complete_check$site, prop_complete_check$Year,
                       round(prop_complete_check$prop_GS_comp, 2), collapse = "\n", sep = " ")))
}

return(gs_WL_stats)

}

wl_stats_comb <- rbind(calc_WL_stats(df = wl_gilm, col_match = "_WL") |> mutate(site_type = "SEN"),
                       calc_WL_stats(df = grme_wide, col_match = "GRME_") |> mutate(site_type = "INT")) |>
  arrange(desc(site_type), site, Year)

write.csv(wl_stats_comb, "./results/water_level_statistics_SEN_GRME.csv", row.names = F)

