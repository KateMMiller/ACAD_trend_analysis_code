#------------------------------------------
# Plotting hydrology data collected in ACAD SENT sites and Great Meadow
#------------------------------------------

library(tidyverse)
library(patchwork)
# devtools::install_github("katemmiller/climateNETN")
library(climateNETN)
library(ggh4x)

theme_wet <- function(){
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "#696969", fill = "white",
                                        linewidth = 0.4),
        plot.background = element_blank(),
        strip.background = element_rect(color = "#696969", fill = "grey90", linewidth = 0.4),
        legend.key = element_blank(),
        axis.line.x = element_line(color = "#696969", linewidth = 0.4),
        axis.line.y = element_line(color = "#696969", linewidth = 0.4),
        axis.ticks = element_line(color = "#696969", linewidth = 0.4))
}

wl_sen <- read.csv("./data/ACAD_data/well_prec_data_2013-2025.csv")
wl_sen$year_fac <- as.factor(wl_sen$Year)
wl_sen <- wl_sen |> mutate(timestamp2 = ifelse(hr == 0,
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

head(wl_sen)
write.csv(wl_sen, "./data/ACAD_data/SEN_water_level_precip_data_2013-2025.csv", row.names = F)

gilm_sum <- wl_sen |> filter(Year >= 2016) |>
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

grme_bands <- function(plot_num){
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
    labs(y = "Water Level (cm)", x = NULL, title = "Great Meadow INT-1") +
    scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90, 120), limits = c(-65, 125)) +
    scale_x_continuous(breaks = c(135, 166, 196, 227, 258, 288),
                       labels = c("May-15", "Jun-15", "Jul-15",
                                  "Aug-15", "Sep-15", "Oct-15"),
                       guide = guide_axis(minor.ticks = T)) +
    geom_hline(yintercept = 0, color = 'black') +
    geom_hline(yintercept = -30, color = 'black', lty = 2) +
    theme(title = element_text(size = 9), legend.position = 'bottom') +
    guides(fill = guide_legend(nrow = 2, byrow = T),
           color = guide_legend(nrow = 2, byrow = T))}

p_grme1 <- grme_bands(1)
p_grme5 <- grme_bands(5)

p_grme2 <- grme_bands(2)
p_grme4 <- grme_bands(4)
p_grme6 <- grme_bands(6)

p_grme3 <- grme_bands(3)

sen_bands <- function(df, y, ptitle){
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
      scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90, 120), limits = c(-65, 125)) +
      # scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90, 120), limits = c(-65, 125)) +
      scale_x_continuous(breaks = c(135, 166, 196, 227, 258, 288),
                         labels = c("May-15", "Jun-15", "Jul-15",
                                    "Aug-15", "Sep-15", "Oct-15"),
                         guide = guide_axis(minor.ticks = T)) +
      geom_hline(yintercept = 0, color = 'black') +
      geom_hline(yintercept = -30, color = 'black', lty = 2) +
      theme(title = element_text(size = 9), legend.position = 'bottom') +
      guides(fill = guide_legend(nrow = 2, byrow = T),
             color = guide_legend(nrow = 2, byrow = T))
  )
  return(p)
}


p_bigh <- sen_bands(df = wl_sen, y = "BIGH_WL", ptitle = "Big Heath")
p_duck <- sen_bands(df = wl_sen, y = "DUCK_WL", ptitle = "Duck Pond")
p_gilm <- sen_bands(df = wl_sen, y = "GILM_WL", ptitle = "Gilmore Meadow")
p_hebr <- sen_bands(df = wl_sen, y = "HEBR_WL", ptitle = "Heath Brook")
p_hidg <- sen_bands(df = wl_sen, y = "HODG_WL", ptitle = "Hodgdon Swamp")
p_lihu <- sen_bands(df = wl_sen, y = "LIHU_WL", ptitle = "Little Hunter")
p_nemi <- sen_bands(df = wl_sen, y = "NEMI_WL", ptitle = "New Mills Meadow - NW")
p_wmtn <- sen_bands(df = wl_sen, y = "WMTN_WL", ptitle = "Western Mtn. Swamp")

# Riverine
p_grme1 + p_grme5 + p_gilm + plot_layout(axes = "collect", guides = 'collect') & theme(legend.position = 'bottom')
p_gilm + p_grme1 + p_grme5 + plot_layout(axes = "collect", guides = 'collect') & theme(legend.position = 'bottom')
ggsave("./results/Great_1_5_vs_Gilmore_water_level_distributions.png", width = 10, height = 5)

p_hebr + p_lihu + p_gilm + plot_layout(axes = "collect", guides = "collect") & theme(legend.position = 'bottom')

# Depression
p_duck + p_nemi + plot_layout(axes = "collect", guides = "collect") & theme(legend.position = 'bottom')


# ggsave("./results/Great_vs_Gilmore_water_level_distributions.png", width = 10, height = 6)

# Add rug for when drought conditions
# drgt <- getClimDrought(park = "ACAD", years = 2020:2025)

# drgt <- drgt |>
#  filter(County == "Hancock County") |>
#  select(DSCI, ValidStart)

# new_drgt <- data.frame(Date = seq.Date(min(drgt$ValidStart), max(drgt$ValidStart), 1))

# new_drgt2 <- left_join(new_drgt, drgt, by = c("Date" = "ValidStart")) |>
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
#          month = format(Date, "%d"),
#          doy = as.numeric(format(Date, "%j")),
#          year = as.numeric(format(Date, "%Y")),
#          year_fac = as.factor(year)) |>
#   fill(DSCI, .direction = 'down') |>
#   mutate(drgt = ifelse(DSCI > 0, 1, NA_real_)) |>
#   filter(!is.na(drgt)) |>
#   filter(doy >= 135 & doy <= 274)
#
# wl_sen2 <- left_join(wl_sen, new_drgt2, by = c("doy", "Year" = "year", "year_fac", "Date"))
# head(wl_sen2)

# facet by year
# ggplot(wl_sen2 |> filter(Year > 2019) |> droplevels(),
#        aes(x = doy_h, y = GILM_WL, group = year_fac)) +
#   geom_line(linewidth = 0.75, aes(color = "Gilmore Meadow")) + theme_wet() +
#   geom_line(data = wl_grme |> filter(Year > 2019) |> droplevels() |> filter(plot.num == 1),
#             linewidth = 0.75,
#             aes(x = doy_h, y = water.depth, group = year_fac, color = "Great Meadow")) +
#   labs(y = "Water Level (cm)", x = NULL) +
#   scale_color_manual(values = c("Gilmore Meadow" = "#0A60D1", "Great Meadow" = "#FFBB14")) +
#   scale_x_continuous(breaks = c(135, 166, 196, 227, 258),
#                      limits = c(135, 275),
#                      labels = c("May-15", "Jun-15", "Jul-15", "Aug-15", "Sep-15")) +
#   geom_hline(yintercept = 0) +
#   theme(legend.title = element_blank(), legend.position = "bottom") +
#   geom_rug(data = new_drgt2 |> filter(year > 2019), stat = 'identity',
#            aes(x = doy, y = drgt, group = year_fac),
#            color = "dimgrey", linewidth = 1.5) +
#   facet_wrap(~year_fac, ncol = 2)
#
# head(wl_sen2)

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
head(wl_sen)

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
            WL_med = median(water_level_cm, na.rm = TRUE),
            WL_sd = sd(water_level_cm, na.rm = TRUE),
            WL_min = suppressWarnings(min(water_level_cm, na.rm = TRUE)),
            WL_max = suppressWarnings(max(water_level_cm, na.rm = TRUE)),
            WL_u95 = suppressWarnings(quantile(water_level_cm, probs = 0.975, na.rm = TRUE)),
            WL_l95 = suppressWarnings(quantile(water_level_cm, probs = 0.025, na.rm = TRUE)),
            max_inc = suppressWarnings(max(change_WL, na.rm = TRUE)),
            max_dec = suppressWarnings(min(change_WL, na.rm = TRUE)),
            prop_GS_comp = length(which(!is.na(water_level_cm)))/n()*100,
            .groups = "drop")

head(well_gs_stats)

# Calculate change in WL from average Jun to average September
well_gs_month <- well_prp_long2 |> group_by(Year, mon, site) |>
  summarise(WL_mean = mean(water_level_cm, na.rm = TRUE),
            .groups = 'drop') |>
  filter(mon %in% c("Jun","Sep")) |> droplevels() |>
  #spread(mon, WL_mean) |>
  pivot_wider(names_from = mon, values_from = WL_mean) |>
  mutate(GS_change = Sep - Jun)


well_gs_prop1 <- well_prp_long2 |> mutate(over_0 = ifelse(water_level_cm >= 0 & !is.na(water_level_cm), 1, 0),
                                          bet_0_neg30 = ifelse(water_level_cm < 0 & water_level_cm >= -30 &
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
            check = prop_over_0cm + prop_bet_0_neg30cm + prop_under_neg30cm,
            .groups = 'drop')


gs_WL_stats <- list(well_gs_stats, well_gs_month[,c("Year","site","GS_change")], well_gs_prop) |>
  reduce(left_join, by = c("Year", "site"))

# Missing water level data from 2017, change to NA
metrics <- c("WL_mean","WL_sd","WL_min","WL_max", "max_inc","max_dec", "prop_GS_comp",
             "GS_change", "prop_over_0cm","prop_bet_0_neg30cm","prop_under_neg30cm" ,
             "check")

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

wl_stats_comb <- rbind(calc_WL_stats(df = wl_sen, col_match = "_WL") |> mutate(site_type = "SEN"),
                       calc_WL_stats(df = grme_wide, col_match = "GRME_") |> mutate(site_type = "INT")) |>
  arrange(desc(site_type), site, Year) |> data.frame() |>
  mutate(site_type2 = factor(ifelse(site_type == "SEN", "Sentinel", "Great Meadow"),
                             levels = c("Sentinel", "Great Meadow")),
         site_name = sub("_WL", "", site))

# write.csv(wl_stats_comb, "./results/water_level_statistics_SEN_GRME.csv", row.names = F)

head(wl_stats_comb)
# adding in HGM Class
vmmi <- read.csv('./data/ACAD_data/Vegetation_MMI_COW_2011-2025_ACAD_RAM_SENT_GRME.csv') |>
  filter(site_type %in% c("ACAD RAM", "ACAD Sent.")) |>
  rename(YEAR = Year, SiteCode = Code) |>
  mutate(cycle = case_when(YEAR < 2016 ~ 1,
                           between(YEAR, 2016, 2020) ~ 2,
                           YEAR > 2020 ~ 3))
site_hgm <- vmmi |> filter(cycle == 3) |> select(SiteCode, HGM_Class)
site_hgm$HGM_Class[site_hgm$SiteCode %in% c("R-04", "R-19", "R-31", "GILM")] <- "Riverine"
site_hgm$HGM_Class[site_hgm$SiteCode %in% c("R-13")] <- "Depression"


wl_stats_comb$site_name <- factor(wl_stats_comb$site_name,
                                  levels = c("BIGH", "DUCK", "GILM", "HEBR",
                                             "HODG", "LIHU", "NEMI", "WMTN",
                                             "GRME_01", "GRME_02", "GRME_03",
                                             "GRME_04", "GRME_05", "GRME_06"))

wl_stats_comb2 <- left_join(wl_stats_comb, site_hgm, by = c("site_name" = "SiteCode")) |>
  mutate(year_range = WL_max - WL_min)

wl_stats_comb2$HGM_Class[grepl("LIHU", wl_stats_comb2$site_name)] <- "Riverine"
wl_stats_comb2$HGM_Class[wl_stats_comb2$site_name %in% c("GRME_01", "GRME_05")] <- "Riverine"
wl_stats_comb2$HGM_Class[wl_stats_comb2$site_name %in% c("GRME_02", "GRME_04", "GRME_06")] <- "Depression"
wl_stats_comb2$HGM_Class[wl_stats_comb2$site_name %in% c("GRME_03")] <- "Slope"

wl_prop_long <- wl_stats_comb2 |> select(Year, site_name, HGM_Class,
                                         prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm) |>
  pivot_longer(cols = starts_with("prop"), names_to = "level", values_to = "prop") |>
  mutate(level = factor(case_when(grepl("over_0cm", level) ~ "surface water",
                                  grepl("bet_0_neg30", level) ~ "saturated",
                                  grepl("under_neg30", level) ~ "below 30cm"),
                        levels = c("surface water", "saturated", "below 30cm")))

head(wl_prop_long)

# wl_prop_long$site_name <- factor(wl_prop_long$site_name,
#                                   levels = c("BIGH", "DUCK", "GILM", "HEBR",
#                                              "HODG", "LIHU", "NEMI", "WMTN",
#                                              "GRME_01", "GRME_02", "GRME_03",
#                                              "GRME_04", "GRME_05", "GRME_06"))
wl_prop_long <- wl_prop_long |> mutate(hgm_abbr = case_when(HGM_Class == "Flats" ~ "FLT",
                                                            HGM_Class == "Depression" ~ "DEP",
                                                            HGM_Class == "Riverine" ~ "RIV",
                                                            HGM_Class == "Slope" ~ 'SLP'),
                                       site_hgm = paste0(site_name, "-", hgm_abbr))

unique(wl_prop_long$site_hgm)

wl_prop_long$site_hgm <- factor(wl_prop_long$site_hgm,
                                levels = c("DUCK-DEP", "NEMI-DEP", "GRME_02-DEP", "GRME_04-DEP", "GRME_06-DEP",
                                           "GILM-RIV", "HEBR-RIV", "LIHU-RIV", "GRME_01-RIV", "GRME_05-RIV",
                                           "HODG-SLP", "WMTN-SLP", "GRME_03-SLP", "BIGH-FLT"))

#write.csv(wl_stats_comb2, "./results/water_level_statistics_SEN_GRME.csv", row.names = F)

wl_prop_long <- wl_prop_long |> mutate(site_type = factor(ifelse(!grepl("GRME", site_name),"SEN", "INT"),
                                                          levels = c("SEN", "INT")))

table(wl_prop_long$hgm_abbr)

df <- wl_prop_long |> filter(Year >= 2016) |> filter(grepl("GRME|GILM", site_name)) |> filter(hgm_abbr == "RIV")

riv <-
ggplot(df, aes(x = Year, y = prop, group = level, fill = level)) +
  geom_bar(stat = 'identity', color = 'dimgrey') +
  facet_nested_wrap(~site_type + site_name, nrow = 1) +
  scale_fill_manual(values = c("#3288bd", "#7DC780", "#ffd16e"),
                    name = "Water Level Class") +
  theme_wet() +
  scale_x_continuous(breaks = seq(2016, 2024, 2),
                     limits = c(2015.5, 2025.5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = NULL, y = "Proportion of growing season")

df2 <- wl_prop_long |> filter(Year >= 2016) |> filter(hgm_abbr == "DEP")
dep <-
  ggplot(df2, aes(x = Year, y = prop, group = level, fill = level)) +
  geom_bar(stat = 'identity', color = 'dimgrey') +
  facet_nested_wrap(~site_type + site_name, nrow = 1) +
  scale_fill_manual(values = c("#3288bd", "#7DC780", "#ffd16e"),
                    name = "Water Level Class") +
  theme_wet() +
  scale_x_continuous(breaks = seq(2016, 2024, 2),
                     limits = c(2015.5, 2025.5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = NULL, y = "Proportion of growing season")

head(wl_prop_long)

df3 <- wl_prop_long |> filter(Year >= 2016) |> filter(hgm_abbr == "SLP")
slp <-
  ggplot(df3, aes(x = Year, y = prop, group = level, fill = level)) +
  geom_bar(stat = 'identity', color = 'dimgrey') +
  facet_nested_wrap(~site_type + site_name, nrow = 1) +
  scale_fill_manual(values = c("#3288bd", "#7DC780", "#ffd16e"),
                    name = "Water Level Class") +
  theme_wet() +
  scale_x_continuous(breaks = seq(2016, 2024, 2),
                     limits = c(2015.5, 2025.5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = NULL, y = "Proportion of growing season")

riv
dep
slp

cols = c("BIGH" = "#d53e4f",
         "DUCK" = "#f46d43",
         "GILM" = "#fdae61",
         "HEBR" = "#fee08b",
         "HODG" = "#e6f598",
         "LIHU" = "#abdda4",
         "NEMI" = "#66c2a5",
         "WMTN" = "#3288bd",
         "GRME_01" = "#a6bddb",
         "GRME_02" = "#74a9cf",
         "GRME_03" = "#3690c0",
         "GRME_04" = "#0570b0",
         "GRME_05" = "#045a8d",
         "GRME_06" = "#023858")

shps = c("BIGH" = 24, "DUCK" = 25, "GILM" = 23, "HEBR" = 21,
         "HODG" = 22, "LIHU" = 24, "NEMI" = 25, "WMTN" = 23,
         "GRME_01" = 24, "GRME_02" = 25, "GRME_03" = 23,
         "GRME_04" = 21, "GRME_05" = 24, "GRME_06" = 25)

szs = c(2, 2, 2.5, 3, 2.5, 2)

wl_stats2 <- wl_stats_comb2 |> #filter(!site_name %in% c("GRME_02", "GRME_03", "GRME_04", "GRME_05", "GRME_06"))
  filter(!site_name %in% c("GRME_01", "GRME_02", "GRME_03", "GRME_04", "GRME_06"))

wl_stats_site <- wl_stats_comb2 |>
  summarize(med_range = round(median(year_range, na.rm = T), 2),
            med_WL = round(median(WL_med, na.rm = T), 2),
            med_inc = round(median(max_inc, na.rm = T), 2),
            med_max = round(median(WL_max, na.rm = T), 2),
            med_min = round(median(WL_min, na.rm = T), 2),
            med_GS_chg = round(median(GS_change, na.rm = T), 2),
            .by = c("site_name", "HGM_Class")) |>
  arrange(med_range)

wl_stats_site
write.csv(wl_stats_site, "./results/wl_summary_stats.csv", row.names = F)
head(wl_stats2)

ggplot(wl_stats_comb2 |> filter(Year >= 2016) |>
         filter(site_name %in% c("GRME_01", "GILM", "GRME_05")),
       aes(x = Year, y = WL_med, group = site_name, fill = site_name, shape = site_name)) + #,
  #fill = site_name, shape = site_name)) +
  geom_ribbon(aes(ymin = WL_l95, ymax = WL_u95), alpha = 0.2, color = 'grey') +
  #geom_errorbar(aes(ymin = WL_l95, ymax = WL_u95)) +
  geom_line() +
  geom_point(color ='dimgrey') +
  #facet_wrap(~site_name, ncol = 1) +
  scale_fill_manual(values = cols) +
  scale_shape_manual(values = shps) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(2016, 2024, 2),
                     limits = c(2015.5, 2025.5)) +
  theme_wet() +
  labs(y = "Median Water Level (cm)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(wl_stats_comb2 |> filter(Year >= 2016) |>
         filter(site_name %in% c("GRME_01", "GILM", "GRME_05")),
    aes(x = Year, y = WL_med, group = site_name)) + #,
        #fill = site_name, shape = site_name)) +
  #geom_ribbon(aes(ymin = WL_l95, ymax = WL_u95), alpha = 0.2, color = 'grey') +
  geom_errorbar(aes(ymin = WL_l95, ymax = WL_u95)) +
  geom_line() +
  geom_point(color ='dimgrey') +
  facet_wrap(~site_name, ncol = 1) +
  # scale_fill_manual(values = cols) +
  # scale_shape_manual(values = shps) +
  # scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(2016, 2024, 2),
                     limits = c(2015.5, 2025.5)) +
  theme_wet() +
  labs(y = "Median Water Level (cm)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


ggsave("./results/GS_water_level_stats.png", width = 10, height = 6)

#--- growing season precipitation ---
library(tidyverse)
#pak::pak('katemmiller/climateNETN')
library(climateNETN)
library(patchwork)
data("NETN_clim_norms")
data("NETN_clim_annual")
acad_norm_gsppt <- NETN_clim_norms |> filter(UnitCode == "ACAD") |> filter(month %in% c(5:9)) |>
  select(UnitCode, month, ppt_norm_1901_2000)

avg_tot_gsppt <- summarize(acad_norm_gsppt, total = sum(ppt_norm_1901_2000))

acad_gsppt <- NETN_clim_annual |> filter(UnitCode == "ACAD") |> filter(month %in% c(5:9)) |>
  filter(year > 2010) |>
  filter(year < 2026) |>
  summarize(tot_gsppt = sum(ppt),
            tot_gsppt_cm = tot_gsppt/10,
            .by = c(year)) |>
  mutate(avg_gsppt_cm = (avg_tot_gsppt$total)/10)

plot_p <-
ggplot(acad_gsppt, aes(x = year, y = tot_gsppt_cm)) +
  #geom_bar(stat = 'identity', fill = '#9AC1E3', color = 'dimgrey') +
  geom_line(color = '#9AC1E3', linewidth = 1) +
  geom_point(fill = '#9AC1E3', color = 'dimgrey', size = 4, shape = 21) +
  geom_hline(yintercept = acad_gsppt$avg_gsppt_cm, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(2011, 2026, 1), limits = c(2011, 2026), expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(0, 70, 10), limits = c(0, 70)) +
  theme_NETN() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Growing Season (May - Sept)", y = "Growing season total precip. (cm)")

plot_d <-
plotClimDrought(park = "ACAD", years = 2011:2025, legend_position = "bottom", x_pad = c(0.01, 0.01))

plot_p / plot_d + plot_layout(axes = "collect_x")

ggsave("./results/total_precip_and_drought_by_year.png", width = 10, height = 8)
