#------------------------------------------
# Code for mixed models and plotting of ACAD RAM and SENT data
#------------------------------------------
library(tidyverse)
library(lme4)
library(broom.mixed)
library(patchwork)
library(ggrepel)
library(lmerTest)

# All site table of ratings
vmmi_all <- read.csv("./results/VegetationMMI_NWCA_PROB_ACAD_GRME_most_recent_REF.csv")
vmmi_all$HGM_Class[grepl("LIHU", vmmi_all$Code)] <- "Riverine"
vmmi_pct_rat <- vmmi_all |>
  mutate(vmmi_rating_orig = ifelse(vmmi > 65.22746, "Good",
                                   ifelse(vmmi < 52.785, "Poor", "Fair")))
vmmi_pct_rat_orig <- vmmi_pct_rat |>
  filter(Year > 2020) |>
  filter(!(site_type == "ACAD GRME" & Year < 2025)) |>
  filter(!(site_type == "ACAD GILM" & Year < 2025)) |>
  mutate(vmmi_n = n(), .by = c("site_type")) |>
  summarize(vmmi_tot = sum(!is.na(vmmi)),
            vmmi_pct = (vmmi_tot/first(vmmi_n))*100,
            .by = c("vmmi_rating_orig", "site_type"))

vmmi_pct_rat_orig$vmmi_rat_fac <- factor(vmmi_pct_rat_orig$vmmi_rating_orig, levels = c("Good", "Fair", "Poor"))

vmmi_pct_rat_orig2 <- vmmi_pct_rat_orig |> select(-vmmi_rating_orig) |>
  arrange(site_type, vmmi_rat_fac) |>
  pivot_wider(names_from = vmmi_rat_fac, values_from = c(vmmi_tot, vmmi_pct),
              values_fill = 0)

vmmi_pct_rat_orig2

vmmi_pct_rat_new <- vmmi_pct_rat |>
  filter(Year > 2020) |>
  filter(!(site_type == "ACAD GRME" & Year < 2025)) |>
  filter(!(site_type == "ACAD GILM" & Year < 2025)) |>
  mutate(vmmi_n = n(), .by = c("site_type")) |>
  summarize(vmmi_tot = sum(!is.na(vmmi)),
            vmmi_pct = (vmmi_tot/first(vmmi_n))*100,
            .by = c("vmmi_rating", "site_type"))

vmmi_pct_rat

vmmi_pct_rat_new$vmmi_rat_fac <- factor(vmmi_pct_rat_new$vmmi_rating, levels = c("Good", "Fair", "Poor"))

vmmi_pct_rat_new2 <- vmmi_pct_rat_new |> select(-vmmi_rating) |>
  arrange(site_type, vmmi_rat_fac) |>
  pivot_wider(names_from = vmmi_rat_fac, values_from = c(vmmi_tot, vmmi_pct),
              values_fill = 0)

vmmi_pct_rat_new2

# Split out ACAD sites for analysis and plotting
vmmi_acad <- read.csv("./data/ACAD_data/Vegetation_MMI_COW_2011-2025_ACAD_RAM_SENT_GRME.csv") |>
  mutate(site_type = ifelse(Panel == 0, "SENT", "RAM"))
vmmi_acad$HGM_Class[grepl("LITH", vmmi_acad$Code)] <- "Riverine"

vmmi_ram <- vmmi_acad |> filter(grepl("R-", Code))
vmmi_ram_status <- vmmi_ram |> filter(Year > 2020)
table(vmmi_ram_status$vmmi_rating)

vmmi_grme <- vmmi_acad |> filter(grepl("GR", Code))

GRME <- rbind(vmmi_ram |> filter(Code %in% c("R-04", "R-13", "R-19")) |> filter(Year > 2020),
              vmmi_acad |> filter(grepl("GRME", Code)) |> filter(Year == 2025))|>
  mutate(site = "GRME") |> filter(site_type == "RAM")

GILM <- rbind(vmmi_ram |> filter(Code == "R-31") |> filter(Year > 2020),
              vmmi_acad |> filter(Code == "GILM") |> filter(Year > 2020),
              vmmi_acad |> filter(grepl("GIME", Code)) |> filter(Year == 2025)) |>
  mutate(site = "GILM")

comb_gm <- rbind(GRME, GILM)

GILM_v_GRME <- comb_gm |> summarize(median = median(vmmi),
                                    se = sd(vmmi)/sqrt(n()),
                                    .by = "site")

comb_gm

# write.csv(comb_gm, "./data/ACAD_data/VMMI_GRME_vs_GILM.csv", row.names = F)

thresh <- c(41.48136, 60.94853)

theme_wet <- function(){
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "#696969", fill = "white",
                                        linewidth = 0.4),
        plot.background = element_blank(),
        strip.background = element_rect(color = "#696969", fill = "grey90", linewidth = 0.4),
        legend.key = element_blank(),
        axis.line.x = element_line(color = "#696969", linewidth = 0.4),
        axis.line.y = element_line(color = "#696969", linewidth = 0.4),
        axis.ticks = element_line(color = "#696969", linewidth = 0.4))
}

# Coef of Wetness
# -5 Obligate wetland
# -3 Facultative wetland
# 0 Facultative
# 3 Facultative upland
# 5 Obligate upland

#---- Sentinel plot -----
vmmi_sen <- vmmi_acad |> filter(site_type == "SENT")
vmmi_sen$Code[vmmi_sen$Code == "GRME"] <- "GRME (IAH)"
vmmi_sen$HGM_Class[vmmi_sen$Code == "GILM"] <- "Riverine"
vmmi_sen$HGM_Class[vmmi_sen$Code == "LITH"] <- "Riverine"

table(vmmi_sen$Code)
head(vmmi_sen)
table(vmmi_sen$Code, vmmi_sen$HGM_Class)

ggplot(vmmi_sen, aes(x = Year, y = vmmi, color = vmmi_rating, fill = vmmi_rating,
                      group = Code, shape = vmmi_rating)) + theme_wet() +
  geom_point() + geom_line() +
  scale_color_manual(values = c("Poor" = "indianred", "Fair" = "gold", "Good" = "green2"), name = "VMMI rating") +
  scale_fill_manual(values = c("Poor" = "indianred", "Fair" = "gold", "Good" = "green2"), name = "VMMI rating") +
  scale_shape_manual(values = c("Poor" = 25, "Fair" = 21, "Good" = 24), name = "VMMI rating") +
  #facet_wrap(~Code, ncol = 5) +
  facet_wrap(~HGM_Class) +
  ylim(0, 100) +
  labs(y = "Vegetation MMI") +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = 'black'),
        axis.text.y = element_text(color = 'black')) +
  scale_x_continuous(breaks = c(2011, 2016, 2021), limits = c(2010, 2022))

table(vmmi_sen$Code)
head(vmmi_sen)
table(vmmi_sen$HGM_Class, vmmi_sen$Code)

vmmi_sen$label <- ifelse(vmmi_sen$Year == max(vmmi_sen$Year) &
                           vmmi_sen$Code == "GILM",
                         vmmi_sen$Code, NA_character_)

ggplot(vmmi_sen, aes(x = Year, y = vmmi, Group = Code)) + theme_wet() +
  facet_wrap(~HGM_Class) +
  ylim(0, 100) +
  labs(y = "Vegetation MMI", X = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = 'black'),
        axis.text.y = element_text(color = 'black')) +
  scale_x_continuous(breaks = c(2011, 2016, 2021), limits = c(2010.9, 2021.1)) +
  annotate(geom = "rect", xmin = 2010.9, xmax = 2021.1, ymin = 0, ymax = 41.48136,
           fill = "#CC6666", alpha = 0.5) +
  annotate(geom = "rect", xmin = 2010.9, xmax = 2021.1, ymin = 41.48136, ymax = 60.94853,
           fill = "#FFF394", alpha = 0.5) +
  annotate(geom = "rect", xmin = 2010.9, xmax = 2021.1, ymin = 60.94853, ymax = 100,
           fill = "#88CF89", alpha = 0.5) +
  geom_line(color = 'dimgrey') + geom_point(color = 'black', fill = 'dimgrey', shape = 21) +
  geom_label_repel(aes(label = label), #nudge_x = 0.1,
                   nudge_y = -12, na.rm = T, min.segment.length = 1)

# ggsave("./results/Vegetation_MMI_SEN_facet.png", height = 5, width = 6)

head(vmmi_sen)

#---- Trend analysis ----
vmmi_ram$year_cen <- vmmi_ram$Year - min(vmmi_ram$Year)
vmmi_ram$year_fac <- as.factor(vmmi_ram$year_cen)

# VMMI Trends
vmmimod_full <- lmer(vmmi ~ year_cen * HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
vmmimod_main <- lmer(vmmi ~ year_cen + HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
vmmimod_hgm <- lmer(vmmi ~ HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
vmmimod_trend <- lmer(vmmi ~ year_cen + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
vmmimod_null <- lmer(vmmi ~ 1 + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)

arrange(AIC(vmmimod_full, vmmimod_main, vmmimod_hgm, vmmimod_trend, vmmimod_null), AIC)
plot(vmmimod_hgm)
qqnorm(residuals(vmmimod_hgm))
hist(residuals(vmmimod_hgm))
summary(vmmimod_hgm)
tidy(vmmimod_hgm)
anova(vmmimod_full) # checking for 'pretending' variables (Sutherland et al 2023)

# Mean C trends
meancmod_full <- lmer(meanC ~ year_cen * HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
meancmod_main <- lmer(meanC ~ year_cen + HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
meancmod_hgm <- lmer(meanC ~ HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
meancmod_trend <- lmer(meanC ~ year_cen + (1 + year_cen|Code), data = vmmi_ram)
meancmod_null <- lmer(meanC ~ 1 + (1 + year_cen|Code), data = vmmi_ram)

arrange(AIC(meancmod_full, meancmod_main, meancmod_hgm, meancmod_trend, meancmod_null), AIC)
plot(meancmod_hgm)
qqnorm(residuals(meancmod_hgm))
hist(residuals(meancmod_hgm))
summary(meancmod_hgm)
anova(meancmod_full) # checking for 'pretending' variables (Sutherland et al 2023)

# % Bryophyte Trends
bryomod_full <- lmer(Bryophyte_Cover ~ year_cen * HGM_Class + (1|Code), data = vmmi_ram)
bryomod_main <- lmer(Bryophyte_Cover ~ year_cen + HGM_Class + (1|Code), data = vmmi_ram)
bryomod_hgm <- lmer(Bryophyte_Cover ~ HGM_Class + (1|Code), data = vmmi_ram) # random slopes failed to converge, so rand. int.
bryomod_trend <- lmer(Bryophyte_Cover ~ year_cen + (1|Code), data = vmmi_ram)
bryomod_null <- lmer(Bryophyte_Cover ~ 1 + (1|Code), data = vmmi_ram)

# Bryo3 didn't converge. Diagnostics aren't very good. Bryo is kind of a weird metric
arrange(AIC(bryomod_full, bryomod_main, bryomod_hgm, bryomod_trend, bryomod_null), AIC)
plot(bryomod_hgm)
qqnorm(residuals(bryomod_hgm)) # a bit funky in the tails
hist(residuals(bryomod_hgm)) # potentially leptokurtic - heavier tails, but not terrible
summary(bryomod_hgm)
tidy(bryomod_hgm)
anova(bryomod_full) # checking for 'pretending' variables (Sutherland et al 2023)

# % Tolerant Trends
tolmod_full <- lmer(Cover_Tolerant ~ year_cen * HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
tolmod_main <- lmer(Cover_Tolerant ~ year_cen + HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
tolmod_hgm <- lmer(Cover_Tolerant ~ HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
tolmod_trend <- lmer(Cover_Tolerant ~ year_cen + (1 + year_cen|Code), data = vmmi_ram)
tolmod_null <- lmer(Cover_Tolerant ~ 1 + (1 + year_cen|Code) , data = vmmi_ram)

arrange(AIC(tolmod_full, tolmod_main, tolmod_hgm, tolmod_trend, tolmod_null), AIC)
plot(tolmod_full)
qqnorm(residuals(tolmod_full))
hist(residuals(tolmod_full))
summary(tolmod_full)
tidy(tolmod_full)
anova(tolmod_full) # checking for 'pretending' variables (Sutherland et al 2023)
summary(tolmod_full) # further inspection is a difference in Flats in intercept,
# and weak trend (deltaAIC = 2 ~ 0.147 pvalue)
summary(tolmod_main)

# Coef of Wetness Trends
wetmod_full <- lmer(mean_wet ~ year_cen * HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
wetmod_main <- lmer(mean_wet ~ year_cen + HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
wetmod_hgm <- lmer(mean_wet ~ HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
wetmod_trend <- lmer(mean_wet ~ year_cen + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
wetmod_null <- lmer(mean_wet ~ 1 + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)

arrange(AIC(wetmod_full, wetmod_main, wetmod_hgm, wetmod_trend, wetmod_null), AIC)
plot(wetmod_hgm)
qqnorm(residuals(wetmod_hgm))
hist(residuals(wetmod_hgm))
summary(wetmod_hgm)
tidy(wetmod_hgm)
anova(wetmod_full) # checking for 'pretending' variables (Sutherland et al 2023)
table(vmmi_ram$HGM_Class) # depression is the intercept

# Plot results
vmmi_newdat <- vmmi_ram |> select(Code, Year, meanC:vmmi_rating, mean_wet, HGM_Class, year_cen)

vmmi_pred <- cbind(vmmi_newdat,
                   pred_vmmi = predict(vmmimod_hgm, newdata = vmmi_ram),
                   pred_meanC = predict(meancmod_hgm, newdata = vmmi_ram),
                   pred_tol = predict(tolmod_full, newdata = vmmi_ram),
                   pred_wet = predict(vmmimod_hgm, newdata = vmmi_ram)
                   )
head(vmmi_pred)

vmmi_est <- data.frame(tidy(vmmimod_hgm) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class", term), gsub("HGM_Class", "", term), "Depression"),
         add = ifelse(HGM_Class == "Depression", 0, .data$estimate[.data$HGM_Class == "Depression"]),
         intercept = ifelse(HGM_Class == "Depression", estimate, estimate + add),
         resp = "vmmi")

meanC_est <- data.frame(tidy(meancmod_hgm) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class", term), gsub("HGM_Class", "", term), "Depression"),
         add = ifelse(HGM_Class == "Depression", 0, .data$estimate[.data$HGM_Class == "Depression"]),
         intercept = ifelse(HGM_Class == "Depression", estimate, estimate + add),
         resp = "meanC")

#++++++ Dist Tol. plotting/checking ++++++
# Dist. Tolerant model has interaction to accommodate
# for Supplemental table S2
# Full
tol_est <-
  data.frame(tidy(tolmod_full) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class", term), gsub("HGM_Class|year_cen:", "", term), "Depression"),
         type = ifelse(grepl("year_cen", term), "slope", "intercept"),
         add = case_when(HGM_Class == "Depression" ~ 0,
                         type == "intercept" ~ .data$estimate[.data$HGM_Class == "Depression"
                                                              & .data$type == "intercept"],
                         type == "slope" ~ .data$estimate[.data$HGM_Class == "Depression"
                                                          & .data$type == "slope"],
                         TRUE ~ NA_real_),
         est_corr = ifelse(HGM_Class == "Depression", estimate, estimate + add),
         resp = "Cover_Tolerant") |>
  arrange(HGM_Class, type)

#tol_ci <- tidy(as(tolmod_full, "merModLmerTest"), effects = 'fixed', conf.int = T, conf.method = 'profile')
tol_ci <- tidy(tolmod_full, effects = 'fixed', conf.int = T) |> data.frame()

tol_est2 <- left_join(tol_est, tol_ci |> select(term, conf.low, conf.high), by = c("term")) |>
  arrange(type, HGM_Class) |>
  mutate(lowerCI = add + conf.low,
         upperCI = add + conf.high)
head(tol_est2)

tol_est_wide <- tol_est2 |> select(HGM_Class, type, estimate = est_corr) |>
  pivot_wider(names_from = type, values_from = c(estimate)) |>
  select(HGM_Class, int = intercept, slp = slope) |> data.frame()

tol_est_wide2 <- tol_est2 |>
  select(HGM_Class, type, est = est_corr, lCI = lowerCI, uCI = upperCI) |>
  pivot_wider(names_from = type, values_from = c(est, lCI, uCI)) |>
  select(HGM_Class, intercept = est_intercept,
         int_lCI = lCI_intercept,
         int_uCI = uCI_intercept,
         slope = est_slope,
         slope_lCI = lCI_slope,
         slope_uCI = uCI_slope) |> data.frame()

head(tol_est_wide2)

options(digits = 4)
ggplot(tol_est_wide2, aes(x = slope, y = HGM_Class)) + theme_wet() +
  geom_point(color = 'black', size = 2.5, shape = 21, fill = "dimgrey") +
  geom_errorbar(aes(xmin = slope_lCI, xmax = slope_uCI), color = 'dimgrey') +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  scale_y_discrete(limits = rev) +
  labs(y = "HGM Class", x = "Slope (% cover/year)")

# ggsave("./results/Pct_tol_coef_plots.png", height = 5, width = 6)

# Main
tol_est_main <-
  data.frame(tidy(tolmod_main) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class", term), gsub("HGM_Class|year_cen:", "", term), "Depression"),
         type = ifelse(grepl("year_cen", term), "slope", "intercept"),
         add = case_when(HGM_Class == "Depression" ~ 0,
                         type == "intercept" ~ .data$estimate[.data$HGM_Class == "Depression"
                                                              & .data$type == "intercept"],
                         type == "slope" ~ .data$estimate[.data$HGM_Class == "Depression"
                                                          & .data$type == "slope"],
                         TRUE ~ NA_real_),
         est_corr = ifelse(HGM_Class == "Depression", estimate, estimate + add),
         resp = "Cover_Tolerant") |>
  arrange(HGM_Class, type)

tol_ci_main <- tidy(tolmod_main, effects = 'fixed', conf.int = T) |> data.frame()

tol_est_m2 <- left_join(tol_est_main,
                        tol_ci_main |> select(term, conf.low, conf.high), by = c("term")) |>
  arrange(type, HGM_Class) |>
  mutate(lowerCI = add + conf.low,
         upperCI = add + conf.high)
head(tol_est_m2)

tol_est_m_wide <- tol_est_m2 |> select(HGM_Class, type, estimate = est_corr) |>
  pivot_wider(names_from = type, values_from = c(estimate)) |>
  select(HGM_Class, int = intercept, slp = slope)

tol_est_m_wide2 <- tol_est_m2 |>
  select(HGM_Class, type, est = est_corr, lCI = lowerCI, uCI = upperCI) |>
  pivot_wider(names_from = type, values_from = c(est, lCI, uCI)) |>
  select(HGM_Class, intercept = est_intercept,
         int_lCI = lCI_intercept, int_uCI = uCI_intercept,
         slope = est_slope,
         slope_lCI = lCI_slope, slope_uCI = uCI_slope)

tol_ci_trend <- tidy(tolmod_trend, effects = 'fixed', conf.int = T) |> data.frame()
tol_ci_HGM <- tidy(tolmod_hgm, effects = 'fixed', conf.int = T) |> data.frame()


tol_est <- data.frame(tidy(tolmod_hgm) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class", term), gsub("HGM_Class", "", term), "Depression"),
         add = ifelse(HGM_Class == "Depression", 0, .data$estimate[.data$HGM_Class == "Depression"]),
         intercept = ifelse(HGM_Class == "Depression", estimate, estimate + add),
         resp = "Cover_Tolerant")
#++++ End % cover tolerant ++++

bryo_est <- data.frame(tidy(bryomod_hgm) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class", term), gsub("HGM_Class", "", term), "Depression"),
         add = ifelse(HGM_Class == "Depression", 0, .data$estimate[.data$HGM_Class == "Depression"]),
         intercept = ifelse(HGM_Class == "Depression", estimate, estimate + add),
         resp = "Bryophyte_Cover")

wet_est <- data.frame(tidy(wetmod_hgm) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class", term), gsub("HGM_Class", "", term), "Depression"),
         add = ifelse(HGM_Class == "Depression", 0, .data$estimate[.data$HGM_Class == "Depression"]),
         intercept = ifelse(HGM_Class == "Depression", estimate, estimate + add),
         resp = "mean_wet")

params <- rbind(vmmi_est, meanC_est, tol_est, bryo_est, wet_est)

pred_plot <- function(param, ylabel, yran = NA, thresh = TRUE){
  ints <- params |> filter(resp == param)
  yrange <-
    if(any(is.na(yran))){
      c(floor(min(vmmi_pred[,param])), ceiling(max(vmmi_pred[,param])))
    } else {yran}

  ggplot(vmmi_pred, aes(x = Year, y = .data[[param]])) +
    theme_wet() +
    {if(thresh == TRUE) annotate(geom = "rect",
                                 xmin = 2011, xmax = 2026, ymin = 0, ymax = 41.48136,
                                 fill = "#CC6666", alpha = 0.5)} +
    {if(thresh == TRUE) annotate(geom = "rect",
                                 xmin = 2011, xmax = 2026, ymin = 41.48136, ymax = 60.94853,
                                 fill = "#FFF394", alpha = 0.5)} +
    {if(thresh == TRUE) annotate(geom = "rect",
                                 xmin = 2011, xmax = 2026, ymin = 60.94853, ymax = 100,
                                 fill = "#88CF89", alpha = 0.5)} +

    geom_point(aes(group = Code), alpha = 0.6, color = '#474747') +
    geom_line(aes(group = Code), alpha = 0.6, color = '#474747') +
    scale_y_continuous(limits = yrange) +
    scale_x_continuous(limits = c(2011, 2026), breaks = seq(2011, 2026, 3))+
    geom_abline(data = ints,
                aes(intercept = intercept, slope = rep(0, 4),
                    group = HGM_Class),
                lwd = 0.75, linetype = 'dashed') +
    labs(y = ylabel, x = NULL) +
    #guides(color = guide_legend(override.aes = list(alpha = 1))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))}


pred_plot("vmmi", "Vegetation MMI", yran = c(0, 100)) +
  facet_wrap(~HGM_Class)

# # ggsave("./results/Vegetation_MMI_RAM_facet.png", height = 5, width = 6)

pred_plot("meanC", "Mean C", thresh = T) + facet_wrap(~HGM_Class)

pred_plot("Cover_Tolerant", "% Cover Tolerant", thresh = F) + facet_wrap(~HGM_Class)

pred_plot("Bryophyte_Cover", "% Cover Bryophyte", thresh = F) + facet_wrap(~HGM_Class)

pred_plot("mean_wet", "Mean Wetness", thresh = F) + facet_wrap(~HGM_Class)

# # ggsave("./results/mean_wetness_RAM_facet.png", height = 5, width = 6)


pred_plot2 <- function(param, ylabel, yran = NA){
  ints <- params |> filter(resp == param)
  yrange <-
    if(any(is.na(yran))){
      c(floor(min(vmmi_pred[,param])), ceiling(max(vmmi_pred[,param])))
    } else {yran}

  ggplot(vmmi_pred, aes(x = Year, y = .data[[param]],
                        fill = HGM_Class, color = HGM_Class,
                        shape = HGM_Class, size = HGM_Class)) +
    theme_wet() +
    geom_line(aes(group = Code), alpha = 0.4, linewidth = 0.2) +
    geom_point(aes(group = Code), alpha = 0.4) +
    scale_y_continuous(limits = yrange) +
    scale_x_continuous(limits = c(2011, 2026), breaks = seq(2011, 2026, 3))+
    scale_color_manual(values = c("Depression" = "#70D8CF",#"#5EC962",
                                  "Flats" = "#994F00", #"#994455",
                                  "Riverine" = "#053ac3", #"#3B528B",
                                  "Slope" = "#FFBF30"),
                       name = NULL, aesthetics = c("fill", "color")
                       ) +
    scale_shape_manual(values = c("Depression" =19,
                                  "Flats" = 22,
                                  "Riverine" = 24,
                                  "Slope" = 25),
                       name = NULL) +
    scale_size_manual(values = c("Depression" = 2.5,
                                  "Flats" = 2,
                                  "Riverine" = 2,
                                  "Slope" = 2),
                       name = NULL) +
    geom_segment(data = ints,
                 aes(x = 2012, xend = 2025, y = intercept, yend = intercept,
                     group = HGM_Class, color = HGM_Class),
                 linetype = 'dashed', #"F1",
                 lwd = 1.5,
                 show.legend = F) +
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    labs(y = ylabel, x = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}

pred_plot2("vmmi", "Veg. MMI", yran = c(0, 100))
# # ggsave("./results/Vegetation_MMI_RAM.png", height = 4, width = 6)
pred_plot2("mean_wet", "Mean Wetness")
# # ggsave("./results/mean_wetness_RAM.png", height = 4, width = 6)
pred_plot2("meanC", "Mean C", yran = c(2.5, 6.5))
pred_plot2("Invasive_Cover", "% Inv. Cov", yran = c(0, 10))
pred_plot2("Bryophyte_Cover", "% Bryo. Cov")
#pred_plot2("Cover_Tolerant", "% Dist. Tol.")

#----- VMMI distributions among site types -----
# ACAD sites
vmmi_acad <- read.csv("./data/ACAD_data/Vegetation_MMI_2011-2025_ACAD_RAM_SENT_GRME.csv") |>
  mutate(site_type = case_when(grepl("R-", Code) ~ "ACAD RAM",
                               grepl("GRME0|GRME10", Code) ~ "ACAD GRME",
                               Code %in% c("BIGH", "DUCK", "FRAZ", "GILM", "GRME", "HEBR",
                                           "HODG", "LITH", "NEMI", "WMTN") ~ "ACAD Sent.",
                               grepl("GIME", Code) ~ "ACAD GILM",
                               TRUE ~ "UNK"
  )) |> select(Code, Year, vmmi, vmmi_rating, site_type)

vmmi_grme <- vmmi_acad |> filter(site_type == "ACAD GRME") |> filter(Year == 2025)
vmmi_sent <- vmmi_acad |> filter(site_type == "ACAD Sent.") |> filter(Year == 2021)
vmmi_ram <- vmmi_acad |> filter(site_type == "ACAD RAM") |> filter(Year > 2021)

nwca_all <- read.csv("./data/EPA_compiled/Vegetation_MMI_2011-2021_EPA_allsites.csv")
table(nwca_all$US_L3CODE)

nwca_prob <- nwca_all |>
  filter(YEAR %in% c(2021, 2022)) |>
  filter(SITETYPE == "PROB") |>
  select(Code = SITE_ID, Year = YEAR, vmmi, vmmi_rating) |>
  mutate(site_type = "EPA Prob.")

nwca_ref <- nwca_all |>
  filter(site_type_fac == "REF") |>
  select(Code = SITE_ID, Year = YEAR, vmmi, vmmi_rating) |>
  mutate(site_type = "EPA Ref.") # ACAD Sent. not included

vmmi_comb <- rbind(vmmi_ram, vmmi_sent,
                   vmmi_grme, nwca_prob, nwca_ref)

vmmi_comb$site_type_fac <- factor(vmmi_comb$site_type,
                                  levels = c("EPA Ref.", "EPA Prob.", "ACAD Sent.", "ACAD RAM",
                                             "ACAD GRME"))#, "ACAD GILM"))

# Add great meadow sites here, after updating the thresholds for ratings.
ggplot(vmmi_comb,
       aes(x = site_type_fac, y = vmmi)) +
  geom_boxplot(outliers = F) + theme_wet() +
  geom_jitter(alpha = 0.2) +
  labs(x = "Site Disturbance Type", y = "Vegetation MMI") +
  geom_hline(yintercept = thresh[1], linewidth = 0.5, color = "#696969",
             linetype = 'dashed') +
  geom_hline(yintercept = thresh[2], linewidth = 0.75, color = "#696969")

# ggsave("./results/VMMI_distribution_site_type_ACAD_EPA.png", height = 4, width = 6)

#--- Number of stressors vs VMMI ---
head(vmmi_comb)
buff_all <- read.csv("./results/Stressor_Counts_NWCA_PROB_ACAD_GRME_most_recent_REF.csv")
head(buff_all)
table(buff_all$site_type, useNA = 'always')

vmmi_nwca1 <- read.csv("./data/EPA_compiled/Vegetation_MMI_2011-2021_EPA_allsites.csv") |>
  select(-HGM_Class)

site_all <- read.csv("./data/EPA_compiled/Site_Information_2011-2021.csv")
head(vmmi_nwca1)

vmmi_nwca <- left_join(vmmi_nwca1, site_all |> select(UID, HGM_Class = WETCLS_HGM),
                        by = c("UID")) |>
  select(Code = SITE_ID, Year = YEAR, site_type,
         meanC, bryocov = bryo_cov, invcov = inv_cov,
         covtol = disttol_cov, vmmi, vmmi_rating, HGM_Class)

vmmi_prob21 <- vmmi_nwca |>
  filter(grepl("PROB", site_type)) |>
  filter(Year %in% c(2021, 2022))

vmmi_ref <- vmmi_nwca |>
  filter(site_type == "REF")

names(vmmi_ref)
names(vmmi_prob21)

vmmi_acad <- read.csv("./data/ACAD_data/Vegetation_MMI_2011-2025_ACAD_RAM_SENT_GRME.csv") |>
  filter(!grepl("GIME", Code)) |> # drop Gilmore Meadow intensification
  mutate(site_type = case_when(Panel %in% 1:4 ~ "ACAD RAM",
                               Panel == -1 ~ "ACAD GRME",
                               Panel == 0 ~ "ACAD Sent.")) |>
  select(Code, Year, site_type,
         meanC, bryocov = Bryophyte_Cover, invcov = Invasive_Cover,
         covtol = Cover_Tolerant, vmmi, vmmi_rating, HGM_Class) |>
  filter((site_type == "ACAD RAM" & Year > 2020) |
           (site_type == "ACAD GRME" & Year == 2025)|
             (site_type == "ACAD Sent." & Year == 2021))

table(vmmi_acad$Year, vmmi_acad$site_type)

vmmi_comb <- rbind(vmmi_prob21, vmmi_ref, vmmi_acad)
table(complete.cases(vmmi_comb$vmmi))
head(buff_all)

vmmi_buff <- full_join(vmmi_comb |> select(-site_type),
                        buff_all |> select(Code, site_type, Year, AA, BUFF),
                         by = c("Code", "Year")) |>
  filter(!is.na(vmmi)) |>
  filter(!is.na(BUFF))

# cleanup HGM_Class
vmmi_buff$HGM_Class_clean <-
  case_when(vmmi_buff$HGM_Class %in% c("Depression", "DEPRESSION", "DPRSS") ~ "Depression",
            vmmi_buff$HGM_Class %in% c("FLATS", "Flats") ~ "Flats",
            vmmi_buff$HGM_Class %in% c("FRINGE", "LACUSTRINE") ~ "Lacustrine",
            vmmi_buff$HGM_Class %in% c("Riverine", "RIVERINE") ~ "Riverine",
            vmmi_buff$HGM_Class %in% c("SLOPE", "Slope") ~ "Slope",
            TRUE ~ "Unknown")
vmmi_buff2 <- vmmi_buff |> filter(!HGM_Class_clean %in% c("Unknown", "Lacustrine")) |>
  mutate(stress_total = AA + BUFF)

table(vmmi_buff$HGM_Class, vmmi_buff$HGM_Class_clean)
table(vmmi_buff$site_type, useNA = 'always')
table(complete.cases(vmmi_buff$vmmi))

head(vmmi_buff)

ggplot(vmmi_buff2, aes(x = AA, y = vmmi)) +
  geom_point() +
  geom_smooth(method = 'lm') + theme_wet()

ggplot(vmmi_buff2, aes(x = BUFF, y = vmmi)) +
  geom_point() + geom_smooth(method = "lm") + theme_wet()

buffmod <- lm(vmmi ~ BUFF, data = vmmi_buff2[-34,])
summary(buffmod)
plot(buffmod)

aamod_full <- lm(vmmi ~ AA * HGM_Class_clean, data = vmmi_buff2)
aamod_add <- lm(vmmi ~ AA + HGM_Class_clean, data = vmmi_buff2)
aamod_HGM <- lm(vmmi ~ HGM_Class_clean, data = vmmi_buff2)
aamod_AA <- lm(vmmi ~ AA, data = vmmi_buff2)

arrange(AIC(aamod_full, aamod_add, aamod_HGM, aamod_AA), AIC)

#aamod_add
summary(aamod_add)
par(mfrow = c(2,2))
plot(aamod_add)
par(mfrow = c(1,1))

vmmi_buff2$HGM_Class <- vmmi_buff2$HGM_Class_clean
vmmi_buff3 <- vmmi_buff2 |> filter(site_type %in% c("EPA Prob."))
#  filter(site_type %in% c("EPA Prob.", "ACAD RAM"))

stressmod_full <- lm(vmmi ~ stress_total * HGM_Class_clean, data = vmmi_buff3)
stressmod_add <- lm(vmmi ~ stress_total + HGM_Class_clean, data = vmmi_buff3)
stressmod_stress <- lm(vmmi ~ stress_total, data = vmmi_buff3)
stressmod_hgm <- lm(vmmi ~ HGM_Class_clean, data = vmmi_buff3)
stressmod_null <- lm(vmmi ~ 1, data = vmmi_buff3)

arrange(round(AIC(stressmod_full, stressmod_add,
                  stressmod_hgm, stressmod_stress, stressmod_null),2), AIC)

summary(stressmod_add) # R2 = 0.33 #EPA Prob; 0.39 for EPA Prob and ACAD RAM
par(mfrow = c(2,2))
plot(stressmod_add) # not bad!
par(mfrow = c(1,1))

hist(residuals(stressmod_add)) # not bad!

tidy(stressmod_add)
intercept <- tidy(stressmod_add)$estimate[1]

stress_est <- data.frame(tidy(stressmod_add)) |>
  select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class_clean", term), gsub("HGM_Class_clean", "", term),
                            ifelse(term == "(Intercept)", "Depression", NA)),
         beta1 = ifelse(HGM_Class == "Depression", estimate, intercept + estimate),
         beta = ifelse(is.na(beta1), estimate, beta1),
         resp = "total_stress_vs_vmmi") |>
  select(-beta1)

stress_est

head(vmmi_buff2)
head(vmmi_buff3)

stress_p <-
ggplot(vmmi_buff3, aes(x = stress_total, y = vmmi,
                       fill = HGM_Class_clean, color = HGM_Class_clean,
                       shape = HGM_Class_clean, size = HGM_Class_clean)) +
    theme_wet() +
    geom_point(alpha = 0.4) +
    ylim(0, 100) + xlim(0, 15) +
    scale_color_manual(values = c("Depression" = "#70D8CF",#"#5EC962",
                                  "Flats" = "#994F00", #"#994455",
                                  "Riverine" = "#053ac3", #"#3B528B",
                                  "Slope" = "#FFBF30"),
                       name = NULL, aesthetics = c("fill", "color")) +
    scale_shape_manual(values = c("Depression" =19,
                                  "Flats" = 22,
                                  "Riverine" = 24,
                                  "Slope" = 25),
                       name = NULL) +
    scale_size_manual(values = c("Depression" = 2.5,
                                 "Flats" = 2,
                                 "Riverine" = 2,
                                 "Slope" = 2),
                      name = NULL) +
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    labs(y = "Veg MMI", x = "# Stressors", subtitle = "EPA Prob.")

stress_p2 <-
stress_p +
  geom_abline(intercept = stress_est$beta[stress_est$HGM_Class == "Depression"],
              slope = stress_est$beta[stress_est$term == "stress_total"],
              lwd = 1, show.legend = F,
              color = "#70D8CF") +
  geom_abline(intercept = stress_est$beta[stress_est$HGM_Class == "Flats"],
              slope = stress_est$beta[stress_est$term == "stress_total"],
              lwd = 1, show.legend = F,
              color = "#994F00") +
  geom_abline(intercept = stress_est$beta[stress_est$HGM_Class == "Riverine"],
              slope = stress_est$beta[stress_est$term == "stress_total"],
              lwd = 1, show.legend = F,
              color = "#053ac3") +
  geom_abline(intercept = stress_est$beta[stress_est$HGM_Class == "Slope"],
              slope = stress_est$beta[stress_est$term == "stress_total"],
              lwd = 1, show.legend = F,
              color = "#FFBF30")

stress_p2

vmmi_buff3 |> group_by(HGM_Class) |>
  summarize(mean_mmi = mean(vmmi))

# ACAD RAM only
vmmi_buffa <- vmmi_buff2 |> filter(site_type == "ACAD RAM")
stressmod_fulla <- lm(vmmi ~ stress_total * HGM_Class_clean, data = vmmi_buffa)
stressmod_adda <- lm(vmmi ~ stress_total + HGM_Class_clean, data = vmmi_buffa)
stressmod_stressa <- lm(vmmi ~ stress_total, data = vmmi_buffa)
stressmod_hgma <- lm(vmmi ~ HGM_Class_clean, data = vmmi_buffa)
stressmod <- lm(vmmi ~ 1, vmmi_buffa)

arrange(round(AIC(stressmod_fulla, stressmod_adda,
          stressmod_hgma, stressmod_stressa, stressmod),2), AIC)

summary(stressmod_adda) # R2 = 0.57
par(mfrow = c(2,2))
plot(stressmod_adda) # not bad!
par(mfrow = c(1,1))
hist(residuals(stressmod_adda)) # not bad!

tidy(stressmod_adda)
intercepta <- tidy(stressmod_adda)$estimate[1]

stress_esta <- data.frame(tidy(stressmod_adda)) |>
  select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class_clean", term), gsub("HGM_Class_clean", "", term),
                            ifelse(term == "(Intercept)", "Depression", NA)),
         beta1 = ifelse(HGM_Class == "Depression", estimate, intercepta + estimate),
         beta = ifelse(is.na(beta1), estimate, beta1),
         resp = "total_stress_vs_vmmi") |>
  select(-beta1)

stress_esta

stress_pa <-
  ggplot(vmmi_buffa, aes(x = stress_total, y = vmmi,
                         fill = HGM_Class, color = HGM_Class,
                         shape = HGM_Class, size = HGM_Class)) +
  theme_wet() +
  geom_point(alpha = 0.4) +
  ylim(0, 100) + xlim(0, 15) +
  scale_color_manual(values = c("Depression" = "#70D8CF",#"#5EC962",
                                "Flats" = "#994F00", #"#994455",
                                "Riverine" = "#053ac3", #"#3B528B",
                                "Slope" = "#FFBF30"),
                     name = NULL, aesthetics = c("fill", "color")) +
  scale_shape_manual(values = c("Depression" =19,
                                "Flats" = 22,
                                "Riverine" = 24,
                                "Slope" = 25),
                     name = NULL) +
  scale_size_manual(values = c("Depression" = 2.5,
                               "Flats" = 2,
                               "Riverine" = 2,
                               "Slope" = 2),
                    name = NULL) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  labs(y = "Veg MMI", x = "# Stressors", subtitle = "ACAD RAM")

stress_pa2 <-
stress_pa +
  geom_abline(intercept = stress_esta$beta[stress_esta$HGM_Class == "Depression"],
              slope = stress_esta$beta[stress_esta$term == "stress_total"],
              lwd = 1, show.legend = F,
              color = "#70D8CF") +
  geom_abline(intercept = stress_esta$beta[stress_esta$HGM_Class == "Flats"],
              slope = stress_esta$beta[stress_esta$term == "stress_total"],
              lwd = 1, show.legend = F,
              color = "#994F00") +
  geom_abline(intercept = stress_esta$beta[stress_esta$HGM_Class == "Riverine"],
              slope = stress_esta$beta[stress_esta$term == "stress_total"],
              lwd = 1, show.legend = F,
              color = "#053ac3") +
  geom_abline(intercept = stress_esta$beta[stress_esta$HGM_Class == "Slope"],
              slope = stress_esta$beta[stress_esta$term == "stress_total"],
              lwd = 1, show.legend = F,
              color = "#FFBF30")

stress_p2 + stress_pa2 + plot_layout(guides = 'collect', axis_titles = 'collect') &
  theme(legend.position = 'bottom')

# ggsave("./results/vegmmi_vs_stressors_EPA_ACAD.png", height = 5, width = 8)

# For every additional stressor, there's a 3 point decrease in mean VMMI
# Flats have the highest intercept and Riverine have the lowest.

#--- ACAD Ordination plots ---
#grme_spp <- read.csv('./data/ACAD_data/ACAD_wetland_Species_List_20260126.csv')
library(tidyverse)
library(ggrepel)
library(wetlandACAD)
library(vegan)

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

# Combine RAM and SEN species data
importRAM(export_protected = T, type = 'zip',
  filepath = "./data/ACAD_data/NETN_Wetland_RAM_Data_20260608_NPSonly.zip")
ram_spp1 <- VIEWS_RAM$species_list

ram_spp <- ram_spp1 |> filter(Visit_Type == "VS") |>
  select(SiteCode, YEAR = Year, SYMBOL = PLANTS_Code) |>
  mutate(SiteType = "RAM",
         present = 1)

epa_acad <- read.csv("./data/EPA_compiled/vegetation_mmi_2011-2021_ACAD_ref.csv")
acad_uids <- sort(unique(epa_acad$UID))
sen_spp1 <- read.csv('./data/EPA_compiled/plant_cover_2011-2021.csv') |>
  filter(UID %in% acad_uids)

# Clean up species names inconsistencies across years
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "ALINR"] <- "ALIN2"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "ANPOG"] <- "ANPO"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "CATUT"] <- "CATU5"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "CAECE"] <- "CAEC"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "CALAA"] <- "CALAA"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "CAMAI2"] <- "CAMA12"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "ERANA3"] <- "ERAN6"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "LEGR"] <- "RHGR3"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "LIBOL2"] <- "LIBO3"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "MOCA7"] <- "MOPE6"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "NULUV"] <- "NUVA2"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "OSCI"] <- "OSCI2"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "PLCL"] <- "GYCL"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "SAPUP6"] <- "SAPU4"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "SOULL2"] <- "SOUL"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "SPALL"] <- "SPAL2"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "THPAP"] <- "THPA"
sen_spp1$SYMBOL[sen_spp1$SYMBOL == "VINU"] <- "VINUC"

sen_spp <- sen_spp1 |>
  mutate(SiteCode = case_when(grepl("301", SITE_ID) ~ "DUCK",
                              grepl("302", SITE_ID) ~ "WMTN",
                              grepl("303", SITE_ID) ~ "BIGH",
                              grepl("304", SITE_ID) ~ "GILM",
                              grepl("305", SITE_ID) ~ "LITH",
                              grepl("306", SITE_ID) ~ "NEMI",
                              grepl("307", SITE_ID) ~ "GRME",
                              grepl("308", SITE_ID) ~ "HEBR",
                              grepl("309", SITE_ID) ~ "HODG",
                              grepl("310", SITE_ID) ~ "FRAZ")) |>
  summarize(cov = sum(COVER, na.rm = T),
            .by = c(SYMBOL, SiteCode, YEAR)) |>
  mutate(present = ifelse(cov > 0, 1, 0),
         SiteType = "SEN") |>
  arrange(SiteCode, SYMBOL, YEAR) |>
  select(SiteCode, YEAR, SYMBOL, SiteType, present)

spp_comb <- rbind(ram_spp, sen_spp)

data.frame(table(spp_comb$SiteCode, spp_comb$YEAR, spp_comb$SYMBOL)) |>
  filter(Freq > 1)

spp_wide <- spp_comb |> arrange(SYMBOL, SiteCode) |> filter(!is.na(SYMBOL)) |>
  mutate(cycle = case_when(YEAR < 2016 ~ 1,
                           between(YEAR, 2016, 2020) ~ 2,
                           YEAR > 2020 ~ 3)) |>
  pivot_wider(names_from = SYMBOL, values_from = present, values_fill = 0) |>
  arrange(SiteType, SiteCode, YEAR) |>
  data.frame()

row.names(spp_wide) <- paste0(spp_wide$SiteCode, "_", spp_wide$cycle)

# drop species found <4 times (ie only in 1 plot every visit)
rare_spp <- spp_comb |> summarize(num_pres = sum(present), .by = c(SYMBOL)) |>
  filter(num_pres < 4) |> select(SYMBOL)

spp_wide2 <- spp_comb |> arrange(SYMBOL, SiteCode) |>
  filter(!is.na(SYMBOL)) |>
  filter(!SYMBOL %in% rare_spp$SYMBOL) |>
  mutate(cycle = case_when(YEAR < 2016 ~ 1,
                           between(YEAR, 2016, 2020) ~ 2,
                           YEAR > 2020 ~ 3)) |>
  pivot_wider(names_from = SYMBOL, values_from = present, values_fill = 0) |>
  arrange(SiteType, SiteCode, YEAR) |>
  data.frame()

# vmmi and cow for envfit
vmmi <- read.csv('./data/ACAD_data/Vegetation_MMI_COW_2011-2025_ACAD_RAM_SENT_GRME.csv') |>
  filter(site_type %in% c("ACAD RAM", "ACAD Sent.")) |>
  rename(YEAR = Year, SiteCode = Code) |>
  mutate(cycle = case_when(YEAR < 2016 ~ 1,
                           between(YEAR, 2016, 2020) ~ 2,
                           YEAR > 2020 ~ 3))

# Ordination
nmds2 <- metaMDS(spp_wide2[,6:ncol(spp_wide2)], distance = 'jaccard', k = 2, maxit = 100,
                 autotransform = FALSE)
stressplot(nmds2)
nmds2 # stress = 0.135

spp_env <- left_join(spp_wide |> select(SiteCode:cycle),
                     vmmi,
                     by = c("SiteCode", "YEAR", "cycle"))

spp_envfit <- envfit(nmds2, spp_env |> select(MeanC = meanC,
                                              BryoCov = Bryophyte_Cover,
                                              InvCov = Invasive_Cover,
                                              STolCov = Cover_Tolerant,
                                              VegMMI = vmmi,
                                              MeanWet = mean_wet,
                                              cycle))
spp_fit <- envfit(nmds2, spp_wide2[,6:ncol(spp_wide2)])

# plot(nmds2, display = 'sites')
# ordihull(nmds2,
#          spp_wide$SiteCode, display = 'sites', draw = 'lines')

site_scores1 <- as.data.frame(scores(nmds2, display = 'sites'))
site_scores2 <- cbind(spp_wide[,1:5], site_scores1) #|>
site_scores2$site_lab <- ifelse(site_scores2$SiteCode %in% c("R-13", "R-04", "R-19"), paste0("GRME"),
                          ifelse(site_scores2$SiteCode %in% c("R-31", "GILM"), paste0("GILM"),
                            NA_character_))
site_scores2$site_lab <- ifelse(site_scores2$SiteCode %in% c("GILM"), paste0("GILM-S"), site_scores2$site_lab)

site_hgm <- vmmi |> filter(cycle == 3) |> select(SiteCode, HGM_Class)
site_hgm$HGM_Class[site_hgm$SiteCode %in% c("R-13", "R-04", "R-19", "R-31", "GILM")] <- "Riverine"

site_scores <- left_join(site_scores2,
                         site_hgm,
                         by = "SiteCode")

spp_scores <- as.data.frame(scores(spp_fit, display = 'vectors'))
spp_scores <- cbind(spp_scores, pval = spp_fit$vectors$pvals)
sig_spp_scores <- spp_scores |> filter(pval <= 0.05)

env_scores <- as.data.frame(scores(spp_envfit, display = 'vectors'))
env_scores <- cbind(env_scores, pval = spp_envfit$vectors$pvals)
sig_env_scores <- env_scores |> filter(pval <= 0.05) |> data.frame()
sig_env_scores$var <- row.names(sig_env_scores)

table(site_scores$SiteCode)

ggplot(site_scores, aes(x = NMDS1, y = NMDS2)) + theme_wet() +
  geom_point(aes(color = SiteType, shape = SiteType, size = SiteType)) +
  scale_color_manual(values = c("#82B9D1", "#4d9221")) +
  scale_shape_manual(values = c(19, 17)) +
  scale_size_manual(values = c(2.25, 2.25)) +
  geom_path(aes(group = SiteCode, color = SiteType), alpha = 0.8) + #,
   # arrow = arrow(length = unit(0.1, "inches"), type = 'closed'))
  geom_text_repel(data = site_scores |> filter(cycle == 3),
                  aes(label = SiteCode), size = 3.5)

ggplot(site_scores, aes(x = NMDS1, y = NMDS2)) + theme_wet() +
  geom_point(aes(color = SiteType, shape = SiteType, size = SiteType)) +
  scale_color_manual(values = c("#82B9D1", "#4d9221")) +
  scale_shape_manual(values = c(19, 17)) +
  scale_size_manual(values = c(2.25, 2.25)) +
  geom_path(aes(group = SiteCode, color = SiteType), alpha = 0.8) +
  geom_text_repel(data = site_scores |> filter(cycle == 3) |> filter(site_lab %in% c("GILM", "GRME", "GILM-S")),
                  aes(label = site_lab, color = SiteType),
                    size = 3.5, segment.size = 0.1, segment.color = 'dimgrey',
                    min.segment.length = 0.01, show.legend = F) +
  geom_segment(data = sig_env_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.1, "inches")), color = 'dimgrey', lwd = 0.8,
               show.legend = F) +
  geom_text_repel(data = sig_env_scores, aes(x = NMDS1, y = NMDS2, label = var),
                  show.legend = F)

# ggsave("./results/ACAD_ordination_plot_envfit.png", height = 5, width = 6)

# By HGM Class
ggplot(site_scores, aes(x = NMDS1, y = NMDS2)) + theme_wet() +
  geom_point(aes(fill = HGM_Class, shape = HGM_Class, size = HGM_Class), color = 'dimgrey') +
  scale_fill_manual(values = c(Depression = "#70D8CF",
                               Flats = "#994F00",
                               Riverine = "#053ac3",
                               Slope = "#FFBF30"), name = "HGM Class") +
  scale_shape_manual(values = c(21, 22, 24, 25), name = "HGM Class") +
  scale_size_manual(values = c(2.5, 2.25, 2, 2), name = "HGM Class") +
  geom_path(aes(group = SiteCode), color = 'dimgrey', alpha = 0.8,
                arrow = arrow(length = unit(0.1, "inches"), type = 'open')) +
  geom_text_repel(data = site_scores |> filter(cycle == 3) |> filter(site_lab %in% c("GILM", "GILM-S", "GRME")),
                  aes(label = site_lab), # color = SiteType),
                  size = 3.5, segment.size = 0.1, segment.color = 'dimgrey',
                  min.segment.length = 0.01, show.legend = F) +
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  geom_segment(data = sig_env_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.1, "inches")), color = 'blue', lwd = 0.6,
               show.legend = F) +
  geom_text_repel(data = sig_env_scores, aes(x = NMDS1, y = NMDS2, label = var), color = 'blue',
                  show.legend = F)

# ggsave("./results/ACAD_ordination_plot_HGM_envfit.png", height = 5, width = 6.5)

ggplot(site_scores, aes(x = NMDS1, y = NMDS2)) + theme_wet() +
  geom_path(aes(group = SiteCode, color = SiteType), alpha = 0.8) + #,
  geom_point(aes(color = SiteType)) +
  scale_color_manual(values = c("#82B9D1", "#4d9221")) +
  geom_segment(data = sig_env_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.1, "inches")), color = 'dimgrey', lwd = 0.6) +
  geom_text_repel(data = sig_env_scores, aes(x = NMDS1, y = NMDS2, label = var))

sig_env_scores
head(spp_envfit)









