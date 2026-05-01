#------------------------------------------
# Code for mixed models and plotting of ACAD RAM and SENT data
#------------------------------------------
library(tidyverse)
library(lme4)
library(broom.mixed)
library(patchwork)

vmmi_comb <- read.csv("./data/ACAD_data/Vegetation_MMI_COW_2011-2025_ACAD_RAM_SENT_GRME.csv") |>
  mutate(site_type = ifelse(Panel == 0, "SENT", "RAM"))
head(vmmi_comb)

vmmi_ram <- vmmi_comb |> filter(grepl("R-", Code))
vmmi_grme <- vmmi_comb |> filter(grepl("GR", Code))

GRME <- rbind(vmmi_ram |> filter(Code %in% c("R-04", "R-13", "R-19")) |> filter(Year > 2020),
              vmmi_comb |> filter(grepl("GRME", Code)) |> filter(Year == 2025))|>
  mutate(site = "GRME")


GILM <- rbind(vmmi_ram |> filter(Code == "R-31") |> filter(Year > 2020),
              vmmi_comb |> filter(Code == "GILM") |> filter(Year > 2020),
              vmmi_comb |> filter(grepl("GIME", Code)) |> filter(Year == 2025)) |>
  mutate(site = "GILM")

comb_gm <- rbind(GRME, GILM)
write.csv(comb_gm, "./data/ACAD_data/VMMI_GRME_vs_GILM.csv", row.names = F)

thresh <- c(41.48136, 60.94853)

theme_wet <- function(){
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "#696969", fill = "white",
                                        linewidth = 0.4),
        plot.background = element_blank(), strip.background = element_rect(color = "#696969",
                                                                                                                        fill = "grey90", size = 0.4), legend.key = element_blank(),
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

#---- Trend analysis ----
vmmi_ram$year_cen <- vmmi_ram$Year - min(vmmi_ram$Year)
vmmi_ram$year_fac <- as.factor(vmmi_ram$year_cen)

# VMMI Trends
vmmimod_full <- lmer(vmmi ~ year_cen + HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
vmmimod3 <- lmer(vmmi ~ HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
vmmimod2 <- lmer(vmmi ~ year_cen + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
vmmimod1 <- lmer(vmmi ~ 1 + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)

AIC(vmmimod1, vmmimod3, vmmimod2, vmmimod_full)
plot(vmmimod3)
qqnorm(residuals(vmmimod3))
hist(residuals(vmmimod3))
summary(vmmimod3)
tidy(vmmimod3)

# Mean C trends
meancmod_full <- lmer(meanC ~ year_cen + HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
meancmod3 <- lmer(meanC ~ HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
meancmod2 <- lmer(meanC ~ year_cen + (1 + year_cen|Code), data = vmmi_ram)
meancmod1 <- lmer(meanC ~ 1 + (1 + year_cen|Code), data = vmmi_ram)

AIC(meancmod1, meancmod3, meancmod2, meancmod_full)
plot(meancmod3)
qqnorm(residuals(meancmod3))
hist(residuals(meancmod3))
summary(meancmod3)

# % Bryophyte Trends
bryomod_full <- lmer(Bryophyte_Cover ~ year_cen + HGM_Class + (1|Code), data = vmmi_ram)
bryomod3 <- lmer(Bryophyte_Cover ~ HGM_Class + (1|Code), data = vmmi_ram) # random slopes failed to converge, so rand. int.
bryomod2 <- lmer(Bryophyte_Cover ~ year_cen + (1|Code), data = vmmi_ram)
bryomod1 <- lmer(Bryophyte_Cover ~ 1 + (1|Code), data = vmmi_ram)

# Bryo3 didn't converge. Diagnostics aren't very good. Bryo is kind of a weird metric
anova(bryomod_full, bryomod3, bryomod2, bryomod1)
AIC(bryomod1, bryomod3, bryomod2, bryomod_full)
plot(bryomod3)
qqnorm(residuals(bryomod3)) # a bit funky in the tails
hist(residuals(bryomod3)) # potentially leptokurtic - heavier tails, but not terrible
summary(bryomod3)
tidy(bryomod3)

# % Tolerant Trends
tolmod_full <- lmer(Cover_Tolerant ~ year_cen + HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
tolmod3 <- lmer(Cover_Tolerant ~ HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
tolmod2 <- lmer(Cover_Tolerant ~ year_cen + (1 + year_cen|Code), data = vmmi_ram)
tolmod1 <- lmer(Cover_Tolerant ~ 1 + (1 + year_cen|Code), data = vmmi_ram)

anova(tolmod_full, tolmod3, tolmod2, tolmod1)
AIC(tolmod1, tolmod3, tolmod2, tolmod_full)
plot(tolmod3)
qqnorm(residuals(tolmod3))
hist(residuals(tolmod3))
summary(tolmod3)
tidy(tolmod3)

# Coef of Wetness Trends
wetmod_full <- lmer(mean_wet ~ year_cen + HGM_Class + (1|Code), data = vmmi_ram)
wetmod3 <- lmer(mean_wet ~ HGM_Class + (1|Code), data = vmmi_ram)
wetmod2 <- lmer(mean_wet ~ year_cen + (1|Code), data = vmmi_ram)
wetmod1 <- lmer(mean_wet ~ 1 + (1|Code), data = vmmi_ram)

anova(wetmod_full, wetmod3, wetmod2, wetmod1)
AIC(wetmod1, wetmod3, wetmod2, wetmod_full)
plot(wetmod3)
qqnorm(residuals(wetmod3))
hist(residuals(wetmod3))
summary(wetmod3)
tidy(wetmod3)

table(vmmi_ram$HGM_Class) # depression is the intercept

# Plot results
vmmi_newdat <- vmmi_ram |> select(Code, Year, meanC:vmmi_rating, mean_wet, HGM_Class, year_cen)

vmmi_pred <- cbind(vmmi_newdat,
                   pred_vmmi = predict(vmmimod3, newdata = vmmi_ram),
                   pred_meanC = predict(meancmod3, newdata = vmmi_ram),
                   pred_tol = predict(tolmod3, newdata = vmmi_ram),
                   pred_wet = predict(vmmimod3, newdata = vmmi_ram)
                   )
head(vmmi_pred)

vmmi_est <- data.frame(tidy(vmmimod3) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class", term), gsub("HGM_Class", "", term), "Depression"),
         add = ifelse(HGM_Class == "Depression", 0, .data$estimate[.data$HGM_Class == "Depression"]),
         intercept = ifelse(HGM_Class == "Depression", estimate, estimate + add),
         resp = "vmmi")

meanC_est <- data.frame(tidy(meancmod3) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class", term), gsub("HGM_Class", "", term), "Depression"),
         add = ifelse(HGM_Class == "Depression", 0, .data$estimate[.data$HGM_Class == "Depression"]),
         intercept = ifelse(HGM_Class == "Depression", estimate, estimate + add),
         resp = "meanC")

tol_est <- data.frame(tidy(tolmod3) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class", term), gsub("HGM_Class", "", term), "Depression"),
         add = ifelse(HGM_Class == "Depression", 0, .data$estimate[.data$HGM_Class == "Depression"]),
         intercept = ifelse(HGM_Class == "Depression", estimate, estimate + add),
         resp = "Cover_Tolerant")

bryo_est <- data.frame(tidy(bryomod3) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
  mutate(HGM_Class = ifelse(grepl("HGM_Class", term), gsub("HGM_Class", "", term), "Depression"),
         add = ifelse(HGM_Class == "Depression", 0, .data$estimate[.data$HGM_Class == "Depression"]),
         intercept = ifelse(HGM_Class == "Depression", estimate, estimate + add),
         resp = "Bryophyte_Cover")

wet_est <- data.frame(tidy(wetmod3) |> filter(effect == "fixed")) |> select(term, estimate, std.error) |>
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

  ggplot(vmmi_pred, aes(x = Year, y = .data[[param]])) + #,
                        #fill = HGM_Class, color = HGM_Class,
                        #shape = HGM_Class)) +
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
    # scale_color_manual(values = c("Depression" = "#70D8CF",
    #                               "Flats" = "#994F00",
    #                               "Riverine" = "#053AC3",
    #                               "Slope" = "#FFBF30"),
    #                    name = NULL, aesthetics = c("fill", "color")) +
    # scale_shape_manual(values = c("Depression" = 21,
    #                               "Flats" = 22,
    #                               "Riverine" = 24,
    #                               "Slope" = 25),
    #                    name = NULL) +
    geom_abline(data = ints,
                aes(intercept = intercept, slope = rep(0, 4),
                    group = HGM_Class),
                lwd = 0.75, linetype = 'dashed') +
    labs(y = ylabel, x = NULL) +
    #guides(color = guide_legend(override.aes = list(alpha = 1))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))}


pred_plot("vmmi", "Vegetation MMI", yran = c(0, 100)) +
  facet_wrap(~HGM_Class)

ggsave("./results/Vegetation_MMI_RAM_facet.png", height = 5, width = 6)

pred_plot("meanC", "Mean C", thresh = T) + facet_wrap(~HGM_Class)

pred_plot("mean_wet", "Mean Wetness", thresh = F) + facet_wrap(~HGM_Class)
ggsave("./results/mean_wetness_RAM_facet.png", height = 5, width = 6)


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
# ggsave("./results/Vegetation_MMI_RAM.png", height = 4, width = 6)
pred_plot2("mean_wet", "Mean Wetness")
# ggsave("./results/mean_wetness_RAM.png", height = 4, width = 6)
pred_plot2("meanC", "Mean C", yran = c(2.5, 6.5))
pred_plot2("Invasive_Cover", "% Inv. Cov", yran = c(0, 2))
pred_plot2("Bryophyte_Cover", "% Bryo. Cov")
pred_plot2("Cover_Tolerant", "% Dist. Tol.")

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

ggsave("./results/VMMI_distribution_site_type_ACAD_EPA.png", height = 4, width = 6)

#--- Number of stressors vs VMMI ---
head(vmmi_comb)
buff_all <- read.csv("./results/Stressor_Counts_NWCAPROB_ACAD_GRME_most_recent_REF.csv")
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

ggsave("./results/vegmmi_vs_stressors_EPA_ACAD.png", height = 5, width = 8)

# For every additional stressor, there's a 3 point decrease in mean VMMI
# Flats have the highest intercept and Riverine have the lowest.
