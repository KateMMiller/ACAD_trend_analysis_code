#-------------------------------------------------------------------------
# Compile EPA NWCA data for sites in the NCE reporting unit including ACAD
# Sentinel sites.
#-------------------------------------------------------------------------

# EPA NWCA data were downloaded in 01_download_NWCA_NCE_data.R.
# Currently includes PROB and HAND sites. Will split them after calculating
# the VegMMI for each site

#--- Params ----
library(tidyverse)
export_path <- "./data/EPA_compiled/"

#--- Combining data across years ----
# For the following datasets, I'm row binding data across visits by intersecting the columns each dataset has
# in common to start with. There may be columns in later datasets that need adding, but I'll deal
# with that later.

#---- Site Info ----
site11 <- read.csv("./data/epa_nce/nwca2011_site_info.csv")
site16 <- read.csv("./data/epa_nce/nwca2016_site_info.csv")
site21 <- read.csv("./data/epa_nce/nwca2021_site_info.csv")

comm_site_names <- intersect(intersect(names(site11), names(site16)), names(site21))

site_all <- rbind(site11[,comm_site_names], site16[,comm_site_names], site21[,comm_site_names]) |>
  arrange(UNIQUE_ID, DATE_COL, VISIT_NO)

site_all$Date <- as.POSIXct(site_all$DATE_COL, format = "%m/%d/%Y")
site_all$Year <- format(site_all$Date, format = "%Y")

site_freq <- data.frame(table(site_all$UNIQUE_ID))

table(site_all$RPT_UNIT) #431 NCE
table(site_all$VISIT_NO) #431 1
table(site_all$Year) # 12 in 2022?

head(site_all)

site_nce <- site_all |> filter(US_L3CODE %in% c(49, 50, 51, 56, 58, 59,
                                                60, 61, 62, 82, 83, 84)) # 137
mmi_uids <- sort(unique(site_nce$UID))

write.csv(site_nce, paste0(export_path, "Site_Information_2011-2021.csv"), row.names = F)

#---- Plant data ----
# 1. Compile # of plots sampled per site x visit (should be 5 for most)
# 2. Link C and Native status to species data by GEOD_ID/NWC_CREG code.
    # Do each visit separately?
    # Move plant taxa csvs from epa_all into a taxa specific folder for easier compiling
# 3. Compile Bryophyte cover using the vegetation type data

# Veg plot data
plot11 <- read.csv('./data/epa_nce/nwca2011_vegplotloc.csv')
plot16 <- read.csv('./data/epa_nce/nwca2016_veg_plot_location_data.csv')
plot21 <- read.csv('./data/epa_nce/nwca2021_vegplotloc_wide_data.csv')

# Dates are different formats across years
plot11$Date <- as.Date(plot11$DATE_COL, format = "%d-%b-%y")
plot11$Year <- format(plot11$Date, "%Y")
plot16$Date <- as.Date(plot16$DATE_COL, format = "%m/%d/%Y")
plot16$Year <- format(plot16$Date, "%Y")
plot21$Date <- as.Date(plot21$DATE_COL, format = "%d-%b-%y")
plot21$Year <- format(plot21$Date, "%Y")

comm_p_names <- intersect(intersect(names(plot11), names(plot16)), names(plot21))

plot_all <- rbind(plot11[,comm_p_names], plot16[,comm_p_names], plot21[,comm_p_names]) |>
  arrange(SITE_ID, Year, VISIT_NO) |>
  filter(UID %in% mmi_uids)

plot_sum <- plot_all |> group_by(UID, SITE_ID, VISIT_NO, Date, Year) |>
  summarize(num_plots = sum(!is.na(PLOT), na.rm = T),
            miss_plots = sum(is.na(PLOT)),
            .groups = "drop")

table(plot_sum$Year) # 2011: 45; 2016: 128; 2021: 113; 2022: 11
table(complete.cases(plot_all$PLOT)) #189 FALSE; 483 TRUE; Lots of PLOT blanks in 2021.
# Not clear what the blanks in 2021 are about yet - were they not sampled?

write.csv(plot_all, paste0(export_path, "Vegetation_Plot_Location_2011-2021.csv"), row.names = F)

# Bryophyte cover
bryo11 <- read.csv("./data/epa_nce/nwca2011_vegtype_grndsurf.csv") |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES)
bryo11$YEAR <- 2011

bryo16 <- read.csv('./data/epa_nce/nwca2016_vegetation_type_data.csv') |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES, DATE_COL)
bryo16$YEAR <- format(as.Date(bryo16$DATE_COL, format = "%m/%d/%Y"), "%Y")
table(bryo16$YEAR, useNA = 'always')

bryo21 <- read.csv('./data/epa_nce/nwca2021_vegtype_wide_data.csv') |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES, DATE_COL)
head(bryo21)
bryo21$YEAR <- format(as.Date(bryo21$DATE_COL, "%d-%b-%y"), "%Y")
table(bryo21$YEAR, useNA = 'always')

bryo_all1 <- rbind(bryo11, bryo16 |> select(-DATE_COL), bryo21 |> select(-DATE_COL)) |>
  filter(UID %in% mmi_uids)

bryo_all <- left_join(bryo_all1, site_nce, by = c("UID", "SITE_ID", "VISIT_NO"))

write.csv(bryo_all, paste0(export_path, "Bryophyte_Cover_2011-2021.csv"), row.names = F)

# Plant Cover Data
# 2011 plant data
cov11a <- read.csv("./data/epa_nce/nwca2011_plant_pres_cvr.csv") |>
  select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, LINE, PLOT,
         NWC_CREG16, SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW)
site11a <- read.csv("./data/epa_nce/nwca2011_site_info.csv") |>
  select(UID, SITE_ID, UNIQUE_ID, SITETYPE, LAT_DD83, LON_DD83, COE_REG_ID, NWC_CREG11, NWC_CREG16, US_L3CODE)
cov11b <- left_join(cov11a, site11a, by = c("UID", "SITE_ID", "UNIQUE_ID", "NWC_CREG16"))
head(cov11b)

# Bring in C, native, wetland indicator status
taxa11 <- read.csv("./data/EPA_all/nwca2011_planttaxa.csv", fileEncoding = "latin1")
cnat11 <- read.csv('./data/EPA_all/nwca2011_planttaxa_cc_natstat.csv', fileEncoding = "latin1")
wis11 <- read.csv('./data/EPA_all/nwca2011_planttaxa_wis.csv', fileEncoding = 'latin1')

cov11t <- left_join(cov11b, taxa11 |> select(SPECIES_NAME_ID, USDA_NAME, ORDER, FAMILY, GENUS,
                                             GROWTH_HABIT, DURATION),
                    by = c("SPECIES_NAME_ID"))

cov11cnat <- left_join(cov11t, cnat11 |> select(-PUBLICATION_DATE),
                       by = c("SPECIES_NAME_ID", "USDA_NAME", "NWC_CREG11" = "GEOG_ID"))
cov11wis <- left_join(cov11cnat, wis11 |> select(-PUBLICATION_DATE),
                      by = c("SPECIES_NAME_ID", "USDA_NAME", "COE_REG_ID" = "GEOG_ID")) |>
  rename(NWCA_NAME = USDA_NAME) |>
  mutate(SYMBOL = NA_character_)|>
  filter(UID %in% mmi_uids)

head(cov11wis) # future years use different name.

# cov11wis ready to join with remaining years.

# 2016 plant data
cov16a <- read.csv('./data/epa_nce/nwca2016_plant_species_cover_height_data.csv') |>
  select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, LINE, PLOT,
         NWC_CREG16, SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW)

site16a <- read.csv("./data/epa_nce/nwca2016_site_info.csv") |>
  select(UID, SITE_ID, UNIQUE_ID, SITETYPE, LAT_DD83, LON_DD83, COE_REG_ID, NWC_CREG11, NWC_CREG16, US_L3CODE)
cov16b <- left_join(cov16a, site16a, by = c("UID", "SITE_ID", "UNIQUE_ID", "NWC_CREG16"))
head(cov16b)

# Bring in C, native, wetland indicator status
taxa16 <- read.csv("./data/EPA_taxa_lists/nwca_2016_plant_taxa.csv")
names(taxa16)[1] <- "SPECIES_NAME_ID" # fix weird encoding that added symbols before name
c16 <- read.csv("./data/EPA_taxa_lists/nwca_2016_plant_cvalues.csv")
names(c16)[1] <- "SPECIES_NAME_ID" # fix weird encoding that added symbols before name
nat16 <- read.csv('./data/EPA_taxa_lists/nwca_2016_plant_native_status.csv')
names(nat16)[1] <- "SPECIES_NAME_ID" # fix weird encoding that added symbols before name
wis16 <- read.csv('./data/EPA_taxa_lists/nwca_2016_plant_wis.csv')
names(wis16)[1] <- "SPECIES_NAME_ID" # fix weird encoding that added symbols before name

cov16t <- left_join(cov16b, taxa16 |> select(SPECIES_NAME_ID, NWCA_NAME, ORDER, FAMILY, GENUS,
                                             GROWTH_HABIT, DURATION, SYMBOL = ACCEPTED_SYMBOL),
                    by = c("SPECIES_NAME_ID"))
cov16nat <- left_join(cov16t, nat16 |> select(-PUBLICATION_DATE),
                    by = c("SPECIES_NAME_ID", "NWCA_NAME", "NWC_CREG11" = "GEOG_ID"))
cov16c <- left_join(cov16nat, c16 |> select(-PUBLICATION_DATE, -GEOG_TYPE),
                    by = c("SPECIES_NAME_ID", "NWCA_NAME", "NWC_CREG16" = "GEOG_ID"))
cov16wis <- left_join(cov16c, wis16 |> select(-PUBLICATION_DATE, -GEOG_TYPE),
                      by = c("SPECIES_NAME_ID", "NWCA_NAME", "COE_REG_ID" = "GEOG_ID"))|>
  filter(UID %in% mmi_uids)

head(cov16wis) # 2016 data ready to join with other years (some columns )

# 2021 plant data
cov21a <- read.csv('./data/epa_nce/nwca2021_plant_wide_data.csv')
cov21a$YEAR <- format(as.Date(cov21a$DATE_COL, format = "%d-%b-%y"), "%Y")

site21a <- read.csv("./data/epa_nce/nwca2021_site_info.csv") |>
  select(UID, SITE_ID, UNIQUE_ID, SITETYPE, LAT_DD83, LON_DD83, COE_REG_ID, PSTL_CODE, COE_REG_ID,
         NWC_CREG16, US_L3CODE)

cov21b <- left_join(cov21a, site21a, by = c("UID", "SITE_ID", "UNIQUE_ID", "PSTL_CODE"))

# Bring in C, native, wetland indicator status
taxa21 <- read.csv("./data/EPA_taxa_lists/nwca21_planttaxa-data.csv")
c21 <- read.csv("./data/EPA_taxa_lists/nwca21_plantcval-data.csv")
nat21 <- read.csv('./data/EPA_taxa_lists/nwca21_plantnative-data.csv')
wis21 <- read.csv('./data/EPA_taxa_lists/nwca21_plantwis-data.csv')

cov21t <- left_join(cov21b, taxa21 |> select(SPECIES_NAME_ID, NWCA_NAME, ORDER, FAMILY, GENUS,
                                             GROWTH_HABIT, DURATION, SYMBOL = ACCEPTED_SYMBOL),
                    by = c("SPECIES_NAME_ID"))
cov21nat <- left_join(cov21t, nat21 |> select(-PUBLICATION_DATE),
                      by = c("SPECIES_NAME_ID", "NWCA_NAME", "PSTL_CODE" = "GEOG_ID"))
cov21c <- left_join(cov21nat, c21 |> select(-PUBLICATION_DATE, -GEOG_TYPE),
                    by = c("SPECIES_NAME_ID", "NWCA_NAME", "NWC_CREG16" = "GEOG_ID"))
cov21wis <- left_join(cov21c, wis21 |> select(-PUBLICATION_DATE, -GEOG_TYPE),
                      by = c("SPECIES_NAME_ID", "NWCA_NAME", "COE_REG_ID" = "GEOG_ID"))|>
  filter(UID %in% mmi_uids)

head(cov21wis) # 2021 data ready to join with other years (some columns )

# column name updates before combining
cov11 <- cov11wis |> select(UID, UNIQUE_ID, SITETYPE, SITE_ID, LAT_DD83, LON_DD83, YEAR, VISIT_NO, PLOT,
                            US_L3CODE,
                            STATE = NWC_CREG11, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            SPECIES_NAME_ID, SYMBOL,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CC, NATSTAT = NWCA_NATSTAT, WIS, ECOIND, ALIEN)

cov16 <- cov16wis |> select(UID, UNIQUE_ID, SITETYPE, SITE_ID, LAT_DD83, LON_DD83, YEAR, VISIT_NO, PLOT,
                            US_L3CODE,
                            SPECIES_NAME_ID, SYMBOL,
                            STATE = NWC_CREG11, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CVAL, NATSTAT = NWCA_NATSTAT, WIS, ECOIND = ECOIND1, ALIEN)

cov21 <- cov21wis |> select(UID, UNIQUE_ID, SITETYPE, SITE_ID, LAT_DD83, LON_DD83, YEAR, VISIT_NO, PLOT,
                            US_L3CODE,
                            STATE = PSTL_CODE, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            SPECIES_NAME_ID, SYMBOL,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CVAL, NATSTAT = NWCA_NATSTAT, WIS, ECOIND = ECOIND1, ALIEN)

covcomb1 <- rbind(cov11, cov16, cov21)
covcomb1$NWCA_NAME <- gsub("×", "x", covcomb1$NWCA_NAME)

covcomb <- covcomb1 |> filter(UID %in% mmi_uids) |>
  mutate(COC_reg = case_when(STATE == "IN" ~ "IN",
                             STATE == "MI" ~ "MI",
                             STATE == "MN" ~ "MN",
                             STATE == "WI" ~ "WI",
                             TRUE ~ as.character(US_L3CODE)))

head(covcomb)

table(covcomb$STATE, covcomb$YEAR) # NCE states only
table(covcomb$CREG, covcomb$YEAR)
table(covcomb$WISREG, covcomb$YEAR)
table(covcomb$US_L3CODE, covcomb$YEAR)

#---- Compile CoC Values ----
# Ecoregions to include
ecoreg = c(49, 50, 51, 56, 58, 59, 60, 61, 62, 82, 83, 84)

# Combine the CoC lists
c_mn <- read.csv("./data/EPA_taxa_lists/COCs/MN_FQA_Milburn_2007.csv") |> mutate(COC_reg = "MN")
c_wi <- read.csv("./data/EPA_taxa_lists/COCs/WI_FQA_N_SC_Chung_2017.csv") |> mutate(COC_reg = "WI") # NCE only includes L3: 50 and 51 (N and SC)
c_mi <- read.csv("./data/EPA_taxa_lists/COCs/MI_FQA_Merjent_2024.csv") |> mutate(COC_reg = "MI")
c_in <- read.csv("./data/EPA_taxa_lists/COCs/IN_FQA_Roth_2019.csv") |> mutate(COC_reg = "IN")
c_ne_ma <- read.csv("./data/EPA_taxa_lists/COCs/NE_MidAtlantic_Ecoreg_FQA_Faber_2021.csv", fileEncoding = "Latin1") |>
  select(SYMBOL = USDA.Accepted.Taxa..Symbol,
         Accepted.Scientific.Name,
         C_82 = Acadian.Plains...Hlils..82..CoC,
         C_58 = Northeastern.Highlands..58..CoC,
         C_59 = Northeast.Coastal..59..CoC,
         C_83 = Eastern.Great.Lakes..83..CoC,
         C_84 = Atlantic.Coastal.Pine.Barrens..84..CoC,
         C_60_61 = Allegheny.Plateau..Glaciated..60.61..CoC,
         C_62_69_70 = Allegheny.Plateau..UNglaciated..62.69.70..CoC,
         C_66_67_68 = Ridge...Valley..66.67.68..CoC,
         C_45_64 = Piedmont..45..64..CoC,
         C_63_65 = Mid.Atlantic.Coastal.Plain..63.65..CoC) |>
  pivot_longer(C_82:C_63_65, names_to = "COC_reg", values_to = "CVal") |>
  filter(!is.na(CVal)) |>
  mutate(SCIENTIFIC_NAME = toupper(Accepted.Scientific.Name)) |>
  select(SYMBOL, SCIENTIFIC_NAME, CVal, COC_reg)

head(c_ne_ma)

mw <- rbind(c_mn, c_wi, c_mi, c_in) |>
  mutate(SCIENTIFIC_NAME = toupper(Scientific.Name),
         SYMBOL = NA_character_) |>
  select(SYMBOL, SCIENTIFIC_NAME, CVal = C, COC_reg)
head(mw)

coc_list <- rbind(c_ne_ma, mw) |>
  arrange(SCIENTIFIC_NAME, COC_reg) |>
  group_by(SCIENTIFIC_NAME) |> fill(SYMBOL, .direction = "updown")

table(complete.cases(coc_list$SYMBOL)) # 3154 missing SYMBOLS, but going to wait to fix, to see which ones
# show up in the epa data.

# get species list from cover data
spplist1 <- covcomb |> select(SPECIES_NAME_ID, SYMBOL, NWCA_NAME, US_L3CODE, CREG, STATE) |>
  arrange(NWCA_NAME, SYMBOL) |>
  distinct() |>
  fill(SYMBOL, .direction = "downup", .by = c(SPECIES_NAME_ID, NWCA_NAME)) |>
  mutate(COC_reg = paste0(STATE, "_", US_L3CODE),
         present = 1) |>
  arrange(COC_reg, NWCA_NAME)

cregs <- spplist1 |> select(CREG, COC_reg, US_L3CODE) |> unique()
head(cregs)
table(complete.cases(spplist1$SYMBOL)) #359 missing symbols b/c 2011 data didn't include

spplist2 <- left_join(spplist1, taxa21 |> rename(SYMBOL21 = ACCEPTED_SYMBOL),
                      by = c("SPECIES_NAME_ID", "NWCA_NAME")) |>
  select(-SYMBOL) |>
  select(SPECIES_NAME_ID, NWCA_NAME, SYMBOL = SYMBOL21, everything()) |>
  filter(!is.na(SPECIES_NAME_ID))   # blank row came in from 2021 data

table(complete.cases(spplist2$SYMBOL)) # only 30; 4 unique symbols missing in 2021 data.

# Fixing the missing symbols manually (from 2011 and 2016 data)
spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91513] <- "ARONIA"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91513] <- "ARONI2"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91514] <- "ARONIA xPRUNIFOLIA"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91514] <- "ARPR2"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91516] <- "ARONIA MELANOCARPA"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91516] <- "ARME6"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91538] <- "BOLBOSCHOENUS FLUVIATILIS"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91538] <- "BOFL3"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91577] <- "CAREX BILLINGSII"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91577] <- "CABI22"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91707] <- "ERECHTITES HIERACIIFOLIUS"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91707] <- "ERHI12"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 131571] <- "OENOTHERA GAURA"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 131571] <- "OEGA"

table(complete.cases(spplist2$SYMBOL)) # Only FALSE are SHRUB, which I'll delete.

spplist3 <- spplist2 |> filter(!is.na(NWCA_NAME))
table(complete.cases(spplist3$SYMBOL)) # all TRUE

spplist4 <- spplist3 |>
  mutate(COC_reg2 = case_when(STATE == "IN" ~ "IN",
                              STATE == "MI" ~ "MI",
                              STATE == "MN" ~ "MN",
                              STATE == "WI" ~ "WI",
                              TRUE ~ substr(COC_reg, 4, 5))) |>
  select(SPECIES_NAME_ID, NWCA_NAME, SYMBOL, COC_reg = COC_reg2, CREG) |>
  distinct() |> arrange(NWCA_NAME, COC_reg)

head(spplist4)

coc_list2 <- coc_list |>
  mutate(COC_reg2 = sub("C_", "", COC_reg)) |>
  separate_wider_delim(cols = COC_reg2, delim = "_",
                       names = c("X1", "X2", "X3"),
                       too_few = "align_start") |>
  select(-COC_reg) |>
  pivot_longer(X1:X3, names_to = "junk", values_to = "COC_reg") |>
  filter(!is.na(COC_reg)) |> select(-junk) |>
  distinct()

coc_list_miss_sym <- coc_list2 |> filter(is.na(SYMBOL)) |> select(SYMBOL, CVal, COC_reg) |>
  distinct()

sppcoc <- left_join(spplist4, coc_list2, by = c("SYMBOL", "COC_reg"))
sppcoc_miss <- sppcoc |> filter(is.na(SCIENTIFIC_NAME))
write.csv(sppcoc_miss, "./data/EPA_taxa_lists/COCs/Species_missing_COCs.csv", row.names = F)

# Where I don't have CVal from newer sources, use values EPA assigned from 2021
# The values closely match COCs for MN, WI, MI, IN Coefs., but that are hard
# to match because the state FQA sources have weird nomenclature.

taxa21_cval <- read.csv("./data/EPA_taxa_lists/nwca21_plantcval-data.csv") |>
  select(SPECIES_NAME_ID, GEOG_ID, NWCA_CVAL)

sppcoc2 <- left_join(sppcoc, taxa21_cval, by = c("SPECIES_NAME_ID",
                                                 "CREG" = "GEOG_ID")) |>
  mutate(CVAL_final = ifelse(is.na(CVal), NWCA_CVAL, CVal),
         COC_diff = as.numeric(CVal) - as.numeric(NWCA_CVAL)) # warnings drop UND, which is OK

head(sppcoc2)
table(sppcoc2$CVal, sppcoc2$NWCA_CVAL, useNA = 'always')

write.csv(sppcoc2, "./data/EPA_taxa_lists/COCs/Full_COC_List_Pre-Final.csv", row.names = F)
# Had to manually clean up duplicates and synonyms by hand, so path toe Full_COC_List_Final.csv
# is unfortunately not documented in code.

# Using USDA PLANTS database to establish invasive status.
#      https://plants.usda.gov/noxious-invasive-search
# Doesn't appear that that state-level designations are all that helpful. Going to go with,
# if it's invasive in any state in the reporting region, it's invasive in the analysis. I
# can't think of an example where that isn't true.
#
# Invasive list needs the USDA symbol to be joined to the species in the cover data. However,
# the 2011 taxa tables don't include the symbol. I'm using the taxa 2016 and 2021 tables to
# fill in as much of those holes, then will manually add the missing species.

inv_spp1 <- read.csv("./data/EPA_taxa_lists/USDA_PLANTS_Invasive_Species_By_State_20251217_clean.csv") |>
  select(SYMBOL = Accepted.Symbol)

# Pruning by hand - species to drop from invasive list
drop_spp <- c("BIDEN", "CALLI6", "CASE13", "CRATA", "EPILO", "GEUM", "IMPAT", "2UNK",
              "MALUS", "MEAR4", "OXALI", "POPR", "RUHI", "RUID", "THDA", "URDI", "VIOP", "VIRE7")

inv_spp <- inv_spp1 |> filter(!SYMBOL %in% drop_spp) |> unique()

# Need to make every species have a symbol to connect C values. Using the taxa21 table for the join
covcomb2 <- right_join(taxa21 |> select(SPECIES_NAME_ID, SYMBOL = ACCEPTED_SYMBOL), # |> unique(),
                       covcomb |> select(-SYMBOL),
                       by = c("SPECIES_NAME_ID")) |>
  filter(!is.na(SPECIES_NAME_ID)) |> # drops 2 blanks fro 2021
  arrange(SITE_ID, YEAR, NWCA_NAME) |>
  mutate(INVASIVE = ifelse(SYMBOL %in% inv_spp$SYMBOL, 1, 0))

# Manually dropped duplicates b/c required reviewing each record to determine fix.
coc <- read.csv("./data/EPA_taxa_lists/COCs/FULL_COC_List_Final.csv")
covcomb_final <- left_join(covcomb2, coc,
                           by = c("SPECIES_NAME_ID", "NWCA_NAME", "SYMBOL", "CREG", "COC_reg")) |>
  select(-SCIENTIFIC_NAME)

head(covcomb_final)
table(complete.cases(covcomb_final$SYMBOL)) # all TRUE
write.csv(covcomb_final, "./data/EPA_compiled/Plant_Cover_2011-2021.csv", row.names = F)

#---- Compiling VMMI from EPA data ----
# Compile plot list to left_join results with
plot_list1 <- covcomb_final |> select(UID, UNIQUE_ID, SITETYPE, SITE_ID, LAT_DD83, LON_DD83,
                                      YEAR, STATE, COC_reg, CREG, WISREG, US_L3CODE) |>
  unique() |> arrange(UNIQUE_ID)

# It appears that the plot tables don't always include data on number of plots, so I can't
# use that to sum up the number of plots per site. The cover data is pretty consistent, with only
# one site not having 5 veg plots. Going to just use that to sum up the number of veg plots for
# site level averaging.

# count plots from cover data
num_plots1 <- covcomb_final |> select(UID, UNIQUE_ID, SITETYPE, SITE_ID, YEAR, PLOT) |> unique() |>
  group_by(UID, UNIQUE_ID, SITE_ID, YEAR) |>
  summarize(num_vplots = sum(!is.na(PLOT)), .groups = 'drop')

plot_list <- left_join(plot_list1, num_plots1, by = c("UID", "UNIQUE_ID",  "SITE_ID", "YEAR"))
table(plot_list$num_vplots, useNA = 'always')

# % Bryophyte
bryo_sum <- bryo_all |> group_by(UID, SITE_ID, VISIT_NO, YEAR) |>
  summarize(bryo_sum = sum(BRYOPHYTES, na.rm = T), .groups = 'drop')

plot_bryo <- left_join(plot_list, bryo_sum, by = c("UID", "SITE_ID", "YEAR")) |>
  mutate(bryo_cov = bryo_sum/num_vplots) |> select(-bryo_sum)

# Mean C
head(covcomb_final)
table(covcomb_final$CVAL_final, useNA = 'always')

# Calculating meanC by making a list of all species on the plot, then calc. C,
# so species found in all 5 veg plots don't count more in the mean C than
# rare species that only occur once
cval <- covcomb_final |> mutate(CVAL_num = as.numeric(CVAL_final)) |>
  #filter(!is.na(CVAL_num)) |>
  filter(COVER > 0) |>
  select(UID, UNIQUE_ID, SITE_ID, YEAR, CVAL_num, SPECIES_NAME_ID, NWCA_NAME) |>
  unique() |>
  group_by(UID, UNIQUE_ID, SITE_ID, YEAR) |>
  summarize(
    #totalC = sum(CVAL_num, na.rm = T),
    # numSpp = sum(!is.na(CVAL_num)),
    # meanC2 = totalC/numSpp,
    meanC = mean(CVAL_num, na.rm = T),
    # Cdiff = meanC2-meanC,
    .groups = 'drop')

#ignore NAs warning. Used to intentionally drop non-numeric values
head(cval)
head(plot_bryo)

plot_br_cv <- left_join(plot_bryo, cval, by = c("UID", "UNIQUE_ID", "SITE_ID", "YEAR"))
head(plot_br_cv)

# % Cover Disturbance Tolerant & Invasive
dist_inv_sum1 <- covcomb_final |> mutate(CVAL_num = as.numeric(CVAL_final)) |>
  #filter(!is.na(CVAL_num)) |>
  mutate(cov_disttol = ifelse(CVAL_num <= 4, COVER, 0),
         cov_inv = ifelse(INVASIVE == 1, COVER, 0))

dist_inv_sum <- dist_inv_sum1 |>
  group_by(UID, UNIQUE_ID, SITE_ID, YEAR) |>
  summarize(disttol_sum = sum(cov_disttol, na.rm = T),
            invcov_sum = sum(cov_inv, na.rm = T),
            .groups = 'drop')

dist_inv_plot <- left_join(plot_list, dist_inv_sum,
                           by = c("UID", "UNIQUE_ID", "SITE_ID", "YEAR")) |>
  mutate(disttol_cov = disttol_sum/num_vplots,
         inv_cov = invcov_sum/num_vplots)

#--- Reevaluate VMMI thresholds ---
# Because the CoCs changed so much between 2011, the data the VMMI thresholds are based on,
# this section of code reassesses the thresholds using the same screening process as in 2011
# to determine reference sites. Note that ACAD hand-picked sites have been sampled 3 times,
# but only the sites that passed the screening in the earliest survey were included in the
# threshold calculations (5/10), so as not to overly influence threshold predictions.

# Compile stressor data
# 2011 Buffer Stressor data, which also includes stressors in the AA
buff11 <- read.csv("./data/epa_nce/nwca2011_bufferchar.csv") |>
  select(UID, PLOT, LOCATION, DATE_COL, SITE_ID, VISIT_NO,
         AGR_ANIMAL, AGR_DAIRY, AGR_FALLOW_OLD, AGR_FALLOW_RECENT, AGR_GRAVEL,
         AGR_IRRIGATION, AGR_NURSERY, AGR_ORCHARD, AGR_PASTURE, AGR_RANGE, AGR_ROW,
         AGR_RURAL, BIRDSFOOT_TREFOIL, CANADA_THISTLE, CHEATGRASS, COMMON_BUCKTHORN,
         COMMON_REED, GARLIC_MUSTARD, GIANT_REED, HAB_CLEAR_CUT, HAB_EROSION, HAB_FOREST_BURNED,
         HAB_GRASS_BURNED, HAB_GRAZED, HAB_HERBICIDE, HAB_HERBIVORY, HAB_MOWING, HAB_ORV,
         HAB_PLANTATION, HAB_SELECTIVE_CUT, HAB_SHRUB, HAB_SOIL, HAB_TRAILS, HIMALAYAN_BLACKBERRY,
         HYD_DDRR, HYD_DITCH, HYD_EXCAVATION, HYD_FILL, HYD_IMPERVIOUS, HYD_INLETS, HYD_PIPE,
         HYD_SEDIMENT, HYD_SOIL, HYD_WALL, HYD_WATER, IND_MILITARY, IND_OIL, JAPAN_KNOTWEED,
         JOHNSON_GRASS, KNOTWEED, KUDZU, MILE_A_MINUTE, MULTIFLORA_ROSE,
         PERENNIAL_PEPPERWEED, POISON_HEMLOCK, PURPLE_LOOSESTRIFE, REED_CANARY_GRASS, RES_DUMPING,
         RES_FOUR, RES_GOLF, RES_GRAVEL, RES_LANDFILL, RES_LAWN, RES_LOT, RES_RES, RES_TRASH, RES_TWO,
         RES_URBAN, TAMARISK, WATER_HYACINTH, WATERMILFOIL) |>
  filter(VISIT_NO == 1)

head(buff11)
table(buff11$PLOT, buff11$LOCATION)
buff11_long <- buff11 |> pivot_longer(cols = c(AGR_ANIMAL:WATERMILFOIL),
                                      names_to = "Stressor", values_to = "Present") |>
  filter(!(LOCATION %in% c("N", "E", "S", "W") & PLOT == 0)) |> # these rows are not part of stressor checklist
  mutate(weight = case_when(LOCATION == "AA" ~ 1,
                            PLOT == 1 ~ 1,
                            PLOT == 2 ~ 0.44,
                            PLOT == 3 ~ 0.23
                            ),
         stress = ifelse(Present == "Y", 1, 0))   # weight buffer plot by distance to the center
buff11_long$YEAR <- 2011

buff11_sum1 <- buff11_long |> group_by(UID, SITE_ID, PLOT, LOCATION, YEAR, weight) |>
  summarize(num_stressors = sum(stress),
            stress_wgt = num_stressors * first(weight),
            .groups = "drop")

buff11_sum <- buff11_sum1 |> group_by(UID, SITE_ID, YEAR) |>
  summarize(stress_score = sum(stress_wgt),
            dist = case_when(stress_score == 0 ~ "MIN",
                             stress_score > 0 & stress_score < 2 ~ "INT",
                             stress_score >= 2 ~ "MOST"),
            .groups = 'drop')

# buff11_sum ready to bind with future data

# 2016 Buffer Stressor data
buff16 <- read.csv("./data/epa_nce/nwca2016_buffer_characterization_stressors_data.csv") |>
  select(UID, PLOT, LOCATION, DATE_COL, SITE_ID, VISIT_NO,
         AGR_ANIMAL, AGR_DAIRY, AGR_FALLOW_OLD, AGR_FALLOW_RECENT, #AGR_GRAVEL,
         AGR_IRRIGATION, #AGR_NURSERY,
         AGR_ORCHARD, AGR_OTHER, AGR_PASTURE, AGR_RANGE, AGR_ROW,
         AGR_RURAL, HAB_CLEAR_CUT, HAB_EROSION, HAB_FOREST_BURNED,
         HAB_GRASS_BURNED, HAB_GRAZED, HAB_HERBICIDE_PESTICIDE, HAB_HERBIVORY, HAB_MOWING, HAB_ORV,
         HAB_PLANTATION, HAB_SALT, HAB_SELECTIVE_CUT, HAB_SHRUB, HAB_SOIL, HAB_TRAILS,
         HYD_DDRR, HYD_DITCH, HYD_EXCAVATION, HYD_FILL, HYD_IMPERVIOUS, HYD_INLETS, HYD_OTHER,
         HYD_PIPE, HYD_SEDIMENT, HYD_SOIL, HYD_WALL, HYD_WATER,
         IND_MINING, IND_OIL_GAS, IND_OTHER,
         RES_DUMPING, #RES_FOUR,
         RES_GOLF, #RES_GRAVEL, RES_LANDFILL,
         RES_LAWN, RES_LOT, RES_OTHER, RES_POWER,
         RES_RES, RES_ROAD, RES_TRASH, #RES_TWO,
         RES_URBAN) |>
  filter(VISIT_NO == 1)

head(buff16)

buff16_long <- buff16 |> pivot_longer(cols = c(AGR_ANIMAL:RES_URBAN),
                                      names_to = "Stressor", values_to = "Present") |>
  filter(!(LOCATION %in% c("N", "E", "S", "W") & PLOT == 0)) |> # these rows are not part of stressor checklist
  mutate(weight = case_when(LOCATION == "AA" ~ 1,
                            PLOT == 1 ~ 1,
                            PLOT == 2 ~ 0.44,
                            PLOT == 3 ~ 0.23
  ),
  stress = ifelse(Present == "Y", 1, 0))   # weight buffer plot by distance to the center

buff16_long$YEAR <- 2016

buff16_sum1 <- buff16_long |> group_by(UID, SITE_ID, PLOT, LOCATION, YEAR, weight) |>
  summarize(num_stressors = sum(stress),
            stress_wgt = num_stressors * first(weight),
            .groups = "drop")

buff16_sum <- buff16_sum1 |> group_by(UID, SITE_ID, YEAR) |>
  summarize(stress_score = sum(stress_wgt),
            dist = case_when(stress_score == 0 ~ "MIN",
                             stress_score > 0 & stress_score < 2 ~ "INT",
                             stress_score >= 2 ~ "MOST"),
            .groups = 'drop')

# buff16_sum ready to bind with future data


# 2021 Buffer Stressor data
buff21 <- read.csv("./data/epa_nce/nwca2021_physalt_wide_data.csv") |>
  select(UID, PLOT, LOCATION, DATE_COL, SITE_ID, VISIT_NO, SOILHD_IMPERVIOUS_SURFACE,
         SOILHD_NONPAVED_TRAILS, SOILHD_PAVED_ROADS, SOILHD_RR_POLE_PILING,
         SOILHD_SOIL_COMPACTION, SOILHD_UNPAVED_ROADS, SOILHD_VEHICLE_RUTS, SOILHD_WELLS_PIPELINE,
         SOILMD_EXCAVATION_DREDGING, SOILMD_LANDFILL, SOILMD_MINE, SOILMD_PILE,
         SOILMD_SOIL_DEPOSITION, SOILMD_SOIL_EROSION, SOILMD_TILLING_PLOWING, SOILMD_TRASH,
         VEGREM_BROWSING, VEGREM_BROWSING_TYPE, VEGREM_CLEAR_CUT, VEGREM_FIRE, VEGREM_GRAZING,
         VEGREM_GRAZING_TYPE, VEGREM_HERBICIDE_PESTICIDE, VEGREM_MOWING_CLEARING,
         VEGREM_PEST_DAMAGE, VEGREM_SELECTIVE_CUT, VEGREP_ABANDONED_FIELD, VEGREP_CROPS,
         VEGREP_FALLOW_RESTING, VEGREP_INTRO_PLANTS, VEGREP_LAWN,
         VEGREP_PASTURE, VEGREP_RANGE, VEGREP_SILVICULTURE, WADSUB_CHANNELS_RUTS, WADSUB_CULVERT,
         WADSUB_DITCH, WADSUB_DRAIN_TILING, WADSUB_IMPERVIOUS_INPUT, WADSUB_IRRIGATION,
         WADSUB_PIPE, WADSUB_WATER_WITHDRAWAL, WOBSTR_CONTROL_STRUCTURE, WOBSTR_DAM, WOBSTR_DIKE_BERM_LEVEE,
         WOBSTR_PILE, WOBSTR_POND, WOBSTR_ROAD_RAISED_BED, WOBSTR_SILVICULTURE_BEDDING,
         WOBSTR_WALL_RIPRAP) |> # Dropped NONE_PRESENT columns from import
  filter(VISIT_NO == 1)

head(buff21)

buff21_long <- buff21 |> pivot_longer(cols = c(SOILHD_IMPERVIOUS_SURFACE:WOBSTR_WALL_RIPRAP),
                                      names_to = "Stressor", values_to = "Present") |>
  filter(!(LOCATION %in% c("N", "E", "S", "W") & PLOT == 0)) |> # these rows are not part of stressor checklist
  mutate(weight = case_when(LOCATION == "AA" ~ 1,
                            PLOT == 1 ~ 1,
                            PLOT == 2 ~ 0.44,
                            PLOT == 3 ~ 0.23
  ),
  stress = ifelse(Present %in% c("L", "M", "H", "Y"), 1, 0))   # weight buffer plot by distance to the center

buff21_long$YEAR <- format(as.Date(buff21_long$DATE_COL, format = "%d-%b-%y"), format = "%Y")
table(buff21_long$YEAR[is.na(buff21_long$YEAR)],
      buff21_long$DATE_COL[is.na(buff21_long$YEAR)], useNA = "always") # all the missing years are from 2021 b/c diff format
buff21_long$YEAR[is.na(buff21_long$YEAR)] <- 2021

buff21_sum1 <- buff21_long |> group_by(UID, SITE_ID, PLOT, LOCATION, YEAR, weight) |>
  summarize(num_stressors = sum(stress),
            stress_wgt = num_stressors * first(weight),
            .groups = "drop")

buff21_sum <- buff21_sum1 |> group_by(UID, SITE_ID, YEAR) |>
  summarize(stress_score = sum(stress_wgt),
            dist = case_when(stress_score == 0 ~ "MIN",
                             stress_score > 0 & stress_score < 2 ~ "INT",
                             stress_score >= 2 ~ "MOST"),
            .groups = 'drop')

# buff16_sum ready to bind with future data
head(buff21_sum)

# Combine buffer data across years
buff_comb1 <- rbind(buff11_sum, buff16_sum, buff21_sum)
head(buff_comb1)

buff_comb <- left_join(site_nce, buff_comb1, by = c("UID", "SITE_ID", "Year" = "YEAR"))

head(buff_comb)
table(buff_comb$SITETYPE, buff_comb$dist, buff_comb$Year)
# 2011 - MIN: 32 HAND, 26 PROB
# 2016 - MIN: 32 HAND, 32 PROB
# 2021 - MIN: 13 HAND, 16 PROB

# Take the earliest sampling of each reference site for the threshold calculation.
# Using the earliest sampling because we have the most data earlier than later,
# and the metrics were determined by the early data.
buff_ref <- buff_comb |> filter(SITETYPE == "HAND") |> filter(dist == "MIN") |>
  filter(!WETCLS_HGM %in% "TIDAL") |> # only want freshwater
  arrange(Year, UNIQUE_ID) |>
  group_by(UNIQUE_ID) |> slice(1) |> data.frame() |>
  select(UID, UNIQUE_ID, SITETYPE, SITE_ID, LAT_DD83, LON_DD83, PSTL_CODE, RPT_UNIT,
         US_L3CODE, WETCLS_EVL, WETCLS_GRP, WETCLS_HGM, Year, stress_score, dist) |>
  filter(!UID %in% c(207585, 206737, 207752)) #drop ACAD sentinel sites added in 2016

head(buff_ref)
nrow(buff_ref) # 62 observations
ref_uids <- sort(unique(buff_ref$UID))

write.csv(buff_ref, "./data/EPA_compiled/Reference_sites_2011-2021.csv", row.names = F)

# Use full dataset to update floor and ceiling, and buff_ref dataset to update thresholds.
vmmi1 <- left_join(plot_br_cv, dist_inv_plot |> select(UID, UNIQUE_ID, SITE_ID, YEAR,
                                                       disttol_cov, inv_cov),
                   by = c("UID", "UNIQUE_ID", "SITE_ID", "YEAR"))

# Calculate floor and ceiling are the 5% and 95% of all sites for each metric
buff_vmmi <- left_join(buff_comb |> select(UID, UNIQUE_ID, SITETYPE, SITE_ID, dist, WETCLS_HGM),
                       vmmi1,
                       by = c("UID", "UNIQUE_ID", "SITETYPE", "SITE_ID")) |>
  mutate(site_type = ifelse(SITETYPE == "HAND" & dist == "MIN", "REF", paste0(dist, "_", SITETYPE)))

head(buff_vmmi)
(rangeMeanC <- quantile(buff_vmmi$meanC, probs = c(0.05, 0.95))) # 3.000877, 7.060764
(rangeBryo <- quantile(buff_vmmi$bryo_cov, probs = c(0.05, 0.95))) # 0, 98.98
(rangeDistTol <- quantile(buff_vmmi$disttol_cov, probs = c(0.05, 0.95))) #1.106, 150.551
(rangeInvCov <- quantile(buff_vmmi$inv_cov, probs = c(0.05, 0.95))) # 0, 76.585

# Now to check against original vegMMI thresholds. First adjusting individual metrics using the
# new floor and ceiling calculated above.
plot_vmmi <- buff_vmmi |>
  mutate(meanC_adj1 = ifelse(meanC < rangeMeanC[1], rangeMeanC[1],
                             ifelse(meanC > rangeMeanC[2], rangeMeanC[2], meanC)),
         meanC_adj2 = ((meanC_adj1 - rangeMeanC[1])/(rangeMeanC[2] - rangeMeanC[1])) * 10,

         covtol_adj1 = ifelse(disttol_cov < rangeDistTol[1], rangeDistTol[1],
                              ifelse(disttol_cov > rangeDistTol[2], rangeDistTol[2], disttol_cov)),
         covtol_adj2 = ((((covtol_adj1 - rangeDistTol[1])/(rangeDistTol[2] - rangeDistTol[1]))*10) - 10) * -1,

         invcov_adj1 = ifelse(inv_cov < rangeInvCov[1], rangeInvCov[1],
                              ifelse(inv_cov > rangeInvCov[2], rangeInvCov[2], inv_cov)),
         invcov_adj2 = ((((invcov_adj1 - rangeInvCov[1])/(rangeInvCov[2] - rangeInvCov[1]))*10) - 10) * -1,

         bryo_adj1 = ifelse(bryo_cov < rangeBryo[1], rangeBryo[1],
                             ifelse(bryo_cov > rangeBryo[2], rangeBryo[2], bryo_cov)),
         bryo_adj2 = ((bryo_adj1 - rangeBryo[1])/(rangeBryo[2] - rangeBryo[1])) * 10,

         vmmi1 = meanC_adj2 + covtol_adj2 + invcov_adj2 + bryo_adj2)

# Determine floor and ceiling of vmmi
(rangeVMMI <- range(plot_vmmi$vmmi1)) # 0.2667, 40.0

plot_vmmi <- plot_vmmi |>
  mutate(vmmi2 = ifelse(vmmi1 < rangeVMMI[1], rangeVMMI[1], vmmi1), # set to min for future calcs., not needed here
         vmmi = ((vmmi2 - rangeVMMI[1])/(rangeVMMI[2] - rangeVMMI[1])) * 100)

# Plot the different disturbance categories to check that it makes sense
plot_vmmi$site_type_fac <- factor(plot_vmmi$site_type, levels = c("REF", "MIN_PROB", "INT_HAND", "INT_PROB",
                                                                  "MOST_HAND", "MOST_PROB"))
table(plot_vmmi$WETCLS_HGM)

plot_vmmi$HGM_Class <- case_when(plot_vmmi$WETCLS_HGM %in% c("DEPRESSION", "DPRSS") ~ "Depression",
                                 plot_vmmi$WETCLS_HGM %in% c("FLATS") ~ "Flats",
                                 plot_vmmi$WETCLS_HGM %in% c("FRINGE", "LACUSTRINE") ~ "Lacustrine",
                                 plot_vmmi$WETCLS_HGM %in% c("SLOPE") ~ "Slope",
                                 plot_vmmi$WETCLS_HGM == "RIVERINE" ~ "Riverine",
                                 plot_vmmi$WETCLS_HGM %in% c("TIDAL", "UND") ~ "Other",
                                 TRUE ~ "UNK"
                                 )
table(plot_vmmi$HGM_Class, plot_vmmi$WETCLS_HGM)
table(plot_vmmi$WETCLS_HGM, plot_vmmi$YEAR)
table(plot_vmmi$HGM_Class)

# Plot VMMI distribution
theme_wet <- function(){
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "#696969", fill = "white",
                                        linewidth = 0.4),
        plot.background = element_blank(), strip.background = element_rect(color = "#696969",
                                                                                                                        fill = "grey90", size = 0.4), legend.key = element_blank(),
        axis.line.x = element_line(color = "#696969", size = 0.4),
        axis.line.y = element_line(color = "#696969", size = 0.4),
        axis.ticks = element_line(color = "#696969", size = 0.4))
}

ggplot(plot_vmmi,
       aes(x = site_type_fac, y = vmmi)) +
  geom_boxplot(outliers = F) + theme_wet() +
  geom_jitter(alpha = 0.2) +
  labs(x = "Site Disturbance Type", y = "Veg. MMI")

ggsave("./results/VMMI_distribution_site_type.png", height = 4, width = 6)

ggplot(plot_vmmi |> filter(HGM_Class %in% c("Depression", "Flats", "Lacustrine", "Riverine", "Slope")),
       aes(x = site_type_fac, y = vmmi)) +
  geom_boxplot() + theme_wet() +
  geom_jitter(alpha = 0.2) +
  labs(x = "Site Disturbance Type", y = "Veg. MMI") +
  facet_wrap(~HGM_Class) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Calculate thresholds
vmmi_ref <- plot_vmmi |> filter(UID %in% ref_uids)
thresh <- quantile(vmmi_ref$vmmi, probs = c(0.05, 0.25))
thresh[1] # Fair/Poor thresh = 41.48136
thresh[2] # Good/Fair thresh = 60.94853

plot_vmmi <- plot_vmmi |>
  mutate(vmmi_rating_orig = ifelse(vmmi > 65.22746, "Good", ifelse(vmmi < 52.785, "Poor", "Fair")),
         vmmi_rating = ifelse(vmmi > thresh[2], "Good", ifelse(vmmi < thresh[1], "Poor", "Fair")))

table(plot_vmmi$vmmi_rating) # new thresholds aren't as steep, and make more sense given CoC changes
table(plot_vmmi$vmmi_rating_orig)

ggplot(plot_vmmi, aes(x = YEAR, y = vmmi, color = vmmi_rating_orig)) + theme_wet() +
  geom_point() + #facet_wrap(~STATE) +
  scale_color_manual(values = c("Poor" = "#CC6666", "Fair" = "#DED864", "Good" = "#6AB06A")) +
  facet_wrap(~SITETYPE)

plot_vmmi$vmmi_rating_fac <- factor(plot_vmmi$vmmi_rating, levels = c("Good", "Fair", "Poor"))
plot_vmmi$site_type2 <- factor(case_when(plot_vmmi$site_type_fac %in% c("REF") ~ "REF",
                                         plot_vmmi$site_type_fac %in%
                                           c("MIN_PROB", "INT_PROB", "MOST_PROB") ~ "PROB",
                                         TRUE ~ "OTH"),
                               levels = c("REF", "PROB", "OTH"))

table(plot_vmmi$site_type2)

ggplot(plot_vmmi |> filter(site_type_fac %in% c("REF", "MIN_PROB", "INT_PROB", "MOST_PROB")) |>
         droplevels(),
       aes(x = YEAR, y = vmmi, color = vmmi_rating_fac, fill = vmmi_rating_fac,
           shape = vmmi_rating_fac)) +
  theme_wet() +
  geom_jitter(width = 0.2) + #facet_wrap(~STATE) +
  scale_color_manual(values = c("Poor" = "#CC6666", "Fair" = "#DED864", "Good" = "#6AB06A"),
                     name = "Rating") +
  scale_fill_manual(values = c("Poor" = "#CC6666", "Fair" = "#DED864", "Good" = "#6AB06A"),
                     name = "Rating") +
  scale_shape_manual(values = c("Poor" = 25, "Fair" = 19, "Good" = 24),
                     name = "Rating") +
  facet_wrap(~site_type2) +
  geom_hline(yintercept = thresh[1], linewidth = 0.5, color = "#696969",
             linetype = 'dashed') +
  geom_hline(yintercept = thresh[2], linewidth = 0.75, color = "#696969") +
  labs(y = "Vegetation MMI", x = NULL)

# ACAD sites
acad_sites <- paste(paste0("R", 301:310, collapse = "|"),
                    paste0("HP", 301:310, collapse = "|"), sep = "|")

acad_vmmi <- plot_vmmi |> filter(grepl(acad_sites, SITE_ID)) |>
  mutate(local_code = case_when(grepl("301", SITE_ID) ~ "DUCK",
                                grepl("302", SITE_ID) ~ "WMTN",
                                grepl("303", SITE_ID) ~ "BIGH",
                                grepl("304", SITE_ID) ~ "GILM",
                                grepl("305", SITE_ID) ~ "LITH",
                                grepl("306", SITE_ID) ~ "NEMI",
                                grepl("307", SITE_ID) ~ "GRME",
                                grepl("308", SITE_ID) ~ "HEBR",
                                grepl("309", SITE_ID) ~ "HODG",
                                grepl("310", SITE_ID) ~ "FRAZ"))
table(acad_vmmi$vmmi_rating)
table(acad_vmmi$vmmi_rating_orig)

ggplot(acad_vmmi, aes(x = YEAR, y = vmmi, color = vmmi_rating, group = local_code)) + theme_bw() +
  geom_point() + geom_line() +
  scale_color_manual(values = c("Poor" = "indianred", "Fair" = "gold", "Good" = "green2")) +
  facet_wrap(~local_code) +
  ylim(0, 100)

ggplot(acad_vmmi, aes(x = YEAR, y = vmmi, color = vmmi_rating_orig, group = local_code)) + theme_bw() +
  geom_point() + geom_line() +
  scale_color_manual(values = c("Poor" = "indianred", "Fair" = "gold", "Good" = "green2")) +
  facet_wrap(~local_code) +
  ylim(0, 100)

acad_uids <- unique(acad_vmmi$UID)

write.csv(acad_vmmi, "./data/EPA_compiled/Vegetation_MMI_2011-2021_ACAD_REF.csv", row.names = F)
write.csv(plot_vmmi |> filter(!UID %in% acad_uids),
           "./data/EPA_compiled/Vegetation_MMI_2011-2021_EPA_allsites.csv", row.names = F)
write.csv(plot_vmmi |> filter(!UID %in% acad_uids) |> filter(SITETYPE == "PROB"),
          "./data/EPA_compiled/Vegetation_MMI_2011-2021_EPA_PROB.csv", row.names = F)

# Compile stressors for 2021 PROB and all year reference plots
# Stressors in 2021
buff21_longb <- buff21 |> pivot_longer(cols = c(SOILHD_IMPERVIOUS_SURFACE:WOBSTR_WALL_RIPRAP),
                                      names_to = "Stressor", values_to = "Present") |>
  filter(!(LOCATION %in% c("N", "E", "S", "W") & PLOT == 0)) |> # these rows are not part of stressor checklist
  mutate(stress = ifelse(Present %in% c("Y", "L", "M", "H"), 1, 0),
         loc = ifelse(LOCATION == "AA", "AA", "BUFF"))   # weight buffer plot by distance to the center

buff21_longb$YEAR <- format(as.Date(buff21_longb$DATE_COL, format = "%d-%b-%y"), format = "%Y")
table(buff21_longb$YEAR[is.na(buff21_longb$YEAR)],
      buff21_longb$DATE_COL[is.na(buff21_longb$YEAR)], useNA = "always") # all the missing years are from 2021 b/c diff format
buff21_longb$YEAR[is.na(buff21_longb$YEAR)] <- 2021

# The same stressor can be recorded in different buffer plots, so have to simplify to P/A first
buff21_sum1b <- buff21_longb |> group_by(UID, SITE_ID, loc, YEAR, Stressor) |>
  summarize(stress2 = sum(stress),
            stress_pa = ifelse(stress2 > 0, 1, 0),
            .groups = "drop")

# Sum the number of unique stressors per site and location type (AA or BUFF)
buff21_sum1c <- buff21_sum1b |> group_by(UID, SITE_ID, loc, YEAR) |>
  summarize(num_stressors = sum(stress_pa),
            .groups = "drop")

buff21_wide <- buff21_sum1c |> pivot_wider(names_from = loc, values_from = num_stressors)
buff21_wide$YEAR <- as.numeric(buff21_wide$YEAR)

# buff16_sum ready to bind with future data
table(buff21_wide$AA)
table(buff21_wide$BUFF)
head(buff21_wide)

# Connect with site type
head(site21)
buff21_site <- right_join(site21 |> select(UID, SITE_ID, YEAR, SITETYPE),
                          buff21_wide, by = c("UID", "SITE_ID", "YEAR"))

# write.csv(buff21_site, "./results/Stressor_Counts_2021_NWCA.csv", row.names = F)

# Reference plot stressors
# 2011
buff11_sum1b <- buff11_long |>
  mutate(loc = ifelse(LOCATION == "AA", "AA", "BUFF")) |>
  group_by(UID, SITE_ID, loc, YEAR, Stressor) |>
  summarize(stress2 = sum(stress),
            stress_pa = ifelse(stress2 > 0, 1, 0),
            .groups = 'drop')

buff11_sum1c <- buff11_sum1b |> group_by(UID, SITE_ID, loc, YEAR) |>
  summarize(num_stressors = sum(stress_pa),
            .groups = 'drop')
buff11_wide <- buff11_sum1c |> pivot_wider(names_from = loc, values_from = num_stressors)
buff11_wide$YEAR <- as.numeric(buff11_wide$YEAR)
buff11_site <- right_join(site11 |> select(UID, SITE_ID, SITETYPE),
                          buff11_wide, by = c("UID", "SITE_ID"))

#2016
buff16_sum1b <- buff16_long |>
  mutate(loc = ifelse(LOCATION == "AA", "AA", "BUFF")) |>
  group_by(UID, SITE_ID, loc, YEAR, Stressor) |>
  summarize(stress2 = sum(stress),
            stress_pa = ifelse(stress2 > 0, 1, 0),
            .groups = 'drop')

buff16_sum1c <- buff16_sum1b |> group_by(UID, SITE_ID, loc, YEAR) |>
  summarize(num_stressors = sum(stress_pa),
            .groups = 'drop')
buff16_wide <- buff16_sum1c |> pivot_wider(names_from = loc, values_from = num_stressors)
buff16_wide$YEAR <- as.numeric(buff16_wide$YEAR)

buff16_site <- right_join(site16 |> select(UID, SITE_ID, SITETYPE),
                          buff16_wide, by = c("UID", "SITE_ID"))

buff_all <- rbind(buff11_site, buff16_site, buff21_site)
ref_uid <- plot_vmmi$UID[plot_vmmi$site_type_fac == "REF"]
prob_min_uid <- plot_vmmi$UID[plot_vmmi$site_type_fac == "MIN_PROB"]
hand_int_uid <- plot_vmmi$UID[plot_vmmi$site_type_fac == "INT_HAND"]
prob_int_uid <- plot_vmmi$UID[plot_vmmi$site_type_fac == "INT_PROB"]
hand_most_uid <- plot_vmmi$UID[plot_vmmi$site_type_fac == "MOST_HAND"]
prob_most_uid <- plot_vmmi$UID[plot_vmmi$site_type_fac == "MOST_PROB"]

buff_all$site_type2 <- factor(case_when(buff_all$UID %in% ref_uid ~ "REF",
                                        buff_all$UID %in% prob_min_uid ~ "MIN_PROB",
                                        buff_all$UID %in% hand_int_uid ~ "INT_HAND",
                                        buff_all$UID %in% prob_int_uid ~ "INT_PROB",
                                        buff_all$UID %in% hand_most_uid ~ "MOST_HAND",
                                        buff_all$UID %in% prob_most_uid ~ "MOST_PROB",
                                        TRUE ~ "OTH"),
                               levels = c("REF", "MIN_PROB", "INT_HAND", "INT_PROB",
                                          "MOST_HAND", "MOST_PROB", "OTH"))
buff_all$tot_stress <- buff_all$AA + buff_all$BUFF
buff_all$site_type2[buff_all$site_type2 == "OTH" & buff_all$tot_stress >= 2] <- "MOST_PROB"
buff_all$site_type2[buff_all$site_type2 == "OTH" & buff_all$tot_stress < 2] <- "INT_PROB"

buff_ref <- buff_all |> filter(UID %in% ref_uids) |> droplevels()
head(buff_all)

buff_all_long <- buff_all |> select(-tot_stress) |>
                   pivot_longer(cols = c(AA, BUFF),
                                names_to = "loc", values_to = "num_stress")
head(buff_all_long)

ggplot(buff_all_long,
       aes(x = site_type2, y = num_stress)) +
  geom_boxplot(outliers = F) + theme_wet() +
  geom_jitter(alpha = 0.15, width = 0.2) +
  labs(x = "Site Disturbance Type", y = "# Stressors") +
  facet_wrap(~loc) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave("./results/Stressor_boxplots_EPA.png", width = 8, height = 5)

write.csv(buff_ref, "./data/EPA_compiled/Stressor_Counts_REF_2011-2021.csv", row.names = F)
write.csv(buff_all |> droplevels(), "./data/EPA_compiled/Stressor_Counts_EPA_all_2011-2021.csv",
          row.names = F)
