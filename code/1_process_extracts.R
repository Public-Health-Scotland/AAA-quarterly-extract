##########################################################
# 1_process_extracts.R
# Karen Hotopp
# 14/10/2022
# Script 1 of ?
# 
# Imports and formats BOXI extracts for AAA quarterly review
# Quarterly extracts collected: 1 March, 1 June, 1 Sept, 1 Dec
# Outputs are checked in further scripts;
# March and September outputs used for MEG reports
# 
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

#### 1: Housekeeping ####
## Packages
library(here)
library(dplyr)
library(haven)
library(magrittr)
library(stringr)
library(forcats)
library(readr)
library(phsmethods)
library(tidylog)


rm(list = ls())


## Values
year <- 2022
month <- "09"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                 "/", year, month)

gp_path <- paste0("/conf/linkage/output/lookups/Unicode/National Reference Files",
                  "/gpprac.sav")
##!! KH: I don't see an RDS (or CSV) version of this file... Where to get?

simd_path <- paste0("/conf/linkage/output/lookups/Unicode/Deprivation",
                    "/postcode_2022_2_simd2020v2.rds")


#### 2: Main extract ####
## Import and rename ---
quarter <- read_csv(paste0(wd_path, "/raw_data/ISD.CSV"), 
                    col_names = FALSE, 
                    col_types=cols(X5 = col_date("%Y%m%d"),
                                   X12 = col_date("%Y%m%d"),
                                   X16 = col_date("%Y%m%d"),
                                   X19 = col_date("%Y%m%d"),
                                   X20 = col_character(),
                                   X25 = col_date("%Y%m%d"),
                                   X26 = col_date("%Y%m%d"),
                                   X27 = col_date("%Y%m%d"),
                                   X28 = col_character(),
                                   X33 = col_date("%Y%m%d"),
                                   X34 = col_date("%Y%m%d"),
                                   X35 = col_date("%Y%m%d"),
                                   X36 = col_character(),
                                   X41 = col_character(),
                                   X42 = col_character(),
                                   X45 = col_character())) %>% 
  glimpse()

names(quarter) <- c("chi", "upi", "surname", "forename", "dob", "postcode",
                    "hbres", "sex", "pat_elig", "pat_inelig", "location_code",
                    "date_screen", "att_dna", "screen_type", "screen_exep",
                    "date_result", "followup_recom", "screen_result", "date_referral",
                    "result_outcome", "surg_method", "audit_flag", "audit_result",
                    "audit_outcome", "date_offer_sent", "date_surgery",
                    "date_seen_outpatient", "aneurysm_related", "apl_measure",
                    "apt_measure", "largest_measure", "result_verified", 
                    "date_verified", "date_death", "date_referral_true", 
                    "first_outcome", "audit_fail_reason", "audit_fail_1", 
                    "audit_fail_2", "audit_fail_3", "audit_fail_4", 
                    "audit_fail_5", "audit_batch_fail", "audit_batch_outcome",
                    "referral_error_manage", "practice_code", "hb_surgery")

## Reformat ---
table(quarter$hb_surgery) ## Where is D? Cumbria
table(quarter$hbres) ## Where is E? Northumbria

quarter %<>%
  select(-surname, -forename) %>% 
  rename(pc8 = postcode) %>% 
  mutate(hb_screen = str_sub(location_code, -1),
         hb_screen = case_when(hb_screen=="A" ~ "Ayrshire & Arran",
                               hb_screen=="B" ~ "Borders",
                               hb_screen=="Y" ~ "Dumfries & Galloway",
                               hb_screen=="F" ~ "Fife",
                               hb_screen=="V" ~ "Forth Valley",
                               hb_screen=="N" ~ "Grampian",
                               hb_screen=="G" ~ "Greater Glasgow & Clyde",
                               hb_screen=="H" ~ "Highland",
                               hb_screen=="L" ~ "Lanarkshire",
                               hb_screen=="S" ~ "Lothian",
                               hb_screen=="R" ~ "Orkney",
                               hb_screen=="Z" ~ "Shetland",
                               hb_screen=="T" ~ "Tayside",
                               hb_screen=="W" ~ "Western Isles",
                               hb_screen=="M" ~ "Scotland",
                               hb_screen=="C" ~ "Argyll & Clyde",
                               hb_screen=="D" ~ "Cumbria",
                               hb_screen=="E" ~ "Northumbria",
                               hb_screen==TRUE ~ "Outwith Scotland"),
         hb_screen = fct_relevel(hb_screen, c("Argyll & Clyde", 
                                              "Ayrshire & Arran", "Borders",
                                              "Dumfries & Galloway", "Fife",
                                              "Forth Valley", "Grampian", 
                                              "Greater Glasgow & Clyde",
                                              "Highland", "Lanarkshire",
                                              "Lothian", "Orkney",
                                              "Shetland", "Tayside", 
                                              "Western Isles", "Scotland",
                                              "Cumbria", "Northumbria",
                                              "Outwith Scotland"))
         ) %>% 
  mutate(hbres = case_when(hbres=="A" ~ "Ayrshire & Arran",
                           hbres=="B" ~ "Borders",
                           hbres=="Y" ~ "Dumfries & Galloway",
                           hbres=="F" ~ "Fife",
                           hbres=="V" ~ "Forth Valley",
                           hbres=="N" ~ "Grampian",
                           hbres=="G" ~ "Greater Glasgow & Clyde",
                           hbres=="H" ~ "Highland",
                           hbres=="L" ~ "Lanarkshire",
                           hbres=="S" ~ "Lothian",
                           hbres=="R" ~ "Orkney",
                           hbres=="Z" ~ "Shetland",
                           hbres=="T" ~ "Tayside",
                           hbres=="W" ~ "Western Isles",
                           hbres=="M" ~ "Scotland",
                           hbres=="C" ~ "Argyll & Clyde",
                           hbres=="D" ~ "Cumbria",
                           hbres=="E" ~ "Northumbria", # Should this just be Borders?
                           hbres==TRUE ~ "Outwith Scotland"),
         hbres = fct_relevel(hbres, c("Argyll & Clyde", 
                                      "Ayrshire & Arran", "Borders",
                                      "Dumfries & Galloway", "Fife",
                                      "Forth Valley", "Grampian", 
                                      "Greater Glasgow & Clyde",
                                      "Highland", "Lanarkshire",
                                      "Lothian", "Orkney",
                                      "Shetland", "Tayside", 
                                      "Western Isles", "Scotland",
                                      "Cumbria", "Northumbria",
                                      "Outwith Scotland"))
  )
# Warning messages say that there are no entries for particular levels;
# Accept

table(quarter$hbres, useNA = "ifany") 
table(quarter$hb_screen, useNA = "ifany")


## Add financial year and quarter ---
# Create financial year/quarter from screening date
quarter %<>%
  mutate(financial_year = extract_fin_year(date_screen),
         financial_quarter = qtr(date_screen, format="short")) %>% 
  # financial_quarter should be represented by a number
  mutate(financial_quarter = str_sub(financial_quarter, 1, 3),
         financial_quarter = case_when(financial_quarter == "Jan" ~ 4,
                                       financial_quarter == "Apr" ~ 1,
                                       financial_quarter == "Jul" ~ 2,
                                       financial_quarter == "Oct" ~ 3),
         fy_quarter = paste0(financial_year, "_", financial_quarter)) %>% 
  mutate(fy_quarter = if_else(fy_quarter == "NA NA", "", fy_quarter)) %>%  
  arrange(upi, fy_quarter) %>% 
  glimpse()


## Match GP practice codes ---
# First letter in practice_code variable string represents HB, 
# but easier to merge if removed
quarter %<>%
  mutate(gp_prac = str_sub(practice_code, 2, 5),
         gp_prac = as.numeric(gp_prac)) %>% 
  glimpse()

gp_link <- read_sav(gp_path) %>% 
  mutate(gp_prac = str_sub(praccode, 1, 4),
         gp_prac = as.numeric(gp_prac)) %>%
  # data now has two rows where gp_prac==9999; keep where add1=="unknown"
  # and remove add1/add2=="PATIENTS REGISTERED WITH A GP IN ENG WAL OR N IRE"
  filter(praccode != 99995) %>% 
  select(gp_prac, add1) %>% 
  # use title case to clean practice names
  mutate(add1 = str_to_title(add1)) %>% 
  rename(practice_name = add1) %>% 
  glimpse()

quarter <- left_join(quarter, gp_link, by="gp_prac")


## Match SIMD ---
simd <- read_rds(simd_path) %>% 
  select(pc8, ca2019, 
         simd2020v2_sc_quintile, 
         simd2020v2_hb2019_quintile) %>% 
  glimpse()

quarter <- left_join(quarter, simd, by="pc8")


## Create definitive screen results ---
# A measurement category is derived for definitive screen results i.e. positive,
# negative, external postive or external negative results unless the follow up
# recommendation is immediate recall ('05').
# This new category does not include technical fails, non-visualisations or 
# immediate recalls.
quarter %<>%
  mutate(aaa_size = case_when(screen_result %in% c("01", "02", "05", "06") &
                                (followup_recom != "05" | 
                                   is.na(followup_recom)) ~ largest_measure)) %>%
  mutate(aaa_size_group = case_when(aaa_size >= 0 &
                                      aaa_size <= 2.9 ~ "negative",
                                    aaa_size >= 3 &
                                      aaa_size <= 4.4 ~ "small",
                                    aaa_size >= 4.5 &
                                      aaa_size <= 5.4 ~ "medium",
                                    aaa_size >= 5.5 &
                                      aaa_size <= 10.5 ~ "large",
                                    aaa_size >= 10.6 ~ "very large error"))


## Any other variables to be created, insert here ---






## Prepare file to save ---
quarter %<>%
  arrange(upi, date_screen) %>% 
  rename(postcode = pc8) %>% 
  mutate(hbres = recode(hbres, "Northumbria" = "Borders")) %>%  # Why not done above?
  select(financial_year:fy_quarter, chi:dob, sex, postcode,
         practice_code, practice_name, pat_elig,
         hbres, ca2019, hb_screen,
         simd2020v2_sc_quintile, simd2020v2_hb2019_quintile,
         location_code, screen_type, date_offer_sent, date_screen,
         att_dna, screen_result, screen_exep, followup_recom,
         apl_measure, apt_measure, largest_measure, aaa_size, aaa_size_group,
         date_result, result_verified, date_verified,
         date_referral, date_referral_true, date_seen_outpatient,
         result_outcome, first_outcome, referral_error_manage,
         date_surgery, hb_surgery, surg_method, date_death,
         aneurysm_related, audit_flag, audit_result, audit_fail_reason,
         audit_fail_1, audit_fail_2, audit_fail_3, audit_fail_4, audit_fail_5,
         audit_outcome, audit_batch_fail, audit_batch_outcome) %>% 
  glimpse()

saveRDS(quarter, paste0(wd_path, 
                        "/output/aaa_extract_", year, month, ".rds"))


#### 3: Exclusions extract ####
## Import and process ---
exclude <- read_csv(paste0(wd_path, "/raw_data/ISD-Exclusions.CSV"), 
                    col_names = FALSE, 
                    col_types=cols(X5 = col_date("%Y%m%d"),
                                   X7 = col_date("%Y%m%d"),
                                   X8 = col_date("%Y%m%d"))) %>%
  select(X1, X2, X5, X6, X7, X8, X9) %>% 
  arrange(X2) %>% 
  glimpse()

names(exclude) <- c("chi", "upi", "dob", "pat_inelig",
                    "date_start", "date_end", "pat_elig_rec")

# ## Add financial year and quarter ---
# # Create financial year/quarter from starting date
# exclude %<>%
#   mutate(financial_year = extract_fin_year(date_start),
#          financial_quarter = qtr(date_start, format="short")) %>% 
#   # financial_quarter should be represented by a number
#   mutate(financial_quarter = str_sub(financial_quarter, 1, 3),
#          financial_quarter = case_when(financial_quarter == "Jan" ~ 4,
#                                        financial_quarter == "Apr" ~ 1,
#                                        financial_quarter == "Jul" ~ 2,
#                                        financial_quarter == "Oct" ~ 3),
#          fy_quarter = paste0(financial_year, " ", financial_quarter)) %>% 
#   mutate(fy_quarter = if_else(fy_quarter == "NA NA", "", fy_quarter)) %>%  
#   select(financial_year:fy_quarter, 
#          chi:pat_elig_rec) %>% 
#   arrange(upi, fy_quarter) %>% 
#   glimpse()
#   
#   CHECK IF THIS IS ACCURATE: IS date_start COMPARABLE TO date_screen???


## Write out ---
saveRDS(exclude, paste0(wd_path, 
                        "/output/aaa_exclusions_", year, month, ".rds"))


