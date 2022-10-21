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
## Import and rename ~~~
quarter <- read_csv(paste0(wd_path, "/raw_data/ISD.CSV"), 
                    col_names = FALSE, 
                    col_types=cols(X5 = col_date("%Y%m%d"),
                                   X12 = col_date("%Y%m%d"),
                                   X16 = col_date("%Y%m%d"),
                                   X19 = col_date("%Y%m%d"),
                                   X25 = col_date("%Y%m%d"),
                                   X26 = col_date("%Y%m%d"),
                                   X27 = col_date("%Y%m%d"),
                                   X33 = col_date("%Y%m%d"),
                                   X34 = col_date("%Y%m%d"),
                                   X35 = col_date("%Y%m%d"))) %>% 
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

## Reformat ~~~
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
# Warning messages that there are no entries for particular levels

table(quarter$hbres, useNA = "ifany") 
table(quarter$hb_screen, useNA = "ifany")


## Match GP practice codes ~~~

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


## Match SIMD ~~~
simd <- read_rds(simd_path) %>% 
  select(pc8, ca2019, 
         simd2020v2_sc_quintile, 
         simd2020v2_hb2019_quintile) %>% 
  glimpse()

quarter <- left_join(quarter, simd, by="pc8")


## Prepare file to save ~~~
quarter %<>%
  arrange(upi, date_screen) %>% 
  rename(postcode = pc8) %>% 
  mutate(hbres = recode(hbres, "Northumbria" = "Borders")) %>%  # Why not done above?
  select(chi:dob, sex, postcode,
         practice_code, practice_name, pat_elig,
         hbres, ca2019, hb_screen,
         simd2020v2_sc_quintile, simd2020v2_hb2019_quintile,
         location_code, screen_type, date_offer_sent, date_screen,
         att_dna, screen_result, screen_exep, followup_recom,
         apl_measure, apt_measure, largest_measure,
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
## Import and process ~~~
exclude <- read_csv(paste0(wd_path, "/raw_data/ISD-Exclusions.CSV"), 
                    col_names = FALSE, 
                    col_types=cols(X5 = col_date("%Y%m%d"),
                                   X7 = col_date("%Y%m%d"),
                                   X8 = col_date("%Y%m%d"))) %>%
  select(X1, X2, X5, X6, X7, X8, X9) %>% 
  arrange(X1) %>% 
  glimpse()

names(exclude) <- c("chi", "upi", "dob", "pat_inelig",
                    "date_start", "date_end", "pat_elig_rec")

## Write out ~~~
saveRDS(exclude, paste0(wd_path, 
                        "/output/aaa_exclusions_", year, month, ".rds"))


