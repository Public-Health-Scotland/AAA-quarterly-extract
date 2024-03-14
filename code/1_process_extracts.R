##########################################################
# 1_process_extracts.R
# Karen Hotopp
# 14/10/2022
# Script 1 of 3
# 
# Imports and formats BOXI extracts for AAA quarterly review
# Quarterly extracts collected: 1 March, 1 June, 1 Sept, 1 Dec
# Outputs are checked in further scripts;
# March and September outputs used for MEG KPI reports
# 
# Written/run on R Studio Server
# R version 3.6.1
# Revised/run on Posit WB
# R version 4.1.2
##########################################################

#### 1: Housekeeping ####
## Packages
library(dplyr)
library(haven)
library(magrittr)
library(stringr)
library(forcats)
library(readr)
library(phsmethods)
library(lubridate)
library(tidylog)


rm(list = ls())
gc()


## Values
year <- 2024
month <- "03"
date_download <- "20240301"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                 "/", year, month)

gp_path <- paste0("/conf/linkage/output/lookups/Unicode/National Reference Files",
                  "/gpprac.csv") # Changed from .sav 2Jun23

simd_path <- paste0("/conf/linkage/output/lookups/Unicode/Deprivation",
                    "/postcode_2024_1_simd2020v2.rds")


#### 2: Main extract ####
### Import and rename ---
# All columns should be character except date variables listed below.
quarter <- read_csv(paste0(wd_path, "/data/ISD_", date_download, ".CSV"), 
                    col_names = FALSE, 
                    col_types=cols(.default = "c",
                                   X5 = col_date("%Y%m%d"),
                                   X12 = col_date("%Y%m%d"),
                                   X16 = col_date("%Y%m%d"),
                                   X19 = col_date("%Y%m%d"),
                                   X25 = col_date("%Y%m%d"),
                                   X26 = col_date("%Y%m%d"),
                                   X27 = col_date("%Y%m%d"),
                                   X29 = col_double(),
                                   X30 = col_double(),
                                   X31 = col_double(),
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

### Reformat ---
table(quarter$hb_surgery) ## Where is D? Cumbria
table(quarter$hbres) ## Where is E? Northumbria

quarter %<>%
  select(-surname, -forename) %>% 
  rename(pc8 = postcode) %>% 
  # calculate age at screening
  mutate(age_at_screening = age_calculate(dob, date_screen),
         hb_screen = str_sub(location_code, -1),
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
# Accept (mostly non-Scottish HBs are not valid factor levels)

table(quarter$hbres, useNA = "ifany") 
table(quarter$hb_screen, useNA = "ifany")


### Add financial year and quarter ---
# Check latest dates of date_screen for fct_relevel of financial_year & fy_surgery
tail(table(quarter$date_screen))

## Create financial year/quarter from screening date
# function - creates vector of all FYs from screening date,
# adds financial year/quarter and fy_quarter columns from screening date, 
# then uses vector of FYs to relevel financial_year
assign_screen_fy_quarter <- function(data){
  
  list_fin_years <- data %>%
    mutate(fy = extract_fin_year(date_screen)) %>%
    select(fy) %>% 
    filter(!is.na(fy)) %>% 
    unique() %>% 
    arrange(fy) %>% # despite character column, still manages to arrange - feel free to update if any better ideas
    pull() 
  
  
  data <- data %>% 
    mutate(financial_year = extract_fin_year(date_screen),
           financial_quarter = qtr(date_screen, format="short")) %>% 
    # financial_quarter should be represented by a number
    mutate(financial_quarter = str_sub(financial_quarter, 1, 3),
           financial_quarter = case_when(financial_quarter == "Jan" ~ 4,
                                         financial_quarter == "Apr" ~ 1,
                                         financial_quarter == "Jul" ~ 2,
                                         financial_quarter == "Oct" ~ 3),
           fy_quarter = paste0(financial_year, "_", financial_quarter)) %>% 
    mutate(fy_quarter = if_else(fy_quarter == "NA_NA", "unrecorded", fy_quarter)) %>%  
    mutate(financial_year = fct_relevel(financial_year, 
                                        list_fin_years)
    ) %>% 
    arrange(upi, fy_quarter) %>% 
    glimpse()
  
}

quarter <- assign_screen_fy_quarter(quarter)

# Check latest financial_year values, ensure match check above
tail(table(quarter$financial_year))


## Create financial year from surgery date (year of surgery)
tail(table(quarter$date_surgery))

quarter %<>%
  mutate(fy_surgery = extract_fin_year(date_surgery)) %>%
  mutate(fy_surgery = fct_relevel(fy_surgery,
                                           c("2012/13", "2013/14", "2014/15",
                                             "2015/16", "2016/17", "2017/18",
                                             "2018/19", "2019/20", "2020/21",
                                             "2021/22", "2022/23", "2023/24"))) %>%
  relocate(fy_surgery, .after=date_surgery) %>%
  glimpse()


### Match GP practice codes ---
# First letter in practice_code variable string represents HB (usually), 
# but easier to merge if removed
quarter %<>%
  mutate(gp_prac = str_sub(practice_code, 2, 5),
         gp_prac = as.numeric(gp_prac)) %>% 
  glimpse()

gp_link <- read_csv(gp_path) %>% 
  mutate(gp_prac = str_sub(praccode, 1, 4),
         gp_prac = as.numeric(gp_prac)) %>%
  # # data now has two rows where gp_prac==9999; keep where add1=="unknown"
  # # and remove add1/add2=="PATIENTS REGISTERED WITH A GP IN ENG WAL OR N IRE"
  # filter(praccode != 99995) %>% 
  # use title case to clean practice names
  mutate(practice_name = str_to_title(`add 1`)) %>% 
  select(gp_prac, practice_name) %>% 
glimpse()

quarter <- left_join(quarter, gp_link, by="gp_prac")


### Match SIMD ---
simd <- read_rds(simd_path) %>% 
  select(pc8, ca2019, 
         simd2020v2_sc_quintile, 
         simd2020v2_hb2019_quintile) %>% 
  glimpse()

quarter <- left_join(quarter, simd, by="pc8")


### Create definitive screen results ---
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

### Calculate eligibility period ---
# Eligibility period is the financial year in which the person turned 66. 
#!! Rewrite as a function so that new lines don't need added when later 
#!! financial years become relevant.
# Boards have differing start dates for eligiblity
# aged65_onstartdate counts if a person was 65 when their specific board started screening
# over65_onstartdate counts if a person was over 65 when their specific board started screening.
# eligibility_period combines the above two variables into one.

quarter %<>%
  mutate(eligibility_period = case_when(
    between(dob, dmy("01-04-1947"), dmy("31-03-1948")) ~ "Turned 66 in year 2013/14",
    between(dob, dmy("01-04-1948"), dmy("31-03-1949")) ~ "Turned 66 in year 2014/15",
    between(dob, dmy("01-04-1949"), dmy("31-03-1950")) ~ "Turned 66 in year 2015/16",
    between(dob, dmy("01-04-1950"), dmy("31-03-1951")) ~ "Turned 66 in year 2016/17",
    between(dob, dmy("01-04-1951"), dmy("31-03-1952")) ~ "Turned 66 in year 2017/18",
    between(dob, dmy("01-04-1952"), dmy("31-03-1953")) ~ "Turned 66 in year 2018/19",
    between(dob, dmy("01-04-1953"), dmy("31-03-1954")) ~ "Turned 66 in year 2019/20",
    between(dob, dmy("01-04-1954"), dmy("31-03-1955")) ~ "Turned 66 in year 2020/21",
    between(dob, dmy("01-04-1955"), dmy("31-03-1956")) ~ "Turned 66 in year 2021/22",
    between(dob, dmy("01-04-1956"), dmy("31-03-1957")) ~ "Turned 66 in year 2022/23",
    between(dob, dmy("01-04-1957"), dmy("31-03-1958")) ~ "Turned 66 in year 2023/24",
    between(dob, dmy("01-04-1958"), dmy("31-03-1959")) ~ "Turned 66 in year 2024/25"
  ),
  age65_onstartdate = case_when(
    hbres == "Ayrshire & Arran" &
      between(dob, dmy("01-06-1947"), dmy("31-05-1948")) ~ 1,
    hbres == "Borders" &
      between(dob, dmy("09-08-1946"), dmy("08-08-1947")) ~ 1,
    hbres == "Dumfries & Galloway" &
      between(dob, dmy("24-07-1947"), dmy("23-07-1948")) ~ 1,
    hbres == "Fife" &
      between(dob, dmy("09-01-1947"), dmy("08-01-1948")) ~ 1,
    hbres == "Forth Valley" &
      between(dob, dmy("18-09-1947"), dmy("17-09-1948")) ~ 1,
    hbres == "Grampian" &
      between(dob, dmy("03-10-1946"), dmy("02-10-1947")) ~ 1,
    hbres == "Greater Glasgow & Clyde" &
      between(dob, dmy("06-02-1947"), dmy("05-02-1948")) ~ 1,
    hbres == "Highland" &
      between(dob, dmy("29-06-1946"), dmy("28-06-1947")) ~ 1,
    hbres == "Lanarkshire" &
      between(dob, dmy("01-04-1947"), dmy("31-03-1948")) ~ 1,
    hbres == "Lothian" &
      between(dob, dmy("09-08-1946"), dmy("08-08-1947")) ~ 1,
    hbres == "Orkney" &
      between(dob, dmy("03-10-1946"), dmy("02-10-1947")) ~ 1,
    hbres == "Shetland" &
      between(dob, dmy("03-10-1946"), dmy("02-10-1947")) ~ 1,
    hbres == "Tayside" &
      between(dob, dmy("09-01-1947"), dmy("08-01-1948")) ~ 1,
    hbres == "Western Isles" &
      between(dob, dmy("29-06-1946"), dmy("28-06-1947")) ~ 1,
    TRUE ~ 0
  ),
  over65_onstartdate = case_when(
    hbres == "Ayrshire & Arran" & dob < dmy("01-06-1947") ~ 1,
    hbres == "Borders" & dob < dmy("09-08-1946") ~ 1,
    hbres == "Dumfries & Galloway" & dob < dmy("24-07-1947") ~ 1,
    hbres == "Fife" & dob < dmy("09-01-1947") ~ 1,
    hbres == "Forth Valley" & dob < dmy("18-09-1947") ~ 1,
    hbres == "Grampian" & dob < dmy("03-10-1946") ~ 1,
    hbres == "Greater Glasgow & Clyde" & dob < dmy("06-02-1947") ~ 1,
    hbres == "Highland" & dob < dmy("29-06-1946") ~ 1,
    hbres == "Lanarkshire" & dob < dmy("01-04-1947") ~ 1,
    hbres == "Lothian" & dob < dmy("09-08-1946") ~ 1,
    hbres == "Orkney" & dob < dmy("03-10-1946") ~ 1,
    hbres == "Shetland" & dob < dmy("03-10-1946") ~ 1,
    hbres == "Tayside" & dob < dmy("09-01-1947") ~ 1,
    hbres == "Western Isles" & dob < dmy("29-06-1946") ~ 1,
    TRUE ~ 0
  ),
  dob_eligibility = case_when(
    over65_onstartdate == 1 ~ "Over eligible age cohort: age 66+ on start date",
    age65_onstartdate == 1 ~ "Older cohort: age 65 on start date",
    !is.na(eligibility_period) & age65_onstartdate == 0 ~ eligibility_period
  ))


## Any other variables to be created, insert here ---






## Prepare file to save ---
quarter %<>%
  arrange(upi, date_screen) %>% 
  rename(postcode = pc8) %>% 
  mutate(hbres = recode(hbres, "Northumbria" = "Borders")) %>%  # Why not done above?
  select(financial_year:fy_quarter, chi:dob, sex, postcode,
         practice_code, practice_name, pat_elig, hbres, 
         eligibility_period, age65_onstartdate, over65_onstartdate,
         dob_eligibility, ca2019, hb_screen,
         simd2020v2_sc_quintile, simd2020v2_hb2019_quintile,
         location_code, screen_type, date_offer_sent, date_screen, age_at_screening,
         att_dna, screen_result, screen_exep, followup_recom,
         apl_measure, apt_measure, largest_measure, aaa_size, aaa_size_group,
         date_result, result_verified, date_verified,
         date_referral, date_referral_true, date_seen_outpatient,
         result_outcome, first_outcome, referral_error_manage,
         date_surgery, fy_surgery, hb_surgery, surg_method, date_death,
         aneurysm_related, audit_flag, audit_result, audit_fail_reason,
         audit_fail_1, audit_fail_2, audit_fail_3, audit_fail_4, audit_fail_5,
         audit_outcome, audit_batch_fail, audit_batch_outcome) %>% 
  glimpse()

saveRDS(quarter, paste0(wd_path, 
                        "/output/aaa_extract_", year, month, ".rds"))


#### 3: Exclusions extract ####
## Import and process ---
exclude <- read_csv(paste0(wd_path, "/data/ISD-Exclusions_", 
                           date_download, ".CSV"), 
                    col_names = FALSE, 
                    col_types=cols(.default = "c",
                                   X5 = col_date("%Y%m%d"),
                                   X7 = col_date("%Y%m%d"),
                                   X8 = col_date("%Y%m%d"))) %>%
  select(X1, X2, X5, X6, X7, X8, X9) %>% 
  arrange(X2) %>% 
  glimpse()

names(exclude) <- c("chi", "upi", "dob", "pat_inelig",
                    "date_start", "date_end", "pat_elig_rec")

## Add financial year and quarter ---
exclude %<>%
  mutate(financial_year = extract_fin_year(as.Date(date_start)),
         month = format(as.Date(date_start, format = "%Y-%m-%d"),"%m"),
         financial_quarter = case_when(month %in% c('04','05','06') ~ 1,
                                       month %in% c('07','08','09') ~ 2,
                                       month %in% c('10','11','12') ~ 3,
                                       month %in% c('01','02','03') ~ 4),
         fin_month = case_when(month %in% c('04','05','06','07','08','09',
                                            '10','11','12') ~ as.numeric(month)-3,
                               month == '03' ~ 12,
                               month == '02' ~ 11,
                               month == '01' ~ 10)) %>%
  select(-month) %>% 
  select(financial_year:fin_month,
         chi:pat_elig_rec) %>%
  arrange(upi, financial_year, financial_quarter) %>%
  glimpse()

## Write out ---
saveRDS(exclude, paste0(wd_path, 
                        "/output/aaa_exclusions_", year, month, ".rds"))

