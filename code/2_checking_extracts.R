##########################################################
# 2_checking_extracts.R
# Karen Hotopp
# 19/10/2022
# Script 2 of ?
# 
# Checks processed BOXI extracts for AAA quarterly review
# Quarterly extracts collected: 1 March, 1 June, 1 Sept, 1 Dec
# March and September outputs used for MEG reports
# 
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

#### 1: Housekeeping ####
## Packages
library(here)
library(dplyr)
library(magrittr)
library(stringr)
#library(forcats)
library(readr)
library(phsmethods)
library(validate)
library(tidylog)


rm(list = ls())


## Values
year <- 2022
month <- "09"
# Financial year and quarter of current extract
fyq_current <- "2022/23 2"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                 "/", year, month)


#### 2: Call in extract ####
quarter <- read_rds(paste0(wd_path, "/output/aaa_extract_202209.rds")) %>% 
  # exclude cases with results obtained outside of Scotland 
  filter(!screen_result %in% c("05", "06")) %>% 
  # create financial year/quarter from screening date
  mutate(financial_year = extract_fin_year(date_screen),
         financial_quarter = qtr(date_screen, format="short")) %>% 
  # financial_quarter should be represented by a number
  mutate(financial_quarter = str_sub(financial_quarter, 1, 3),
         financial_quarter = case_when(financial_quarter == "Jan" ~ 4,
                                       financial_quarter == "Apr" ~ 1,
                                       financial_quarter == "Jul" ~ 2,
                                       financial_quarter == "Oct" ~ 3),
         fy_quarter = paste0(financial_year, " ", financial_quarter)) %>% 
  mutate(fy_quarter = if_else(fy_quarter == "NA NA", "", fy_quarter)) %>%  
  arrange(upi, fy_quarter) %>% 
  # id variable for matching validator checks
  mutate(id = row_number(), .before = chi) %>% 
  glimpse()

length((unique(quarter$upi)))
# 366,571 of 523071 records


#### 3. Validate data ####
### A. Check root data ----
check_roots <- validator("sex" = sex == "01",
                         "missing_postcode" = !is.na(postcode),
                         "missing_simd" = !is.na(simd2020v2_sc_quintile),
                         "missing_gp" = !is.na(practice_code))

review_roots <- confront(quarter, check_roots, key  ="id")
summary(review_roots)


### B. Check dates ----
check_dates <- validator("date_offer_screen" = date_offer_sent <= date_screen,
                         "date_screen_result" = date_screen <= date_result,
                         "date_result_verified" = date_result < date_verified,
                         "date_verified_referral" = date_verified < date_referral,
                         "date_referral_refTrue" = date_referral < date_referral_true,
                         "date_refTrue_seen_outpatient" = date_referral_true < date_seen_outpatient, # Is this accurate??
                         "date_seen_outpatient_surgery" = date_seen_outpatient < date_surgery, # Is this accurate??
                         "date_surgery_death" = date_surgery < date_death,
                         "correct_FYQ" = fy_quarter <= fyq_current)

review_dates <- confront(quarter, check_dates, key  ="id")
summary(review_dates)

# ####
# ## Screenings by quarter
# annual <- quarter %>% 
#   mutate(count_screens = if_else(is.na(date_screen), 0, 1)) %>% 
#   group_by(fy_quarter) %>% 
#   summarize(screenings = sum(count_screens)) %>% 
#   ungroup()
# 
# tail(annual)  
# # Note that last three lines have not happened yet (for this data set):
# # 4 2022/23 3        1054
# # 5 2025/26 2           1
# # 6 2056/57 2           1
# # 
# # These were appointments that were set up with the wrong dates
# # and subsequently cancelled by the screening provider.
# 
# ## Remove screen_result == NA  
# temp <- quarter %>% 
#   filter(!is.na(screen_result)) %>% 
#   mutate(count_screens = if_else(is.na(date_screen), 0, 1)) %>% 
#   group_by(fy_quarter) %>% 
#   summarize(screenings = sum(count_screens)) %>% 
#   ungroup() 
# 
# tail(temp) 
# ####


### C. Check results ----
## "passes" are to be reviewed
check_results <- validator("patients_seen_screened" = att_dna == "05",
                           "not_recorded_result" = att_dna == "05" & 
                             is.na(screen_result),
                           "not_recorded_followup" = att_dna == "05" & 
                             is.na(followup_recom),
                           "no_result_followup" = att_dna == "05" & 
                             is.na(screen_result) & is.na(followup_recom),
                           "invalid_measure_1" = screen_result == "01" &
                             largest_measure < 3,
                           "invalid_measure_2" = screen_result == "02" &
                             largest_measure >= 3)

review_results <- confront(quarter, check_results, key  ="id")
summary(review_results)


### D. Check audits ----
## "passes" are to be reviewed
check_audits <- validator("total_audits" = audit_flag == "01",
                          "audit_fail" = audit_result == "02",
                          "not_recorded_fail_reason" = audit_result == "02" & 
                            is.na(audit_fail_reason),
                          "not_recorded_fail_detail" = !is.na(audit_fail_reason) & 
                            is.na(audit_fail_1),
                          "not_recorded_audit_outcome" = audit_result == "02" &
                            is.na(audit_outcome),
                          "not_recorded_batch_outcome" = !is.na(audit_batch_fail) &
                            is.na(audit_batch_outcome))

review_audits <- confront(quarter, check_audits, key  ="id")
summary(review_audits)


#### 4. Combine checks ####
# make a data.frame
review_df <- as.data.frame(review)
# rearrange data.frame
review_df %<>%
  pivot_wider(id_cols = c('id'),
              names_from = c('name'),
              values_from = c('value'))
# join back onto dataset using key
quarter_check <- quarter %>%
  left_join(review_df, by = c('id'))
quarter_check
# View results that did not pass
quarter_check %>% filter(!correct_FYQ == TRUE) %>%
  View()







names(quarter)







