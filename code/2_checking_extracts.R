##########################################################
# 2_checking_extracts.R
# Karen Hotopp
# 19/10/2022
# Script 2 of 3
# 
# Checks processed BOXI extracts for AAA quarterly review
# Quarterly extracts collected: 1 March, 1 June, 1 Sept, 1 Dec
# March and September outputs used for QPMG reports
# 
# Written/run on R Studio Server
# R version 3.6.1
# Revised/run on Posit WB
# R version 4.1.2
##########################################################

## Some things to think about:
## What should this script be doing? Are there outputs and if so where do they
## go? Is it reasonable to make graphical outputs for people running script to
## interpret the data?
## Do we need to create an Excel file?
## Conversely, if we find issues in the data, would it be worth creating outputs 
## to send to the HBs to review errors/questions? Or maybe that is for the 
## pre-September audit??



#### 1: Housekeeping ####
## Packages
library(dplyr)
library(stringr)
library(readr)
library(validate)
library(arsenal)
library(tidylog)


rm(list = ls())
gc()


## Values
year <- 2025
month <- "03"
previous <- 202412
# Financial year and quarter of current extract
#March = q1
#June = q2
#Sept = q3
#Dec = q4
fyq_current <- "2025/26_1"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                 "/", year, month)
previous_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                        "/", previous)

#### 2: Call in extract ####
quarter <- read_rds(paste0(wd_path, "/output/aaa_extract_", year, month, ".rds")) %>% 
  # exclude cases with results obtained outside of Scotland 
  filter(!screen_result %in% c("05", "06")) %>% 
  # id variable for matching validator checks
  mutate(id = row_number(), .before = financial_year) %>% 
  glimpse()
  
length(unique(quarter$upi))
# 202403: 421,311 of 609,003 records
# 202406: 430,862 of 623,564 records
# 202409: 440,047 of 638,150 records
# 202412: 449,520 of 652,939 records
# 202503: 459,454 of 667,297 records

#### 3. Validate data ####
## Rules are written such that a "pass" is an outlier to be investigated.
## For example, if date_death is before date_surgery, that is a "pass".
## This allows pass results (logical TRUE) to more easily be converted to 
## an integer for summarizing (sections 4 & 5).

### A. Check root data ----
check_roots <- validator("sex" = sex != "01",
                         "HB_res_screen" = hbres != hb_screen,
                         "missing_postcode" = is.na(postcode),
                         "missing_simd" = is.na(simd2020v2_sc_quintile),
                         "missing_gp" = is.na(practice_code))

review_roots <- confront(quarter, check_roots, key  ="id")
summary(review_roots)


### B. Check dates ----
check_dates <- validator("date_offer_screen" = date_offer_sent > date_screen,
                         "date_screen_result" = date_screen > date_result,
                         "date_result_verified" = date_result > date_verified,
                         "date_verified_referral" = date_verified > date_referral,
                         "date_refTrue_seenOP" = 
                           date_referral_true > date_seen_outpatient,
                         "date_surgery_death" = date_surgery > date_death,
                         "correct_FYQ" = fy_quarter != "unrecorded" & 
                           fy_quarter > fyq_current)

review_dates <- confront(quarter, check_dates, key  ="id")
summary(review_dates)

####
## Screenings by quarter
# This checks records that "passed" the correct_FYQ validation
annual <- quarter %>%
  mutate(count_screens = if_else(is.na(date_screen), 0, 1)) %>%
  group_by(fy_quarter) %>%
  summarize(screenings = sum(count_screens)) %>%
  ungroup()

tail(annual)
# 1 2024/25_4       12236
# 2 2025/26_1         213
# 3 2025/26_2           1
# 4 2025/26_3          49
# 5 2056/57_2           1
# 6 unrecorded          0
#
# Note that last four quarters have not happened yet (for this data set):
# AMc: 2025/26_3 have been checked with Highland - all test clinics, so safe to ignore I think

## Remove screen_result == NA
# to look at trend from previous six quarters
temp <- quarter %>%
  filter(!is.na(screen_result)) %>%
  mutate(count_screens = if_else(is.na(date_screen), 0, 1)) %>%
  group_by(fy_quarter) %>%
  summarize(screenings = sum(count_screens)) %>%
  ungroup()

tail(temp)
#  fy_quarter  screenings
# 1 2023/24_3        9043
# 2 2023/24_4        9444
# 3 2024/25_1        9183
# 4 2024/25_2        9111
# 5 2024/25_3        8797
# 6 2024/25_4        5424

rm(annual, temp)
####


### C. Check results ----
check_results <- validator("patient_attend" = att_dna == "05",
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

## Which HBs performed screens but have no result or f/up recommendation?
hb_norf <- quarter %>%
  filter(att_dna == "05", is.na(followup_recom)) %>% 
  arrange(hb_screen, fy_quarter)

table(droplevels(hb_norf$hb_screen))
# Grampian        Greater Glasgow & Clyde          Highland          Lothian 

table(droplevels(hb_norf$hb_screen), hb_norf$fy_quarter)
# Follow up with HBs if any occur in current fy_quarter
# AMc note: Lothian 2024/25_1 - has been contacted and reviewed

rm(hb_norf) ## ONLY DELETE IF NO NEED TO CONTACT HBs!


### D. Check audits ----
check_audits <- validator("total_audits" = audit_flag == "01",
                          "audit_fail" = audit_result == "02",
                          "not_recorded_fail_reason" = audit_result == "02" & 
                            is.na(audit_fail_reason),
                          "not_recorded_fail_detail" = !is.na(audit_fail_reason) & 
                            is.na(audit_fail_1),
                          "not_recorded_audit_outcome" = audit_result == "02" &
                            is.na(audit_outcome),
                          "not_recorded_batch_fail" = !is.na(audit_batch_fail),
                          "not_recorded_batch_outcome" = !is.na(audit_batch_fail) &
                            is.na(audit_batch_outcome))

review_audits <- confront(quarter, check_audits, key  ="id")
summary(review_audits)

## Recall advice for audits that failed
table(quarter$audit_result, quarter$audit_outcome, useNA = "ifany")
## Key for audit_result:
# 01 Standard met
# 02 Standard not met
## Key for audit_outcome:
# 01 Immediate recall
# 02 Recall in current cycle
# 03 No Recall – Satisfactory interim scan
# 04 No Recall - Referred to vascular
# 05 No Recall - Verified by 2nd opinion

## Which HBs did not record the reason an audit failed?
hb_no_fail_reason <- quarter %>%
  filter(audit_result == "02", is.na(audit_fail_reason)) %>% 
  arrange(hb_screen, fy_quarter)

table(droplevels(hb_no_fail_reason$hb_screen))
# Borders 

table(droplevels(hb_no_fail_reason$hb_screen), hb_no_fail_reason$fy_quarter)
# Follow up with HBs if any occur in current fy_quarter

rm(hb_no_fail_reason) ## ONLY DELETE IF NO NEED TO CONTACT HBs!

## Which HBs did not record an audit outcome?
hb_no_outcome <- quarter %>%
  filter(audit_result == "02", is.na(audit_outcome)) %>% 
  arrange(hb_screen, fy_quarter)

table(droplevels(hb_no_outcome$hb_screen))
# Borders 

table(droplevels(hb_no_outcome$hb_screen), hb_no_outcome$fy_quarter)
# Follow up with HBs if any occur in current fy_quarter

rm(hb_no_outcome) ## ONLY DELETE IF NO NEED TO CONTACT HBs!


### E. Derived variable audits ----
table(quarter$aaa_size_group)

very_large <- quarter[(quarter$aaa_size_group == "very large error" &
                         !is.na(quarter$aaa_size_group)),]

table(very_large$fy_quarter)
# Follow up with HBs if any occur in current fy_quarter

rm(very_large) ## ONLY DELETE IF NO NEED TO CONTACT HBs!


#### 4a. Combine checks -- summaries ####
# Not sure what to do with this for now. Can be compared to previous run's
# output (create an archive file?) to see changes??
## Make summaries objects
roots <- summary(review_roots)
dates <- summary(review_dates)
results <- summary(review_results)
audits <- summary(review_audits)

## Combine
summary_checks <- rbind(roots, dates, results, audits) %>% 
  glimpse()


#### 4b. Combine checks -- dataframe ####
# The final output of this pathway mimics what was created in SPSS, which was 
# eyeball-compared to previous run. Can create archive with this instead of 
# above for R comparison...?
## Make data.frames
review_roots_df <- as.data.frame(review_roots)
review_dates_df <- as.data.frame(review_dates)
review_results_df <- as.data.frame(review_results)
review_audits_df <- as.data.frame(review_audits)

## Join and rearrange
review <- rbind(review_roots_df, review_dates_df, 
                review_results_df, review_audits_df) %>%
  pivot_wider(id_cols = c('id'),
              names_from = c('name'),
              values_from = c('value')) %>% 
  # change logical vectors to integers to be able to summarize
  mutate(across(.cols = sex:not_recorded_batch_outcome, .fns = as.integer)) %>% 
  glimpse()

## Join back on to main dataset using key
quarter_checks <- quarter %>%
  left_join(review, by = c('id')) %>% 
  glimpse

# # To view results that stand out:
# quarter_checks %>% filter(!HB_res_screen == 1) %>%
#  View()

rm(check_roots, check_dates, check_results, check_audits,
   review_roots, review_dates, review_results, review_audits,
   review_roots_df, review_dates_df, review_results_df, review_audits_df,
   review, roots, dates, results, audits)


#### 5. Summarize ####
#### Scotland ----
summary_scot <- quarter_checks %>%
  group_by(fy_quarter) %>% 
  summarize(screening_n = sum(!is.na(screen_result)),
            patient_n = length(unique(upi)),
            attend_n = sum(patient_attend,na.rm=TRUE),
            missing_postcode_n = sum(missing_postcode,na.rm=TRUE),
            missing_simd_n = sum(missing_simd,na.rm=TRUE),
            missing_gp_n = sum(missing_gp,na.rm=TRUE),
            date_screen_before_offer_n = sum(date_offer_screen,na.rm=TRUE),
            date_result_before_screen_n = sum(date_screen_result,na.rm=TRUE),
            date_verified_before_result_n = sum(date_result_verified,na.rm=TRUE),
            date_referral_before_verified_n = sum(date_verified_referral,na.rm=TRUE),
            date_seenOP_before_refTrue_n = sum(date_refTrue_seenOP,na.rm=TRUE),
            date_death_before_surgery_n = sum(date_surgery_death,na.rm=TRUE),
            not_recorded_result_n = sum(not_recorded_result,na.rm=TRUE),
            not_recorded_followup_n = sum(not_recorded_followup,na.rm=TRUE),
            no_result_followup_n = sum(no_result_followup,na.rm=TRUE),
            invalid_measure1_n = sum(invalid_measure_1,na.rm=TRUE),
            invalid_measure2_n = sum(invalid_measure_2,na.rm=TRUE),
            audits_n = sum(audit_flag == "01"),
            audit_fail_n = sum(audit_result == "02"),
            not_recorded_fail_reason_n = sum(not_recorded_fail_reason,na.rm=TRUE),
            not_recorded_fail_detail_n = sum(not_recorded_fail_detail,na.rm=TRUE),
            not_recorded_batch_outcome_n = sum(not_recorded_batch_outcome,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(hbres = "Scotland", .before = fy_quarter) %>% 
  glimpse()


#### HBs ----
summary_hb <- quarter_checks %>%
  group_by(hbres, fy_quarter) %>% 
  summarize(screening_n = sum(!is.na(screen_result)),
            patient_n = length(unique(upi)),
            attend_n = sum(patient_attend,na.rm=TRUE),
            missing_postcode_n = sum(missing_postcode,na.rm=TRUE),
            missing_simd_n = sum(missing_simd,na.rm=TRUE),
            missing_gp_n = sum(missing_gp,na.rm=TRUE),
            date_screen_before_offer_n = sum(date_offer_screen,na.rm=TRUE),
            date_result_before_screen_n = sum(date_screen_result,na.rm=TRUE),
            date_verified_before_result_n = sum(date_result_verified,na.rm=TRUE),
            date_referral_before_verified_n = sum(date_verified_referral,na.rm=TRUE),
            date_seenOP_before_refTrue_n = sum(date_refTrue_seenOP,na.rm=TRUE),
            date_death_before_surgery_n = sum(date_surgery_death,na.rm=TRUE),
            not_recorded_result_n = sum(not_recorded_result,na.rm=TRUE),
            not_recorded_followup_n = sum(not_recorded_followup,na.rm=TRUE),
            no_result_followup_n = sum(no_result_followup,na.rm=TRUE),
            invalid_measure1_n = sum(invalid_measure_1,na.rm=TRUE),
            invalid_measure2_n = sum(invalid_measure_2,na.rm=TRUE),
            audits_n = sum(audit_flag == "01"),
            audit_fail_n = sum(audit_result == "02"),
            not_recorded_fail_reason_n = sum(not_recorded_fail_reason,na.rm=TRUE),
            not_recorded_fail_detail_n = sum(not_recorded_fail_detail,na.rm=TRUE),
            not_recorded_batch_outcome_n = sum(not_recorded_batch_outcome,na.rm=TRUE)) %>% 
  ungroup() %>% 
  glimpse()


summary <- rbind(summary_scot, summary_hb)


#### Save files ----
saveRDS(summary, paste0(wd_path, "/checks/aaa_checks_summary_", 
                        year, month, ".rds"))
saveRDS(summary_checks, paste0(wd_path, "/checks/aaa_checks_finyear_", 
                               year, month, ".rds"))

rm(summary_hb, summary_checks, quarter_checks)


#### 6. Compare ####
## Bring in records from previous extract run to do comparison of numbers
historic_checks <- readRDS(paste0(previous_path, "/checks/aaa_checks_summary_",
                                  previous, ".rds"))

# should match
names(historic_checks)
names(summary)

# should be same except most recent fy_quarters will have increased
table(historic_checks$fy_quarter)
table(summary$fy_quarter) 


hist_scot <- historic_checks[historic_checks$hbres == "Scotland",]


# should be one fy_quarter extra in summary_scot
table(hist_scot$fy_quarter)
table(summary_scot$fy_quarter) 
# add in blank rows for any additional fy_quarters to make up difference
# (number of observations should be equal before able to compare)
# will need to add one row each quarter; may need to add multiple if a data
# entry error has been made and a further fy_quarter has been added to dataset

hist_scot %<>% 
  add_row(hbres="Scotland", fy_quarter="2025/26_1", screening_n=0, patient_n=0,
          attend_n=0, missing_postcode_n=0, missing_simd_n=0, missing_gp_n=0,
          date_screen_before_offer_n=0, date_result_before_screen_n=0,
          date_verified_before_result_n=0, date_referral_before_verified_n=0,
          date_seenOP_before_refTrue_n=0, date_death_before_surgery_n=0,
          not_recorded_result_n=0, not_recorded_followup_n=0,
          no_result_followup_n=0, invalid_measure1_n=0, invalid_measure2_n=0, 
          audits_n=0,audit_fail_n=0,not_recorded_fail_reason_n=0,
          not_recorded_fail_detail_n=0,not_recorded_batch_outcome_n=0,
          # to calculate placement index, identify row index of the same fy_quarter
          # in summary_scot table and change number below to match
          .before = 53) %>% 
  # only need to use below in case of data entry error!
  # add_row(hbres="Scotland", fy_quarter="2025/26_3", screening_n=0, patient_n=0,
  #         attend_n=0, missing_postcode_n=0, missing_simd_n=0, missing_gp_n=0,
  #         date_screen_before_offer_n=0, date_result_before_screen_n=0,
  #         date_verified_before_result_n=0, date_referral_before_verified_n=0,
  #         date_seenOP_before_refTrue_n=0, date_death_before_surgery_n=0,
  #         not_recorded_result_n=0, not_recorded_followup_n=0,
  #         no_result_followup_n=0, invalid_measure1_n=0, invalid_measure2_n=0,
  #         audits_n=0,audit_fail_n=0,not_recorded_fail_reason_n=0,
  #         not_recorded_fail_detail_n=0,not_recorded_batch_outcome_n=0,
  #         # to calculate placement index, identify row index of the same fy_quarter
  #         # in summary_scot table and change number below to match
  #         .before = 53) %>%
  # above converts as.integer to numeric; needs to be converted back
  mutate(across(3:24, as.integer))


# should match
table(hist_scot$fy_quarter)
table(summary_scot$fy_quarter) 


## Compare historic v current
summary(comparedf(hist_scot, summary_scot))
# most (if not all) changes should be focused around the last handful of 
# data rows; look at comparisons in "Table: Differences detected..."
# Generally, numbers should increase from values.x to values.y


# ## Write out to checks folder
# # Is this needed??
# # Run manual checks in .csv files if needed.
# write_csv(hist_scot, paste0(wd_path, "/checks/Scotland_historic_", year, month, ".csv"))
# write_csv(summary_scot, paste0(wd_path, "/checks/Scotland_summary_", year, month, ".csv"))
