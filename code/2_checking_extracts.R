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

## Some things to think about:
## What should this script be doing? Are there outputs and if so where do they
## go? Is it reasonable to make graphical outputs for people running script to
## interpret the data?
## Do we need to create an Excel file?
## Conversly, if we find issues in the data, would it be worth creating outputs 
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


## Values
year <- 2022
month <- "09"
# Financial year and quarter of current extract
fyq_current <- "2022/23_2"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                 "/", year, month)
previous_path <- paste0("PHI_conf/AAA/Topics/Screening/extracts",
                        "/202206") # Don't have a fancy way to automate this, so will need to just be hand-entered

#### 2: Call in extract ####
quarter <- read_rds(paste0(wd_path, "/output/aaa_extract_202209.rds")) %>% 
  # exclude cases with results obtained outside of Scotland 
  filter(!screen_result %in% c("05", "06")) %>% 
  # id variable for matching validator checks
  mutate(id = row_number(), .before = financial_year) %>% 
  glimpse()
  
length(unique(quarter$upi))
# 366,571 of 523071 records


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
                         "correct_FYQ" = fy_quarter > fyq_current)

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
table(droplevels(hb_norf$hb_screen), hb_norf$fy_quarter)


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
table(quarter$audit_result, quarter$audit_outcome)
## Key for audit_result:
# 01 – Standard met
# 02 – Standard not met
## Key for audit_outcome:
# 01 - Immediate recall
# 02 - Recall in current cycle
# 03 - No Recall – Satisfactory interim scan
# 04 - No Recall - Referred to vascular
# 05 - No Recall - Verified by 2nd opinion


### E. Derived variable audits ----
table(quarter$aaa_size_group)

very_large <- quarter[(quarter$aaa_size_group == "very large error" &
                         !is.na(quarter$aaa_size_group)),]
## Check if these are current FY/quarter; if so, should be sent back to HBs for checking
table(very_large$fy_quarter)

## Then recode to "large"
# q1 <- quarter %>%
#   mutate(aaa_size_group = recode(aaa_size_group,
#                                  "very large error" = "large"))
## Should this be removed? Would be better to keep both & change filter to 
## include both where processed in scripts?


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

rm(check_roots, check_dates, check_results, check_audits,
   review_roots, review_dates, review_results, review_audits,
   review_roots_df, review_dates_df, review_results_df, review_audits_df,
   roots, dates, results, audits)

# # View results that stand out
# quarter_checks %>% filter(!HB_res_screen == 1) %>%
#   View()


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
            date_surgery_before_seenOP_n = sum(date_seenOP_surgery,na.rm=TRUE),
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
  # # pivot to have displayed across fy_quarters; not needed?
  # pivot_longer(cols = "screening_n":"not_recorded_batch_outcome_n",
  #              names_to = "summaries", values_to = "values") %>%
  # mutate(values = if_else(is.na(values), 0, as.numeric(values)),
  #        fy_quarter = if_else(fy_quarter == "NA_NA",
  #                             "unrecorded", fy_quarter)) %>%
  # pivot_wider(names_from = fy_quarter,
  #             values_from = values) %>%
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
            date_surgery_before_seenOP_n = sum(date_seenOP_surgery,na.rm=TRUE),
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


#### 6. Compare ####
## Bring in records from previous extract run to do comparison of numbers
old_path <-paste0("/PHI_conf/AAA/Portfolio/Data/RoutineExtracts",
                  "/", year, "0601") # delete in future, once files are moved to new set-up
historic_checks <- readRDS(paste0(old_path, "/aaa_checks_summary_202206.rds")) 
# change 'old_path' for 'previous_path' once new folders set up

names(historic_checks)
names(summary)

table(historic_checks$fy_quarter)
table(summary$fy_quarter) # should be same except most recent fy_quarters will have increased

hist_scot <- historic_checks[historic_checks$hbres == "Scotland",]


table(hist_scot$fy_quarter)
table(summary_scot$fy_quarter) # should be same except most recent fy_quarters will have increased


summary(comparedf(hist_scot, summary_scot))


write_csv(hist_scot, paste0(wd_path, "/temp/Scotland_historic.csv"))
write_csv(summary_scot, paste0(wd_path, "/temp/Scotland_summary.csv"))
