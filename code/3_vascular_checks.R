##########################################################
# 3_vascular_checks.R
# Karen Hotopp
# 24/10/2022
# Script 3 of ?
# 
# Checks processed BOXI extracts and checks vascular data
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

## This can probably be added to script 2


## Notes from SPSS: 
## August 2022: going forward these checks need updated to include the board of surgery. field
## Boards added restropective data for board of surgery during summer 2022.


#### 1: Housekeeping ####
## Packages
library(dplyr)
library(stringr)
library(readr)
library(validate)
library(openxlsx)
library(tidylog)


rm(list = ls())


## Values
year <- 2022
month <- "09"
date_extract <- "2022-09-14"
date_cutoff <- "2022-09-01" # Should be day extract prepared... should it be last day of previous month??


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                 "/", year, month)


#### 2: Call in extract ####
quarter <- read_rds(paste0(wd_path, "/output/aaa_extract_202209.rds")) %>% 
  # select cases with vascular referrals 
  filter(!is.na(date_referral_true),
         date_screen <= as.Date(date_cutoff)) %>%
  # need to preserve NA values in result outcome for validation checks **WHY??**
  mutate(result_outcome = if_else(is.na(result_outcome),
                                  "99", result_outcome)) %>%
  # id variable for matching validator checks
  mutate(id = row_number(), .before = financial_year) %>% 
  glimpse()

range(quarter$date_screen)
# "2012-08-13" "2022-09-01"
range(quarter$date_referral_true)
# "2012-08-15" "2022-09-02"


#### 3. Validate data ####
## Rules are written such that a "pass" is an outlier to be investigated.
## For example, if date_death is before date_surgery, that is a "pass".
## This allows pass results (logical TRUE) to more easily be converted to 
## an integer for summarizing (sections 4 & 5).

### A. Referred in error ----
check_refs <- validator("not_recorded_recommend" = result_outcome %in% c("02","06")
                        & is.na(referral_error_manage))

review_refs <- confront(quarter, check_refs, key  ="id")
summary(review_refs)


### B. Check dates ----
## Remove patients where OP appointment not needed
quarter_date <- quarter %>%
  filter(result_outcome != "02",
         largest_measure >= 5.5) %>%
  glimpse()

check_dates <- validator("date_screen_extract" = date_screen > date_extract,
                         "date_surgery_na" = is.na(date_surgery),
                         "date_ref_refTrue" = financial_year > "2015/16" &
                           abs(date_referral - date_referral_true) > 3,
                         "date_seenOP_na" = is.na(date_seen_outpatient),
                         "date_death_na" = result_outcome %in% c("04","05","07","12","16") &
                           is.na(date_death),
                         "date_refTrue_screen" = date_referral_true < date_screen,
                         "date_seenOP_screen" = date_seen_outpatient < date_screen,
                         "date_surgery_screen" = date_surgery < date_screen,
                         "date_surgery_seenOP" = !is.na(date_surgery) &
                           date_surgery < date_seen_outpatient)

review_dates <- confront(quarter_date, check_dates, key  ="id")
summary(review_dates)


### C. Check result outcomes ----
quarter_outcome <- quarter %>%
  filter(largest_measure >= 5.5) %>%
  mutate(days_screen_extract = as.Date(date_extract) - date_screen, # needed?
         days_screen_death = date_death - date_screen,
         days_surgery_death = date_death - date_surgery) %>% 
  glimpse()

check_outcomes <- validator("result_outcome_na" = result_outcome == "99", # not sure why can't leave as NAs
                            "outcome_no_OP" = result_outcome %in% c("01","02","03","04","05") &
                              (!is.na(date_seen_outpatient) | !is.na(surg_method) |
                              !is.na(date_surgery)),
                            "outcome_no_OP_died04" = result_outcome == "04" &
                              days_screen_death > 10,
                            "outcome_no_OP_died05" = result_outcome == "05" &
                              days_screen_death <= 10,
                            "outcome_no_surgery" = result_outcome %in% 
                              c("06","07","08","09","10","11","12","13","14","18") &
                              (!is.na(surg_method) | !is.na(date_surgery)),
                            "outcome_surgery" = result_outcome %in% c("15","16") &
                              (is.na(surg_method) | is.na(date_surgery)),
                            "outcome_surgery15" = result_outcome == "15" &
                              days_surgery_death <= 30,
                            "outcome_surgery16" = result_outcome == "16" &
                              days_surgery_death > 30,
                            "outcome_surg_abandon" = surg_method == "03" &
                              (result_outcome != "20" | is.na(date_surgery)),
                            "outcome_no_final" = result_outcome %in% 
                              c("09","10","14","17","18","19"))#,

review_outcomes <- confront(quarter_outcome, check_outcomes, key  ="id")
summary(review_outcomes)


#### 4. Create data subsets ####
### Non-final outcomes
nonfinal <- quarter %>%
  filter(result_outcome %in% c("09","10","14","17","18","19")) %>% 
  select(financial_year, fy_quarter, upi, hbres,
         date_screen, screen_type, largest_measure,
         screen_result, followup_recom, 
         date_referral, date_referral_true, date_seen_outpatient,
         result_outcome, first_outcome, referral_error_manage,
         surg_method, date_surgery, date_death, aneurysm_related) %>%
  arrange(hbres, fy_quarter)

table(droplevels(nonfinal$hbres))
table(droplevels(nonfinal$hbres), nonfinal$financial_year)


### Other final outcomes
otherfinal <- quarter %>%
  filter(result_outcome == "20") %>% 
  select(financial_year, fy_quarter, upi, hbres,
         date_screen, screen_type, largest_measure,
         screen_result, followup_recom, 
         date_referral, date_referral_true, date_seen_outpatient,
         result_outcome, first_outcome, referral_error_manage,
         surg_method, date_surgery, date_death, aneurysm_related) %>%
  arrange(hbres, fy_quarter)

table(droplevels(otherfinal$hbres))
table(otherfinal$financial_year, droplevels(otherfinal$hbres))


### Vascular appointment not needed 
appt_notreq <- quarter %>%
  filter(largest_measure < 5.5 |
         result_outcome == "02") %>% 
  select(financial_year, fy_quarter, upi, hbres,
         date_screen, screen_type, largest_measure,
         screen_result, followup_recom, 
         date_referral, date_referral_true, date_seen_outpatient,
         result_outcome, first_outcome, referral_error_manage,
         surg_method, date_surgery, date_death, aneurysm_related) %>% 
  arrange(hbres, fy_quarter)

table(appt_notreq$financial_year, droplevels(appt_notreq$hbres))
table(appt_notreq$largest_measure, appt_notreq$result_outcome)
table(appt_notreq$referral_error_manage)


### Outcomes for patients who died
mort <- quarter %>%
  filter(result_outcome %in% c("04","05","07","12","16")) %>% 
  select(financial_year, fy_quarter, upi, hbres,
         date_screen, screen_type, largest_measure,
         screen_result, followup_recom, 
         date_referral, date_referral_true, date_seen_outpatient,
         result_outcome, first_outcome, surg_method,
         date_surgery, date_death, aneurysm_related) %>% 
  arrange(hbres, fy_quarter)

mort_table <- table(mort$financial_year, droplevels(mort$hbres))


#### 5. Write to Excel ####
## Will need to have conversation with Garrick et al. about how this is sent
## out and how often (annually v with each quarter??).
## Then, will be able to decide final format of printing out to Excel:
## Currently printing out into a single workbook to send to leads, but better
## to send to HBs??

wb <- createWorkbook()
options("openxlsx.borderStyle" = "thin")
modifyBaseFont(wb, fontSize = 11, fontName = "Arial")

addWorksheet(wb, sheetName = "Appointment not needed", gridLines = FALSE)
writeDataTable(wb, sheet = "Appointment not needed", appt_notreq, colNames = TRUE,
               startRow = 14)#, tableStyle = "tableStyle")



addWorksheet(wb, sheetName = "Outcomes", gridLines = FALSE)



addWorksheet(wb, sheetName = "Deaths", gridLines = FALSE)
writeDataTable(wb, sheet = "Deaths", as.data.frame(mort_table), colNames = TRUE,
               startRow = 1, tableStyle = "TableStyleLight9")
writeDataTable(wb, sheet = "Deaths", mort, colNames = TRUE,
               startRow = 14, tableStyle = "TableStyleLight9")
setColWidths(wb, "Deaths", cols = 1:ncol(mort), widths = "auto")



saveWorkbook(wb, paste0(wd_path, "/output/Vascular_checks_", 
                        year, month, ".xlsx"), overwrite = TRUE)
