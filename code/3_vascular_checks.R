##########################################################
# 3_vascular_checks.R
# Karen Hotopp
# 24/10/2022
# Script 3 of 3
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
year <- 2023
month <- "03"
date_extract <- "2023-03-01"
# Cutoff should be day extract prepared... should it be last day of previous month??
date_cutoff <- "2023-03-01" 
today <- paste0("Workbook created ", Sys.Date())


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts", "/", year, month)


#### 2: Call in extract ####
quarter <- read_rds(paste0(wd_path, "/output/aaa_extract_", year, month, ".rds")) %>% 
  # select cases with vascular referrals 
  filter(!is.na(date_referral_true),
         date_screen <= as.Date(date_cutoff)) %>%
  # # need to preserve NA values in result outcome for validation checks **WHY??**
  # mutate(result_outcome = if_else(is.na(result_outcome),
  #                                 "99", result_outcome)) %>%
  # id variable for matching validator checks
  mutate(id = row_number(), .before = financial_year) %>% 
  glimpse()

range(quarter$date_screen)
# "2012-08-13" "2023-02-23"
range(quarter$date_referral_true)
# "2012-08-15" "2023-02-25"


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

## Check: these will likely be ongoing cases with most recent financial year;
# can also check result_outcome, as likely cases that are ongoing.
no_OPdate <- quarter_date[is.na(quarter_date$date_seen_outpatient),]
table(no_OPdate$result_outcome, useNA = "ifany")

rm(no_OPdate, quarter_date)


### C. Check result outcomes ----
quarter_outcome <- quarter %>%
  filter(largest_measure >= 5.5) %>%
  mutate(days_screen_extract = as.Date(date_extract) - date_screen, # needed?
         days_screen_death = date_death - date_screen,
         days_surgery_death = date_death - date_surgery) %>% 
  glimpse()

check_outcomes <- validator("result_outcome_na" = is.na(result_outcome),
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
                              c("09","10","14","17","18","19"))

review_outcomes <- confront(quarter_outcome, check_outcomes, key  ="id")
summary(review_outcomes)

## Check: these will likely be ongoing cases with most recent financial year.
no_outcome <- quarter[is.na(quarter$result_outcome),]
table(no_outcome$result_outcome, useNA = "ifany") ## Is this correct? Seems a little obvious...

rm(no_outcome, quarter_outcome)


#### 4. Create data subsets ####

### Vascular appointment not needed ---
## Records list
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

## HBs
appt_notreq_hb <- appt_notreq %>% 
  group_by(hbres) %>% 
  count(hbres) %>% 
  ungroup() %>% 
  pivot_wider(names_from = hbres, values_from = n)

## HBs by year
appt_notreq_year <- appt_notreq %>% 
  arrange(financial_year) %>% 
  group_by(hbres, financial_year) %>% 
  count(hbres) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = n, 
              names_sort =TRUE) %>% 
  replace(is.na(.), 0)


### Non-final outcomes ---
## Records list
nonfinal <- quarter %>%
  filter(result_outcome %in% c("09","10","14","17","18","19")) %>% 
  select(financial_year, fy_quarter, upi, hbres,
         date_screen, screen_type, largest_measure,
         screen_result, followup_recom, 
         date_referral, date_referral_true, date_seen_outpatient,
         result_outcome, first_outcome, referral_error_manage,
         surg_method, date_surgery, date_death, aneurysm_related) %>%
  arrange(hbres, fy_quarter)

## HBs
nonfinal_hb <- nonfinal %>% 
  group_by(hbres) %>% 
  count(hbres) %>% 
  ungroup() %>% 
  pivot_wider(names_from = hbres, values_from = n)

## HBs by year
nonfinal_year <- nonfinal %>% 
  arrange(financial_year) %>% 
  group_by(hbres, financial_year) %>% 
  count(hbres) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = n, 
              names_sort =TRUE) %>% 
  replace(is.na(.), 0)


### Other final outcomes ---
## Records list
otherfinal <- quarter %>%
  filter(result_outcome == "20") %>% 
  select(financial_year, fy_quarter, upi, hbres,
         date_screen, screen_type, largest_measure,
         screen_result, followup_recom, 
         date_referral, date_referral_true, date_seen_outpatient,
         result_outcome, first_outcome, referral_error_manage,
         surg_method, date_surgery, date_death, aneurysm_related) %>%
  arrange(hbres, fy_quarter)

## HBs
otherfinal_hb <- otherfinal %>% 
  group_by(hbres) %>% 
  count(hbres) %>% 
  ungroup() %>% 
  pivot_wider(names_from = hbres, values_from = n)

## HBs by year
otherfinal_year <- otherfinal %>% 
  arrange(financial_year) %>% 
  group_by(hbres, financial_year) %>% 
  count(hbres) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = n, 
              names_sort =TRUE) %>% 
  replace(is.na(.), 0)

table(appt_notreq$largest_measure, appt_notreq$result_outcome)
table(appt_notreq$referral_error_manage)
## Key for referral_error_manage:
# 01 Discharged
# 02 Surveillance 3 months
# 03 Surveillance 12 months


### Outcomes for patients who died ---
## Records list
mort <- quarter %>%
  filter(result_outcome %in% c("04","05","07","12","16")) %>% 
  select(financial_year, fy_quarter, upi, hbres,
         date_screen, screen_type, largest_measure,
         screen_result, followup_recom, 
         date_referral, date_referral_true, date_seen_outpatient,
         result_outcome, first_outcome, surg_method, 
         date_surgery, date_death, aneurysm_related) %>% 
  arrange(hbres, fy_quarter)

## HBs
mort_hb <- mort %>% 
  group_by(hbres) %>% 
  count(hbres) %>% 
  ungroup() %>% 
  pivot_wider(names_from = hbres, values_from = n)

## HBs by year
mort_year <- mort %>% 
  arrange(financial_year) %>% 
  group_by(hbres, financial_year) %>% 
  count(hbres) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = n, 
              names_sort =TRUE) %>% 
  replace(is.na(.), 0)


#### 5. Write to Excel ####
## Will need to have conversation with Garrick et al. about how this is sent
## out and how often (annually v with each quarter??).
## Then, will be able to decide final format of printing out to Excel:
## Currently printing out into a single workbook to send to leads, but better
## to send to HBs??

### Setup workbook ---
## Reset variable names
records_var <- c("Financial Year", "FY and Quarter", "UPI", "HB of Residence",
                 "Date Screened", "Screening Type", "Largest Measurement",
                 "Screening Result", "Follow-up Recommendation", "Date of Referral",
                 "Actual Date of Referral", "Date Seen in Outpatient", 
                 "Result Outcome", "First Outcome", "Referral Error Management", 
                 "Surgery Method", "Date of Surgery", "Date of Death", 
                 "Aneurysm Related Death")
records_mort <- c("Financial Year", "FY and Quarter", "UPI", "HB of Residence",
                  "Date Screened", "Screening Type", "Largest Measurement",
                  "Screening Result", "Follow-up Recommendation", "Date of Referral",
                  "Actual Date of Referral", "Date Seen in Outpatient", 
                  "Result Outcome", "First Outcome", "Surgery Method", 
                  "Date of Surgery", "Date of Death", "Aneurysm Related Death")

## Styles
title_style <- createStyle(fontSize = 12, halign = "Left", textDecoration = "bold")
table_style <- createStyle(valign = "Bottom", halign = "Left",
                           border = "TopBottomLeftRight")

## Titles
title_date <- paste0(month.name[as.numeric(month)], " ", year)

## Setup workbook
wb <- createWorkbook()
options("openxlsx.borderStyle" = "thin")
modifyBaseFont(wb, fontSize = 11, fontName = "Arial")


### Vascular appointment not needed ---
addWorksheet(wb, sheetName = "Appointment not needed", gridLines = FALSE)
writeData(wb, sheet = "Appointment not needed", appt_notreq_hb, startRow = 5)
writeData(wb, sheet = "Appointment not needed", appt_notreq_year, startRow = 9)
writeDataTable(wb, sheet = "Appointment not needed", appt_notreq, startRow = 25)

# titles
title_appt_notreq <- "Vascular appointment not needed by health board and financial year"
writeData(wb, "Appointment not needed", title_appt_notreq, startRow = 1, startCol = 1)
writeData(wb, "Appointment not needed", title_date, startRow = 2, startCol = 1)
writeData(wb, "Appointment not needed", today, startRow = 3, startCol = 1)
addStyle(wb, "Appointment not needed", title_style, rows = 1:2, cols = 1)

# table headers
names(appt_notreq) <- records_var
addStyle(wb, "Appointment not needed", title_style, rows = 5, cols = 1:ncol(appt_notreq_hb))
addStyle(wb, "Appointment not needed", title_style, rows = 9, cols = 1:ncol(appt_notreq_year))
addStyle(wb, "Appointment not needed", title_style, rows = 25, cols = 1:ncol(appt_notreq))

# tables
addStyle(wb, "Appointment not needed", table_style, rows = 5:6, 
         cols = 1:ncol(appt_notreq_hb), gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Appointment not needed", table_style, rows = 9:(9+nrow(appt_notreq_year)), 
         cols = 1:ncol(appt_notreq_year), gridExpand = TRUE, stack = TRUE)

setColWidths(wb, "Appointment not needed", cols = 1:ncol(appt_notreq), 
             widths = "auto")


### Non-final outcomes ----
addWorksheet(wb, sheetName = "Non-final Outcomes", gridLines = FALSE)
writeData(wb, sheet = "Non-final Outcomes", nonfinal_hb, startRow = 5)
writeData(wb, sheet = "Non-final Outcomes", nonfinal_year, startRow = 9)
writeDataTable(wb, sheet = "Non-final Outcomes", nonfinal, startRow = 25)

# titles
title_nonfinal <- "Non-final outcomes by health board and financial year"
writeData(wb, "Non-final Outcomes", title_nonfinal, startRow = 1, startCol = 1)
writeData(wb, "Non-final Outcomes", title_date, startRow = 2, startCol = 1)
writeData(wb, "Non-final Outcomes", today, startRow = 3, startCol = 1)
addStyle(wb, "Non-final Outcomes", title_style, rows = 1:2, cols = 1)

# table headers
names(nonfinal) <- records_var
addStyle(wb, "Non-final Outcomes", title_style, rows = 5, cols = 1:ncol(nonfinal_hb))
addStyle(wb, "Non-final Outcomes", title_style, rows = 9, cols = 1:ncol(nonfinal_year))
addStyle(wb, "Non-final Outcomes", title_style, rows = 25, cols = 1:ncol(nonfinal))

# tables
addStyle(wb, "Non-final Outcomes", table_style, rows = 5:6, 
         cols = 1:ncol(nonfinal_hb), gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Non-final Outcomes", table_style, rows = 9:(9+nrow(nonfinal_year)), 
         cols = 1:ncol(nonfinal_year), gridExpand = TRUE, stack = TRUE)

setColWidths(wb, "Non-final Outcomes", cols = 1:ncol(nonfinal), 
             widths = "auto")


### Other final outcomes ----
addWorksheet(wb, sheetName = "Other Final Outcomes", gridLines = FALSE)
writeData(wb, sheet = "Other Final Outcomes", otherfinal_hb, startRow = 5)
writeData(wb, sheet = "Other Final Outcomes", otherfinal_year, startRow = 9)
writeDataTable(wb, sheet = "Other Final Outcomes", otherfinal, startRow = 25)

# titles
title_otherfinal <- "Other final outcomes by health board and financial year"
writeData(wb, "Other Final Outcomes", title_otherfinal, startRow = 1, startCol = 1)
writeData(wb, "Other Final Outcomes", title_date, startRow = 2, startCol = 1)
writeData(wb, "Other Final Outcomes", today, startRow = 3, startCol = 1)
addStyle(wb, "Other Final Outcomes", title_style, rows = 1:2, cols = 1)

# table headers
names(otherfinal) <- records_var
addStyle(wb, "Other Final Outcomes", title_style, rows = 5, cols = 1:ncol(otherfinal_hb))
addStyle(wb, "Other Final Outcomes", title_style, rows = 9, cols = 1:ncol(otherfinal_year))
addStyle(wb, "Other Final Outcomes", title_style, rows = 25, cols = 1:ncol(otherfinal))

# tables
addStyle(wb, "Other Final Outcomes", table_style, rows = 5:6, 
         cols = 1:ncol(otherfinal_hb), gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Other Final Outcomes", table_style, rows = 9:(9+nrow(otherfinal_year)), 
         cols = 1:ncol(otherfinal_year), gridExpand = TRUE, stack = TRUE)

setColWidths(wb, "Other Final Outcomes", cols = 1:ncol(otherfinal), 
             widths = "auto")


### Deaths ----
addWorksheet(wb, sheetName = "Deaths", gridLines = FALSE)
writeData(wb, sheet = "Deaths", mort_hb, startRow = 5)
writeData(wb, sheet = "Deaths", mort_year, startRow = 9)
writeDataTable(wb, sheet = "Deaths", mort, startRow = 25)

# titles
title_mort <- "Deaths by health board and financial year"
writeData(wb, "Deaths", title_mort, startRow = 1, startCol = 1)
writeData(wb, "Deaths", title_date, startRow = 2, startCol = 1)
writeData(wb, "Deaths", today, startRow = 3, startCol = 1)
addStyle(wb, "Deaths", title_style, rows = 1:2, cols = 1)

# table headers
names(mort) <- records_mort
addStyle(wb, "Deaths", title_style, rows = 5, cols = 1:ncol(mort_hb))
addStyle(wb, "Deaths", title_style, rows = 9, cols = 1:ncol(mort_year))
addStyle(wb, "Deaths", title_style, rows = 25, cols = 1:ncol(mort))

# tables
addStyle(wb, "Deaths", table_style, rows = 5:6, 
         cols = 1:ncol(mort_hb), gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Deaths", table_style, rows = 9:(9+nrow(mort_year)), 
         cols = 1:ncol(mort_year), gridExpand = TRUE, stack = TRUE)

setColWidths(wb, "Deaths", cols = 1:ncol(mort), widths = "auto")


### Save ----
saveWorkbook(wb, paste0(wd_path, "/output/Vascular_checks_", 
                        year, month, ".xlsx"), overwrite = TRUE)
