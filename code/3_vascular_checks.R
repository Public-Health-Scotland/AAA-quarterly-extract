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
#library(here)
library(dplyr)
#library(magrittr)
library(stringr)
#library(forcats)
library(readr)
#library(phsmethods)
library(validate)
library(tidylog)


rm(list = ls())


## Values
year <- 2022
month <- "09"
date_extract <- "2022-09-14"

# Financial year and quarter of current extract
fyq_current <- "2022/23_2"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                 "/", year, month)


#### 2: Call in extract ####
quarter <- read_rds(paste0(wd_path, "/output/aaa_extract_202209.rds")) %>% 
  # select cases with vascular referrals 
  filter(!is.na(date_referral_true),
         largest_measure >= 5.5) %>% 
  # need to preserve NA values in result outcome for validation checks
  mutate(result_outcome = if_else(is.na(result_outcome), 
                                  "99", result_outcome)) %>% 
  filter(result_outcome != "02") %>%
  # id variable for matching validator checks
  mutate(id = row_number(), .before = financial_year) %>% 
  mutate(total_referrals = 1) %>% 
  glimpse()

range(quarter$date_referral_true)
# "2012-08-15" "2022-09-14"


#### 3. Validate data ####
## Rules are written such that a "pass" is an outlier to be investigated.
## For example, if date_death is before date_surgery, that is a "pass".
## This allows pass results (logical TRUE) to more easily be converted to 
## an integer for summarizing (sections 4 & 5).

### A. Check referral data ----
check_refs <- validator("not_recorded_recommend" = result_outcome %in% c("05","06")
                        & is.na(referral_error_manage)) #,
                         "missing_simd" = is.na(simd2020v2_sc_quintile),
                         "missing_gp" = is.na(practice_code))

review_roots <- confront(quarter, check_refs, key  ="id")
summary(review_roots)












