##########################################################
# 1_process_extracts.R
# Karen Hotopp
# 14/10/2022
# Script 1 of ?
# Imports and formats BOXI extracts for AAA quarterly review
# Outputs are checked in further scripts;
# March and September outputs used for MEG reports
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

#### 1: Housekeeping ####
## Packages
library(here)
library(dplyr)
library(haven)
#library(forcats)
#library(readr)
library(tidylog)


rm(list = ls())


## Values
year <- 2022
month <- "09"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                 "/", year, month)

simd_path <- paste0("/conf/linkage/output/lookups/Unicode/Geography",
                    "/Scottish Postcode Directory",
                    "/postcode_2022_2_simd2020v2.rds")

gp_path <- paste0("/conf/linkage/output/lookups/Unicode/National Reference Files",
                  "/gpprac.sav")
##!! KH: I don't see an RDS (or CSV) version of this file... Where to get?



#### 2: Main extract ####

quart <- read.csv(paste0(wd_path, "/raw_data/ISD.CSV")) %>%
  glimpse()

##!! KH: SPSS code has 523774 records (1 higher than R)


names(quart) <- c("chi", "upi", "surname", "forename", "dob", "postcode", 
                  "hbres", "sex", "pat_elig", "pat_inelig", "location", 
                  "date_screen", "att_dna", "screen_type", "screen_exep",
                  "date_result", "followup_recom", "screen_result", "date_referral",
                  "result_outcome", "surg_method", "audit_flag", "audit_result", 
                  "audit_outcome", "offer_sent", "date_surgery", 
                  "date_seen_outpatient", "aneurysm_related", "apl_measure",
                  "apt_measure", "largest_measure", "result_verified", "date_verified",
                  "date_death", "date_referral_true", "first_outcome", 
                  "audit_fail_reason", "audit_fail_1", "audit_fail_2", 
                  "audit_fail_3", "audit_fail_4", "audit_fail_5", 
                  "audit_batch_fail", "audit_batch_outcome",
                  "referral_error_manage", "practice code", "surgery_hb")

table(quart$surgery_hb)


##!! KH: add in mutate dates, etc.
##!! Also change board names!

