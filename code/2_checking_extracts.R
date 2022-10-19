##########################################################
# 2_checking_extracts.R
# Karen Hotopp
# 19/10/2022
# Script 2 of ?
# 
# Checks processed BOXI extracts for AAA quarterly review
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


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                 "/", year, month)


#### 2: Call in extract ####
## Financial years and quarters ~~~
quarter <- read_rds(paste0(wd_path, "/output/aaa_extract_202209.rds")) %>% 
  # exclude cases with results obtained outside of Scotland 
  filter(!screen_result %in% c("05", "06")) %>% 
  # create financial year/quarter from screening date
  mutate(financial_year = extract_fin_year(date_screen),
         financial_quarter = qtr(date_screen, format="short")) %>% 
  mutate(id = row_number(), .before = chi) %>% 
  glimpse()

quarter %<>%
  # financial_quarter should be numeric
  mutate(financial_quarter = str_sub(financial_quarter, 1, 3),
         financial_quarter = case_when(financial_quarter == "Jan" ~ 4,
                                       financial_quarter == "Apr" ~ 1,
                                       financial_quarter == "Jul" ~ 2,
                                       financial_quarter == "Oct" ~ 3),
         fy_quarter = paste0(financial_year, " ", financial_quarter)) %>% 
  mutate(fy_quarter = if_else(fy_quarter == "NA NA", "", fy_quarter)) %>%  
  arrange(upi, fy_quarter) %>% 
  glimpse()

table(quarter$fy_quarter, useNA = "ifany")

length((unique(quarter$upi)))
# 366,571 of 523071 records

## Screenings
annual <- quarter %>% 
  mutate(count_screens = if_else(is.na(date_screen), 0, 1)) %>% 
  group_by(financial_year) %>% 
  summarize(screenings = sum(count_screens)) %>% 
  ungroup()

  
  
  
  
  
  
  
  
  

#### 3. Validate data ####
### A. Check dates ----
checks <- validator("sex" = sex %in% "01",
                    "date_offer_screen" = date_offer_sent <= date_screen,
                    "date_screen_result" = date_screen <= date_result,
                    "date_result_verified" = date_result < date_verified,
                    "date_verified_referral" = date_verified < date_referral,
                    "date_referral_refTrue" = date_referral < date_referral_true,
                    "date_refTrue_seen_outpatient" = date_referral_true < date_seen_outpatient, # Is this accurate??
                    "date_seen_outpatient_surgery" = date_seen_outpatient < date_surgery, # Is this accurate??
                    "date_surgery_death" = date_surgery < date_death, # Is this accurate??
                    "correct_FY_Q" = fy_quarter <= "2022/23 2")

review <- confront(quarter, checks, key  ="id")
summary(review)

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
quarter_check %>% filter(!correct_FY_Q == TRUE) %>%
  View()
## Why are there so many (1,056) that rec'd their screening past Sept 2022?



## Check financial years for outliers
table(quarter$financial_year) 
a <- quarter[quarter$financial_year == "2025/26",]
b <- quarter[quarter$financial_year == "2056/57",]
# These should be checked out; to send to ...?
table(quarter$financial_quarter) 




names(quarter)







