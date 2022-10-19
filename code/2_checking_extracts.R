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
#library(haven)
library(magrittr)
#library(stringr)
#library(forcats)
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


#### 2: Main extract ####
## Import ~~~
quarter <- read_rds(paste0(wd_path, "/output/aaa_extract_202209.rds")) %>% 
  # exclude cases with results obtained outside of Scotland 
  filter(!screen_result %in% c("05", "06")) %>% 
  mutate(financial_year = extract_fin_year(date_screen)) %>% 
  mutate(qtr(date_screen, format="short")) %>% 
  glimpse()


## Check financial years for outliers
table(quarter$financial_year) 
a <- quarter[quarter$financial_year == "2025/26",]
b <- quarter[quarter$financial_year == "2056/57",]
# These should be checked out; to send to ...?

















