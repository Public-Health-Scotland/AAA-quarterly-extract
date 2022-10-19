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
#library(readr)
#library(phsmethods)
library(tidylog)


rm(list = ls())


## Values
year <- 2022
month <- "09"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                 "/", year, month)