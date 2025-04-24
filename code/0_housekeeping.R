#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1_process_extracts.R
# Aoife McCarthy
# April 2025
# 
# Creates all housekeeping variables to be used throughout the main scripts
# 
# Written/run on Posit WB
# R version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(stringr)

# Values to change every run ----------------------------------------------

# YYYY and MM of quarterly run (e.g. 12 for Dec, 03 for March)
yymm <- "202503"

# date data was downloaded from Atos - should always be 1st of quarterly month
date_download <- "20250301"

# previous yymm value (3 months prior to current yymm)
previous <- "202412"


# Values to change when lookups change ------------------------------------

## script 1 ----

gp_path <- paste0("/conf/linkage/output/lookups/Unicode/National Reference Files",
                  "/gpprac.csv") 

simd_path <- paste0("/conf/linkage/output/lookups/Unicode/Deprivation",
                    "/postcode_2024_2_simd2020v2.rds")



# Values that shouldn't need changing -------------------------------------

## all scripts ----

# working directory path
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts/", yymm)


## script 1 ----

# Financial year and quarter of current extract
#March = q1
#June = q2
#Sept = q3
#Dec = q4
fyq_current <- paste0(substr(yymm, 1, 4), "/",
                      as.numeric(substr(yymm, 3, 4)) + 1,
                      "_",
                      as.numeric(substr(yymm, 5, 6))/3)


## script 2 ----

# previous run working directory
previous_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", previous)

## script 3 ----

# Extract date should be date extract created, which should be 1st of quarter
date_extract <- paste0(substr(date_download, 1, 4), "-",
                       substr(date_download, 5, 6), "-",
                       substr(date_download, 7, 8))
  

# Cutoff should be the same as date extract
date_cutoff <- date_extract

## script 4 ----


















