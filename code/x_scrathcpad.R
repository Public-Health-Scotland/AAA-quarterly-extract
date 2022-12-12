##############################
### Scratchpad ###
##############################

#### Previous extract ####
## Create pathways in this script and then
## run script 1_process_extracts.R for June 2022 from line 70
## Save output to last run's Output folder

rm(list = ls())


## Values
year <- 2022
month <- "06"
fyq_current <- "2022/23_2"

## Pathways
old_path <-paste0("/PHI_conf/AAA/Portfolio/Data/RoutineExtracts",
                 "/", year, "0601")

gp_path <- paste0("/conf/linkage/output/lookups/Unicode/National Reference Files",
                  "/gpprac.sav")
##!! KH: I don't see an RDS (or CSV) version of this file... Where to get?

simd_path <- paste0("/conf/linkage/output/lookups/Unicode/Deprivation",
                    "/postcode_2022_2_simd2020v2.rds")

#### 1_process_extracts.R ####
#### 2: Main extract ----
## Import and rename ---
quarter <- read_csv(paste0(old_path, "/ISD.CSV"), 
                    col_names = FALSE, 
                    col_types=cols(.default = "c",
                                   X5 = col_date("%Y%m%d"),
                                   X12 = col_date("%Y%m%d"),
                                   X16 = col_date("%Y%m%d"),
                                   X19 = col_date("%Y%m%d"),
                                   X25 = col_date("%Y%m%d"),
                                   X26 = col_date("%Y%m%d"),
                                   X27 = col_date("%Y%m%d"),
                                   X29 = col_double(),
                                   X30 = col_double(),
                                   X31 = col_double(),
                                   X33 = col_date("%Y%m%d"),
                                   X34 = col_date("%Y%m%d"),
                                   X35 = col_date("%Y%m%d"))) %>% 
  glimpse()


### Continue on 1_process_extracts.R ----
## Line 70
## At line 264, back over to this script

saveRDS(quarter, paste0(old_path, "/aaa_extract_", year, month, ".rds"))


#### 2_checking_extracts.R ####
#### 2: Call in extract ----
quarter <- readRDS(paste0(old_path, "/aaa_extract_", year, month, ".rds")) %>% 
  # exclude cases with results obtained outside of Scotland
  filter(!screen_result %in% c("05", "06")) %>%
  # id variable for matching validator checks
  mutate(id = row_number(), .before = financial_year) %>%
  glimpse()

length(unique(quarter$upi))
# 356,451 of 506,867 records


### Continue on 2_checking_extracts.R ----
## Line 69
## At line 324, back over to this script

saveRDS(summary, paste0(old_path, 
                        "/aaa_checks_summary_", year, month, ".rds"))
saveRDS(summary_checks, paste0(old_path, 
                        "/aaa_checks_finyear_", year, month, ".rds"))

