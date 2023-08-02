#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4_QA_audit_June.R
# Karen Hotopp
# 2023-07-17
# 
# Annual audit of quality assurance screening audit and listing of HB of surgery
# (completed using 1 June data)
# 
# Written/run on Posit WB
# R version 4.1.2 (2021-11-01)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Notes:
# This audit is run using the 1 June extract and is to be completed (including
# sending reports to HBs and their system updates) before the 1 September 
# extract is run. This ensures that the QA and surgery data used for the autumn 
# KPIs and subsequent publication are complete.


### 1: Housekeeping ----
## Packages
library(dplyr)
library(readr)
library(openxlsx)
library(tidylog)


rm(list=ls())
gc()


## Values
year <- 2023
month <- "06"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts/", year, month)


## Function
write_QA_report <- function(df1, hb_name, YYMM) {
  
  ### Setup workbook ----
  ## Reset variable names
  records_extract <- c("Financial Year", "UPI", "HB of Residence", 
                       "HB of Screen", "Date of Screen", "Audit Result")
  
  ## Styles
  title_style <- createStyle(fontSize = 14, halign = "Left", textDecoration = "bold")
  table_style <- createStyle(valign = "Bottom", halign = "Left",
                             border = "TopBottomLeftRight")
  
  ## Titles
  title <- paste0("Quality Assurance Audit for ", hb_name)
  today <- paste0("Workbook created ", Sys.Date())
  
  ## Data
  # Inpatients (extract_simd)
  data1 <- df1 |> 
    filter(hb_screen == hb_name)
  
  ## Create workbook
  wb <- createWorkbook()
  options("openxlsx.borderStyle" = "thin",
          "openxlsx.dateFormat" = "dd/mm/yyyy")
  modifyBaseFont(wb, fontSize = 12, fontName = "Arial")
  
  
  ### QA audit--follow-ups ----
  names(data1) <- records_extract
  addWorksheet(wb, sheetName = "QA audit--follow-ups", gridLines = FALSE)
  writeDataTable(wb, "QA audit--follow-ups", data1, startRow = 5)
  
  # titles
  writeData(wb, "QA audit--follow-ups", title, startRow = 1, startCol = 1)
  writeData(wb, "QA audit--follow-ups", today, startRow = 2, startCol = 1)
  
  addStyle(wb, "QA audit--follow-ups", title_style, rows = 1, cols = 1)
  
  # table headers
  addStyle(wb, "QA audit--follow-ups", title_style, rows = 5, cols = 1:ncol(data1))
  
  # tables
  addStyle(wb, "QA audit--follow-ups", table_style, rows = 5:(5+nrow(data1)), 
           cols = 1:ncol(data1), gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, "QA audit--follow-ups", cols = 1:ncol(data1), 
               widths = "auto")
  
  
  ### Save ----
  saveWorkbook(wb, paste0(wd_path, "/output/QA_audit_", hb_name, "_",
                          YYMM, ".xlsx"), overwrite = TRUE)
}


<<<<<<< HEAD
write_HBsurgery_report <- function(df1, hb_name, YYMM) {
  
  ### Setup workbook ----
  ## Reset variable names
  records_extract <- c("Financial Year", "UPI", "HB of Residence", 
                       "HB of Screen", "Date of Surgery", "HB of Surgery")
  
  ## Styles
  title_style <- createStyle(fontSize = 14, halign = "Left", textDecoration = "bold")
  table_style <- createStyle(valign = "Bottom", halign = "Left",
                             border = "TopBottomLeftRight")
  
  ## Titles
  title <- paste0("Hleath Board of Surgery Follow-up for ", hb_name)
  today <- paste0("Workbook created ", Sys.Date())
  
  ## Data
  data1 <- df1 |> 
    filter(hb_screen == hb_name)
  
  ## Create workbook
  wb <- createWorkbook()
  options("openxlsx.borderStyle" = "thin",
          "openxlsx.dateFormat" = "dd/mm/yyyy")
  modifyBaseFont(wb, fontSize = 12, fontName = "Arial")
  
  
  ### QA audit--follow-ups ----
  names(data1) <- records_extract
  addWorksheet(wb, sheetName = "HB of Surgery--follow-ups", gridLines = FALSE)
  writeDataTable(wb, "HB of Surgery--follow-ups", data1, startRow = 5)
  
  # titles
  writeData(wb, "HB of Surgery--follow-ups", title, startRow = 1, startCol = 1)
  writeData(wb, "HB of Surgery--follow-ups", today, startRow = 2, startCol = 1)
  
  addStyle(wb, "HB of Surgery--follow-ups", title_style, rows = 1, cols = 1)
  
  # table headers
  addStyle(wb, "HB of Surgery--follow-ups", title_style, rows = 5, 
           cols = 1:ncol(data1))
  
  # tables
  addStyle(wb, "HB of Surgery--follow-ups", table_style, rows = 5:(5+nrow(data1)), 
           cols = 1:ncol(data1), gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, "HB of Surgery--follow-ups", cols = 1:ncol(data1), 
               widths = "auto")
  
  
  ### Save ----
  saveWorkbook(wb, paste0(wd_path, "/output/HB_Surgery_audit_", hb_name, "_",
                          YYMM, ".xlsx"), overwrite = TRUE)
}



#### 2: QA Audit ----
=======
#### 2: Review Extract QA ####
>>>>>>> 6dc31aa05de6aa549efbd41ec79d62c123966f23
quarter <- read_rds(paste0(wd_path, "/output/aaa_extract_",
                           year, month, ".rds")) |>
  filter(audit_flag == "01")

table(quarter$audit_result, useNA = "ifany")
table(quarter$audit_result, quarter$financial_year, useNA = "ifany")
# 2023-06:
# 01 (standard met): 30,594
# 02 (standard not met): 4,510
# NA (no response): 0


## Identify HBs missing audit_result
audit_qa <- quarter |> 
  filter(is.na(audit_result))

audit_qa <- droplevels(audit_qa)
table(audit_qa$hb_screen, useNA = "ifany")
table(audit_qa$hb_screen, audit_qa$financial_year)

audit_qa <- audit_qa |> 
  select(financial_year, upi, hbres, hb_screen, 
         date_screen, audit_result)


### 3: HB of Surgery ----
## Only want records where a surgery was performed
# (i.e., date of surgery is not empty)
surgery_board_qa <- quarter |> 
  filter(!is.na(date_surgery)) |> 
  mutate(hb_surgery = case_when(hb_surgery=="A" ~ "Ayrshire & Arran",
                                hb_surgery=="B" ~ "Borders",
                                hb_surgery=="Y" ~ "Dumfries & Galloway",
                                hb_surgery=="F" ~ "Fife",
                                hb_surgery=="V" ~ "Forth Valley",
                                hb_surgery=="N" ~ "Grampian",
                                hb_surgery=="G" ~ "Greater Glasgow & Clyde",
                                hb_surgery=="H" ~ "Highland",
                                hb_surgery=="L" ~ "Lanarkshire",
                                hb_surgery=="S" ~ "Lothian",
                                hb_surgery=="R" ~ "Orkney",
                                hb_surgery=="Z" ~ "Shetland",
                                hb_surgery=="T" ~ "Tayside",
                                hb_surgery=="W" ~ "Western Isles",
                                hb_surgery=="M" ~ "Scotland",
                                hb_surgery=="D" ~ "Cumbria",
                                hb_surgery=="E" ~ "Northumbria",
                                hb_surgery==TRUE ~ "Outwith Scotland"),
         hb_surgery = fct_relevel(hb_surgery, c("Ayrshire & Arran", "Borders",
                                                "Dumfries & Galloway", "Fife",
                                                "Forth Valley", "Grampian", 
                                                "Greater Glasgow & Clyde",
                                                "Highland", "Lanarkshire",
                                                "Lothian", "Orkney",
                                                "Shetland", "Tayside", 
                                                "Western Isles", "Scotland",
                                                "Cumbria", "Northumbria",
                                                "Outwith Scotland")))

table(surgery_board_qa$date_surgery, useNA = "ifany")
table(surgery_board_qa$hb_surgery, useNA = "ifany")
# 2023/06: 8 surgeries in Cumbria; 1 NA

## Pull out NAs
surgery_board_qa <- surgery_board_qa |> 
  filter(is.na(hb_surgery)) |> 
  select(financial_year, upi, hbres, hb_screen, 
         date_surgery, hb_surgery)


### 4: Output to Excel ----
## QA Audit
hb_names <- audit_qa |> 
  distinct(hb_screen) |> 
  pull()

for (hb_name in hb_names) {
  
  write_QA_report(audit_qa, hb_name, "202306")
  
}

<<<<<<< HEAD
## HB of Surgery
hb_names <- surgery_board_qa |> 
  distinct(hb_screen) |> 
  pull()

for (hb_name in hb_names) {
  
  write_HBsurgery_report(surgery_board_qa, hb_name, "202306")
  
}
=======
>>>>>>> 6dc31aa05de6aa549efbd41ec79d62c123966f23
