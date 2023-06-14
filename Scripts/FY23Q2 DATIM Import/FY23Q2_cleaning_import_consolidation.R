# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  QC FY23Q2 clean
# REF ID:   df2e3b9f 
# LICENSE:  MIT
# DATE:     2023-06-08


# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(gophr)
library(glue)
library(openxlsx)
library(readxl)

# IMPORT ------------------------------------------------------------------

import_folder <- "Data/Partner Import Files - FY23Q2"
delete_folder <- "Data/Partner Import Files - FY23Q2/Delete Files"


consolidated <- import_folder %>% 
  return_latest("FY23Q2 Cleaning Import file") %>% 
  read_csv() %>% 
  rename(orgUnit_uid = OrgUnit,
         categoryOptionCombo_uid = categoryOptionCombo,
         mech_uid = attributeOptionCombo, 
         value=Value)

anova <- import_folder %>% 
  return_latest("ANOVA") %>% 
  read_excel(sheet = "import")

match <- import_folder %>% 
  return_latest("MATCH") %>% 
  read_excel(sheet = "import")

wrhi_80007 <- import_folder %>% 
  return_latest("WRHI") %>% 
  read_excel(sheet = "import")



three_fac <- consolidated %>% 
  filter(orgUnit_uid %in% c("tmESQvQP3dv", "hmTxIEIMLyL", "Hrd0xgemqNH")) 

df_bind <- bind_rows(anova, match, wrhi_80007) %>% 
  select(dataElement_uid, orgUnit_uid, categoryOptionCombo_uid, mech_uid, value) %>% 
  mutate(period = "2023Q1") %>% 
  select(dataElement_uid, period, orgUnit_uid, categoryOptionCombo_uid, mech_uid, value) %>% 
  bind_rows(three_fac)


setdiff(consolidated, df_bind) %>% 
  write_csv("Dataout/diff_sites.csv")


#issue was with the winburg clinic - remove from consolidated
consolidated %>% 
  filter(orgUnit_uid != "EnDbIkU6vF8") %>% 
  
  
  
  delete_folder %>% 
  return_latest("ANOVA")
