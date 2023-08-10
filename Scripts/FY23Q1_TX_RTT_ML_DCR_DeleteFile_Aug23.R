# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY23Q1 DCR: Delete File TX_RTT ML
# REF ID:   833aade8 
# LICENSE:  MIT
# DATE:     2023-08-10
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)


# GLOBAL VARIABLES --------------------------------------------------------

# SI specific paths/functions  
load_secrets()

disagg_map <- read_sheet("1R6wUo1vK86ysNYGrz9wvuOfCgccxEOW9DGxY9cwvI9Q")
mech_map <- read_sheet("1R6wUo1vK86ysNYGrz9wvuOfCgccxEOW9DGxY9cwvI9Q", sheet = 2) %>% 
  mutate(mech_code = as.character(mech_code))

ref_id <- "833aade8"

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% return_latest("MER_Structured_Datasets_Site_IM_FY21-23_20230616_v2_1_South Africa") %>% 
  read_psd()

# MUNGE --------------------------------------------------------

df_clean <- df %>% 
  filter(fiscal_year == 2023,
         ageasentered == "50+",
         #  sitename == "ec Amadiba Clinic",
         funding_agency == "USAID",
         indicator %in% c("TX_RTT", "TX_ML")) %>% 
  reshape_msd() %>% 
  filter(period == "FY23Q1") %>% 
  select(orgunituid, sitename,mech_code, indicator, indicatortype, standardizeddisaggregate, categoryoptioncomboname, age_2019, ageasentered, funding_agency, period, value)


df_clean %>% 
  tidylog::left_join(disagg_map, by = c("indicator", "categoryoptioncomboname", "indicatortype" = "support_type")) %>% 
  left_join(mech_map, by = c("mech_code")) %>% 
  select(orgunituid, sitename, mech_uid, dataElement, dataElement_uid, categoryoptioncomboname,
         categoryOptionCombo_uid, period, value) %>% 
  mutate(period = "2022Q4") %>% 
  read_csv("Dataout/")
  
  
  select(mech_uid, orgunituid, dataElement_uid, categoryOptionCombo_uid,value, period)