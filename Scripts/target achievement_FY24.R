# AUTHOR:   K. Kehoe | USAID SA
# PURPOSE:  
# REF ID:   cf5a4baf 
# LICENSE:  MIT
# DATE:     2024-09-10
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
  library(here)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "cf5a4baf"

# IMPORT ------------------------------------------------------------------

#genie 
genie_files<-list.files(here("Data"),pattern="Daily")
  
  
genie<-here("Data",genie_files) %>% 
       map(read_psd) %>% 
       reduce(rbind) %>%
       filter(fiscal_year %in% c("2024")) 
  
# CONTEXT FILES IN -------------------------------------------------------------
dsp_lookback<-read_excel(here("Data","dsp_attributes_2024-04-08.xlsx")) %>% 
              rename(agency_lookback=`Agency lookback`) %>% 
              select(-MechanismID)

# MUNGE -------------------------------------------------------------------
genie <- genie %>%  
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) %>% 
  unite(DSPID,mech_code,short_name,sep="",remove=FALSE)

final<-genie %>% 
       #reshape_msd("long") %>% 
       left_join(dsp_lookback,by="DSPID") %>% 
       clean_indicator() %>% 
       filter(indicator %in% c("HTS_TST", "HTS_INDEX", "HTS_TST_POS", "TX_NEW", 
                               "TX_NET_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D", "TB_STAT", 
                               "TB_STAT_D", "TB_PREV", "TB_PREV_D", "PrEP_NEW"), 
              standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"), 
              funding_agency == "USAID") %>% 
    group_by(fiscal_year, funding_agency, indicator, mech_code, psnu) %>% 
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                   sum, na.rm = TRUE), .groups = "drop")


final_long <- final %>% 
              reshape_msd(qtrs_keep_cumulative = TRUE) 


### GT table 
mech_code <- "70310"








