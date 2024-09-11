# AUTHOR:   K. Kehoe | K. Srikanth 
# PURPOSE:  target achievement tables for internal review
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
library(gt)
library(gtExtras)

devtools::install_github("USAID-OHA-SI/gtayblr") #OHA custom gt package - in development

install.packages("pak")
pak::pak("USAID-OHA-SI/gtayblr") # needed to install it this way 
library(gtayblr)


# GLOBAL VARIABLES --------------------------------------------------------

# SI specific paths/functions  
load_secrets()

# Grab metadata

filepath <- si_path() %>% 
  return_latest("PSNU_IM.*South Africa")

metadata <- get_metadata(filepath) 

ref_id <- "cf5a4baf"

glamr::folder_setup() #make sure you are working in the MER project

# IMPORT ------------------------------------------------------------------

#genie 
genie_files<-list.files(here("Data"),pattern="Daily")


genie<-here("Data",genie_files) %>% 
  map(read_psd) %>% 
  reduce(rbind) %>%
  filter(fiscal_year %in% c("2024")) 

#genie <- read_psd(filepath)

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


# FUNCTION -------------------------------------------------------------------

make_target_table <- function(type, name, save = FALSE) {
  
  # Order the indicators:
  custom_order <- c("PrEP_NEW", "HTS_TST","HTS_TST_POS","HTS_INDEX","TX_NEW", 
                    "TX_NET_NEW","TX_CURR","TX_PVLS","TX_PVLS_D", 
                    "TB_STAT","TB_STAT_D", "TB_PREV", "TB_PREV_D" 
  )
  
  
  #filter
  df_genie <- genie %>% 
    left_join(dsp_lookback,by="DSPID") %>% 
    clean_indicator() %>% 
    filter(indicator %in% custom_order, 
           fiscal_year == metadata$curr_fy,
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"), 
           funding_agency == "USAID", 
           DSP_lookback == "Yes") %>% 
    mutate(mech_code = ifelse(mech_code != mechID_lookback, mechID_lookback, mech_code))
  
  #conditional filter by type
  if (type == "mech_code") {
    
    df_genie <- df_genie %>% 
      filter(mech_code == name)
    
  } else if (type == "psnu") {
    df_genie <- df_genie %>% 
      filter(str_detect(psnu, name)) %>% 
      mutate(psnu = str_remove(psnu, "District Municipality")) %>% 
      mutate(psnu = str_remove(psnu, "Metropolitan Municipality"))
  }
  
  #group by summarize by type
  df_genie_final <- df_genie %>% 
    group_by(fiscal_year, funding_agency, indicator, across(all_of(type))
    ) %>% 
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                     sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(indicator = factor(indicator, levels = custom_order)) %>% 
    arrange(indicator) %>% 
    mutate(achievement = cumulative/targets) %>% 
    mutate(achievement = round(achievement, 2)) %>%
    mutate(diff = 1-achievement) %>% 
    gophr:::adorn_achievement(qtr = 3) %>% #color_achievement not an export?
    mutate(achv_desc = as.character(achv_desc)) %>% 
    mutate(achv_desc = ifelse(indicator == "TX_NET_NEW", "NA", achv_desc),
           achievement = ifelse(indicator == "TX_NET_NEW", NA, achievement),
           achv_label = ifelse(indicator == "TX_NET_NEW", NA, achv_label),
           achv_color = ifelse(indicator == "TX_NET_NEW", trolley_grey_light, achv_color),
           cumulative = ifelse(indicator == "TX_NET_NEW", NA, cumulative))
  
  if (type == "mech_code") {
    
    tbl_title <- glue("{df_genie_final$mech_code %>% unique()} - FY24Q3 MER Results and Targets")
  } else if (type == "psnu") {
    tbl_title <- glue("{df_genie_final$psnu %>% unique()} - FY24Q3 MER Results and Targets")
    
  }
  
  #generate gt table - decide what theme we want and if we want spanners
  table <- df_genie_final %>%
    select(indicator, starts_with("qtr"), cumulative, targets, achievement, achv_desc, achv_label, achv_color) %>%
    gt() %>% 
    gtayblr::si_gt_base() %>% 
    cols_label(achv_desc = "Status",
               achv_label = "Legend") %>% 
    sub_missing(missing_text = ".",
    ) %>%
    fmt_number(columns = c(2,3,4,5,6,7),
               decimals = 0) %>%
    fmt_percent(columns = c(8), 
                decimals = 0) %>% 
    text_transform(
      locations = cells_body(columns = c(achv_desc)),
      fn = function(x) {
        lapply(seq_along(x), function(i) {
          # Apply white text color if achv_color is '#697EBC', otherwise black
          if (df_genie_final$achv_color[i] == "#697EBC") {
            text_color <- "white"
          } else {
            text_color <- "black"
          }
          
          # Apply the pill-shaped background and the conditional text color
          glue("<div style='background-color:{df_genie_final$achv_color[i]}; padding: 5px 10px; border-radius: 50px; display: inline-block; width: 120px; text-align: center; white-space: nowrap; color: {text_color};'>{x[i]}</div>")
        })
      }
    ) %>% 
    cols_hide(columns= c(achv_color, qtr4)) %>% 
    tab_header(
      title = tbl_title) %>%
    cols_align(
      align = "left",  # Left-align the indicator column
      columns = c("indicator") ) # Apply to the indicator column
  
  
  if (save == TRUE) {
    table %>%
      gt::gtsave(filename = glue::glue("Images/{name}_{metadata$curr_pd}_target_table.png"))
  }
  
  
  
  
  return(table)
}

# APPLY -------------------------------------------------------------------


#produce tables 

#use mech codes for partners 
make_target_table(type = "mech_code", name = "70287", save = TRUE) 

#use psnu name for psnu table 
make_target_table(type = "psnu", name = "Johannesburg") #uses a str_detect

# ITERATE -------------------------------------------------------------------

#to iterate over multiple psnus, you can list them below

mech_list <- c("70310", "87577","70287","87575","87576", "70290","70301") 
map(mech_list, ~make_target_table(type = "mech_code", name= .x, save = TRUE))

psnu_list <- c("Johannesburg", "Sedibeng", "Cape Town",
               "Capricorn", "Mopani",
               "Gert Sibande", "Nkangala",
               "Harry Gwala", "Cetshwayo", "Ugu",
               "Alfred Nzo", "Buffalo City",
               "Thabo", "Ehlanzeni",
               "Lejweleputswa") 

map(psnu_list, ~make_target_table(type = "psnu", name= .x, save= TRUE))



