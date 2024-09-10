# AUTHOR:   K. Kehoe | USAID SA
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


  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata

  filepath <- si_path() %>% 
  return_latest("PSNU_IM.*South Africa")

    metadata <- get_metadata(filepath) 
  
  ref_id <- "cf5a4baf"

# IMPORT ------------------------------------------------------------------

#genie 
genie_files<-list.files(here("Data"),pattern="Daily")
  
  
genie<-here("Data",genie_files) %>% 
       map(read_psd) %>% 
       reduce(rbind) %>%
       filter(fiscal_year %in% c("2024")) 

genie <- read_psd(filepath)
  
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

make_target_table <- function(type, name) {
  
  df_genie <- genie %>% 
    left_join(dsp_lookback,by="DSPID") %>% 
    clean_indicator() %>% 
    filter(indicator %in% c("HTS_TST", "HTS_INDEX", "HTS_TST_POS", "TX_NEW", 
                            "TX_NET_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D", "TB_STAT", 
                            "TB_STAT_D", "TB_PREV", "TB_PREV_D", "PrEP_NEW"), 
           fiscal_year == metadata$curr_fy,
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"), 
           funding_agency == "USAID")
  
  if (type == "mech_code") {
    
    df_genie <- df_genie %>% 
      filter(mech_code == name)
    
  } else if (type == "psnu") {
    df_genie <- df_genie %>% 
      filter(str_detect(psnu, name))
  }
  
  df_genie_final <- df_genie %>% 
  group_by(fiscal_year, funding_agency, indicator, across(all_of(type))) %>% 
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                     sum, na.rm = TRUE), .groups = "drop") %>% 
    adorn_achievement() %>% 
    mutate(achv_desc = as.character(achv_desc)) %>% 
    mutate(achv_desc = ifelse(indicator == "TX_NET_NEW", "NA", achv_desc),
           achievement = ifelse(indicator == "TX_NET_NEW", NA, achievement),
           achv_color = ifelse(indicator == "TX_NET_NEW", trolley_grey_light, achv_color))
    # mutate(
    #   achv_desc = case_when(
    #     indicator == "TX_NET_NEW" ~ fct_relevel(achv_desc, "NA"),  # Relevel only when indicator is "TX_NET_NEW"
    #     TRUE ~ achv_desc  # Otherwise, keep it unchanged
    #   )
    # )
  
  
  table <- df_genie_final %>%
    select(indicator, starts_with("qtr"), cumulative, targets, achievement, achv_desc, achv_color) %>%
    set_names(~ toupper(.)) %>%  # Automatically capitalize column names
    gt() %>% 
    gtayblr::si_gt_base() %>% 
    #gtExtras::gt_theme_nytimes() %>%
    sub_missing(missing_text = ".",
    ) %>%
   # cols_label(indicator)
    fmt_number(columns = c(2,3,4,5,6,7),
               decimals = 0) %>%
    fmt_percent(columns = c(8), 
               decimals = 0) %>% 
    text_transform(
      locations = cells_body(columns = c(ACHV_DESC)),
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
    # tab_spanner(
    #   label = "FY24 Results",
    #   columns = c(
    #     QTR1, QTR2, QTR3, QTR4, CUMULATIVE
    #   )) %>% 
    # gt::tab_style(
    #   style = list(
    #     gt::cell_text(weight = "bold")), 
    #   locations = gt::cells_column_spanners(spanners = tidyselect::everything())
    # ) %>% 
    # tab_style(
    #   style = cell_borders(
    #     sides = "bottom",
    #     color = "black",
    #     weight = px(2)
    #   ),
    #   locations = cells_column_spanners(spanners = "FY24 Results")
    # ) %>%
    cols_hide(columns= c(ACHV_COLOR, QTR4)) %>% 
    tab_header(
      title = glue("{name} - {metadata$curr_pd} MER Results and Targets"))
    
    
  
  return(table)
}


make_target_table(type = "mech_code", name = "70287")
make_target_table(type = "psnu", name = "Alfred Nzo") #uses a str_detect




