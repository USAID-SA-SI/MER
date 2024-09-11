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

devtools::install_github("USAID-SA-SI/gtayblr") #OHA custom gt package - in development

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
  
  #filter
  df_genie <- genie %>% 
    left_join(dsp_lookback,by="DSPID") %>% 
    clean_indicator() %>% 
    filter(indicator %in% c("HTS_TST", "HTS_INDEX_NEWPOS", "HTS_INDEX_NEWNEG" , "HTS_TST_POS", "TX_NEW", 
                            "TX_NET_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D", "TB_STAT", 
                            "TB_STAT_D", "TB_PREV", "TB_PREV_D", "PrEP_NEW"), 
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
      filter(str_detect(psnu, name))
  }
  
  #group by summarize by type
  df_genie_final <- df_genie %>% 
  group_by(fiscal_year, funding_agency, indicator, across(all_of(type))) %>% 
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                     sum, na.rm = TRUE), .groups = "drop") %>% 
    adorn_achievement(qtr = 3) %>% 
    mutate(achv_desc = as.character(achv_desc)) %>% 
    mutate(achv_desc = ifelse(indicator == "TX_NET_NEW", "NA", achv_desc),
           achievement = ifelse(indicator == "TX_NET_NEW", NA, achievement),
           achv_color = ifelse(indicator == "TX_NET_NEW", trolley_grey_light, achv_color))
  
  #generate gt table - decide what theme we want and if we want spanners
  table <- df_genie_final %>%
    select(indicator, starts_with("qtr"), cumulative, targets, achievement, achv_desc, achv_label, achv_color) %>%
    set_names(~ toupper(.)) %>%  # Automatically capitalize column names
    gt() %>% 
    gtayblr::si_gt_base() %>% 
    #gtExtras::gt_theme_nytimes() %>%
    sub_missing(missing_text = ".",
    ) %>%
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
    cols_hide(columns= c(ACHV_COLOR, QTR3)) %>% 
    tab_header(
      title = glue("{name} - FY24Q3 MER Results and Targets"))
  
  
  if (save == TRUE) {
    table %>%
      gtExtras::gtsave_extra(path = "Images", filename = glue::glue("{name}_{metadata$curr_pd}_target_table.png"))
  }
  
    
    
  
  return(table)
}

#produce tables 

#use mech codes for partners 
make_target_table(type = "mech_code", name = "70301") 

#use psnu name for psnu table 
make_target_table(type = "psnu", name = "Lej") #uses a str_detect

#gtsave_extra(filename = glue::glue("70287_FY24Q3_target_table.png")) #save function not workings


#to iterate over multiple psnus, you can list them below

mech_list <- c("87575","70310", "70301") 
map(mech_list, ~make_target_table(type = "mech_code", name= .x))

psnu_list <- c("Harry Gwala", "King Cetshwayo", "Ugu", "Cape Town", "Lej") 
map(psnu_list, ~make_target_table(type = "psnu", name= .x))


# final<-genie %>% 
#        #reshape_msd("long") %>% 
#        left_join(dsp_lookback,by="DSPID") %>% 
#        clean_indicator() %>% 
#        filter(indicator %in% c("HTS_TST", "HTS_INDEX", "HTS_TST_POS", "TX_NEW", 
#                                "TX_NET_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D", "TB_STAT", 
#                                "TB_STAT_D", "TB_PREV", "TB_PREV_D", "PrEP_NEW"), 
#               standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"), 
#               funding_agency == "USAID") %>% 
#     group_by(fiscal_year, funding_agency, indicator, mech_code, psnu) %>% 
#     summarise(across(c(starts_with("qtr"), cumulative, targets),
#                    sum, na.rm = TRUE), .groups = "drop")
# 
# 
# final_long <- final %>% 
#               reshape_msd(qtrs_keep_cumulative = TRUE) 
# 
# 
# ### GT table 
# mech_code <- "70310"

