# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  check KZN treatment loss after transition of partners (for HIV EPP slides - HQ)
# REF ID:   6c292182 
# LICENSE:  MIT
# DATE:     2024-10-03
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

    filepath <- si_path() %>% return_latest("South Africa")

  # Grab metadata
    metadata <- get_metadata(filepath) 
  
  ref_id <- "6c292182"
  
  #rounding function
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  

# IMPORT ------------------------------------------------------------------
  
df_msd <- read_psd(filepath)

# MUNGE -------------------------------------------------------------------
  
df_msd %>% 
    filter(fiscal_year %in% c(2023, 2024),
           indicator == "TX_CURR",
           funding_agency == "USAID",
           snu1 == "kz KwaZulu-Natal Province",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, indicator, snu1, psnu, funding_agency, mech_code) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop")   %>% 
    reshape_msd() %>% 
    pivot_wider(names_from = "mech_code")
  
  df_kzn <- df_msd %>% 
    filter(fiscal_year %in% c(2023, 2024),
           indicator == "TX_CURR",
           funding_agency == "USAID",
           snu1 == "kz KwaZulu-Natal Province",
           !str_detect(psnu, "Harry Gwala"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, indicator, snu1, psnu, funding_agency, prime_partner_name) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop")   %>% 
    reshape_msd() %>% 
    filter(value != 0) %>% 
    mutate(DSP = ifelse(str_detect(prime_partner_name, "BROADREACH"), "Broadreach", "MATCH"))
  
  df_kzn_nn <- df_msd %>% 
    filter(fiscal_year %in% c(2023, 2024),
           indicator == "TX_NET_NEW",
           funding_agency == "USAID",
           snu1 == "kz KwaZulu-Natal Province",
            !str_detect(psnu, "Harry Gwala"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, indicator, snu1, psnu, funding_agency, prime_partner_name) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop")   %>% 
    reshape_msd() %>% 
    filter(value != 0) %>% 
    mutate(DSP = ifelse(str_detect(prime_partner_name, "BROADREACH"), "Broadreach", "MATCH"))

  
  df_kzn %>% 
    ggplot(aes(x = period, y= value, fill = DSP)) +
    geom_col() +
    facet_wrap(~psnu) +
    geom_text(aes(label = glue::glue("{comma(value)}"),
                 # hjust = -0.1,
                   vjust = -0.5,
                  family = "Source Sans Pro"),  size = 11/.pt) +
    ggplot2::scale_y_continuous(
      labels = function(x) {glue("{label_number(scale_cut = cut_short_scale())(abs(x))}")}, 
    ) +
    scale_fill_manual(values = c(hw_viking, hw_midnight_blue)) +
    si_style_ygrid() +
    labs(x = NULL, y = NULL,
         title = "TX_CURR in USAID-supported KZN Districts" %>% toupper(),
         subtitle = "Transition from Broadreach to MATCH at the end of FY23Q4 and FY24Q1, into FY24Q2",
         caption = metadata$caption)
  
  si_save("Images/KZN_TXCURR.png")
  
  library(gt)
  library(gtExtras)
  
  df_kzn %>% 
    filter(str_detect(period, "FY24")) %>% 
    select(period, psnu, DSP, value) %>% 
    mutate(difference_q1_q2 = ifelse(period == "FY24Q1", value - value[period == "FY24Q2"], NA)) %>% 
    mutate(difference_q1_q3 = ifelse(period == "FY24Q1", value - value[period == "FY24Q3"], NA)) %>% 
    pivot_wider(names_from = "period") %>% 
    gt() %>% 
    sub_missing(missing_text = ".",
    ) 
    
  
  
 df_snu <- df_msd %>% 
    filter(fiscal_year %in% c(2023, 2024),
           indicator == "TX_CURR",
           funding_agency == "USAID",
           snu1 == "kz KwaZulu-Natal Province",
           # !str_detect(psnu, "Harry Gwala"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, indicator, snu1, funding_agency, prime_partner_name) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop")   %>% 
    reshape_msd() %>% 
    filter(value != 0) %>% 
    mutate(DSP = ifelse(str_detect(prime_partner_name, "BROADREACH"), "Broadreach", "MATCH"))
 
 df_kzn %>% 
   ggplot(aes(x = period, y= value, fill = DSP)) +
   geom_col() +
  # facet_wrap(~psnu) +
   geom_text(aes(label = glue::glue("{clean_number(value)}"),
                 # hjust = -0.1,
                 vjust = -0.5,
                 family = "Source Sans Pro"),  size = 12/.pt) +
   ggplot2::scale_y_continuous(
     labels = function(x) {glue("{label_number(scale_cut = cut_short_scale())(abs(x))}")}, 
   ) +
   scale_fill_manual(values = c(hw_viking, hw_midnight_blue)) +
   si_style_ygrid() +
   labs(x = NULL, y = NULL,
        title = "TX_CURR in USAID-supported KZN Districts" %>% toupper(),
        subtitle = "Transition from Broadreach to MATCH at the end of FY23Q4 and FY24Q1, into FY24Q2",
        caption = metadata$caption)
  
  
  # df_kzn_nn %>% 
  #   ggplot(aes(x = period, y= value, fill = DSP)) +
  #   geom_col() +
  #   facet_wrap(~psnu) +
  #   geom_text(aes(label = glue::glue("{clean_number(value)}"),
  #                 # hjust = -0.1,
  #                 vjust = -0.5,
  #                 family = "Source Sans Pro"),  size = 12/.pt) +
  #   ggplot2::scale_y_continuous(
  #     labels = function(x) {glue("{label_number(scale_cut = cut_short_scale())(abs(x))}")}, 
  #   ) +
  #   scale_fill_manual(values = c(hw_viking, hw_midnight_blue)) +
  #   si_style_ygrid() +
  #   labs(x = NULL, y = NULL,
  #        title = "TX_NET_NEW in USAID-supported KZN Districts" %>% toupper(),
  #        subtitle = "Transition from Broadreach to MATCH at the end of FY23Q4 and FY24Q1, into FY24Q2",
  #        caption = metadata$caption)
  

  