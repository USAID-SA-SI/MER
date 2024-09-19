# PURPOSE: Munge and Analysis of Partner info data
# AUTHOR: Karishma Srikanth
# LICENSE: MIT
# DATE: 2024-9-12
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(scales)
    library(googlesheets4)
    library(patchwork)
   # library(getrdone)
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
    
   
    shpdata <- glamr::si_path("path_vector")
    merdata <- glamr::si_path("path_msd")

  # where does data live?
    df_msd <- return_latest(merdata, pattern = "PSNU_IM.*South Africa")
    

  

# LOAD DATA ============================================================================  

  msd <- read_psd(df_msd)

# MUNGE ============================================================================
  
  
  # What are the key mechs for TX_CURR
  mech <- msd %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "TX_CURR", 
                              "PrEP_NEW", "VMMC_CIRC", "OVC_SERV", "GEND_GBV"), 
             standardizeddisaggregate == "Total Numerator", fiscal_year == 2024) %>% 
      group_by(prime_partner_name, mech_name, mech_code, fiscal_year, funding_agency, indicator) %>%
      summarise(targets = sum(targets, na.rm = T)) %>% 
      group_by(fiscal_year, indicator) %>% 
      mutate(share = targets / sum(targets, na.rm = T)) %>% 
      ungroup() %>% 
      complete(indicator, mech_name, fiscal_year)
      
  # Heatmap of results
  # Need to reorder indicators into thematic groups
  
  mech %>% 
    mutate(indicator = fct_relevel(
      indicator, 
      c("GEND_GBV", "PrEP_NEW", "OVC_SERV", "VMMC_CIRC",
        "HTS_TST", "HTS_TST_POS", "HTS_INDEX", 
        "TX_CURR")
    )) %>% 
    filter(funding_agency == "USAID") %>% 
    ggplot(aes(y = fct_reorder(prime_partner_name, share, .desc = F, na.rm = T), #mech_name or prime_parnter_name?
               x = factor(fiscal_year), 
               fill = share)) +
    geom_tile(color = "white") +
    geom_text(aes(label = percent(share, 1), color = ifelse(share>0.3, "white", grey90k)), 
              size = 8/.pt, family = "Source Sans Pro Bold")+
    facet_wrap(~indicator, nrow = 1) +
    scale_fill_si(palette = "moody_blues", na.value = grey10k, label = scales::percent_format(accuracy = 1)) +
    scale_color_identity() +
    si_style_xline(facet_space = 0.5) +
    theme(legend.position = "off")+
    labs(x = NULL, y = NULL) +
    scale_alpha(range = c(0.5, 1))
  
  si_save("Images/Target_distribution_fy24.svg", height = 2.5, width = 10.4, scale = 1.25)
      
      
  
# VIZ ============================================================================

 
 # Map of where we work by Prevention, Treatment & Testing
 psnu <- 
   msd|> 
   filter(indicator %in% c("HTS_TST_POS", "HTS_INDEX", "TX_CURR", 
                           "VMMC_CIRC", "OVC_SERV", "GEND_GBV", "PrEP_NEW"), 
          standardizeddisaggregate == "Total Numerator", fiscal_year == 2024, 
          funding_agency %in% c("HHS/CDC", "USAID")) %>% 
   group_by(psnu, psnuuid, fiscal_year, indicator, funding_agency) %>%
   summarise(targets = sum(targets, na.rm = T)) %>% 
   group_by(fiscal_year, indicator) %>% 
   mutate(share = targets / sum(targets, na.rm = T)) %>% 
   ungroup()
  
  
  # Load sf data
  psnu_sf <- st_read("GIS/psnu.shp")
  ou_sf   <- st_read("GIS/operatingunit.shp")
 
 
 cov <-  full_join(psnu_sf, psnu, by = c("uid" = "psnuuid")) |> 
   mutate(indicator = fct_relevel(
     indicator, 
     c("GEND_GBV", "PrEP_NEW", "OVC_SERV", "VMMC_CIRC",
       "HTS_TST_POS", "HTS_INDEX", 
       "TX_CURR")
   )) |> 
   clean_agency() |> 
   filter(!is.na(indicator)) 
 
 
 # Define bounding box for mainland South Africa (adjust as needed)
 bbox_mainland <- st_bbox(c(xmin = 16, xmax = 33, ymin = -35, ymax = -22), crs = st_crs(cov))
 
 # Ensure geometries are valid before cropping
 ou_sf_valid <- st_make_valid(ou_sf)
 
 # Crop the datasets to the bounding box
 cov_mainland <- st_crop(cov, bbox_mainland)
 ou_sf_mainland <- st_crop(ou_sf_valid, bbox_mainland)
 
 # Plot with cropped data
 ggplot() +
   geom_sf(data = cov_mainland, aes(fill = funding_agency), color = "white", alpha = 0.75) +
   geom_sf(data = ou_sf_mainland, fill = NA, color = grey90k, size = 0.5) +
   scale_fill_manual(values = c("USAID" = old_rose, "CDC" = denim)) +
   facet_wrap(~ indicator, nrow = 1) +
   si_style_map(facet_space = 0.5) +
   theme(legend.position = "none")
   
 
# SPINDOWN ============================================================================

