library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)


current_pd<-"FY22Q4i"



#genie 
genie_files<-list.files(here("Data/site"),pattern="SITE")

genie<-here("Data/site",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>%
  filter(fiscal_year %in% c("2022"),
         funding_agency=="USAID",
         !sitetype=="Above Site",
         !indicator %in% c("HRH_STAFF_NAT","HRH_PRE"))

  
geog_df<-genie %>% 
  distinct(orgunituid,psnu,psnuuid,
           community,communityuid,facility,facilityuid,sitename,sitetype,mech_code,prime_partner_name) %>% 
  relocate(sitetype, .before=sitename) %>% 
  relocate(community, .after=psnuuid) %>% 
  relocate(facility, .after=community) %>% 
  relocate(communityuid, .after=community) %>% 
  relocate(facilityuid, .after=facility)


# DATA CHECK 
check_dsp<-geog_df %>% 
  filter(mech_code %in% c("70290")) %>% 
  group_by(prime_partner_name,psnu,sitetype) %>% 
  count() %>% 
  print()
  
  
# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),"PEPFAR_SD_PARTNER_GEOG",current_pd,".txt",sep="_")

write_tsv(geog_df, file.path(here("Dataout"),filename,na=""))