library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)


current_pd<-"FY22Q4c"

# CONTEXT FILES IN -------------------------------------------------------------
dsp_lookback<-read_excel(here("Data","dsp_attributes_2022-05-17.xlsx")) %>% 
  rename(agency_lookback=`Agency lookback`) %>% 
  select(-MechanismID) %>% 
  filter(DSP=="Yes") %>% 
  select(DSPID,agency_lookback)


#genie 
genie_files<-list.files(here("Data"),pattern="Daily")

genie<-here("Data",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>%
  filter(fiscal_year %in% c("2023"),
         !indicator %in% c("HRH_STAFF_NAT","HRH_PRE"),
         standardizeddisaggregate=="Total Numerator",
         !prime_partner_name=="Default") %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) %>% 
  unite(DSPID,mech_code,short_name,sep="",remove=FALSE) %>% 
  left_join(dsp_lookback,by="DSPID") 

  
geog_df<-genie %>% 
  mutate(technical_area=case_when(
    mech_code %in% c("80002","80004","70311","70307","14295","14631",
                     "81904") ~ "OVC",
    TRUE ~ ""
  )) %>% 
  mutate(FY23_OVC_Cov=case_when(
    technical_area=="OVC" ~ "Y",
    TRUE ~"N"
  )) %>% 
  rename(FY23_DSP=agency_lookback,
         FY23_DREAMS=dreams) %>%
  group_by(psnu,psnuuid,FY23_OVC_Cov,FY23_DSP,FY23_DREAMS) %>% 
  summarize_at(vars(targets),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  filter(FY23_DSP %in% c("HHS/CDC", "USAID")) %>% 
  mutate(FY23_OVC_Cov="Y") %>% 
  distinct(psnuuid,psnu,FY23_OVC_Cov,FY23_DSP,FY23_DREAMS)


  
# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),"FY23_Prev_DSP_Coverage",".txt",sep="_")

write_tsv(geog_df, file.path(here("Dataout"),filename,na=""))