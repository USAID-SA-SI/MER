
# Title: MSD Convert Script
# Author: C. Trapence
# Updated by: G. Sarfaty
# Purpose: Translating the Quarterly NDOH dataset to an MSD
# Date:2022-11-22
# Updated: 2022-12-21
# Load Required libraries
#################################################################################
#                 
#  sources files used in the code include:                                                                            
#              1) TB_ART results from NDOH                                                                        
#              2) Host Country Results Facility based from DATIM Support                                                
#              3) Data Exchange Organisation Units from DATIM Support           
#              4) Genie extract for MSD skeleton    
#
#################################################################################

#Loading required libraries
library(tidytext)
library(tidyverse)
library(lubridate)
library(here)
library(googledrive)
library(glamr)
library(openxlsx)
library(sqldf)
library(gophr)

current_pd<-"FY22Q4"

#GENIE REF TABLE CREATION
genie_files<-list.files(here("Data/site"),pattern="Daily")

Genie<-read_tsv(here("Data/site",genie_files)) 

hierarchy<-Genie %>% 
  filter(fiscal_year=="2022") %>% 
  distinct(orgunituid,sitename,operatingunit,
           operatingunituid,country,snu1,snu1uid,psnu,psnuuid,snuprioritization,
           typemilitary,dreams,communityuid,community,facilityuid,facility,
           sitetype)

data_elements<-Genie %>% 
  distinct(dataelementuid,indicator,numeratordenom,indicatortype,disaggregate,
           standardizeddisaggregate,categoryoptioncombouid,categoryoptioncomboname,
           ageasentered,age_2018,age_2019,trendscoarse,sex,statushiv,statustb,
           statuscx,hiv_treatment_status,otherdisaggregate,otherdisaggregate_sub,
           modality) %>% 
  unite(key,dataelementuid,categoryoptioncomboname,sep="_")



# IMPORT FORMATTED FILE FROM NDoH
tempFile<-read.csv(here("Data","TB_ART_Quarterly 2022-11-17.csv")) %>% 
  mutate(dataelementuid=if_else(dataElementName=="TB_ART_Quarterly (N, TA, Age/Sex/NewExistingArt/HIVStatus): TB/HIV on ART","Szuf9YjHjTL","Qc1AaYpKsjs")) %>% 
  mutate(period= recode(period,"2021Q4"="FY21Q4","2022Q1"="FY22Q1","2022Q2"="FY22Q2","2022Q3"="FY22Q3")) %>% 
  rename(categoryoptioncomboname=coc) %>% 
  unite(key,dataelementuid,categoryoptioncomboname,sep="_") %>% 
  select(-psnu,-organisationUnit)


# MERGE IMPORT REF TABLES FROM GENIE & DATIM API
final_df<-tempFile %>% 
  left_join(data_elements,by="key") %>% 
  rename(mech_code=mechCode,
         orgunituid=orgUnitUID) %>% 
  mutate(mech_code=as.character(mech_code)) %>% 
  left_join(hierarchy,by="orgunituid") %>% 
  rename_official() %>% 
  select(-key) %>% 
  mutate(period_type="results",
         indicator="TB_ART_NDOH_QUARTERLY")



check<-final_df %>% 
  group_by(indicator,period) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  print()


# EXPORT

filename<-paste(Sys.Date(),"TB_ART_QUARTERLY",current_pd,".txt",sep="_")

write_tsv(final_df, file.path(here("Dataout"),filename,na=""))
