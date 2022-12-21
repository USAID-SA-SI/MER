
# Title: MSD Convert Script
# Author: C. Trapence
# Updated by: G. Sarfaty
# Purpose: Translating the Quarterly NDOH dataset to an MSD
# Date:2022-11-22
# Updated: 2022-12-12
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



# CDC processed import format
tempFile<-read.csv(here("Data","TB_ART_Quarterly 2022-11-17.csv")) %>% 
  mutate(dataelementuid=if_else(dataElementName=="TB_ART_Quarterly (N, TA, Age/Sex/NewExistingArt/HIVStatus): TB/HIV on ART","Szuf9YjHjTL","Qc1AaYpKsjs")) %>% 
  mutate(period= recode(period,"2021Q4"="FY21Q4","2022Q1"="FY22Q1","2022Q2"="FY22Q2","2022Q3"="FY22Q3")) %>% 
  rename(categoryoptioncombo=coc) %>% 
  unite(key,dataelementuid,categoryoptioncombo,sep="_")


# DATIM DATA ELEMENT CODE LIST
Data_elements_cd<-read.csv(here("Data","Data sets, elements and combos paramaterized.csv")) %>% 
  unite(key,dataelementuid,categoryoptioncombo,sep="_",remove=FALSE) %>% 
  select(-dataset)
  # mutate(categoryoptioncombo =gsub(" ","",categoryoptioncombo ))

# MERGE IMPORT W DATA ELEMENT CODE LIST
tempFile1.2<-tempFile %>% 
  left_join(Data_elements_cd,by="key") %>% 
  rename(mech_code=mechCode,
         orgunituid=orgUnitUID) %>% 
  mutate(mech_code=as.character(mech_code))

#GENIE FORMAT
genie_files<-list.files(here("Data/site"),pattern="Daily")

Genie<-read_tsv(here("Data/site",genie_files)) 


Genie_TB_ART<-Genie %>% 
  filter(indicator=="TB_ART")  %>%  
  select(-(qtr1:qtr4),-cumulative,-targets,-approvallevel,-approvalleveldescription) %>%  
  filter(fiscal_year %in% c("2021","2022")) %>% 
  select(-fiscal_year) %>% 
  mutate(statuscx=as.character(statuscx)) %>% 
  distinct_all() %>%
  mutate(source_name="NDoH File")


# FINAL MERGE
final_df<-tempFile1.2 %>% 
  left_join(Genie_TB_ART, by=c("dataelementuid","categoryoptioncombouid","orgunituid",
                               "mech_code")) %>% 
  filter(!is.na(value)) %>% 
  mutate(indicator="TB_ART_QUARTERLY") %>% 
  select(-psnu.y,key) %>% 
  rename(psnu=psnu.x)


# DATA CHECKS ------------------------------------------------------------------
check<-final_df %>% 
  group_by(indicator,period) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  print()

# NAS
NAS<-final_df %>% 
  filter(is.na(sitename)) %>% 
  distinct(period,orgunituid,organisationUnit) %>% 
  rename(sitename=organisationUnit)

genie_sites<-Genie %>% 
  filter(indicator=="TB_ART",
         standardizeddisaggregate=="Total Numerator") %>% 
  reshape_msd("long") %>% 
  distinct(period,orgunituid,sitename)

missing_datim<-NAS %>% 
  anti_join(genie_sites,by=c("period","orgunituid","sitename"))

MFOdBYFcwLV<-tempFile1.2 %>% 
  filter(orgunituid=="MFOdBYFcwLV") %>%
  group_by(orgunituid,period) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE)

# EXPORT
write.xlsx(final_df,file.path(here("Dataout","TB_ART_MSD_2022.xlsx")))
