
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
library(glamr)
library(openxlsx)
library(sqldf)
library(gophr)
library(lubridate)
library(glue)
library(validate)
library(gargle)

setwd("C:/Users/ctrapence/Documents")

fiscal_quarter <- "FY23Q1"


here::here()
#GENIE REF TABLE CREATION
genie_files<-list.files(here::here("Data"),pattern="Daily_TB")

genie_files_FY23<-list.files(here::here("Data"),pattern="Genie_SITE_IM_FY23")

Genie_FY23<-read_delim(here::here("Data",genie_files_FY23)) 

hierarchy_FY23<-Genie_FY23 %>% 
  filter(fiscal_year=="2023") %>% 
  distinct(mech_code,mech_name,orgunituid,sitename,operatingunit,indicatortype,
           operatingunituid,country,snu1,snu1uid,psnu,psnuuid,snuprioritization,
           typemilitary,dreams,communityuid,community,facilityuid,facility,
           sitetype) %>% distinct()


Genie<-read_tsv(here::here("Data",genie_files)) 

hierarchy<-Genie %>% 
  filter(fiscal_year=="2022") %>% 
  distinct(mech_code,mech_name,orgunituid,sitename,operatingunit,indicatortype,
           operatingunituid,country,snu1,snu1uid,psnu,psnuuid,snuprioritization,
           typemilitary,dreams,communityuid,community,facilityuid,facility,
           sitetype) %>% distinct()

hierarchy<-rbind(hierarchy,hierarchy_FY23) %>% distinct() %>% select(-facility)


data_elements<-Genie %>% 
  distinct(dataelementuid,indicator,numeratordenom,indicatortype,disaggregate,
           standardizeddisaggregate,categoryoptioncombouid,categoryoptioncomboname,
           ageasentered,age_2018,age_2019,trendscoarse,sex,statushiv,statustb,
           statuscx,hiv_treatment_status,otherdisaggregate,otherdisaggregate_sub,
           modality) %>% filter(indicator=="TB_ART") %>% 
unite(key,dataelementuid,categoryoptioncombouid) %>% mutate(key=gsub(" ","",tolower(key)))

TB_mapping_file<-list.files(here::here("Data"),pattern="ART_Mapp")

Mapping_file<-read.xlsx(here::here("Data",TB_mapping_file),sheet="Sheet1") %>% select(facility,code,orgunituid)


Mapping_file<-merge(Mapping_file,hierarchy,by=c("orgunituid")) %>% distinct(orgunituid,.keep_all = TRUE) 

# IMPORT FORMATTED FILE FROM NDoH

NDOH<-list.files(here::here("Data"),pattern="Final")
tempfile<-read.xlsx((here::here("Data",NDOH)),sheet = "TB_ART" )  %>% mutate(FineAgeGroup=case_when(FineAgeGroup=="50-54" |FineAgeGroup=="55-59"|FineAgeGroup=="60-64" | FineAgeGroup=="65+"~"50+",TRUE ~FineAgeGroup))

tempfile<-left_join(tempfile,Mapping_file,by=c("Code"="code")) %>% 
  rename(dsd_ta=indicatortype) %>% mutate(dataelementuid=if_else(dsd_ta=="TA","Szuf9YjHjTL","Qc1AaYpKsjs")) %>% 
  mutate(category_temp=case_when(Sex=="Female" & (FineAgeGroup=="10-14" & ARTStatus=="New on ART") ~"LlsXRvw2WCa", Sex=="Male" & (FineAgeGroup=="10-14" & ARTStatus=="New on ART") ~"B3GuvVsPezs",
                                 Sex=="Female" & (FineAgeGroup=="15-19" & ARTStatus=="New on ART") ~"VLZuKB5ZxAS",Sex=="Male" & (FineAgeGroup=="15-19" & ARTStatus=="New on ART") ~"L94UC0mTPiS",
                                 Sex=="Female" & (FineAgeGroup=="20-24" & ARTStatus=="New on ART") ~"FckvpCkm80Y", Sex=="Male" & (FineAgeGroup=="20-24" & ARTStatus=="New on ART") ~"CTdgwVnmU4t",
                                 Sex=="Female" & (FineAgeGroup=="25-29" & ARTStatus=="New on ART") ~"PZfvIT6x87t",Sex=="Male" & (FineAgeGroup=="25-29" & ARTStatus=="New on ART") ~"okUjaLgimz6",
                                 Sex=="Female" & (FineAgeGroup=="30-34" & ARTStatus=="New on ART") ~"sNN69TORr55",Sex=="Male" & (FineAgeGroup=="30-34" & ARTStatus=="New on ART") ~"PjCWWE6SRJc",
                                 Sex=="Female" & (FineAgeGroup=="35-39" & ARTStatus=="New on ART") ~"NM0O8rpozsb",Sex=="Male" & (FineAgeGroup=="35-39" & ARTStatus=="New on ART") ~"o7pTMaoJf1P",
                                 Sex=="Female" & (FineAgeGroup=="40-44" & ARTStatus=="New on ART") ~"uK81Q5JSE2C",Sex=="Male" & (FineAgeGroup=="40-44" & ARTStatus=="New on ART") ~"E7IY48LF1ai",
                                 Sex=="Female" & (FineAgeGroup=="45-49" & ARTStatus=="New on ART") ~"nMY8JaK7MKa",Sex=="Male" & (FineAgeGroup=="45-49" & ARTStatus=="New on ART") ~"cPjx1nSG7kh",
                                 Sex=="Female" & (FineAgeGroup=="50+" & ARTStatus=="New on ART") ~"Kf4QAMTrtmg",Sex=="Male" & (FineAgeGroup=="50+" & ARTStatus=="New on ART") ~"GgZWK5MpPQ5",
                                 Sex=="Female" & (FineAgeGroup=="10-14" & ARTStatus=="Already on ART") ~"kgvhGR4EKcK", Sex=="Male" & (FineAgeGroup=="10-14" & ARTStatus=="Already on ART") ~"GpSNvYc07tz",
                                 Sex=="Female" & (FineAgeGroup=="15-19" & ARTStatus=="Already on ART") ~"zq43uEufKnG",Sex=="Male" & (FineAgeGroup=="15-19" & ARTStatus=="Already on ART") ~"utzUqBePahs",
                                 Sex=="Female" & (FineAgeGroup=="20-24" & ARTStatus=="Already on ART") ~"EJ5vQnO5114", Sex=="Male" & (FineAgeGroup=="20-24" & ARTStatus=="Already on ART") ~"Rmwyz2pyabR",
                                 Sex=="Female" & (FineAgeGroup=="25-29" & ARTStatus=="Already on ART") ~"QU5aTm14uA5",Sex=="Male" & (FineAgeGroup=="25-29" & ARTStatus=="Already on ART") ~"X49cjRccRAw",
                                 Sex=="Female" & (FineAgeGroup=="30-34" & ARTStatus=="Already on ART") ~"kb4QVVLaKnA",Sex=="Male" & (FineAgeGroup=="30-34" & ARTStatus=="Already on ART") ~"pjBfnMMU8yB",
                                 Sex=="Female" & (FineAgeGroup=="35-39" & ARTStatus=="Already on ART") ~"xp6h8T3cOfh",Sex=="Male" & (FineAgeGroup=="35-39" & ARTStatus=="Already on ART") ~"kutUEi1Fp8G",
                                 Sex=="Female" & (FineAgeGroup=="40-44" & ARTStatus=="Already on ART") ~"gzztFm4KH6T",Sex=="Male" & (FineAgeGroup=="40-44" & ARTStatus=="Already on ART") ~"Kx0Ow7YSDv3",
                                 Sex=="Female" & (FineAgeGroup=="45-49" & ARTStatus=="Already on ART") ~"ZfvmeFeTV45",Sex=="Male" & (FineAgeGroup=="45-49" & ARTStatus=="Already on ART") ~"SFpj8nvfCkv",
                                 Sex=="Female" & (FineAgeGroup=="50+" & ARTStatus=="Already on ART") ~"KSZqwBFQVqD",Sex=="Male" & (FineAgeGroup=="50+" & ARTStatus=="Already on ART") ~"fWwSMmi37De") ) %>%
  
                                 dplyr::rename(value=Total)  %>%  mutate(category_temp=ifelse(grepl( "<",FineAgeGroup) & Sex=="Female" & ARTStatus=="New on ART","cW1wQgs5hyV",
                                 ifelse(grepl( "<",FineAgeGroup) & Sex=="Male" & ARTStatus=="New on ART" ,"MQZqURahCb8", ifelse(grepl("1-4",FineAgeGroup) & Sex=="Female" & ARTStatus=="New on ART","JTZmQVEtlTV",
                                 ifelse(Sex=="Male" & grepl("1-4",FineAgeGroup) & ARTStatus=="New on ART","ay15X6h55Py",ifelse(Sex=="Female" & grepl("5-9",FineAgeGroup) & ARTStatus=="New on ART","AGQa2tzIoc1",
                                ifelse(Sex=="Male" & grepl("5-9",FineAgeGroup) & ARTStatus=="New on ART","suJrZbdKKRW",category_temp))))))) %>%  
                                 mutate(category_temp=ifelse(grepl( "<",FineAgeGroup) & Sex=="Female" & ARTStatus=="Already on ART"," Cq5xrLF7MiB",
                                ifelse(grepl( "<",FineAgeGroup) & Sex=="Male" & ARTStatus=="Already on ART" ," E2lY8t3CmI5", ifelse(grepl("1-4",FineAgeGroup) & Sex=="Female" & ARTStatus=="Already on ART"," F6uPBw7dmhp",
                                ifelse(Sex=="Male" & grepl("1-4",FineAgeGroup) & ARTStatus=="Already on ART"," rS9g7UL0rDN",ifelse(Sex=="Female" & grepl("5-9",FineAgeGroup) & ARTStatus=="Already on ART"," ZufsQv0cYSM",
                                ifelse(Sex=="Male" & grepl("5-9",FineAgeGroup) & ARTStatus=="Already on ART"," r1p6hui37CP",category_temp))))))) %>%  
                                unite(key,dataelementuid,category_temp) %>% mutate(key=gsub(" ","",tolower(key)))
      
# MERGE IMPORT REF TABLES FROM GENIE & DATIM API
final_df<-tempfile %>% 
  left_join(data_elements,by="key") %>% 
  mutate(mech_code=as.character(mech_code))  %>% select(-Code,-ARTStatus,-sex,-key,-dsd_ta,-FineAgeGroup,-Facility,-SubDistrict,-District, -Province)%>% 
  
  mutate(period_type="results",
         indicator="TB_ART_NDOH_QUARTERLY",period="FY23Q1") 

final_df<-rename_official(final_df, datim_user = "ctrapencepretoria_ia",datim_pwd = "Clement@1986")

check<-final_df %>% 
  group_by(indicator,period) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  print()

# EXPORT

filename<-paste(Sys.Date(),"TB_ART_QUARTERLY",fiscal_quarter,".txt",sep="_")

write_tsv(final_df, file.path(here("Dataout"),filename,na=""))
