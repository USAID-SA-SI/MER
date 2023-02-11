
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

fiscal_quarter <- "FY23Q1"

df_fac <- clean_mfl()

#GENIE REF TABLE CREATION
genie_files<-list.files(here("Data"),pattern="Daily_TB")

Genie<-read_tsv(here("Data",genie_files)) 

hierarchy<-Genie %>% 
  filter(fiscal_year=="2023") %>% 
  distinct(mech_code,mech_name,orgunituid,sitename,operatingunit,
           operatingunituid,country,snu1,snu1uid,psnu,psnuuid,snuprioritization,
           typemilitary,dreams,communityuid,community,facilityuid,facility,
           sitetype)

data_elements<-Genie %>% 
  distinct(dataelementuid,indicator,numeratordenom,indicatortype,disaggregate,
           standardizeddisaggregate,categoryoptioncombouid,categoryoptioncomboname,
           ageasentered,age_2018,age_2019,trendscoarse,sex,statushiv,statustb,
           statuscx,hiv_treatment_status,otherdisaggregate,otherdisaggregate_sub,
           modality) %>% 
  unite(key,dataelementuid,categoryoptioncombouid,sep="_")


# IMPORT FORMATTED FILE FROM NDoH

NDOH<-list.files(here("Data"),pattern="Final")
tempfile<-read_xlsx((here("Data",NDOH)),sheet = "TB_ART" ) %>% rename(new_ou5_code=Code) %>% mutate(FineAgeGroup=case_when(FineAgeGroup=="50-54" |FineAgeGroup=="55-59"|FineAgeGroup=="60-64" | FineAgeGroup=="65+"~"50+",TRUE ~FineAgeGroup))

tempfile<-left_join(tempfile,df_fac,by=c("new_ou5_code"),USAI)  %>% mutate(dataelementuid=if_else(dsd_ta=="TA","Szuf9YjHjTL","Qc1AaYpKsjs"))%>% 
  mutate(category_temp=case_when(Sex=="Female" & (FineAgeGroup=="<1" & ARTStatus=="New on ART") ~"cW1wQgs5hyV",Sex=="Male" & (FineAgeGroup=="<1" & ARTStatus=="New on ART") ~"MQZqURahCb8",  
                                 Sex=="Female" & (FineAgeGroup=="1-4" & ARTStatus=="New on ART") ~"JTZmQVEtlTV", Sex=="Male" & (FineAgeGroup=="1-4" & ARTStatus=="New on ART")~"ay15X6h55Py" ,
                                 Sex=="Female" & (FineAgeGroup=="5-9" & ARTStatus=="New on ART") ~"AGQa2tzIoc1",Sex=="Male" & (FineAgeGroup=="5-9" & ARTStatus=="New on ART") ~"suJrZbdKKRW",
                                 Sex=="Female" & (FineAgeGroup=="10-14" & ARTStatus=="New on ART") ~"LlsXRvw2WCa", Sex=="Male" & (FineAgeGroup=="10-14" & ARTStatus=="New on ART") ~"B3GuvVsPezs",
                                 Sex=="Female" & (FineAgeGroup=="15-19" & ARTStatus=="New on ART") ~"VLZuKB5ZxAS",Sex=="Male" & (FineAgeGroup=="15-19" & ARTStatus=="New on ART") ~"L94UC0mTPiS",
                                 Sex=="Female" & (FineAgeGroup=="20-24" & ARTStatus=="New on ART") ~"FckvpCkm80Y", Sex=="Male" & (FineAgeGroup=="20-24" & ARTStatus=="New on ART") ~"CTdgwVnmU4t",
                                 Sex=="Female" & (FineAgeGroup=="25-29" & ARTStatus=="New on ART") ~"PZfvIT6x87t",Sex=="Male" & (FineAgeGroup=="25-29" & ARTStatus=="New on ART") ~"okUjaLgimz6",
                                 Sex=="Female" & (FineAgeGroup=="30-34" & ARTStatus=="New on ART") ~"sNN69TORr55",Sex=="Male" & (FineAgeGroup=="30-34" & ARTStatus=="New on ART") ~"PjCWWE6SRJc",
                                 Sex=="Female" & (FineAgeGroup=="35-39" & ARTStatus=="New on ART") ~"NM0O8rpozsb",Sex=="Male" & (FineAgeGroup=="35-39" & ARTStatus=="New on ART") ~"o7pTMaoJf1P",
                                 Sex=="Female" & (FineAgeGroup=="40-44" & ARTStatus=="New on ART") ~"uK81Q5JSE2C",Sex=="Male" & (FineAgeGroup=="40-44" & ARTStatus=="New on ART") ~"E7IY48LF1ai",
                                 Sex=="Female" & (FineAgeGroup=="45-49" & ARTStatus=="New on ART") ~"nMY8JaK7MKa",Sex=="Male" & (FineAgeGroup=="45-49" & ARTStatus=="New on ART") ~"cPjx1nSG7kh",
                                 Sex=="Female" & (FineAgeGroup=="50+" & ARTStatus=="New on ART") ~"Kf4QAMTrtmg",Sex=="Male" & (FineAgeGroup=="50+" & ARTStatus=="New on ART") ~"GgZWK5MpPQ5",
                                 Sex=="Female" & (FineAgeGroup=="<1" & ARTStatus=="Already on ART") ~"Cq5xrLF7MiB",Sex=="Male" & (FineAgeGroup=="<1" & ARTStatus=="Already on ART") ~"E2lY8t3CmI5",  
                                 Sex=="Female" & (FineAgeGroup=="1-4" & ARTStatus=="Already on ART") ~"F6uPBw7dmhp", Sex=="Male" & (FineAgeGroup=="1-4" & ARTStatus=="Already on ART")~"rS9g7UL0rDN" ,
                                 Sex=="Female" & (FineAgeGroup=="5-9" & ARTStatus=="Already on ART") ~"ZufsQv0cYSM",Sex=="Male" & (FineAgeGroup=="5-9" & ARTStatus=="Already on ART") ~"r1p6hui37CP",
                                 Sex=="Female" & (FineAgeGroup=="10-14" & ARTStatus=="Already on ART") ~"kgvhGR4EKcK", Sex=="Male" & (FineAgeGroup=="10-14" & ARTStatus=="Already on ART") ~"GpSNvYc07tz",
                                 Sex=="Female" & (FineAgeGroup=="15-19" & ARTStatus=="Already on ART") ~"zq43uEufKnG",Sex=="Male" & (FineAgeGroup=="15-19" & ARTStatus=="Already on ART") ~"utzUqBePahs",
                                 Sex=="Female" & (FineAgeGroup=="20-24" & ARTStatus=="Already on ART") ~"EJ5vQnO5114", Sex=="Male" & (FineAgeGroup=="20-24" & ARTStatus=="Already on ART") ~"Rmwyz2pyabR",
                                 Sex=="Female" & (FineAgeGroup=="25-29" & ARTStatus=="Already on ART") ~"QU5aTm14uA5",Sex=="Male" & (FineAgeGroup=="25-29" & ARTStatus=="Already on ART") ~"X49cjRccRAw",
                                 Sex=="Female" & (FineAgeGroup=="30-34" & ARTStatus=="Already on ART") ~"kb4QVVLaKnA",Sex=="Male" & (FineAgeGroup=="30-34" & ARTStatus=="Already on ART") ~"pjBfnMMU8yB",
                                 Sex=="Female" & (FineAgeGroup=="35-39" & ARTStatus=="Already on ART") ~"xp6h8T3cOfh",Sex=="Male" & (FineAgeGroup=="35-39" & ARTStatus=="Already on ART") ~"kutUEi1Fp8G",
                                 Sex=="Female" & (FineAgeGroup=="40-44" & ARTStatus=="Already on ART") ~"gzztFm4KH6T",Sex=="Male" & (FineAgeGroup=="40-44" & ARTStatus=="Already on ART") ~"Kx0Ow7YSDv3",
                                 Sex=="Female" & (FineAgeGroup=="45-49" & ARTStatus=="Already on ART") ~"ZfvmeFeTV45",Sex=="Male" & (FineAgeGroup=="45-49" & ARTStatus=="Already on ART") ~"SFpj8nvfCkv",
                                 Sex=="Female" & (FineAgeGroup=="50+" & ARTStatus=="Already on ART") ~"KSZqwBFQVqD",Sex=="Male" & (FineAgeGroup=="50+" & ARTStatus=="Already on ART") ~"fWwSMmi37De") ) %>%  
                                 unite(key,dataelementuid,category_temp,sep="_")%>% filter(usaid_facility!="NA") %>% rename(orgunituid=datim_uid,value=Total)
                                  

# MERGE IMPORT REF TABLES FROM GENIE & DATIM API
final_df<-tempfile %>% 
  left_join(data_elements,by="key")  %>% 
  left_join(hierarchy,by="orgunituid")%>% 
  mutate(mech_code=as.character(mech_code))  %>% select(-ARTStatus,-key,-Sex,-dsd_ta,-ou5uid ,-usaid_facility ,-new_ou5_code,-FineAgeGroup,-Facility,-SubDistrict,-District, -Province)%>% 
  rename_official()  %>% 
  mutate(period_type="results",
         indicator="TB_ART_NDOH_QUARTERLY")



check<-final_df %>% 
  group_by(indicator,period) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  print()


# EXPORT

filename<-paste(Sys.Date(),"TB_ART_QUARTERLY",fiscal_quarter,".txt",sep="_")

write_tsv(final_df, file.path(here("Dataout"),filename,na=""))
