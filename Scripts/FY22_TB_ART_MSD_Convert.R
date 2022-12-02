
# Title: MSD Convert Script
# Author: C. Trapence
# Purpose: Automating the process of Reporting AGYW_PREV for Inter-agency
# Date:2022-11-22
#Load Required libraries
# Red text symbolizes comments

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

#Easy search

file_pattern <- 'Df\\.[0-9]\\.csv' # regexp pattern to match the file name format

#Preparing the working directory for file retrieval and export
setwd("C:\\Users\\ctrapence\\Documents\\Clement Trapence-South Africa WP\\SCRIPTS\\TB_ART\\")

#Reading genie data for setting up the framework
Genie<-read.delim2(("C:\\Users\\ctrapence\\Documents\\Clement Trapence-South Africa WP\\SCRIPTS\\TB_ART\\Genie.txt")) 

#MER Structured -This is to get the structure 
Genie_TB_ART<-Genie %>% filter(indicator=="TB_ART"  | grepl("TB_STAT",indicator))  %>%  select(-(qtr1:qtr4),-cumulative) %>%  filter(fiscal_year==2022) 

#This part is using the file the Abe  consolidated for the previous quarters.We weill tweek this code to use Our process inport file in FY23
tempFile<-read.csv("TB_ART_Quarterly 2022-11-17.csv") %>% mutate(dataelementuid=if_else(dataElementName=="TB_ART_Quarterly (N, TA, Age/Sex/NewExistingArt/HIVStatus): TB/HIV on ART","Szuf9YjHjTL","Qc1AaYpKsjs")) %>% 
 mutate(period= recode(period,"2021Q4"="FY22Q1","2022Q1"="FY22Q2","2022Q2"="FY22Q3","2022Q3"="FY22Q4")) %>% mutate(coc=gsub(" ","",coc))

#Data sets, elements and combos paramaterized from DATIM
Data_elements_cd<-read.csv("Data sets, elements and combos paramaterized.csv")%>% mutate(categoryoptioncombo =gsub(" ","",categoryoptioncombo ))

#Cleaning the category option combo from Abe's file
tempFile1.1<-left_join(tempFile,Data_elements_cd,by=c("coc"="categoryoptioncombo","dataelementuid")) %>%  select (mechCode,attributeOptionCombo,	orgUnitUID,	dataelementuid,	categoryoptioncombouid	,value,	period) %>% rename(dataelementuid=dataelementuid)

#Linking NDOH to genie skeleton

New_genie<-left_join(tempFile1.1,Genie_TB_ART,by=c("orgUnitUID"="orgunituid","categoryoptioncombouid"="categoryoptioncombouid","dataelementuid")) %>% 
  
  filter(!is.na(sitename)) %>%  spread(key = period,value = value) %>% rename_with( str_replace,pattern="FY22Q",replacement="qtr",matches('FY22Q' )) %>%  mutate(cumulative=qtr4) %>% select(-targets)

#Adding TB_STAT_POS to create or mimic DATIM targets#########
#TB_STAT_POS is a denominator for TB_ART 

#MER Structured -This is to get the structure 
Genie_TB_STAT<-Genie %>% filter(indicator=="TB_STAT" & statushiv=="Positive") %>%  select(-targets)  %>%  filter(fiscal_year==2022) %>% mutate(targets=as.numeric(cumulative)) %>%  
  
  select(indicator,statushiv,categoryoptioncomboname,orgunituid,dataelementuid,categoryoptioncombouid,-mech_code,targets) %>% 
  group_by(indicator,statushiv,categoryoptioncomboname,orgunituid,dataelementuid,categoryoptioncombouid,targets) %>% 
  summarise(targets=sum(targets)) %>%  mutate(category_temp=case_when(
  categoryoptioncomboname=="<1, Newly Tested Positives, Female"~"cW1wQgs5hyV",
  categoryoptioncomboname=="<1, Newly Tested Positives, Male"~"MQZqURahCb8",
  categoryoptioncomboname=="1-4, Newly Tested Positives, Female"~"JTZmQVEtlTV",
  categoryoptioncomboname=="1-4, Newly Tested Positives, Male"~"ay15X6h55Py",
  categoryoptioncomboname=="5-9, Newly Tested Positives, Female"~"AGQa2tzIoc1",
  categoryoptioncomboname=="5-9, Newly Tested Positives, Male"~"suJrZbdKKRW",
  categoryoptioncomboname=="10-14, Newly Tested Positives, Female"~"LlsXRvw2WCa",
  categoryoptioncomboname=="10-14, Newly Tested Positives, Male"~"B3GuvVsPezs",
  categoryoptioncomboname=="15-19, Newly Tested Positives, Female"~"VLZuKB5ZxAS",
  categoryoptioncomboname=="15-19, Newly Tested Positives, Male"~"L94UC0mTPiS",
  categoryoptioncomboname=="20-24, Newly Tested Positives, Female"~"FckvpCkm80Y",
  categoryoptioncomboname=="20-24, Newly Tested Positives, Male"~"CTdgwVnmU4t",
  categoryoptioncomboname=="25-29, Newly Tested Positives, Female"~"PZfvIT6x87t",
  categoryoptioncomboname=="25-29, Newly Tested Positives, Male"~"okUjaLgimz6",
  categoryoptioncomboname=="30-34, Newly Tested Positives, Female"~"sNN69TORr55",
  categoryoptioncomboname=="30-34, Newly Tested Positives, Male"~"PjCWWE6SRJc",
  categoryoptioncomboname=="35-39, Newly Tested Positives, Female"~"NM0O8rpozsb",
  categoryoptioncomboname=="35-39, Newly Tested Positives, Male"~"o7pTMaoJf1P",
  categoryoptioncomboname=="40-44, Newly Tested Positives, Female"~"uK81Q5JSE2C",
  categoryoptioncomboname=="40-44, Newly Tested Positives, Male"~"E7IY48LF1ai",
  categoryoptioncomboname=="45-49, Newly Tested Positives, Female"~"nMY8JaK7MKa",
  categoryoptioncomboname=="45-49, Newly Tested Positives, Male"~"cPjx1nSG7kh",
  categoryoptioncomboname=="50-54, Newly Tested Positives, Female"~"Kf4QAMTrtmg",
  # categoryoptioncomboname=="50-54, Newly Tested Positives, Male"~"50-54,Male,Life-longART,New,Positive",
  # categoryoptioncomboname=="55-59, Newly Tested Positives, Female"~"55-59,Female,Life-longART,New,Positive",
  # categoryoptioncomboname=="55-59, Newly Tested Positives, Male"~"55-59,Male,Life-longART,New,Positive",
  # categoryoptioncomboname=="60-64, Newly Tested Positives, Female"~"60-64,Female,Life-long ART,New,Positive",
  # categoryoptioncomboname=="60-64, Newly Tested Positives, Mmale"~"60-64,Male,Life-longART,New,Positive",
  # categoryoptioncomboname=="65+, Newly Tested Positives, Female"~"65+,Female,Life-longART,New,Positive",
  # categoryoptioncomboname=="65+, Newly Tested Positives, Male"~"65+,Male,Life-longART,New,Positive",
  
  categoryoptioncomboname=="<1, Known Positives, Female"~"Cq5xrLF7MiB",
  categoryoptioncomboname=="<1, Known Positives, Male"~"E2lY8t3CmI5",
  categoryoptioncomboname=="1-4, Known Positives, Female"~"F6uPBw7dmhp",
  categoryoptioncomboname=="1-4, Known Positives, Male"~"rS9g7UL0rDN",
  categoryoptioncomboname=="5-9, Known Positives, Female"~"ZufsQv0cYSM",
  categoryoptioncomboname=="5-9, Known Positives, Male"~"r1p6hui37CP",
  categoryoptioncomboname=="10-14, Known Positives, Female"~"kgvhGR4EKcK",
  categoryoptioncomboname=="10-14, Known Positives, Male"~"GpSNvYc07tz",
  categoryoptioncomboname=="15-19, Known Positives, Female"~"zq43uEufKnG",
  categoryoptioncomboname=="15-19, Known Positives, Male"~"utzUqBePahs",
  categoryoptioncomboname=="20-24, Known Positives, Female"~"EJ5vQnO5114",
  categoryoptioncomboname=="20-24, Known Positives, Male"~"Rmwyz2pyabR",
  categoryoptioncomboname=="25-29, Known Positives, Female"~"QU5aTm14uA5",
  categoryoptioncomboname=="25-29, Known Positives, Male"~"X49cjRccRAw",
  categoryoptioncomboname=="30-34, Known Positives, Female"~"kb4QVVLaKnA",
  categoryoptioncomboname=="30-34, Known Positives, Male"~"pjBfnMMU8yB",
  categoryoptioncomboname=="35-39, Known Positives, Female"~"xp6h8T3cOfh",
  categoryoptioncomboname=="35-39, Known Positives, Male"~"kutUEi1Fp8G",
  categoryoptioncomboname=="40-44, Known Positives, Female"~"gzztFm4KH6T",
  categoryoptioncomboname=="40-44, Known Positives, Male"~"Kx0Ow7YSDv3",
  categoryoptioncomboname=="45-49, Known Positives, Female"~"ZfvmeFeTV45",
  categoryoptioncomboname=="45-49, Known Positives, Male"~"SFpj8nvfCkv"
  # categoryoptioncomboname=="50-54, Known Positives, Female"~"50-54,Female,Life-longART,Already,Positive",
  # categoryoptioncomboname=="50-54, Known Positives, Male"~"50-54,Male,Life-longART,Already,Positive",
  # categoryoptioncomboname=="55-59, Known Positives, Female"~"55-59,Female,Life-longART,Already,Positive",
  # categoryoptioncomboname=="55-59, Known Positives, Male"~"55-59,Male,Life-longART,Already,Positive",
  # categoryoptioncomboname=="60-64, Known Positives, Female"~"60-64,Female,Life-long ART,Already,Positive",
  # categoryoptioncomboname=="60-64, Known Positives, Male"~"60-64,Male,Life-long ART,Already,Positive",
  # categoryoptioncomboname=="65+, Known Positives, Female"~"65+,Female,Life-long ART,Already,Positive",
  # categoryoptioncomboname=="65+, Known Positives, Male"~"65+,Male,Life-long ART,Already,Positive")
  ))
#Cleaning the category option combo from Abe's file
tempFile1.3<-left_join(New_genie,Genie_TB_STAT,by=c("categoryoptioncombouid"="category_temp","orgUnitUID"="orgunituid") ) %>%  select(-indicator.y,-statushiv.y  ,- indicator.y,-dataelementuid.y,-categoryoptioncomboname.y ,-categoryoptioncombouid.y)


#Final MSD like output


write.xlsx(tempFile1.3,"TB_ART_MSD_2022.xlsx")





  
