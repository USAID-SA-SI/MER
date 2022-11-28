
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
Genie_TB_ART<-Genie %>% filter(indicator=="TB_ART" )  %>%  select(-(qtr1:qtr4),-cumulative) %>%  filter(fiscal_year==2022) 

#This part is using the file the Abe  consolidated for the previous quarters.We weill tweek this code to use Our process inport file in FY23
tempFile<-read.csv("TB_ART_Quarterly 2022-11-17.csv") %>% mutate(dataelementuid=if_else(dataElementName=="TB_ART_Quarterly (N, TA, Age/Sex/NewExistingArt/HIVStatus): TB/HIV on ART","Szuf9YjHjTL","Qc1AaYpKsjs")) %>% 
 mutate(period= recode(period,"2021Q4"="FY22Q1","2022Q1"="FY22Q2","2022Q2"="FY22Q3","2022Q3"="FY22Q4")) %>% mutate(coc=gsub(" ","",coc))

#Data sets, elements and combos paramaterized from DATIM
Data_elements_cd<-read.csv("Data sets, elements and combos paramaterized.csv")%>% mutate(categoryoptioncombo =gsub(" ","",categoryoptioncombo ))

#Cleaning the category option combo from Abe's file
tempFile1.1<-left_join(tempFile,Data_elements_cd,by=c("coc"="categoryoptioncombo","dataelementuid")) %>%  select (mechCode,attributeOptionCombo,	orgUnitUID,	dataelementuid,	categoryoptioncombouid	,value,	period)

#Linking NDOH to genie skeleton

New_genie<-left_join(tempFile1.1,Genie_TB_ART,by=c("orgUnitUID"="orgunituid","categoryoptioncombouid"="categoryoptioncombouid")) %>% 
  
  filter(!is.na(sitename)) %>%  spread(key = period,value = value) %>% rename_with( str_replace,pattern="FY22Q",replacement="qtr",matches('FY22Q' )) %>%  mutate(cumulative=qtr4) %>% select(-(mechCode:categoryoptioncombouid))

#Final MSD like output
write.xlsx(New_genie,"TB_ART_MSD_2022.xlsx")





  
