# Title:HRH_STAFF_NAT MER Reporting Script
# Author: C. Trapence
# Purpose: Automating the process of Reporting AGYW_PREV for Inter-agency
# Date:2023-10-26
# Updated:2023:10:16 by C.trapence @ 02:10pm
#Load Required libraries
# Red text symbolizes comments

#######################################################################################################################
#  sources files/Inputs used in the code include:                                                                            #
#              1) Data  collected by IP's                                                                          #
#              2) Host Country Results SUB NAT (USG) from DATIM Support                                                #
#              3) Data Exchange Organisation Units from DATIM Support                                                 #
#              4) Mechanisms from DATIM support
#              5) Master Facility List
#######################################################################################################################


#'[GLOBAL VARIABLES --------------------------------------------------------

current_quarter<-"FY23Q4"

# packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, patchwork, googledrive,googlesheets4,openxlsx,lubridate,janitor,readr, esquisse, flextable,stringr,sqldf)

#'[Load Data from Individual Partners google sheets for Level one review
RTC<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1arUiP1-COq1UOF5PLy9eXppZZOHyucgOPnDCcBGe4FU/edit#gid=1970442414"), sheet = "COP22 STAFF_NAT Reporting") %>%
filter(!row_number() %in% c(1, 2)) %>% janitor::row_to_names(1) %>% select(OrgUnit  ,contains("Count")) %>% mutate_at(2:20, as.numeric) %>%
mutate(AttributeOptionCombo="R6zwVobwi58",mech_name="RTC") %>%  mutate(OrgUnit=if_else(OrgUnit=="fs OR Tambo Clinic","fs OR Tambo (Senekal) Clinic",OrgUnit))

ANOVA<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1Wy-3eHpz3WaaLzYQqN7hN8bYbS0nwkUOBG90ZtXP4-4/edit#gid=1970442414"), sheet = "COP22 STAFF_NAT Reporting") %>% filter(!row_number() %in% c(1, 2)) %>% janitor::row_to_names(1) %>% select(OrgUnit  ,contains("Count")) %>% mutate_at(2:20, as.numeric) %>%
mutate(AttributeOptionCombo="LbZtY0khSQw",mech_name="ANOVA")

MatCH<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1jkX5QkF8UOkZRSjJowAzcI_tKe_TfTTYix6kdVNS26k/edit#gid=1970442414"), sheet = "COP22 STAFF_NAT Reporting") %>% filter(!row_number() %in% c(1, 2)) %>% janitor::row_to_names(1) %>% select(OrgUnit  ,contains("Count")) %>% mutate_at(2:20, as.numeric) %>%
mutate(AttributeOptionCombo="Sm6Y3REDZ42",mech_name="MatCH")
WRHI<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/11F1ufkHel5e2JF9Jotf5k-WdhzM-J7fyHEKvcgNwjVM/edit#gid=1970442414"), sheet = "COP20 STAFF_NAT Reporting") %>% filter(!row_number() %in% c(1, 2)) %>% janitor::row_to_names(1) %>% select(OrgUnit  ,contains("Count")) %>% mutate_at(2:20, as.numeric) %>%
mutate(AttributeOptionCombo="Rv3LaFFxBCY",mech_name="WRHI")
BRCH<-read_sheet(as_sheets_id('1fKmJub5vuLLHOltoilHbvwUFj6llRBBzN2a0zHDTpwI'), sheet = "COP22 STAFF_NAT Reporting", col_types="c") %>% filter(!row_number() %in% c(1, 2)) %>% janitor::row_to_names(1) %>% select(OrgUnit  ,contains("Count")) %>% mutate_at(2:20, as.numeric) %>%
mutate(AttributeOptionCombo="koVrJ0HjBxy",mech_name="BRCH")

BRCH<-sqldf("select mech_name, OrgUnit,AttributeOptionCombo   ,sum(`Medical Officer - Position Count`) , sum(`Professional Nurse - Position Count`), sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`),sum(`Other - Clinical - Position Count`) , sum(`Lab Employee - Position Count`) , sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),sum(`Management - Other - Position Count` ), sum( `Social Service - Position Count`), sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`), sum(`Linkage Officer - Position Count` )  ,sum(`WBOT CHW - Position Count`)  , sum(`Other CHW (non-WBOT) - Position Count` ),
sum(`Other - Lay - Position Count`),sum(`Data Capturer - Position Count`),sum(`Data Clerk - Position Count`)  , sum(`Other - HCW\n- Position Count` )     from BRCH group by OrgUnit")


ANOVA<-sqldf("select mech_name,OrgUnit,AttributeOptionCombo   ,sum(`Medical Officer - Position Count`) , sum(`Professional Nurse - Position Count`), sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`),sum(`Other - Clinical - Position Count`) , sum(`Lab Employee - Position Count`) , sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),sum(`Management - Other - Position Count` ), sum( `Social Service - Position Count`), sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`), sum(`Linkage Officer - Position Count` )  ,sum(`WBOT CHW - Position Count`)  , sum(`Other CHW (non-WBOT) - Position Count` ),
sum(`Other - Lay - Position Count`),sum(`Data Capturer - Position Count`),sum(`Data Clerk - Position Count`)  , sum(`Other - HCW\n- Position Count` )     from ANOVA group by OrgUnit")



MatCH<-sqldf("select mech_name,OrgUnit,AttributeOptionCombo   ,sum(`Medical Officer - Position Count`) , sum(`Professional Nurse - Position Count`), sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`),sum(`Other - Clinical - Position Count`) , sum(`Lab Employee - Position Count`) , sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),sum(`Management - Other - Position Count` ), sum( `Social Service - Position Count`), sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`), sum(`Linkage Officer - Position Count` )  ,sum(`WBOT CHW - Position Count`)  , sum(`Other CHW (non-WBOT) - Position Count` ),
sum(`Other - Lay - Position Count`),sum(`Data Capturer - Position Count`),sum(`Data Clerk - Position Count`)  , sum(`Other - HCW\n- Position Count` )     from MatCH group by OrgUnit")




WRHI<-sqldf("select mech_name, OrgUnit,AttributeOptionCombo   ,sum(`Medical Officer - Position Count`) , sum(`Professional Nurse - Position Count`), sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`),sum(`Other - Clinical - Position Count`) , sum(`Lab Employee - Position Count`) , sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),sum(`Management - Other - Position Count` ), sum( `Social Service - Position Count`), sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`), sum(`Linkage Officer - Position Count` )  ,sum(`WBOT CHW - Position Count`)  , sum(`Other CHW (non-WBOT) - Position Count` ),
sum(`Other - Lay - Position Count`),sum(`Data Capturer - Position Count`),sum(`Data Clerk - Position Count`)  , sum(`Other - HCW\n- Position Count` )     from WRHI group by OrgUnit")

RTC<-sqldf("select mech_name, OrgUnit,AttributeOptionCombo   ,sum(`Medical Officer - Position Count`) , sum(`Professional Nurse - Position Count`), sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`),sum(`Other - Clinical - Position Count`) , sum(`Lab Employee - Position Count`) , sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),sum(`Management - Other - Position Count` ), sum( `Social Service - Position Count`), sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`), sum(`Linkage Officer - Position Count` )  ,sum(`WBOT CHW - Position Count`)  , sum(`Other CHW (non-WBOT) - Position Count` ),
sum(`Other - Lay - Position Count`),sum(`Data Capturer - Position Count`),sum(`Data Clerk - Position Count`)  , sum(`Other - HCW\n- Position Count` )     from RTC group by OrgUnit")


partners_data<-bind_rows(BRCH,WRHI,MatCH,ANOVA,RTC) %>% relocate(mech_name,AttributeOptionCombo) %>% rename(facility=OrgUnit)


partners_data <-partners_data %>%  mutate(Clinical=`sum(\`Medical Officer - Position Count\`)`+`sum(\`Professional Nurse - Position Count\`)`+`sum(\`Enrolled Nurse - Position Count\`)`+`sum(\`WBOT OTL - Position Count\`)`
+`sum(\`Other - Clinical - Position Count\`)`,Laboratory=`sum(\`Lab Employee - Position Count\`)`,Pharmacist=`sum(\`Pharmacist / Pharmacist Assistant - Position Count\`)` +`sum(\`Other - Pharmacy - Position Count\`)` ,Management=`sum(\`Management - Other - Position Count\` )`,
Social_Services=`sum( \`Social Service - Position Count\`)` +`sum(\`Other - Social Service - Position Count\`)` ,Lay=`sum(\`Lay Counselor - Position Count\`)`+`sum(\`Linkage Officer - Position Count\` )` +`sum(\`WBOT CHW - Position Count\`)` +`sum(\`Other CHW (non-WBOT) - Position Count\` )` +
  `sum(\`Other - Lay - Position Count\`)`,Other_HCW=`sum(\`Data Capturer - Position Count\`)` +`sum(\`Data Clerk - Position Count\`)`  +`sum(\`Other - HCW\n- Position Count\` )`  )

partners_data<-partners_data%>% select(mech_name,AttributeOptionCombo,facility,Clinical,Laboratory,Pharmacist,Management,Lay, Other_HCW ,Social_Services)

partners_data_v1<- partners_data %>%   pivot_longer(cols = Clinical:Social_Services,     names_to = "code",    values_to = "Value") %>% mutate(Dataelement="Kk4CdspETNQ",
CategoryOptionCombo=case_when(str_detect(code,"Clin")~"mkOfrTuz7tS",
str_detect(code,"Lab")~"T1jZtIrfVkq",str_detect(code,"Lay")~"a9N5X73zhET",str_detect(code,"Mana")~"oaRfTQD4RLG",
str_detect(code,"Other")~"wKH5X6oHquw",str_detect(code,"Pharm")~"VYMJrOJU5rQ",TRUE~"itxIkeWqiE9"),Period="2023Q3")

#partners_data_v2<- sqldf("select * from partners_data_v1 group by facility,CategoryOptionCombo ")
#Checking for all cadres's code
print(distinct(partners_data_v1,code))
#write.xlsx(partners_data_v1,"Data_View.xlsx")
# MFL ---------------------------------------------
# MFL ---------------------------------------------

###Using Org Units
orgunits<-list.files(here("data"),pattern = "Data")
orgunits<-read.csv(here("data", orgunits)) %>% filter(orgunit_internal_id!="KJ6EYihrWdp")
#MFL_FY24_ID<-"1UuDYK4X-Lr8avq4IUsKGWChr17AE4Bc21MlHO3OWB-Y"
#mfl_fy23_id <- "1QWVeT2wXA428WLYBy6-X5vP9A6sXjLjdtmkvLTw3mJA"



#mfl_new_df <- googlesheets4::read_sheet(mfl_fy23_id, sheet = "MFL_FY23")

# #get mech info from MFL
# mech_mfl <- mfl_new_df %>%
#   dplyr::filter(!is.na(OU2name)) %>%
#   janitor::clean_names() %>%
#   dplyr::select(ou5name, datim_uid,partner, mechanism_i_d, mechanism_uid) %>%
#   rename(sitename = ou5name,
#          facilityuid = datim_uid,
#          prime_partner_name = partner,
#          mech_code = mechanism_i_d,
#          mech_uid = mechanism_uid) %>% distinct()


#Final Import data

#Final_Df<-left_join(partners_data_v1 ,mech_mfl,by=c("facility"="sitename","AttributeOptionCombo"="mech_uid"))%>%  rename(OrgUnit=facilityuid)%>% mutate(OrgUnit=if_else(OrgUnit=="ekRXPVQUGw4","eHGSLAgEbWP",OrgUnit))

Final_Df<-left_join(partners_data_v1 ,orgunits,by=c("facility"="orgunit_name")) %>%  rename(OrgUnit=orgunit_internal_id  ) %>% mutate(AttributeOptionCombo="HllvX50cXC0")
#%>% mutate(AttributeOptionCombo="HllvX50cXC0")
#In case of error use this code AttributeOptionCombo="HllvX50cXC0"
#Data Review


#Checking mismatches between MFL & reported data
#Check1<-Final_Df %>% filter(is.na(OrgUnit))

#print(distinct(Final_Df,code))
#write.xlsx(Check1,"HRH_Site_clean.xlsx")

Final_Df_Import<-Final_Df %>% select(Dataelement,Period,OrgUnit,CategoryOptionCombo,AttributeOptionCombo,Value) %>% filter(!is.na(Value) ,Value>0)

Filename<-paste0(current_quarter, "HRH_STAFF_NAT_import",Sys.Date(),".CSV")

write_csv(Final_Df_Import,here("dataout",Filename))



######
