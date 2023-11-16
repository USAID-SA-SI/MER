# Title:HRH_STAFF_NAT MER Reporting Script
# Author: C. Trapence
# Date:2023-10-26
# Updated:2023:11:10 by C.Trapence @ 02:10pm
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
Filename<-paste0(current_quarter, "_Final_HRH_STAFF_NAT_importV2",Sys.Date(),".CSV")

# packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, patchwork, googledrive,googlesheets4,openxlsx,lubridate,janitor,readr, esquisse, flextable,stringr,sqldf)

#'[Load Data from Individual Partners google sheets for Level one review
RTC<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1arUiP1-COq1UOF5PLy9eXppZZOHyucgOPnDCcBGe4FU/edit#gid=1970442414"), sheet = "COP22 STAFF_NAT Reporting") %>%
  filter(!row_number() %in% c(1, 2)) %>% janitor::row_to_names(1) %>% select(OrgUnit  ,contains("Count")) %>% mutate_at(2:20, as.numeric) %>%
  mutate(AttributeOptionCombo="R6zwVobwi58",mech_name="RTC") %>%  mutate(OrgUnit=if_else(OrgUnit=="fs OR Tambo Clinic","fs OR Tambo (Senekal) Clinic",OrgUnit))

ANOVA<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1Wy-3eHpz3WaaLzYQqN7hN8bYbS0nwkUOBG90ZtXP4-4/edit#gid=118260745"), sheet = "COP22 STAFF_NAT Reporting") %>% filter(!row_number() %in% c(1, 2)) %>% janitor::row_to_names(1) %>% select(OrgUnit  ,contains("Count")) %>% mutate_at(2:20, as.numeric) %>%
  mutate(AttributeOptionCombo="LbZtY0khSQw",mech_name="ANOVA")


MatCH<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1jkX5QkF8UOkZRSjJowAzcI_tKe_TfTTYix6kdVNS26k/edit#gid=1970442414"), sheet = "COP22 STAFF_NAT Reporting") %>% filter(!row_number() %in% c(1, 2)) %>% janitor::row_to_names(1) %>% select(OrgUnit  ,contains("Count")) %>% mutate_at(2:20, as.numeric) %>%
  mutate(AttributeOptionCombo="Sm6Y3REDZ42",mech_name="MatCH")
WRHI<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/11F1ufkHel5e2JF9Jotf5k-WdhzM-J7fyHEKvcgNwjVM/edit#gid=1970442414"), sheet = "COP20 STAFF_NAT Reporting") %>% filter(!row_number() %in% c(1, 2)) %>% janitor::row_to_names(1) %>% select(OrgUnit  ,contains("Count")) %>% mutate_at(2:20, as.numeric) %>%
  mutate(AttributeOptionCombo="Rv3LaFFxBCY",mech_name="WRHI")
BRCH<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1Kt3fqBL0Mp6L_pgAcGqiw3EcGVib86fbg3uFWx7jG_M/edit#gid=804516909'), sheet = "COP22 STAFF_NAT Reporting", col_types="c") %>% filter(!row_number() %in% c(1, 2)) %>% janitor::row_to_names(1) %>% select(OrgUnit  ,contains("Count")) %>% mutate_at(2:20, as.numeric) %>%
  mutate(AttributeOptionCombo="koVrJ0HjBxy",mech_name="BRCH")

BRCH<-sqldf("select mech_name, OrgUnit,AttributeOptionCombo   ,sum(`Medical Officer - Position Count`) , sum(`Professional Nurse - Position Count`), sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`),sum(`Other - Clinical - Position Count`) , sum(`Lab Employee - Position Count`) , sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),sum(`Management - Other - Position Count` ), sum( `Social Service - Position Count`), sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`), sum(`Linkage Officer - Position Count` )  ,sum(`WBOT CHW - Position Count`)  , sum(`Other CHW (non-WBOT) - Position Count` ),
sum(`Other - Lay - Position Count`),sum(`Data Capturer - Position Count`),sum(`Data Clerk - Position Count`)  , sum(`Other - HCW\n- Position Count` )     from BRCH group by OrgUnit")


ANOVA_2<-sqldf("select mech_name,OrgUnit,AttributeOptionCombo   ,sum(`Medical Officer - Position Count`) , sum(`Professional Nurse - Position Count`), sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`),sum(`Other - Clinical - Position Count`) , sum(`Lab Employee - Position Count`) , sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),sum(`Management - Other - Position Count` ), sum( `Social Service - Position Count`), sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`), sum(`Linkage Officer - Position Count` )  ,sum(`WBOT CHW - Position Count`)  , sum(`Other CHW (non-WBOT) - Position Count` ),
sum(`Other - Lay - Position Count`),sum(`Data Capturer - Position Count`),sum(`Data Clerk - Position Count`)  , sum(`Other - HCW\n- Position Count` )     from ANOVA group by OrgUnit") %>%
mutate(OrgUnit=case_when(OrgUnit=="lp Sophia Clinic/Sekwai Clinic"~"lp Sophia/Sekwai Clinic",OrgUnit=="wc Gordons Bay CDC"~"wc Gordon's Bay CDC",OrgUnit=="wc Pelican Park Clinic"~"wc Pelican Park Satellite Clinic",
                         OrgUnit=="wc Somerset West Clinic"~"wc Somerset West CDC",OrgUnit =="gp North West University Clinic"~"gp North West University Occupational Health Centre",OrgUnit =="gp Slovoville clinic"~"gp Slovoville Clinic",
                         OrgUnit =="gp Sonder Water Health Post"~"gp Sonder Water Health post",
                         OrgUnit =="lp Hlokomela Non-medical Site"~"lp Hlokomela Non-Medical Site",
                         OrgUnit =="lp Matsotsosela clinic"~"lp Matsotsosela Clinic",
                         OrgUnit =="lp Sophia Clinic/Sekwai Clinic"~"lp Sophia/Sekwai Clinic",
                         OrgUnit =="wc Gordons Bay CDC"~"wc Gordon's Bay CDC",
                         OrgUnit =="wc Pelican Park Clinic"~"wc Pelican Park Satellite Clinic",
                         OrgUnit =="wc Somerset West Clinic"~"wc Somerset West CDC",TRUE ~OrgUnit ))

#write.xlsx(ANOVA_2,"Clean.xlsx")

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


partners_data<-bind_rows(BRCH,WRHI,MatCH,ANOVA_2,RTC) %>% relocate(mech_name,AttributeOptionCombo) %>% rename(facility=OrgUnit) %>% mutate(facility=
  case_when(facility=="mp Amsterdam Mobile"~"mp Amsterdam Mobile 1",
            facility=="kz King Dinizulu Clinic" ~"kz King Dinuzulu Clinic",
            facility=="kz Morrisons Post Clinic"~"kz Morrison's Post Clinic",
            facility=="mp Bettysgoed Clinic"~"mp Betty'sgoed Clinic",
            facility=="mp Klarinet Clinic" ~"mp Klarinet CHC",
            facility=="mp Lillian Mambakazi CHC"~"mp Lilian Mambakazi CHC",
            facility=="mp MS Msimanga Clinic"~"mp MS Msimango Clinic",
            facility=="mp Mbhejeka CHC"~"mp Mbhejeka Clinic",
            facility=="kz Rietvlei Mobile 1"~"Kz Rietvlei Mobile 1",
            facility=="fs Dinkweng clinic"~"fs Dinkweng Clinic",
            facility=="fs Khosatsana Masetjhaba Clinic"~"fs Khosatsana Masetjhaba clinic",
            facility=="fs Makeneng clinic"~"fs Makeneng Clinic",
            facility=="fs Monontsha clinic"~"fs Monontsha Clinic",
            facility=="fs Mphatlalatsane clinic"~"fs Mphatlalatsane Clinic",
            facility=="fs Phuthaditjhaba clinic"~"fs Phuthaditjhaba Clinic",
            facility=="fs Tebang clinic"~"fs Tebang Clinic",
            facility=="fs Thabang clinic"~"fs Thabang Clinic",
            facility=="fs Tina moloi clinic"~"fs Tina Moloi Clinic",
            TRUE~facility)) %>% filter(!facility %in% c("kz Lower Umfolozi War Memorial Hospital","kz Turton CHC","kz Port Shepstone Mobile 1","kz Port Shepstone Mobile 3","kz Port Shepstone Mobile 6",
"kz Port Shepstone Mobile 7"))

#partners_data<-ANOVA_2%>% relocate(mech_name,AttributeOptionCombo) %>% rename(facility=OrgUnit)


partners_datav1 <-partners_data %>%  mutate(Clinical=`sum(\`Medical Officer - Position Count\`)`+`sum(\`Professional Nurse - Position Count\`)`+`sum(\`Enrolled Nurse - Position Count\`)`+`sum(\`WBOT OTL - Position Count\`)`
                                          +`sum(\`Other - Clinical - Position Count\`)`,Laboratory=`sum(\`Lab Employee - Position Count\`)`,Pharmacist=`sum(\`Pharmacist / Pharmacist Assistant - Position Count\`)` +`sum(\`Other - Pharmacy - Position Count\`)` ,Management=`sum(\`Management - Other - Position Count\` )`,
                                          Social_Services=`sum( \`Social Service - Position Count\`)` +`sum(\`Other - Social Service - Position Count\`)` ,Lay=`sum(\`Lay Counselor - Position Count\`)`+`sum(\`Linkage Officer - Position Count\` )` +`sum(\`WBOT CHW - Position Count\`)` +`sum(\`Other CHW (non-WBOT) - Position Count\` )` +
                                            `sum(\`Other - Lay - Position Count\`)`,Other_HCW=`sum(\`Data Capturer - Position Count\`)` +`sum(\`Data Clerk - Position Count\`)`  +`sum(\`Other - HCW\n- Position Count\` )`  )

partners_data2<-partners_datav1%>% select(mech_name,AttributeOptionCombo,facility,Clinical,Laboratory,Pharmacist,Management,Lay, Other_HCW ,Social_Services)

partners_data_v3<- partners_data2 %>%   pivot_longer(cols = Clinical:Social_Services,     names_to = "code",    values_to = "Value") %>% mutate(Dataelement="Kk4CdspETNQ",
                                                                                                                                               CategoryOptionCombo=case_when(str_detect(code,"Clin")~"mkOfrTuz7tS",
                                                                                                                                                                             str_detect(code,"Lab")~"T1jZtIrfVkq",str_detect(code,"Lay")~"a9N5X73zhET",str_detect(code,"Mana")~"oaRfTQD4RLG",
                                                                                                                                                                             str_detect(code,"Other")~"wKH5X6oHquw",str_detect(code,"Pharm")~"VYMJrOJU5rQ",TRUE~"itxIkeWqiE9"),Period="2023Q3")

#partners_data_v2<- sqldf("select * from partners_data_v1 group by facility,CategoryOptionCombo ")
#Checking for all cadres's code
print(distinct(partners_data_v3,code))
#write.xlsx(partners_data_v1,"Data_View.xlsx")
# MFL ---------------------------------------------
# MFL ---------------------------------------------

###Using Org Units
orgunits<-list.files(here("data"),pattern = "Exchange")
orgunits<-read_csv(here("data", orgunits)) %>% filter(orgunit_internal_id!="KJ6EYihrWdp") %>% mutate(Orgunit=orgunit_internal_id)
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

Final_Df<-left_join(partners_data_v3 ,orgunits,by=c("facility"="orgunit_name")) %>%  rename(OrgUnit=orgunit_internal_id  ) %>% mutate(AttributeOptionCombo="HllvX50cXC0")

Final_Df2<-Final_Df %>% filter(is.na(OrgUnit))

#write.xlsx(Final_Df2,"HRH_check6.xlsx")
#%>% mutate(AttributeOptionCombo="HllvX50cXC0")
#In case of error use this code AttributeOptionCombo="HllvX50cXC0"
#Data Review


#Checking mismatches between MFL & reported data
#Check1<-Final_Df %>% filter(is.na(OrgUnit))

#print(distinct(Final_Df,code))
#write.xlsx(Check1,"HRH_Site_clean.xlsx")

Final_Df_Import<-Final_Df %>% select(Dataelement,Period,OrgUnit,CategoryOptionCombo,AttributeOptionCombo,Value) %>% filter(!is.na(Value) ,Value>0)


###Appending data from the HRID Dashboard

HRID<-list.files(here("Data"),pattern="Fact")
HRID_USAID<-read.xlsx(here("Data",HRID))

HRID_Count<-HRID_USAID %>% filter(!is.na(facilityuid) ,str_detect(indicator,"Count"),mechanismID %in% c("70287","70290","70301","70306","70307","70310","70311","80007","80004","80008","82199","81902","81904","14631","14295")) %>%
mutate(mechname=case_when(mechanismID==70310~"ANOVA",mechanismID==70287~"BRCH",mechanismID==80007~"Wits Prevention",mechanismID==70306~"WRHI FSW/TG",mechanismID==70301~"WRHI-USAID/APACE",mechanismID==82199~"FHI360",mechanismID==81902~"Match"
,mechanismID==70290~"RTC",mechanismID==70311~"CINDI",mechanismID==14295~"FHI360",mechanismID=="81904"~"G2G DSD",mechanismID==70307~"HIVSA",mechanismID==80004~"M2M",
mechanismID==80008~"NACOSA GBV",mechanismID==14631~"Pact_inc",TRUE~""))

HRID_CountP2<-HRID_Count %>% mutate(Facility=case_when(Facility=="kz Mondlo No 1 Clinic"~"kz Mondlo 2 Clinic",Facility=="Mfundo Arnold Lushaba"~"kz Mfundo Arnold Lushaba CHC", Facility=="ec Cecilia Makiwane Haem"~"ec Cecilia Makiwane Hospital",
                                                      Facility=="fs Senorita Ntlabathi Hospital"~"fs Senorita Ntlabathi Hospital",
                                                      Facility=="gp Mpumelelo Clinic"~"gp Mpumelelo Clinic" ,Facility=="lp Phelang NGO Clinic"~"lp Phelang Community Centre", Facility=="mp Rockdale CHC"~"mp Rockdale CHC",
                                                      Facility=="mp Tweefontein G CHC"~"mp Tweefontein M Clinic",

                                                      Facility=="mp Tweefontein G Clinic"~"mp Tweefontein G CHC",

                                                       Facility=="mp Waterval Boven Clinic"~"mp Waterval Boven Gateway Clinic",

                                                      Facility=="wc Alphen Satellite Clinic"~"wc Alphen Clinic",TRUE~Facility)) %>% select(psnu,     PrimePartner ,   mechname, mechanismID,  Facility, facilityuid,   Cadre ,  FY2023Q4 )  %>% rename(Value=FY2023Q4) %>%
mutate(facilityuid=case_when(Facility=="ec Cecilia Makiwane Hospital"~	"Vd9jdCAwC0d",
                             Facility=="fs Senorita Ntlabathi Hospital"~"FJJA8Rdd2YC",
                             Facility== "gp Mpumelelo Clinic" ~ "jShhMB84PRa",
                             Facility== "kz Mfundo Arnold Lushaba CHC" ~  "ESLvoqIBQlH",
                             Facility== "kz Mondlo 2 Clinic" ~ "Sd9nCkCLsiz",
                             Facility== "lp Phelang Community Centre" ~ "fNwbKuVabSH",
                             Facility== "mp Kempville CHC" ~ "WVgSBCk9tRa",
                             Facility== "mp Rockdale CHC" ~ "rMjDtGXNyDV",
                             Facility== "mp Tweefontein G CHC" ~ "Wg4yViRpIyi",
                             Facility== "mp Tweefontein M Clinic" ~ "yVPZPpbyPMI",
                             Facility== "mp Waterval Boven Gateway Clinic" ~ "YHgT1ahFKAf",
                             Facility==  "wc Alphen Clinic" ~ "HWz6wRLgUvg",
                             Facility==  "mp Ubuhle Bempilo CHC" ~ "yVPZPpbyPMI",

                             TRUE~ facilityuid))


#write.xlsx(HRID_CountP2,"Check.xlsx")
#print(distinct(HRID_CountP2,mechname ,mechanismID))



HRID_CountP3<-HRID_CountP2 %>%left_join(orgunits,by=c("facilityuid"="orgunit_internal_id"))%>% select(psnu,     PrimePartner ,   mechname, mechanismID,  Facility,Orgunit,   Cadre ,  Value )


HRID_CountP4<-HRID_CountP3%>% mutate(Dataelement="Kk4CdspETNQ",
           CategoryOptionCombo=case_when(str_detect(Cadre,"Clin")~"mkOfrTuz7tS",
                                         str_detect(Cadre,"Lab")~"T1jZtIrfVkq",str_detect(Cadre,"Lay")~"a9N5X73zhET",str_detect(Cadre,"Mana")~"oaRfTQD4RLG",
                                         str_detect(Cadre,"Other")~"wKH5X6oHquw",str_detect(Cadre,"Pharm")~"VYMJrOJU5rQ",TRUE~"itxIkeWqiE9"),Period="2023Q3") %>%  mutate(AttributeOptionCombo="HllvX50cXC0") %>% filter(Value>0) %>% rename(OrgUnit=Orgunit)

HRID_CountP4_Final<-HRID_CountP4%>% select(Dataelement,Period,OrgUnit,CategoryOptionCombo,AttributeOptionCombo,Value) %>% filter(!is.na(Value) ,Value>0) %>%
  group_by(Dataelement,Period,OrgUnit,CategoryOptionCombo,AttributeOptionCombo) %>% summarise(Value=sum(Value))

#Append with the reported HRH results by NDOH

Final_PEPFAR_DOH<-rbind(Final_Df_Import,HRID_CountP4_Final) %>% group_by(Dataelement,Period,OrgUnit,CategoryOptionCombo,AttributeOptionCombo) %>% summarise(Value=sum(Value))

write_csv(Final_PEPFAR_DOH,here("dataout",Filename))
