library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)
library(janitor)
library(tameDP)
library(lubridate)


#psnu_ref
psnu_agency<-read_excel(here("Data", "psnu_agency_ref.xlsx"))


# get PLHIV from Sept 2021 Naomi | Adopted by estimates TWG---------------------
epi_path<-here("Data/Naomi_2021","pepfar_datapack_indicators_2022.csv")

df_epi<-read_csv(epi_path) %>% 
  filter(!indicator_code=="TX_CURR_SUBNAT.R") %>% 
  mutate(age=str_remove_all(age,"="),
         age=str_remove_all(age,'"')) %>% 
  select(psnu_uid,psnu,indicator_code,age,sex,value) %>% 
  rename(indicator=indicator_code,
         psnuuid=psnu_uid,
         ageasentered=age) %>% 
  mutate(period="FY22",
         period_type="cumulative",
         source="NAOMI") %>% 
  left_join(psnu_agency,by="psnu")



#genie -------------------------------------------------------------------------
genie_files<-list.files(here("Data"),pattern="Daily")


genie<-here("Data",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>%
  filter(fiscal_year %in% c("2021","2022","2023"))

df_genie<-genie %>% 
  filter(indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW","TX_CURR","TX_PVLS"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                         "Age/Sex/Indication/HIVStatus",
                                         "Total Numerator")) %>% 
  reshape_msd(direction="long") %>% 
  filter(period_type %in% c("cumulative","targets")) %>% 
  mutate(source="DATIM")

snupriority<-df_genie %>% 
  distinct(psnuuid,snuprioritization)


df_epi<-df_epi %>% 
  left_join(snupriority,by="psnuuid")


# HIERARCHY
hierarchy<-genie %>% 
  distinct(snuprioritization,snu1,psnu,psnuuid) %>% 
  mutate(psnu_join=substring(psnu,4))


# PRIVATE SECTOR DATA ----------------------------------------------------------
ps<-read_excel(here("Data", "Private Sector_Semi-Anual Data_Jan-June 2022.xlsx"),
               sheet="ART_Jan-Jun 2022")

ps<-ps %>%
  # filter(!District=="District Unknown") %>%
  mutate(District=case_when(
    District=="Nelson Mandela Bay Metropolitan Municipality" ~ str_remove(District," Metropolitan"),
    str_detect(District,"OR Tambo") ~ "Oliver Tambo District Municipality",
    str_detect(District,"Thabo") ~ "Thabo Mofutsanyane District Municipality",
    str_detect(District,"khanya") ~ "Umkhanyakude District Municipality",
    str_detect(District,"zinyathi")~ "Umzinyathi District Municipality",
    str_detect(District, "ukela") ~ "Uthukela District Municipality",
    str_detect(District,"Mgcawu") ~ "Zwelentlanga Fatman Mgcawu District Municipality",
    TRUE ~ District
  )) %>%
  left_join(hierarchy,by=c("District"="psnu_join")) %>%
  select(-Province,-District) %>%
  mutate(AgeBand=str_remove_all(AgeBand,"years"),
         AgeBand=case_when(
           AgeBand=="Less than one year" ~ "<01",
           AgeBand=="1-4 " ~ "01-04",
           AgeBand=="5-9 " ~ "05-09",
           AgeBand=="65-69 " ~ "65+",
           AgeBand=="70-74 " ~ "65+",
           AgeBand=="75-79 " ~ "65+",
           AgeBand=="80-84 " ~ "65+",
           AgeBand=="85 +" ~ "65+",
           TRUE ~ AgeBand
         )) %>%
  mutate(AgeBand=str_trim(AgeBand)) %>%
  mutate(ageasentered=AgeBand) %>%
  rename(sex=Gender,
         value=`Sum of NumberOfARTBeneficiaries`) %>%
  select(-AgeBand) %>%
  group_by_if(is.character) %>%
  summarize_at(vars(value),sum,na.rm=TRUE) %>%
  ungroup() %>%
  mutate(standardizeddisaggregate="Age/Sex/HIVStatus",
         period="FY22",
         period_type="cumulative",
         numeratordenom="N",
         indicator="TX_CURR_PS",
         DSP="PS",
         source="Private Sector") %>% 
  left_join(psnu_agency,by="psnu")




# CASH PAYING CLIENTS ----------------------------------------------------------
cash<-read_excel(here("Data", "ART Cash separate years 2017 - 2021 Unique patient counts.xlsx"),
               sheet="2021 Pivot",
               skip=2)


cash<-cash %>% 
  mutate(District=case_when(
    District_SANAC=="Nelson Mandela Bay Metropolitan Municipality" ~ str_remove(District_SANAC," Metropolitan"),
    str_detect(District_SANAC,"OR Tambo") ~ "Oliver Tambo District Municipality",
    str_detect(District_SANAC,"Thabo") ~ "Thabo Mofutsanyane District Municipality",
    str_detect(District_SANAC,"khanya") ~ "Umkhanyakude District Municipality",
    str_detect(District_SANAC,"zinyathi")~ "Umzinyathi District Municipality",
    str_detect(District_SANAC, "ukela") ~ "Uthukela District Municipality",
    str_detect(District_SANAC,"Mgcawu") ~ "Zwelentlanga Fatman Mgcawu District Municipality",
    TRUE ~ District_SANAC
  )) %>%
  left_join(hierarchy,by=c("District"="psnu_join")) %>%
  select(-DrProvince,-District_SANAC,-District) %>% 
  rename(ageasentered=AgeBand,
         sex=Gender,
         value=`Sum of Mutual excl Counts`) %>% 
  mutate(sex=case_when(
    sex=="Unspecified" ~ "Unknown",
    TRUE ~ sex),
    ageasentered=case_when(
      ageasentered=="25 and older" ~ "25+",
      ageasentered=="15 - 24" ~ "15-24",
      ageasentered=="0 to 14" ~ "0-14",
      TRUE ~ ageasentered
    )) %>% 
  mutate(standardizeddisaggregate="Age/Sex/HIVStatus",
         period="FY22",
         period_type="cumulative",
         numeratordenom="N",
         indicator="TX_CURR_CASH",
         DSP="CASH",
         source="Cash Paying") %>% 
  left_join(psnu_agency,by="psnu")

# DHIS -------------------------------------------------------------------------
prioritization_dhis<-genie %>% 
  distinct(snuprioritization,psnu,psnuuid)

dhis_22<-read_excel(here("Data/dhis/2022", "webDHIS ZA OU5 CDC dataset update v1Nov2022.xlsx")) %>% 
  setNames(., c('national',
                'province',
                'psnu',
                'community',
                'code',
                'facility',
                'indicator_id',
                'indicator',
                format(as.Date(as.numeric(names(.)[9:26]), 
                               origin = '1899-12-30'), '%m-%Y')))

dhis_22<-dhis_22 %>% 
  filter(indicator %in% c("ART adult remain on ART end of period",
                          "ART child under 15 years remain on ART end of period",
                          "ART client remain on ART end of month - sum")) %>%
  gather(mon_yr,value,"06-2021":"11-2022") %>% 
  filter(mon_yr %in% c("09-2022")) %>% 
  mutate(mon_yr=my(mon_yr)) %>% 
  mutate(period=quarter(mon_yr, with_year = TRUE, fiscal_start = 10),
         period=stringr::str_remove(period, "20"),
         period=str_replace_all(period,"\\.","Q"),
         period=paste0("FY",period),
         period_type="cumulative",
         source="DHIS2",
         source_name="DHIS2",
         standardizeddisaggregate=case_when(
           indicator=="ART client remain on ART end of month - sum" ~ "Total Numerator",
           TRUE ~ "Age/Sex/HIVStatus"
         ),
         ageasentered=case_when(
           indicator=="ART child under 15 years remain on ART end of period" ~ "<15",
           indicator=="ART adult remain on ART end of period" ~ "15+",
           TRUE ~ ""
         ),
         trendscoarse=case_when(
           indicator=="ART child under 15 years remain on ART end of period" ~ "<15",
           indicator=="ART adult remain on ART end of period" ~ "15+",
           TRUE ~ ""
         ),
         indicator="TX_CURR_90",
         period="FY22") %>% 
  rename(snu1=province) %>% 
  select(-code,-indicator_id,-mon_yr,-national,-community,-facility) %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  mutate(psnu=case_when(
    psnu=="fs Thabo Mofutsanyana District Municipality" ~ "fs Thabo Mofutsanyane District Municipality",
    TRUE ~ psnu)) %>% 
  left_join(prioritization_dhis,by="psnu") %>% 
  left_join(psnu_agency,by="psnu")
  
  
  

#NDOH TARGETS
doh_t<-read_excel(here("Data", "Targets program.xlsx")) %>% 
  clean_names() %>% 
  select(-province) %>% 
  gather(indicator, value, hiv_test:troa) %>% 
  rename(psnu=district) %>% 
  mutate(psnu=case_when(
    psnu=="fs Thabo Mofutsanyana District Municipality" ~ "fs Thabo Mofutsanyane District Municipality",
    TRUE ~ psnu),
    indicator=case_when(
      indicator=="hiv_test" ~ "HTS_TST",
      indicator=="hiv_pos" ~ "HTS_TST_POS",
      indicator=="start_art" ~ "TX_NEW",
      indicator=="troa" ~ "TX_CURR_90"
    )) %>% 
  left_join(hierarchy, by="psnu") %>% 
  select(-psnu_join) %>% 
  mutate(standardizeddisaggregate="Total Numerator",
         period="FY23",
         period_type="targets",
         numeratordenom="N",
         source="NDoH") %>% 
  left_join(psnu_agency,by="psnu")
  
  

#COMBINE

df_final<-bind_rows(df_epi,ps,cash,doh_t) %>%
  mutate(trendscoarse=case_when(
    ageasentered %in% c("0-14","<01","01-04","05-09","10-14") ~ "<15",
    ageasentered %in% c("15-19","20-24","25-29","30-34","35-39","40-44",
                        "45-49","50-54","55-59","60-64","65+","25+","15-24")~ "15+")) %>% 
  bind_rows(df_genie,dhis_22) %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) %>% 
  select(-country) %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  mutate(indicator2=indicator,
         value2=value) %>% 
  spread(indicator2,value2) 

  

# EXPORT
filename<-paste(Sys.Date(),"Naomi_MER_DHIS_NDoH","indicator_age_v1.6.txt",sep="_")

write_tsv(df_final, file.path(here("Dataout"),filename,na=""))

