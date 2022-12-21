library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)
library(janitor)
library(tameDP)





# get PLHIV from Sept 2021 Naomi | Adopted by estimates TWG--
epi_path<-here("Data/Naomi_2021","pepfar_datapack_indicators_2022.csv")

df_epi<-read_csv(epi_path) %>% 
  filter(indicator_code=="PLHIV.T_1") %>% 
  mutate(age=str_remove_all(age,"="),
         age=str_remove_all(age,'"')) %>% 
  select(psnu_uid,psnu,indicator_code,age,sex,value) %>% 
  rename(indicator=indicator_code,
         psnuuid=psnu_uid,
         ageasentered=age) %>% 
  mutate(period="FY22",
         period_type="cumulative",
         indicator="PLHIV")



#genie 
genie_files<-list.files(here("Data"),pattern="Daily")


genie<-here("Data",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>%
  filter(fiscal_year %in% c("2022","2023"))

df_genie<-genie %>% 
  filter(indicator=="TX_CURR",
         standardizeddisaggregate=="Age/Sex/HIVStatus") %>% 
  reshape_msd(direction="long") %>% 
  filter(period=="FY22",
         period_type=="cumulative")

snupriority<-df_genie %>% 
  distinct(psnuuid,snuprioritization)


df_epi<-df_epi %>% 
  left_join(snupriority,by="psnuuid")


# HIERARCHY
hierarchy<-genie %>% 
  distinct(snuprioritization,snu1,psnu,psnuuid) %>% 
  mutate(psnu_join=substring(psnu,4))


# PRIVATE SECTOR DATA
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
         DSP="PS")




# CASH PAYING CLIENTS
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
         DSP="CASH")


#COMBINE

df_final<-bind_rows(df_epi,ps,cash) %>%
  mutate(trendscoarse=case_when(
    ageasentered %in% c("0-14","<01","01-04","05-09","10-14") ~ "<15",
    ageasentered %in% c("15-19","20-24","25-29","30-34","35-39","40-44",
                        "45-49","50-54","55-59","60-64","65+","25+","15-24")~ "15+")) %>% 
  bind_rows(df_genie) %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) 

  

# EXPORT
filename<-paste(Sys.Date(),"Naomi_MER","indicator_age_v1.0.txt",sep="_")

write_tsv(df_final, file.path(here("Dataout"),filename,na=""))

