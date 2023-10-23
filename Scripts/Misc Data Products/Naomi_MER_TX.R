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


# get PLHIV from Sept 2022 Naomi | Adopted by estimates TWG---------------------
epi_path<-here("Data/Naomi_2022","pepfar_datapack_indicators_2023.csv")

df_epi<-read_csv(epi_path) %>% 
  filter(!indicator_code=="TX_CURR_SUBNAT.R") %>% 
  mutate(age=str_remove_all(age,"="),
         age=str_remove_all(age,'"')) %>% 
  select(psnu_uid,psnu,indicator_code,age,sex,value,calendar_quarter) %>% 
  rename(indicator=indicator_code,
         psnuuid=psnu_uid,
         ageasentered=age) %>% 
  mutate(period=case_when(
    calendar_quarter=="CY2023Q3" ~ "FY23",
    calendar_quarter=="CY2024Q3" ~ "FY24",
    calendar_quarter=="CY2025Q3" ~ "FY25",
    TRUE ~ ""),
         period_type="cumulative",
         source="NAOMI") %>% 
  left_join(psnu_agency,by="psnu") %>% 
  select(-calendar_quarter)



#genie -------------------------------------------------------------------------
genie_files<-list.files(here("Data"),pattern="PSNU_IM") #using MSD to get FY21


genie<-here("Data",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>% # appending all Geni files found
  filter(fiscal_year %in% c("2021","2022","2023"))

df_genie<-genie %>% 
  filter(indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW","TX_CURR","TX_PVLS"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                         "Age/Sex/Indication/HIVStatus",
                                         "Total Numerator")) %>% 
  reshape_msd(direction="long") %>% #creates period column 
  filter(period_type %in% c("cumulative","targets")) %>% 
  mutate(source="DATIM")

snupriority<-df_genie %>% 
  distinct(psnuuid,snuprioritization)


df_epi<-df_epi %>% 
  left_join(snupriority,by="psnuuid")


# DATA CHECK -------------------------------------------------------------------
data_check<-df_genie %>% 
  filter(standardizeddisaggregate=="Total Numerator",
         indicator =="TX_CURR",
         period_type %in% c("cumulative"),
         period %in% c("FY23")) %>% 
  group_by(funding_agency,indicator,period) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  spread(funding_agency,value)


print(data_check)

# HIERARCHY
hierarchy<-genie %>% 
  distinct(snuprioritization,snu1,psnu,psnuuid) %>% 
  mutate(psnu_join=substring(psnu,4))


# PRIVATE SECTOR DATA ----------------------------------------------------------
ps<-read_excel(here("Data/ps", "Total ART _ Viral Suppression_SA.xlsx"),
               sheet="ART Beneficiaries")


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
         period="FY23",
         period_type="cumulative",
         numeratordenom="N",
         indicator="TX_CURR_PS",
         DSP="PS",
         source="Private Sector") %>% 
  left_join(psnu_agency,by="psnu")





# CASH PAYING CLIENTS ----------------------------------------------------------
cash<-read_excel(here("Data/ps", "HIV Cash Paying Patients data-Jan-Dec 2022_Sharing.xlsx"),
                 sheet="2022 Pivot",
                 skip=2)



cash<-cash %>% 
      clean_names() %>% 
      mutate(district=case_when(
        sanac_district=="Nelson Mandela Bay Metropolitan Municipality" ~ str_remove(sanac_district," Metropolitan"),
        str_detect(sanac_district,"OR Tambo") ~ "Oliver Tambo District Municipality",
        str_detect(sanac_district,"Thabo") ~ "Thabo Mofutsanyane District Municipality",
        str_detect(sanac_district,"khanya") ~ "Umkhanyakude District Municipality",
        str_detect(sanac_district,"zinyathi")~ "Umzinyathi District Municipality",
        str_detect(sanac_district, "ukela") ~ "Uthukela District Municipality",
        str_detect(sanac_district,"Mgcawu") ~ "Zwelentlanga Fatman Mgcawu District Municipality",
        TRUE ~ sanac_district
      )) %>%
      left_join(hierarchy,by=c("district"="psnu_join")) %>%
      select(-district) %>% 
      rename(ageasentered=age_band,
             sex=gender,
             value=sum_of_counts) %>% 
      mutate(sex=case_when(
        sex=="Unspecified" ~ "Unknown",
        TRUE ~ sex),
        ageasentered=case_when(
          ageasentered=="25 and older" ~ "25+",
          ageasentered=="15 - 24" ~ "15-24",
          ageasentered=="0 to 14" ~ "0-14",
          ageasentered=="< 5" ~"<5",
          ageasentered=="5 to 9" ~ "05-09",
          ageasentered=="10 to 14" ~ "10-14",
          ageasentered=="15 to 19" ~ "15-19",
          ageasentered=="20 to 24" ~ "20-24",
          ageasentered=="25 to 29" ~ "25-29",
          ageasentered=="30 to 34" ~ "30 to 34",
          ageasentered=="35 to 39" ~ "35-39",
          ageasentered=="40 to 44" ~ "40 to 44",
          ageasentered=="45 to 49" ~ "45-49",
          ageasentered=="50 to 54" ~ "50-54",
          ageasentered=="55 to 59" ~ "55-59",
          ageasentered=="60 to 64" ~ "60-64",
          ageasentered=="65 to 69" ~ "65-69",
          ageasentered=="70 to 74" ~ "70-74",
          ageasentered=="75 to 79" ~ "75-79",
          ageasentered=="> 80" ~ "80+",
          ageasentered=="Other"~ "Unknown Age",
          TRUE ~ ageasentered
        )) %>% 
      mutate(standardizeddisaggregate="Age/Sex/HIVStatus",
             period="FY22",
             period="FY23",
             period_type="cumulative",
             numeratordenom="N",
             indicator="TX_CURR_CASH",
         DSP="CASH",
         source="Cash Paying") %>% 
  left_join(psnu_agency,by="psnu")


# DHIS -------------------------------------------------------------------------
prioritization_dhis<-genie %>% 
  distinct(snuprioritization,psnu,psnuuid)

dhis_23<-read_excel(here("Data/dhis/2023", "webDHIS ZA OU5 CDC AGG dataset 20230909_Final.xlsx")) %>% 
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

dhis_23<-dhis_23 %>% 
  filter(indicator %in% c("ART adult remain on ART end of period",
                          "ART child under 15 years remain on ART end of period",
                          "ART client remain on ART end of month - sum")) %>%
  gather(mon_yr,value,"02-2022":"06-2023") %>% 
  filter(mon_yr %in% c("12-2022","06-2023")) %>% 
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
         indicator="TX_CURR_90") %>% 
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


#Hierarchy for NDoH Short Names
hierarchy_short<-hierarchy %>% 
  mutate(short_name=psnu_join,
         short_name=str_remove_all(short_name, " District Municipality"),
         short_name=str_remove_all(short_name, " Metropolitan Municipality"))

  
#Hierarchy for NDoH Short Names
hierarchy_short<-hierarchy %>% 
  mutate(short_name=psnu_join,
       short_name=str_remove_all(short_name, " District Municipality"),
      short_name=str_remove_all(short_name, " Metropolitan Municipality"))


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


doh_t_new<-read_excel(here("Data", "FY2223_Draft Targets.xlsx"),
                      skip=1) %>% 
  setNames(., c('psnu',
                'TX_CURR_90-15+-FY24',
                'TX_CURR_90-<15-FY24',
                'TX_CURR_90-15+-FY25',
                'TX_CURR_90-<15-FY25',
                'TX_CURR_90-15+-FY26',
                'TX_CURR_90-<15-FY26',
                'TX_NEW-15+-FY24',
                'TX_NEW-<15-FY24',
                'TX_NEW-15+-FY25',
                'TX_NEW-< 15-FY25',
                'TX_NEW-15+-FY26',
                'TX_NEW-<15+-FY26')) %>% 
  gather(indicator,value,'TX_CURR_90-15+-FY24':'TX_NEW-<15+-FY26') %>% 
  separate(indicator,into=c("indicator","ageasentered","period"),sep="-") %>% 
  mutate(psnu=case_when(
    psnu=="Thabo Mofutsanyana" ~ "Thabo Mofutsanyane",
    TRUE ~ psnu)) %>% 
  mutate(short_name=psnu) %>% 
  left_join(hierarchy_short, by="short_name") %>% 
  filter(!is.na(psnuuid)) %>% 
  select(-psnu.x,-short_name,-psnu_join) %>% 
  rename(psnu=psnu.y) %>% 
  mutate(standardizeddisaggregate="Total Numerator",
         period_type="targets",
         numeratordenom="N",
         source="NDoH") %>% 
  left_join(psnu_agency,by="psnu") %>% 
  mutate(trendscoarse=ageasentered)
  
  

#COMBINE

df_final<-bind_rows(df_epi,ps,cash,doh_t) %>%
  mutate(trendscoarse=case_when(
    ageasentered %in% c("0-14","<01","01-09", "01-04","05-09","10-14") ~ "<15",
    ageasentered %in% c("15-19","20-24","25-29","25-34", "30-34","35-39", "35-49", "40-44",
                        "45-49","50+", "50-54","55-59","60-64","65+","25+","15-24")~ "15+")) %>% 
  bind_rows(df_genie,dhis_23,doh_t_new) %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) %>% 
  select(-country) %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  mutate(indicator2=indicator,
         value2=value) %>% 
  spread(indicator2,value2)

# age_check<-df_final %>% 
#   ungroup() %>% 
#   filter(source=="NAOMI") %>% 
#   distinct(trendscoarse,ageasentered)
# 
# prinf(age_check)

# EXPORT
filename<-paste(Sys.Date(),"Naomi_MER_DHIS_NDoH","indicator_age_v1.0.txt",sep="_")

write_tsv(df_final, file.path(here("Dataout"),filename,na=""))


