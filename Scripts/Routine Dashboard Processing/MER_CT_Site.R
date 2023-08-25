library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(janitor)
library(glamr)

# devtools::install_github("USAID-OHA-SI/glamr")


current_pd<-"FY23Q3i" #change each time to reflect current period

# READ IN FILES ----------------------------------------------------------------
ind_ref<-pull(read_excel(here("Data", "indicator_ref.xlsx"),
              sheet="TX"))


disaggs<-read_excel(here("Data","Dimension_DataElements_DATIMGenie_TopLevelDisaggregate.xlsx")) %>% 
  clean_names() %>% 
  select(-data_element_source) %>% 
  unite(disagg_key,indicator,numerator_denominator,standardized_disaggregate,remove=TRUE)


#genie 
genie_files<-list.files(here("Data/site"),pattern="Daily")


genie<-here("Data/site",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>%
  filter(fiscal_year %in% c("2022","2023"))

print(distinct(genie,fiscal_year))



#MSD
msd_files<-list.files(here("Data/site"),pattern="Structured")

msd<-here("Data/site",msd_files) %>%
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>%
  reduce(rbind)


#subset & merge ----------------------------------------------------------------
msd<-msd %>%
  filter(fiscal_year %in% c("2020","2021"))

print(distinct(msd,fiscal_year))


final<-rbind(genie,msd) %>%
  filter(indicator %in% ind_ref)

rm(genie,msd)




# CONTEXT FILES IN -------------------------------------------------------------
dsp_lookback<-read_excel(here("Data","dsp_attributes_2022-05-17.xlsx")) %>% 
  rename(agency_lookback=`Agency lookback`) %>% 
  select(-MechanismID)


# site_att<-read_tsv(here("Data/site_att", "2023_02_03_site_attributes.txt")) %>% 
#   rename(orgunituid=orgunit_uid)
# 
# 
# doh_att<-read_csv(here("Data/site_att", "vwOrgunitStructureOU5.csv")) %>% 
#   clean_names() %>% 
#   select(ou5name,org_unit_ownership,
#          org_unit_rural_urban,
#          org_unit_type) %>% 
#   rename(facility=ou5name)



# CREATE LAG DF AND LAG CALCS --------------------------------------------------

#create a full set of periods
df_complete<- final %>%
  filter(indicator=="TX_CURR",
         standardizeddisaggregate=="Age/Sex/HIVStatus") %>% 
  reshape_msd("long") %>% 
  filter(period_type=="results") %>% 
  complete(period, nesting(orgunituid, mech_code, standardizeddisaggregate,
                           ageasentered,sex,period_type,indicatortype), 
           fill = list(value = 0),
           explicit = FALSE) %>% 
  group_by(mech_code, orgunituid) %>% 
  fill(operatingunit, country, snu1, snu1uid, psnu, psnuuid, facility,
       sitename, operatingunituid, snuprioritization, typemilitary,
       dreams, communityuid,community,facilityuid,sitetype,
       numeratordenom,indicatortype,disaggregate,categoryoptioncomboname,
       age_2018,age_2019,trendscoarse,statushiv,statustb,statuscx,
       hiv_treatment_status,otherdisaggregate,otherdisaggregate_sub,
       modality,source_name,
       funding_agency, mech_name, prime_partner_name,
       indicator,.direction = "downup") %>% 
  ungroup() 
  
  

#complete check
df_check_complete<-df_complete %>% 
  filter(facility=="ec Burnshill Clinic",
         standardizeddisaggregate=="Age/Sex/HIVStatus",
         period_type=="results",
         mech_code=="160585") %>% 
  select(period,mech_code,ageasentered,sex,value)


# Lag calcs using the complete period df
lag1<-df_complete %>% 
  mutate(indicator2=indicator,
         value2=value) %>% 
  spread(indicator2,value2) %>%  
  group_by(orgunituid,mech_code,standardizeddisaggregate,ageasentered,sex,
           period_type,indicatortype) %>% 
  mutate(TX_CURR_LAG1 = lag(TX_CURR, 1, order_by = period)) %>% 
  ungroup() %>% 
  mutate(indicator="TX_CURR_LAG1",
         value=TX_CURR_LAG1,
         source_name="Derived") %>% 
  select(-TX_CURR)


lag2<-df_complete %>% 
  # filter(indicator=="TX_CURR") %>% 
  # reshape_msd("long") %>% 
  # group_by_if(is.character) %>% 
  # summarize_at(vars(value),sum,na.rm=TRUE)%>% 
  # ungroup() %>% 
  mutate(indicator2=indicator,
         value2=value) %>% 
  spread(indicator2,value2) %>%  
  group_by(orgunituid,mech_code,standardizeddisaggregate,ageasentered,sex,
           period_type,indicatortype) %>% 
  mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period)) %>% 
  ungroup() %>% 
  mutate(indicator="TX_CURR_LAG2",
         value=TX_CURR_LAG2,
         source_name="Derived") %>% 
  select(-TX_CURR)

  
# CONTEXT MERGE ----------------------------------------------------------------
# final<-final %>% 
#   mutate(short_name=psnu,
#          short_name=str_replace_all(short_name, "District Municipality","DM"),
#          short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) %>% 
#   unite(DSPID,mech_code,short_name,sep="",remove=FALSE) %>% 
#   mutate(HIGHBURDEN=case_when(
#     psnu=="gp City of Johannesburg Metropolitan Municipality" ~ "YES",
#     psnu=="gp City of Tshwane Metropolitan Municipality" ~ "YES",
#     psnu=="gp Ekurhuleni Metropolitan Municipality" ~ "YES",
#     psnu=="kz eThekwini Metropolitan Municipality" ~ "YES",
#     psnu=="wc City of Cape Town Metropolitan Municipality" ~ "YES",
#     TRUE ~ "NO"
#   ),
#   focus_district=case_when(
#     snuprioritization %in% c("2 - Scale-Up: Aggressive",
#                              "1 - Scale-Up: Saturation") ~ "YES",
#     TRUE ~ "NO"
#   ))

# transform --------------------------------------------------------------------
final<-final %>% 
  reshape_msd("long") %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE)%>% 
  ungroup() %>% 
  mutate(indicator2=indicator,
         value2=value) %>% 
  spread(indicator2,value2) %>% 
  bind_rows(lag1,lag2) %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) %>% 
  unite(DSPID,mech_code,short_name,sep="",remove=FALSE) %>% 
  mutate(HIGHBURDEN=case_when(
    psnu=="gp City of Johannesburg Metropolitan Municipality" ~ "YES",
    psnu=="gp City of Tshwane Metropolitan Municipality" ~ "YES",
    psnu=="gp Ekurhuleni Metropolitan Municipality" ~ "YES",
    psnu=="kz eThekwini Metropolitan Municipality" ~ "YES",
    psnu=="wc City of Cape Town Metropolitan Municipality" ~ "YES",
    TRUE ~ "NO"
  ),
  focus_district=case_when(
    snuprioritization %in% c("2 - Scale-Up: Aggressive",
                             "1 - Scale-Up: Saturation") ~ "YES",
    TRUE ~ "NO"
  )) %>% 
  left_join(dsp_lookback,by="DSPID") %>% 
  mutate(partner_other=case_when(
    mech_code=="70301" ~ "WRHI-USAID",
    mech_code=="70306" ~ "WRHI FSW/TG",
    mech_code=="18483" ~ "WRHI-CDC",
    mech_code=="80007" ~ "Wits Prevention",
    mech_code=="17207" ~ "WRHI KP",
    mech_code=="17038" ~ "WRHI TB/HIV",
    mech_code=="17028" ~ "WRHI Prioity Populations",
    TRUE ~ prime_partner_name)) %>% 
  unite(disagg_key,indicator,numeratordenom,standardizeddisaggregate,remove=FALSE) %>% 
  left_join(disaggs,by="disagg_key") %>% 
  select(-disagg_key)  
  # left_join(site_att,by="orgunituid") %>%
  # left_join(doh_att,by="facility")
  

# sense check
data_check<-final %>% 
  filter(standardizeddisaggregate=="Age/Sex/HIVStatus",
         indicator %in% c("TX_CURR","TX_CURR_LAG1","TX_CURR_LAG2"),
         period_type %in% c("results"),
         period %in% c("FY22Q1","FY22Q2","FY22Q3","FY22Q4",
                       "FY23Q1","FY23Q2")) %>% 
  group_by(indicator,period) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  spread(indicator,value)


print(data_check)


  
# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),"MER_CTX",current_pd,"site_level_fy20-23.txt",sep="_")

write_tsv(final, file.path(here("Dataout"),filename,na=""))



