library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)

memory.limit(size=500000)


current_pd<-"FY22Q2c"

# READ IN FILES ----------------------------------------------------------------
ind_ref<-pull(read_excel(here("Data", "indicator_ref.xlsx"),
              sheet="TX"))


#genie 
genie_files<-list.files(here("Data"),pattern="PSNU_IM")

genie<-here("Data",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>%
#   filter(fiscal_year %in% c("2021"))
# 
# 
# 
# # MSD
# msd_files<-list.files(here("Data"),pattern="MER")
# 
# msd<-here("Data",msd_files) %>% 
#   map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
#   reduce(rbind)


#subset & merge ----------------------------------------------------------------
# msd<-msd %>%
#   filter(fiscal_year %in% c("2018","2019","2020"))
# 
# final<-rbind(genie,msd) %>% 
#   filter(indicator %in% ind_ref)
# 
# rm(genie,msd)


final<-genie %>% 
  filter(indicator %in% ind_ref)

# CONTEXT FILES IN -------------------------------------------------------------
dsp_lookback<-read_excel(here("Data","dsp_attributes_2022-05-17.xlsx")) %>% 
  rename(agency_lookback=`Agency lookback`) %>% 
  select(-MechanismID)

# EPI IN -----------------------------------------------------------------------
epi<-read_tsv(here("Data","2022-07-23_Naomi_Thembisa_processed.txt"))



# CONTEXT MERGE ----------------------------------------------------------------
final<-final %>% 
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
  ))


# transform --------------------------------------------------------------------
final<-final %>% 
  reshape_msd("long") %>% 
  bind_rows(epi) %>% 
  mutate(year=as.character(year)) %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE)%>% 
  ungroup() %>% 
  mutate(indicator2=indicator,
         value2=value) %>% 
  spread(indicator2,value2) %>% 
  left_join(dsp_lookback,by="DSPID") %>% 
  mutate(partner_other=case_when(
    mech_code=="70301" ~ "WRHI-USAID",
    mech_code=="70306" ~ "WRHI FSW/TG",
    mech_code=="18483" ~ "WRHI-CDC",
    mech_code=="80007" ~ "Wits Prevention",
    mech_code=="17207" ~ "WRHI KP",
    mech_code=="17038" ~ "WRHI TB/HIV",
    mech_code=="17028" ~ "WRHI Prioity Populations",
    TRUE ~ prime_partner_name)
    
  )
  
  
  
# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),"MER_CTX_Epi_Integrated",current_pd,"attributes.txt",sep="_")

write_tsv(final, file.path(here("Dataout"),filename,na=""))