library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)


current_pd<-"FY23Q1c"

# READ IN FILES ----------------------------------------------------------------
ind_ref<-pull(read_excel(here("Data", "indicator_ref_PLM_SRE.xlsx"),
              sheet="TX"))


psnu_agency<-read_excel(here("Data", "psnu_agency_ref.xlsx")) %>% 
  rename(DSP_Lead=funding_agency)


dsp_lookback<-read_excel(here("Data","dsp_attributes_2022-05-17.xlsx")) %>% 
  rename(agency_lookback=`Agency lookback`) %>% 
  select(-MechanismID)

#genie 
genie_files<-list.files(here("Data/site/plm"),pattern="Structured") #using MSD

genie<-here("Data/site/plm",genie_files) %>% 
  map(read_psd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) 


final<-genie %>% 
  filter(indicator %in% ind_ref,
         fiscal_year %in% c("2021","2022","2023"))



# CONTEXT MERGE ----------------------------------------------------------------
final<-final %>% 
  left_join(psnu_agency,by="psnu") %>% 
  filter(DSP_Lead=="USAID",
         funding_agency=="USAID",
         !str_detect(standardizeddisaggregate,"KeyPop")) %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) %>% 
  unite(DSPID,mech_code,short_name,sep="",remove=FALSE) %>% 
  left_join(dsp_lookback,by="DSPID") %>% 
  filter(DSP=="Yes") %>% 
  select(-(DSP_Lead:agency_lookback),
         -(prime_partner_name:award_number)) %>% 
  mutate(fiscal_year=as.character(fiscal_year)) %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(targets:cumulative),sum,na.rm=TRUE)
  

#data check
data_check<-final %>% 
  filter(standardizeddisaggregate=="Total Numerator") %>% 
  group_by(indicator,fiscal_year) %>% 
  summarise_at(vars(targets:cumulative),sum,na.rm=TRUE) %>% 
  prinf()
  
# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),"MER_CTX",current_pd,"_sitelevel_PLM_SRE.txt",sep="_")

write_tsv(final, file.path(here("Dataout"),filename,na=""))