library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)
library(Wavelength)
library(janitor)





# CONTEXT FILES IN -------------------------------------------------------------
ind_ref<-pull(read_excel(here("Data", "indicator_ref.xlsx"),
                         sheet="TX"))

doh_att<-read_csv(here("Data/site_att", "vwOrgunitStructureOU5.csv")) %>% 
  clean_names() %>% 
  select(ou5code,ou5name,org_unit_ownership,
         org_unit_rural_urban,
         org_unit_type) %>% 
  mutate(org_ownership_agg=case_when(
    str_detect(org_unit_ownership,"Gov") ~ "Government",
    org_unit_ownership=="Province Aided" ~ "Government",
    TRUE ~ org_unit_ownership
  ))


genie_files<-list.files(here("Data/site"),pattern="SITE")

genie<-here("Data/site",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>% 
  filter(indicator %in% ind_ref,
         fiscal_year %in% c("2023"),
         sitetype=="Facility") %>% 
  mutate(facility=case_when(
    facilityuid=="ccoMcvHS9I7" ~ "mp Witbank Mobile 4",
    facilityuid=="gDE3JdT0Qc3" ~ "mp MS Msimanga Clinic",
    facilityuid=="cl2hr7tF6rd" ~ "kz Turton Mobile 4",
    facilityuid=="SExIRXHNcOY" ~ "kz Rietvlei Mobile 1",
    facilityuid=="CLF1nFAMXtX" ~ "gp Engage Men's Health Drop-In Centre Non-Medical Site",
    facilityuid=="OjWzDVKaNoh" ~ "gp Alexandra 18th Avenue Clinic",
    facilityuid=="Vr2mcgevU2v" ~ "fs Senorita Ntlabathi Hospital",
    facilityuid=="bzDt3yXPUQw" ~ "gp Sonder Water Health Post",
    TRUE ~ facility
  )) %>% 
  left_join(doh_att,by=c("facility"="ou5name"))
         


# site counts ------------------------------------------------------------------
pepfar_sites<-genie %>% 
  filter(standardizeddisaggregate=="Total Numerator") %>% 
  group_by(facilityuid) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>%
  count() %>% 
  print()


USAID_sites<-genie %>% 
  filter(standardizeddisaggregate=="Total Numerator",
         funding_agency=="USAID") %>% 
  group_by(facilityuid) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  count() %>% 
  print()


pepfar_by_type<-genie %>% 
  filter(standardizeddisaggregate=="Total Numerator") %>% 
  group_by(org_ownership_agg) %>% 
  summarise(pepfar_sites=n_distinct(facilityuid)) %>% 
  print()


usaid_by_type<-genie %>% 
  filter(standardizeddisaggregate=="Total Numerator",
         funding_agency=="USAID") %>% 
  group_by(org_ownership_agg) %>% 
  summarise(usaid_sites=n_distinct(facilityuid)) %>% 
  print()
  

doh_sites<-doh_att %>% 
  group_by(org_ownership_agg) %>% 
  summarize(site_types=n_distinct(ou5code)) %>% 
  print()


misaligned_DATIMnames<-genie%>%
  filter(standardizeddisaggregate=="Total Numerator") %>% 
  distinct(facilityuid,facility) %>% 
  anti_join(doh_att,by=c("facility"="ou5name"))


# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),"NDoH_site_counts.csv",sep="_")

write_csv(doh_sites, file.path(here("Dataout"),filename,na=""))


filename<-paste(Sys.Date(),"misaligned_DATIM_names.csv",sep="_")

write_csv(misaligned_DATIMnames, file.path(here("Dataout"),filename,na=""))