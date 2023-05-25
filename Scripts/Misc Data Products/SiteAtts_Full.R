library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)
library(gisr)
library(Wavelength)
library(janitor)
library(RPostgres)
library(aws.s3)
library(redshiftTools)
library(RSQLite)
library(RPostgreSQL)


memory.limit(size=500000)


# GLOBALS

myuser <- "gsarfaty_SA"

conn <- dbConnect(RPostgres::Postgres(),
                  dbname = "data-centre-prod_db",
                  host = "data-centre-prod-redshift-cluster.c9cxhng7vlgm.af-south-1.redshift.amazonaws.com",
                  port = "5439",
                  user = "awsuser",
                  password = "&f[la%S7f-db")
#enter parameters and credentials

# CONTEXT FILES IN -------------------------------------------------------------
onehundred<-read_excel(here("Data/site_att","1hundred_in_1hundred.xlsx")) %>% 
  mutate(facility_status="100 in 100") %>% 
  rename(facility=`1hundred_in_1hundred`)

# 
# site_att<-read_tsv(here("Data/site_att", "2023_02_03_site_attributes.txt")) %>% 
#   rename(orgunituid=orgunit_uid)


doh_att<-read_csv(here("Data/site_att", "vwOrgunitStructureOU5.csv")) %>% 
  clean_names() %>% 
  select(ou5code,ou5name,org_unit_ownership,
         org_unit_rural_urban,
         org_unit_type)

DATIM_hierarchy<-pull_hierarchy("cDGPF739ZZr", myuser, askpass::askpass()) %>%
  mutate(facility=str_sub(orgunit,4)) %>%
  select(snu1,psnu,psnuuid,orgunit,orgunituid,latitude,longitude) %>% 
  rename(facility=orgunit)

pops <- dbGetQuery(conn,"SELECT * FROM refpops") %>%
  filter(is_current=="TRUE") %>%
  select(orgunit_uid) %>%
  mutate(pops_site="Yes") %>%
  rename(orgunituid=orgunit_uid)


staffing_model<-dbGetQuery(conn,"SELECT * FROM refstaffing_model") %>% 
  filter(is_current=="TRUE") %>% 
  select(orgunit_uid,staffing_model) %>% 
  rename(orgunituid=orgunit_uid)


# CONTEXT MERGE ----------------------------------------------------------------


final_df<-doh_att %>% 
  full_join(onehundred,by=c("ou5name"="facility"),keep=TRUE) %>% 
  select(-facility) %>% 
  full_join(DATIM_hierarchy,by=c("ou5name"="facility"),keep=TRUE) %>% 
  left_join(pops,by="orgunituid") %>% 
  left_join(staffing_model,by="orgunituid")


# misaligned_DATIMnames<-DATIM_hierarchy %>% 
#   anti_join(doh_att,by="facility")


distinct100s<-final_df %>% 
  filter(facility_status=="100 in 100") %>% 
  distinct(snu1,psnu,orgunituid,facility,ou5code,ou5name)

# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),"NDoH_DATIM_USAID_siteattributes.txt",sep="_")

write_tsv(final_df, file.path(here("Data/site_att"),filename,na=""))