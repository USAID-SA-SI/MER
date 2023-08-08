library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)
library(Wavelength)
library(janitor)
library(RPostgres)
library(aws.s3)
library(redshiftTools)
library(RSQLite)
library(RPostgreSQL)



# GLOBALS
load_secrets()


#LOCAL FUNCTIONS
source(here("Scripts/00_utilities.R"))

#DB FUNCTION
aws_connect <- function(db_name, db_user, db_pass,
                        db_host,db_port) {
  ## Connections
  DBI::dbConnect(
    drv=RPostgres::Postgres(),
    host = db_host,
    port = as.integer(db_port),
    dbname = db_name,
    user = db_user,
    password = db_pass
  )
}


#ONE TIME ONLY TO STORE CREDENTIALS
# set_key(service = pg_service("DC"), "host")
# set_key(service = pg_service("DC"), "port")
# set_key(service = pg_service("DC"), "database")
# set_key(service = pg_service("DC"), "username")
# set_key(service = pg_service("DC"), "password")


#load keys
db_host <-pg_host("DC")
db_port <- pg_port("DC")
db_name <- pg_database("DC")
db_user <- pg_user("DC")
db_pwd <- pg_pwd("DC")


#establish connection
conn <- aws_connect(db_name = db_name, db_user = db_user,
                    db_pass = db_pwd, db_host = db_host, db_port = db_port)




# CONTEXT FILES IN -------------------------------------------------------------
#MARCH LIST FROM MID_COP MARCH 2023
# onehundred<-read_excel(here("Data/site_att","1hundred_in_1hundred.xlsx")) %>% 
#   mutate(facility_status="100 in 100") %>% 
#   rename(facility=`1hundred_in_1hundred`)


#NEW LIST FROM JULY 2023
onehundred<-read_excel(here("Data/site_att","100 Facilities list.xlsx")) %>% 
  select(Facility) %>% 
  clean_names() %>% 
  mutate(facility_status="100 in 100")



doh_att<-read_csv(here("Data/site_att", "vwOrgunitStructureOU5.csv")) %>% 
  clean_names() %>% 
  select(ou5code,ou5name,org_unit_ownership,
         org_unit_rural_urban,
         org_unit_type)

DATIM_hierarchy<-pull_hierarchy("cDGPF739ZZr", myuser, askpass::askpass()) %>% #myuser is DATIM username
  mutate(facility=str_sub(orgunit,4)) %>%
  select(snu1,psnu,psnuuid,orgunit,orgunituid,latitude,longitude) %>% 
  rename(facility=orgunit)


#USAID ONLY LIST FROM DATA CENTRE
pops <- dbGetQuery(conn,"SELECT * FROM refpops") %>%
  filter(is_current=="TRUE") %>%
  select(orgunit_uid) %>%
  mutate(pops_site="Yes") %>%
  rename(orgunituid=orgunit_uid)


#USAID ONLY LIST FROM DATA CENTRE
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