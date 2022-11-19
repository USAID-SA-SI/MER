library(tidyverse)
library(readxl)
library(gophr)
library(here)
library(glamr)
library(janitor)


current_pd<-"FY22Q3i"

# READ IN FILES ----------------------------------------------------------------

#genie 
genie_files<-list.files(here("Data"),pattern="SITE")


genie<-here("Data",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>%
  filter(fiscal_year %in% c("2022"),
         funding_agency=="USAID") %>% 
  reshape_msd("wide") %>% 
  group_by(mech_code,prime_partner_name,
           psnu,facility,facilityuid,
           indicator,standardizeddisaggregate) %>% 
  summarise_at(vars(fy2022q1:fy2022q3),sum,na.rm=TRUE) %>% 
  ungroup()




mfl<-read_excel(here("Data","USAID_MASTER_FACILITY_LIST_FY22Q3_draft v3.xlsx")) %>% 
  clean_names() %>% 
  select(new_ou5_code,old_ou5code,datim_uid,tx_curr) %>% 
  rename(facilityuid=datim_uid,
         tx_curr_expected=tx_curr)



final<-genie %>% 
  full_join(mfl,by="facilityuid") %>% 
  mutate(DSP=case_when(
    mech_code %in% c("70287","70290",
                     "70310","81902",
                     "70301") ~ "YES",
    TRUE ~ "NO"),
    flag=case_when(
      tx_curr_expected=="YES" & 
        fy2022q3=="0" ~ "FLAG",
      TRUE ~ "OK"
    ))


flagged<-final %>% 
  filter(flag=="FLAG",
         DSP=="YES",
         !facilityuid=="bb2Z51EE6cl")


q2sum_missing<-flagged %>% 
  group_by(psnu) %>% 
  summarize_at(vars(fy2022q2:fy2022q3),sum,na.rm=TRUE)



q3_dsp<-final %>% 
  filter(DSP=="YES",
         fy2022q3>0)

# Dataout ----------------------------------------------------------------------

filename<-paste(Sys.Date(),current_pd,"TX_CURR_Check_vs_MFL.txt",sep="_")

write_tsv(final, file.path(here("Dataout"),filename,na=""))



