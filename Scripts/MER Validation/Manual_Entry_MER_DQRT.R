# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Manual Entry DQRT
# REF ID:   df2e3b9f 
# LICENSE:  MIT
# DATE:     2023-03-30
# UPDATED:  2023-04-18

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(gophr)

# GLOBAL VARIABLES --------------------------------------------------------

genie_folderpath <- "data-raw/MSD-Genie"

file_path <- genie_folderpath %>% 
  return_latest("Genie-SiteByIMs-South-Africa-Daily-2023-03-31_v2") 

# IMPORT ------------------------------------------------------------------

df_msd <- read_psd(file_path)

curr_fy <- 2023
curr_fy_lab <- "FY23"

curr_pd <- "FY23Q1" #change this
prev_fy <- 2022

# get_metadata(file_path)
# prev_fy <- metadata$curr_fy - 1

# MUNGE -------------------------------------------------------------------

#will take a WHILE to run
df_validation <- df_msd %>% 
  filter(funding_agency == "USAID",
         fiscal_year %in% c(prev_fy, metadata$curr_fy),
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(snu1, psnu, sitename, facilityuid, prime_partner_name, mech_code, funding_agency,
           indicator,fiscal_year, standardizeddisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") 

#will take a WHILE to run (check on reporting pd to ensure check is looking where you need)
df_target_result <- df_msd %>% 
  filter(funding_agency == "USAID",
         fiscal_year %in% c(prev_fy, metadata$curr_fy),
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(snu1, psnu, sitename, facilityuid, prime_partner_name, mech_code, funding_agency,
           indicator,fiscal_year, standardizeddisaggregate) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE), .groups = "drop") 


# LEVEL 1 CHECKS -------------------------------------------------------

#1.1: Targets vs. Results if result is blank (NOTE: THIS IS USING CUMULATIVE)

check_1_1 <- df_target_result %>% 
#  reshape_msd() %>% view()
  filter(fiscal_year == curr_fy) %>%
  mutate(check = ifelse(targets > 0 & cumulative == 0, "Targets reported but results blank", NA),
         level = "Level 1",
         today_date = lubridate::today(),
         period = curr_pd,
         type_check = "Result in previous quarter") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period, indicator, check, psnu, sitename)



#1.2: value if reported in previous quarter but zero this quarter

check_1_2 <- df_validation %>% 
  reshape_msd() %>% 
  pivot_wider(names_from = "period") %>% 
  mutate(check = ifelse(`FY22Q4` > 0 & `FY23Q1` == 0 | is.na(`FY23Q1`), "Value reported in previous quarter but missing in this quarter", NA),
         level = "Level 1",
         today_date = lubridate::today(),
         type_check = "Result in previous quarter") %>%
  filter(!is.na(check)) %>% 
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, psnu, sitename)


# LEVEL 2 CHECKS ------------------------------------------

# 2.1: OVC Serv exited without GRAD / OVC_SERV> 10%

df_ovc_grad <- df_msd %>% 
  filter(indicator %in% c("OVC_SERV"),
         funding_agency == "USAID",
         fiscal_year == prev_fy,
         #trendscoarse == "<18",
         standardizeddisaggregate %in% c("Total Numerator","ProgramStatus"),
         otherdisaggregate %in% c(NA, "Exited without Graduation")) %>% 
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator, standardizeddisaggregate, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")

check_2_1 <- df_ovc_grad %>% 
  reshape_msd() %>% 
  filter(period == max(period)) %>% 
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>% 
  mutate(pct = `ProgramStatus` / `Total Numerator`) %>% 
  mutate(check = ifelse(pct > .10, "OVC Serv exited without GRAD / OVC_SERV> 10%", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, psnu, sitename)


# 2.2: OVC_HIVSTAT < OVC_SERV under 18 program status

df_ovc_hivstat <- df_msd %>% 
  filter(indicator %in% c("OVC_HIVSTAT"),
         funding_agency == "USAID",
         fiscal_year == prev_fy,
         standardizeddisaggregate %in% c("Total Numerator")) %>% 
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")

df_ovc_serv_u18 <- df_msd %>% 
  filter(indicator %in% c("OVC_SERV"),
         funding_agency == "USAID",
         fiscal_year == prev_fy,
         trendscoarse == "<18",
         standardizeddisaggregate %in% c("Age/Sex/ProgramStatus")) %>% 
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")

check_2_2 <- df_ovc_hivstat %>% 
  bind_rows(df_ovc_serv_u18) %>% 
  reshape_msd() %>% 
  filter(period == max(period)) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>% 
  mutate(check = ifelse(`OVC_HIVSTAT` != `OVC_SERV`, "OVC_HIVSTAT not equal to OVC_SERV <18", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, psnu, sitename)
  

# 2.3: PrECT CT <= PrEP_CT KP
  # manual entry of KP?

df_prep_kp <- df_msd %>% 
  filter(indicator == "PrEP_CT",
         funding_agency == "USAID",
         standardizeddisaggregate %in% c("Total Numerator", "KeyPop")) %>% 
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator, standardizeddisaggregate, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") 

check_2_3 <- df_prep_kp %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>% 
  mutate(check = ifelse(`Total Numerator` < `KeyPop`, "PrEP_CT<PrEP_CT KeyPop Disagg", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, psnu, sitename)

# 2.4: PrEP_CT <= PrEP_CT <= TestResult Disagg

df_prep_test <- df_msd %>% 
  filter(indicator == "PrEP_CT",
         funding_agency == "USAID",
         standardizeddisaggregate %in% c("Total Numerator", "TestResult")) %>% 
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator, standardizeddisaggregate, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")

check_2_4 <- df_prep_test %>% 
  reshape_msd() %>% 
  filter(period == curr_pd) %>% 
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>% 
  mutate(check = ifelse(`Total Numerator` < `TestResult`, "PrEP_CT<PrEP_CT TestResult Disagg", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, psnu, sitename)




# 2.5: HTS INDEX: INDEX < ACCEPTED

df_index <- df_msd %>% 
  filter(indicator %in% c("HTS_INDEX"),
         funding_agency == "USAID",
         fiscal_year %in% c(curr_fy)) %>%
  filter(standardizeddisaggregate %in% c("1:Age/Sex/IndexCasesOffered","2:Age/Sex/IndexCasesAccepted")) %>% 
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator, standardizeddisaggregate, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")

check_2_5 <- df_index %>% 
  reshape_msd() %>% 
  filter(period == curr_pd) %>% 
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>%
  mutate(check = ifelse(`1:Age/Sex/IndexCasesOffered` < `2:Age/Sex/IndexCasesAccepted`, "Index_offered<Index_accepted", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, psnu, sitename)


# 2.6: HTS_INDEX: Index contacts < contacts_new_pos + contacts_new_neg + contacts_known_pos

df_hts_contacts <- df_msd %>% 
  filter(indicator %in% c("HTS_INDEX")) %>% 
  filter(standardizeddisaggregate %in% c("3:Age Aggregated/Sex/Contacts", "4:Age/Sex/Result")) %>%
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator,
           standardizeddisaggregate, otherdisaggregate, statushiv, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")

check_2_6 <- df_hts_contacts %>% 
  reshape_msd() %>% 
  filter(period == curr_pd) %>% 
  mutate(disagg_name = glue("{otherdisaggregate}_{statushiv}")) %>% 
  mutate(disagg_name = recode(disagg_name, "NA_NA" = "Total Contacts",
                              "NA_Negative" = "Negatives")) %>% 
  select(-c(standardizeddisaggregate:statushiv)) %>% 
  # filter(!str_detect(disagg_name, "NA")) %>% 
  #count(disagg_name, standardizeddisaggregate, otherdisaggregate, statushiv
  pivot_wider(names_from = "disagg_name", values_from = "value") %>%
  mutate(check = ifelse(`Total Contacts` <
                          `Known at Entry_Positive` + `Newly Identified_Negative` + `Newly Identified_Positive`,
                        "Index_contacts <contacts_ new_pos + contacts_ new_neg + contacts_ known_pos", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>% 
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, psnu, sitename)
#select(primepartner,level,today_date,check, period, orgUnitName,` Index/Age Aggregated/Sex/Contacts): Number of contacts`,` Index/Age/Sex/Result): HTS Result`)


# 2.7: HTS_TST_POS 15+ <= HTS_RECENT total num

HTS_TST_POS_15plus <- df_msd %>%
  filter((`indicator`== "HTS_TST" & (categoryoptioncomboname == "15-19, Male, Positive" | categoryoptioncomboname ==  "15-19, Female, Positive"
                                     | categoryoptioncomboname == "20-24, Male, Positive" | categoryoptioncomboname ==  "20-24, Female, Positive"
                                     | categoryoptioncomboname == "25-29, Male, Positive" | categoryoptioncomboname ==  "25-29, Female, Positive"
                                     | categoryoptioncomboname == "30-34, Male, Positive" | categoryoptioncomboname ==  "30-34, Female, Positive"
                                     | categoryoptioncomboname == "35-39, Male, Positive" | categoryoptioncomboname ==  "35-39, Female, Positive"
                                     | categoryoptioncomboname == "40-44, Male, Positive" | categoryoptioncomboname ==  "40-44, Female, Positive"
                                     | categoryoptioncomboname == "45-49, Male, Positive" | categoryoptioncomboname ==  "45-49, Female, Positive"
                                     | categoryoptioncomboname == "50+, Male, Positive" | categoryoptioncomboname ==  "50+, Female, Positive")))


HTS_RECENT <- df_msd %>% 
  filter(indicator %in% c("HTS_RECENT"),
         fiscal_year %in% c(prev_fy, curr_fy)) %>% 
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")


check_2_7 <- bind_rows(HTS_TST_POS_15plus, HTS_RECENT) %>%
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd() %>%   
  filter(period == curr_pd) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(check = ifelse(`HTS_TST` >= `HTS_RECENT`, "HTS_TST_POS (>=15) >=HTS_RECENT", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>% 
  filter(!is.na(check)) %>% 
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, psnu, sitename)


# 2.8: HTS_TST < HTS_TST_POS

df_hts <- df_msd %>% 
  filter(funding_agency == "USAID",
         fiscal_year %in% c(prev_fy, curr_fy),
         indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(snu1, psnu, sitename, facilityuid, prime_partner_name, mech_code, funding_agency,
           indicator,fiscal_year, standardizeddisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") 

check_2_8 <- df_hts  %>%  
  reshape_msd() %>% 
  filter(period == curr_pd) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>% 
  # rename()
  mutate(check = ifelse(`HTS_TST` < `HTS_TST_POS`, "HTS_TST < HTS_TST_POS", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, psnu, sitename)

# CONSOLIDATE CHECKS ---------------------------------------------------------

level1_checks <- bind_rows(
  check_1_1,
  check_1_2) %>% 
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, psnu, sitename)

level2_checks <- bind_rows(
  check_2_1,
  check_2_2,
 # check_2_3,
 # check_2_4,
  check_2_5,
  check_2_6,
  check_2_7
 #,
  #check_2_8
 ) %>% 
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, psnu, sitename)


Master_DQRT <- openxlsx::loadWorkbook("MER Manual Entry checks/MASTER MER DQRT Tracker.xlsx") 


openxlsx::writeData(Master_DQRT, sheet = "Level1", x = level1_checks, 
                    colNames=T, withFilter=T)
openxlsx::writeData(Master_DQRT, sheet = "Level2", x = level2_checks, 
                    colNames=T, withFilter=T)
openxlsx::saveWorkbook(Master_DQRT, "MER Manual Entry checks/MASTER MER DQRT Tracker.xlsx", overwrite = TRUE)       

