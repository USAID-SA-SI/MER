
library(openxlsx)
library(tidyverse)


setwd("C:\\Users\\ctrapence\\Documents\\Clement Trapence-South Africa WP\\Import files\\FY23Q3 Import\\Cleaning")
dir()

CDC_import<-read.csv("CDC_Import_Cleaning_FY23Q3.csv") %>% rename(Dataelement=dataElement,CategoryOptionCombo=categoryOptionCombo,
                                                                      AttributeOptionCombo=attributeOptionCombo , Period=period  ,OrgUnit=OrgUnitID  )

USAID_import<-read.csv( "USAID_FY23Q3 Final Cleaning Import file.csv") %>% rename(Dataelement=dataElement_uid,CategoryOptionCombo=categoryOptionCombo,
                                                                                  AttributeOptionCombo=attributeOptionCombo , Period=period    )

country<-"South Africa"

quarter<-"Q3"

Date<-Sys.Date()

New_data: Country_YYYYQnimport

Updated_Data<-paste0("PEPFAR_",quarter,"_DataImport_Update_",Date,".csv")

Merged_import<-rbind(CDC_import,USAID_import) %>% filter(Value>0)

write.csv(Merged_import,Updated_Data,row.names = FALSE)

Delete_Data<-paste0("PEPFAR_",quarter,"_DeleteData_",Date,".csv")


CDC_Delete<-read.csv("Delete_Cleaning_FY23Q3.csv")


write.csv(CDC_Delete,Delete_Data,row.names = FALSE)


#Initial Import processing 

New_data<-paste0(country,"_2023",quarter,"import")
