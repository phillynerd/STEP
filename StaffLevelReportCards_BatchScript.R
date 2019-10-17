library(rmarkdown)
library(tidyverse)

STEPfinal <- readRDS(paste0("STEPfinal_", format(zoo::as.yearmon(Sys.Date()), format = "%b-%Y"))) %>% 
  filter(EncounterDate >= as.Date("2019/08/01"), #update start date in YYYY/MM/DD format
         EncounterDate <= as.Date("2019/10/01")) #update end date in YYYY/MM/DD format


#School Names####
StaffNames <- STEPfinal %>% 
  filter(is.na(ClinFullName) == F & #removes useless names
         ClinRole2 %in% c("Clinical Social Work Coordinator", "School Behavioral Consultant")) %>% #no report cards for care managers at this time
  distinct(ClinFullName) %>% 
  pull(ClinFullName)


for(i in StaffNames){
    render("StaffLevelReportCards.rmd",
           output_file = paste0("StaffLevelReportCard_Outputs/", 
                                str_replace_all(str_to_title(str_remove_all(i, "[[:punct:]]"))," ","-") ,
                                "_", 
                                Sys.Date(), 
                                ".pdf"),
           params = list(Staff = i))
  }

View(StaffNames)
