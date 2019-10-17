library(rmarkdown)
library(tidyverse)

#make sure raw data goes to the most clean file, usually whatever csv you create at the end of STEP data cleaning and restructuring code.
STEPfinal <- readRDS(paste0("STEPfinal_", format(zoo::as.yearmon(Sys.Date()), format = "%b-%Y"))) %>% 
  filter(EncounterDate >= as.Date("2019/08/01"), #update start date in YYYY/MM/DD format
         EncounterDate <= as.Date("2019/10/01")) #update end date in YYYY/MM/DD format

#School Names####
Roles <- STEPfinal %>% 
  distinct(ClinRole2) %>% 
  filter(is.na(ClinRole2) == F) %>% 
  pull(ClinRole2)#pulls out the schoolname column and turns it into a vector



for(i in Roles){
  render("RoleLevelReportCard.rmd",
         output_file = paste0("RoleLevelReportCard_Outputs/", 
                              str_replace_all(str_to_title(str_remove_all(i, "[[:punct:]]"))," ","-") ,
                              "_", 
                              Sys.Date(), 
                              ".pdf"),
         params = list(ClinRole = i))
}




