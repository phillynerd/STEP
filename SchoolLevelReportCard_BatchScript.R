library(rmarkdown)
library(tidyverse)

#This code assumes your cleaning the data and knitting the reports in the same month. If you're not, this won't work. 
STEPfinal <- readRDS(paste0("STEPfinal_", format(zoo::as.yearmon(Sys.Date()), format = "%b-%Y"))) %>% 
  filter(EncounterDate >= as.Date("2019/08/01"), #update start date in YYYY/MM/DD format
         EncounterDate <= as.Date("2019/10/01")) #update end date in YYYY/MM/DD format

#School Names####
SchoolNames <- STEPfinal %>% 
  distinct(SchoolName) %>% 
  filter(is.na(SchoolName) == F) %>% 
  pull(SchoolName)#pulls out the schoolname column and turns it into a vector
  


for(i in SchoolNames){
  render("SchoolLevelReportCard.rmd",
         output_file = paste0("SchoolLevelReportCard_Outputs/", 
                              str_replace_all(str_to_title(str_remove_all(i, "[[:punct:]]"))," ","-") ,
                              "_", 
                              Sys.Date(), 
                              ".pdf"),
         params = list(School = i))
}




   

