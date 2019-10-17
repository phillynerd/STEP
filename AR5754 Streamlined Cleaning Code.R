#This is based on the cleaning code in the Jess Analysis folder under STEP, but
#this code has been modified to handle the changes made on 8/1/2019 to how data
#were collected.  All col names that end in are now deprecated, and should be
#excluded.  Similarly, any data analysed using this code should be limited to
#only entries on or after 8/1/2019

#libraries####
library(tidyverse)
library(data.table)
library(tidylog)
library(skimr)
library(visdat)
library(lubridate)
library(magrittr)

#reading data and some basic filtering####
STEPraw_orig <- read_csv("RawData/STEP_Encounter_Form-Extract-20191001_BASICCLEANING.csv",
                    guess_max = 100000) %>%  #this ensures readr looks at all data in field before making a guess at data type.
          mutate(EncounterDate = as.Date(EncounterDate, "%m/%d/%Y")) %>% 
          filter(EncounterDate >= as.Date("08/01/2019", "%m/%d/%Y")) #This code can't be used on any data prior to 08/01/2019. Earlier analyses and code archived in Jess Analysis folder in this same directory

STEPraw <- STEPraw_orig

STEPraw %<>%  
  select(-ends_with("_OLD"), -ends_with("_Old")) #removing all old columns

skim(STEPraw)
        
#Cleaning####
STEPraw$ClinRole <- as.factor(STEPraw$ClinRole)
STEPraw$SchoolName <- as.factor(STEPraw$SchoolName)
STEPraw$Child_SpecificEncounters <- as.factor(STEPraw$Child_SpecificEncounters)


#Adding in 0s for NA times####
STEPraw %<>% 
  replace_na(list(Child1_DurationHours = 0,Child1_DurationMinutes = 0,
                  Child2_DurationHours = 0,Child2_DurationMinutes = 0,
                  Child3_DurationHours = 0,Child3_DurationMinutes = 0,
                  Child4_DurationHours = 0,Child4_DurationMinutes = 0,
                  Child5_DurationHours = 0,Child5_DurationMinutes = 0,
                  Child6_DurationHours = 0,Child6_DurationMinutes = 0,
                  Child7_DurationHours = 0,Child7_DurationMinutes = 0,
                  Child8_DurationHours = 0,Child8_DurationMinutes = 0,
                  Child9_DurationHours = 0,Child9_DurationMinutes = 0,
                  Child10_DurationHours = 0,Child10_DurationMinutes = 0,
                  Child11_DurationHours = 0,Child11_DurationMinutes = 0,
                  Child12_DurationHours = 0,Child12_DurationMinutes = 0,
                  Child13_DurationHours = 0,Child13_DurationMinutes = 0,
                  Child14_DurationHours = 0,Child14_DurationMinutes = 0,
                  Child15_DurationHours = 0,Child15_DurationMinutes = 0,
                  School1_DurationHours = 0,School1_DurationMinutes = 0,
                  School2_DurationHours = 0,School2_DurationMinutes = 0,
                  School3_DurationHours = 0,School3_DurationMinutes = 0,
                  School4_DurationHours = 0,School4_DurationMinutes = 0,
                  School5_DurationHours = 0,School5_DurationMinutes = 0,
                  School6_DurationHours = 0,School6_DurationMinutes = 0,
                  School7_DurationHours = 0,School7_DurationMinutes = 0,
                  School8_DurationHours = 0, School8_DurationMinutes = 0,
                  School9_DurationHours = 0, School9_DurationMinutes = 0,
                  School10_DurationHours = 0, School10_DurationMinutes = 0
                  ))

#deleting unnecessary columns and converting to dates####
STEPclean <- STEPraw %>% 
  select(-SchoolName_1, -Address2, -Address2_1, -X3,
         -starts_with("Please")) %>%  #drops 29 variables
  mutate(DOB = as.Date(DOB, "%m/%d/%Y"),
         Submitdate = as.Date(Submitdate, "%m/%d/%Y")) %>%  #Dates 
  filter(is.na(SchoolName) == F) #removes 4 lines of data where schoolname is missing

#Creating durations for each task

STEPduration <- STEPclean %>% 
          #school level variable
  mutate(School1_Dif = as.ITime(paste0(School1_DurationHours, ":", School1_DurationMinutes)),
         School2_Dif = as.ITime(paste0(School2_DurationHours, ":", School2_DurationMinutes)),
         School3_Dif = as.ITime(paste0(School3_DurationHours, ":", School3_DurationMinutes)),
         School4_Dif = as.ITime(paste0(School4_DurationHours, ":", School4_DurationMinutes)),
         School5_Dif = as.ITime(paste0(School5_DurationHours, ":", School5_DurationMinutes)),
         School6_Dif = as.ITime(paste0(School6_DurationHours, ":", School6_DurationMinutes)),
         School7_Dif = as.ITime(paste0(School7_DurationHours, ":", School7_DurationMinutes)),
         School8_Dif = as.ITime(paste0(School8_DurationHours, ":", School8_DurationMinutes)),
         School9_Dif = as.ITime(paste0(School9_DurationHours, ":", School9_DurationMinutes)),
         School10_Dif = as.ITime(paste0(School10_DurationHours, ":", School10_DurationMinutes)),
         #child leve variables
         Child1_Dif = as.ITime(paste0(Child1_DurationHours, ":", Child1_DurationMinutes)),
         Child2_Dif = as.ITime(paste0(Child2_DurationHours, ":", Child2_DurationMinutes)),
         Child3_Dif = as.ITime(paste0(Child3_DurationHours, ":", Child3_DurationMinutes)),
         Child4_Dif = as.ITime(paste0(Child4_DurationHours, ":", Child4_DurationMinutes)),
         Child5_Dif = as.ITime(paste0(Child5_DurationHours, ":", Child5_DurationMinutes)),
         Child6_Dif = as.ITime(paste0(Child6_DurationHours, ":", Child6_DurationMinutes)),
         Child7_Dif = as.ITime(paste0(Child7_DurationHours, ":", Child7_DurationMinutes)),
         Child8_Dif = as.ITime(paste0(Child8_DurationHours, ":", Child8_DurationMinutes)),
         Child9_Dif = as.ITime(paste0(Child9_DurationHours, ":", Child9_DurationMinutes)),
         Child10_Dif = as.ITime(paste0(Child10_DurationHours, ":", Child10_DurationMinutes)),
         Child11_Dif = as.ITime(paste0(Child11_DurationHours, ":", Child11_DurationMinutes)),
         Child12_Dif = as.ITime(paste0(Child12_DurationHours, ":", Child12_DurationMinutes)),
         Child13_Dif = as.ITime(paste0(Child13_DurationHours, ":", Child13_DurationMinutes)),
         Child14_Dif = as.ITime(paste0(Child14_DurationHours, ":", Child14_DurationMinutes)),
         Child15_Dif = as.ITime(paste0(Child15_DurationHours, ":", Child15_DurationMinutes))) %>% 
  select(-contains("DurationHours"), -contains("DurationMinutes"))


#Cleaning clinical roles####
#Creating a ref table with the most common clinical role associated with each clinician
MostCommonRoleByName <- STEPduration %>% 
  group_by(ClinFullName, ClinRole) %>% 
  count() %>% 
  arrange(ClinFullName, -n) %>% 
  group_by(ClinFullName) %>% 
  summarize(ClinRole = first(ClinRole)) %>% 
  rename(ClinRole2 = ClinRole)

STEPduration %<>% 
  #bringing in most common clinical role associated with each clinician
  left_join(MostCommonRoleByName, by = "ClinFullName")


#Going to long format to deal with tasks####
STEPfinal <-
  STEPduration %>% 
  gather(key = "TaskNumber", value = "TaskType", ends_with("Task")) %>% #group_by(TaskNumber) %>% tally()
  mutate(TimeElapsed = case_when(TaskNumber == "Child1_Task" ~ Child1_Dif,
                                 TaskNumber == "Child2_Task" ~ Child2_Dif,
                                 TaskNumber == "Child3_Task" ~ Child3_Dif,
                                 TaskNumber == "Child4_Task" ~ Child4_Dif,
                                 TaskNumber == "Child5_Task" ~ Child5_Dif,
                                 TaskNumber == "Child6_Task" ~ Child6_Dif,
                                 TaskNumber == "Child7_Task" ~ Child7_Dif,
                                 TaskNumber == "Child8_Task" ~ Child8_Dif,
                                 TaskNumber == "Child9_Task" ~ Child9_Dif,
                                 TaskNumber == "Child10_Task" ~ Child10_Dif,
                                 TaskNumber == "Child11_Task" ~ Child11_Dif,
                                 TaskNumber == "Child12_Task" ~ Child12_Dif,
                                 TaskNumber == "Child13_Task" ~ Child13_Dif,
                                 TaskNumber == "School1_Task" ~ School1_Dif,
                                 TaskNumber == "School2_Task" ~ School2_Dif,
                                 TaskNumber == "School3_Task" ~ School3_Dif,
                                 TaskNumber == "School4_Task" ~ School4_Dif,
                                 TaskNumber == "School5_Task" ~ School5_Dif,
                                 TaskNumber == "School6_Task" ~ School6_Dif,
                                 TaskNumber == "School7_Task" ~ School7_Dif,
                                 TRUE ~ as.ITime(00:00:00))) %>% 
  #select(TaskNumber, TaskType, TimeElapsed, ends_with("Dif")) %>% View() - Used this to check case statement
  mutate(ChildID = paste0(ChildFirst,ChildLast)) %>% 
  select(EncounterDate, SchoolName, ClinRole2, TaskNumber, TaskType, TimeElapsed, ChildID, Address1, ClinFullName) %>% 
  mutate(TaskType = ifelse(is.na(TaskType)==T & TimeElapsed != as.ITime(00:00:00), "Unknown", TaskType)) %>% #changing NA to unknown when there's a time btu no task type selected
  mutate(TaskType = as.factor(TaskType),
         EncounterMonth = month(EncounterDate),
         TaskCat = as.factor(ifelse(str_sub(TaskNumber,1,1) == "C", "Child", "School"))) %>% 
  filter(!(is.na(TaskType) == T & TimeElapsed == as.ITime(00:00:00))) %>% #filters out blank task types with durations of zeros
  filter(is.na(EncounterDate) == F) %>%  #currently not catching anything, just a safeguard
  filter(TimeElapsed >= 0) #currently not catching anything, just a safeguard

#note - found cases where there is a task type but no times listed. Confirmed in raw data. 

#Saving STEPfinal for the rmd files
saveRDS(STEPfinal, paste0("STEPfinal_", format(zoo::as.yearmon(Sys.Date()), format = "%b-%Y")))

