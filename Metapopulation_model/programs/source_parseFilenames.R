
## Name: Elizabeth Lee
## Date: 8/4/16
## Function: Parse input and output file name strings by intervention
## Filenames: Anne/Metapopulation_model/final_model_outputs
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################

################################
parseInfilename <- function(outputfilename){
  print(match.call())
  # parse file name to determine which intervention was implemented (contact, travel, holiday timing)
  # intervention code positions (4 digits): contact (1 = red_C_all intervention, 2 = red_C_ageROnly intervention, 3 = red_C_ageROnly_less, 4 = red_C_ageROnly_more), travel (1 = swap_networks intervention), actual holiday timing (1 = actual, 0 = late holiday), 6 weeks forward (1 = 6 wks forward, 0 = 3 wks forward; NA if actual holiday timing = 1)
  
  contact <- ifelse(grepl("disease_red_C_all", outputfilename, fixed = TRUE), 1, 
                    ifelse(grepl("disease_red_C_ageROnly", outputfilename, fixed = TRUE), 
                           ifelse(grepl("ageROnly_less", outputfilename, fixed = TRUE), 3,
                                  ifelse(grepl("ageROnly_more", outputfilename, fixed = TRUE), 4, 2)), 0))
  travel <- ifelse(grepl("travel_swap_networks", outputfilename, fixed = TRUE), 1, 0)
  actual <- ifelse(grepl("actual", outputfilename, fixed = TRUE), 1, 0)
  sixweeks <- ifelse(grepl("late_holiday_6wk", outputfilename, fixed = TRUE), 1, ifelse(!actual, 0, NA))
  
  return(list(contact=contact, travel=travel, actual=actual, sixweeks=sixweeks))
}

################################
parseOutfilename <- function(interventionCodes){
  print(match.call())
  # parse file name to determine which intervention was implemented (contact, travel, holiday timing)
  # intervention code positions (4 digits): contact (1 = red_C_all intervention, 2 = red_C_ageROnly intervention, 3 = red_C_ageROnly_less, 4 = red_C_ageROnly_more), travel (1 = swap_networks intervention), actual holiday timing (1 = actual, 0 = late holiday), 6 weeks forward (1 = 6 wks forward, 0 = 3 wks forward; NA if actual holiday timing = 1)
  
  contact <- ifelse(interventionCodes$contact, ifelse(interventionCodes$contact == 2, "disease_red_C_ageROnly", ifelse(interventionCodes$contact == 3, "disease_red_C_ageROnly_less", ifelse(interventionCodes$contact == 4, "disease_red_C_ageROnly_more", "disease_red_C_all"))), "disease_none")
  travel <- ifelse(interventionCodes$travel, "travel_swap_networks", "travel_none")
  actual <- ifelse(interventionCodes$actual, "actual", "")
  sixweeks <- ifelse(is.na(interventionCodes$sixweek), "", ifelse(interventionCodes$sixweeks, "late_holiday_6wk", "late_holiday_3wk"))
  
  if (actual == "actual"){
    returnString <- paste(contact, travel, actual, sep = "_")
  } else{
    returnString <- paste(contact, travel, sixweeks, sep = "_")
  }
  
  return(returnString)
}