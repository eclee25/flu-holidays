
## Name: Elizabeth Lee
## Date: 8/12/16
## Function: From metapopulation model outputs, remove cities with no epidemic and calculate the total epidemic size for every combination of interventions and holiday timings.
## Filenames: Anne/Metapopulation_model/final_model_outputs
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(ggplot2); require(readr)

# .Rprofile sets wd 'Dropbox (Bansal Lab)/code' and sources "GeneralTools.R" in .myenv
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
source("source_parseFilenames.R")

#### functions #################################

################################
cleanOutputFile <- function(outputfilename){
  print(match.call())
  
  codes <- parseInfilename(outputfilename)
  print(outputfilename)
  print(unlist(codes))
  
  importDat <- read_csv(outputfilename) %>%
    mutate(seed_id = paste(metro_zero, metro_id, sep = "_"))
  
  # list of seed and metro ID combinations that did not produce epidemics
  epiMetros <- importDat %>%
    select(metro_zero, time_step, metro_id, age, total_infected, seed_id) %>%
    group_by(metro_zero) %>% # max time steps may differ by seed
    filter(time_step == max(time_step)) %>%
    summarise(total_infected = sum(total_infected)) %>%
    ungroup %>%
    summarise(total_infected = mean(total_infected)) %>%
    mutate(intervention = ifelse(codes$contact+codes$travel == 2, "holiday", 
                                 ifelse(!(codes$contact+codes$travel), "baseline", 
                                        ifelse(codes$contact, "contact", "travel")))) %>%
    mutate(timing = ifelse(codes$actual, "actual", ifelse(codes$sixweeks, "plus six weeks", "plus three weeks")))

  print("cleaning done")

  return(epiMetros)
}


#### program ################################
setwd('../final_model_outputs') # example with relative file paths
path_infile <- getwd()
outputfiles <- grep("output_", list.files(), value=TRUE)

dir.create(sprintf('../R_export'), showWarnings=FALSE) 
setwd("../R_export")
fullDat <- data.frame()

for (i in 1:length(outputfiles)){
  infile <- paste0(path_infile, "/", outputfiles[i])
  exportDat <- cleanOutputFile(infile)
  fullDat <- bind_rows(fullDat, exportDat)
}

setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
setwd("../Model_Inputs")
popDat <- read_csv("metro_pop.csv", col_type = "_dd", col_names = c("metro_id", "pop"), skip = 1)
fullPop <- sum(popDat$pop)

fullDat2 <- fullDat %>%
  mutate(pop = fullPop) %>%
  mutate(episize = total_infected/pop*100) %>%
  select(intervention, timing, total_infected, pop, episize)

setwd(dirname(sys.frame(1)$ofile))
setwd("../R_export")
write_csv(fullDat2, "episize_summary.csv")