
## Name: Elizabeth Lee
## Date: 8/3/16
## Function: From metapopulation model outputs, remove cities with no epidemic and calculate the average and standard error of the data at each time point. Export these averaged outputs.
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
  
  importDat <- read_csv(outputfilename) %>%
    mutate(seed_id = paste(metro_zero, metro_id, sep = "_"))
  
  # list of seed and metro ID combinations that did not produce epidemics
  noepiMetros <- importDat %>%
    select(metro_zero, time_step, metro_id, age, total_infected, seed_id) %>%
    group_by(metro_zero) %>% # max time steps may differ by seed
    filter(time_step == max(time_step)) %>%
    ungroup %>%
    spread(age, total_infected) %>%
    mutate(total = C+A) %>%
    filter(total == 0) %>%
    distinct(seed_id) %>%
    unlist

    # remove non-epidemics
  cleanDat <- importDat %>%
    filter(!(seed_id %in% noepiMetros))
  print("cleandat done")

  # get mean & SD for current & total infections for each age group and metro_id at every time point (collapse the seeds)
  averageDat <- cleanDat %>%
    group_by(time_step, metro_id, age) %>%
    summarise(currentInfected_mn = mean(currently_infected), 
              currentInfected_sd = sd(currently_infected),
              totInfected_mn = mean(total_infected),
              totInfected_sd = sd(total_infected)) %>%
    arrange(age, metro_id, time_step)
  
  return(averageDat)
}

################################
exportAverageFile <- function(infilename, exportFile){
  
  code <- parseInfilename(infilename)
  codestring <- parseOutfilename(code)
  write_csv(exportFile, paste0("avgOutput_", codestring, ".csv"))
  
  return(paste("export complete", codestring))
}

#### program ################################
setwd('../final_model_outputs') # example with relative file paths
path_infile <- getwd()
outputfiles <- grep("output_", list.files(), value=TRUE)

dir.create(sprintf('../R_export'), showWarnings=FALSE) 
setwd("../R_export")


for (i in 1:length(outputfiles)){
  infile <- paste0(path_infile, "/", outputfiles[i])
  exportDat <- cleanOutputFile(infile)
  exportAverageFile(infile, exportDat)
}
# export 8/3/16
