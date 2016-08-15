
## Name: Elizabeth Lee
## Date: 8/4/16
## Function: Boxplot: distribution of time to peak across averaged metro IDs, by age group
## Filenames: Anne/Metapopulation_model/average_model_outputs
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(ggplot2); require(readr)
require(RColorBrewer)

# .Rprofile sets wd 'Dropbox (Bansal Lab)/code' and sources "GeneralTools.R" in .myenv
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
source("source_parseFilenames.R") # functions: parseInfilename, parseOutfilename

#### parameters #################################
cProp <- 0.23952911899 # proportion of children in each metro ID population
model_peak <- 190
data_peak <- 140
epiStart <- model_peak - data_peak

#### plotting parameters #################################
w <- 6; h <- 4; dp <- 300

#### functions #################################
################################
importDat <- function(infileName){
  print(match.call())
  # read a single data file, adding columns for each code
  
  codes <- parseInfilename(infileName)
  print(infileName)
  print(unlist(codes))
  
  dummyDat <- read_csv(infileName, col_types = "iicd___") %>%
    mutate(intervention = ifelse(codes$contact+codes$travel == 2, "holiday", 
                                 ifelse(!(codes$contact+codes$travel), "baseline", 
                                        ifelse(codes$contact, "contact", "travel")))) %>%
    mutate(timing = ifelse(codes$actual, "actual", ifelse(codes$sixweeks, "plus six weeks", "plus three weeks")))
  
  # filter out metros that were infected only when they were the seed (these are very short)
  rmMetros <- dummyDat %>% 
    filter(time_step==0 & age == "C" & currentInfected_mn==1) %>%
    distinct(metro_id) %>%
    unlist
  dummyDat2 <- dummyDat %>%
    filter(!(metro_id %in% rmMetros))
  
  return(dummyDat2)
}
################################
plotBxp_age <- function(dat, holidayTiming, age, exportPath){
  print(match.call())
  # plot national time series, averaged
  
  if (holidayTiming == 'actual'){
    holidayStart <- 90 
    exportFilename <- paste0(exportPath, sprintf("/peaktime_%s_actual.png", age))
  } else if (holidayTiming == 'plus three weeks'){
    holidayStart <- 111
    exportFilename <- paste0(exportPath, sprintf("/peaktime_%s_late3wks.png", age))
  } else if (holidayTiming == 'plus six weeks'){
    holidayStart <- 132
    exportFilename <- paste0(exportPath, sprintf("/peaktime_%s_late6wks.png", age))
  }
  interventionTimes <- c(holidayStart-7, holidayStart+7)

  dat2 <- dat %>%
    mutate(peak_time = peak_time-epiStart)
  
  exportPlot <- ggplot(dat2, aes(x = intervention, y = peak_time)) +
    geom_boxplot() + 
    geom_hline(yintercept = interventionTimes, colour = "grey") +
    ylab("Time Steps to Peak") +
    theme_bw() +
    theme(text=element_text(size=12), axis.title.x = element_blank(), legend.position = "bottom", legend.title = element_blank())
  ggsave(exportFilename, exportPlot, units = "in", width = w, height = h, dpi = dp)
      
  return(exportPlot)
}
################################

#### program ################################
#### import simulation data ####
setwd('../R_export') # example with relative file paths
path_infile <- getwd()
infiles <- paste(path_infile, grep("avgOutput_", list.files(), value=TRUE), sep="/")

fullDat <- data.frame()
for (infile in infiles){
  appendDat <- importDat(infile)
  fullDat <- bind_rows(fullDat, appendDat)
}
fullDat <- tbl_df(fullDat)

#### import pop data ####
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
setwd("../Model_Inputs")
popDat <- read_csv("metro_pop.csv", col_type = "_dd", col_names = c("metro_id", "pop"), skip = 1)

#### full data by metro ID ####
popDat2 <- popDat %>%
  mutate(C = cProp*pop, A = (1-cProp)*pop) %>%
  select(-pop) %>%
  gather(age, pop, C, A)

mergDat <- full_join(fullDat, popDat2, by = c("metro_id", "age")) %>%
  mutate(infPer10K = currentInfected_mn/pop*10000) 

#### create plot data ####
pltDat <- mergDat %>% 
  mutate(combo = paste(intervention, timing, sep = "_")) %>% 
  mutate(intervention = factor(intervention, levels = c("baseline", "travel", "contact", "holiday"))) %>%
  group_by(combo, metro_id, age) %>%
  filter(infPer10K == max(infPer10K)) %>%
  rename(peak_time = time_step)

cDat <- pltDat %>%
  filter(age == "C")
aDat <- pltDat %>% 
  filter(age == "A")

#### export plots ####
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
dir.create(sprintf('../plot_outputs'), showWarnings=FALSE) 
setwd("../plot_outputs")
path_export <- getwd()

actualCDat <- cDat %>% filter(timing == "actual")
threeCDat <- cDat %>% filter(timing == "plus three weeks" | intervention == "baseline")
sixCDat <- cDat %>% filter(timing == "plus six weeks" | intervention == "baseline")
actualADat <- aDat %>% filter(timing == "actual")
threeADat <- aDat %>% filter(timing == "plus three weeks" | intervention == "baseline")
sixADat <- aDat %>% filter(timing == "plus six weeks" | intervention == "baseline")

actualCPlot <- plotBxp_age(actualCDat, "actual", "child", path_export)
threeCPlot <- plotBxp_age(threeCDat, "plus three weeks", "child", path_export)
sixCPlot <- plotBxp_age(sixCDat, "plus six weeks", "child", path_export)
actualAPlot <- plotBxp_age(actualADat, "actual", "adult", path_export)
threeAPlot <- plotBxp_age(threeADat, "plus three weeks", "adult", path_export)
sixAPlot <- plotBxp_age(sixADat, "plus six weeks", "adult", path_export)

# exported 8/4/16