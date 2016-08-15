
## Name: Elizabeth Lee
## Date: 8/4/16
## Function: age-specific time series, holiday weeks only
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
  
  return(dummyDat)
}
################################
plotNatTS <- function(dat, holidayTiming, age, exportPath){
  print(match.call())
  # plot national time series by age group, averaged
  
  if (holidayTiming == 'actual'){
    holidayStart <- 90
    exportFilename <- paste0(exportPath, sprintf("/holidayNational%s_avg_actual_prevalence.png", age))
  } else if (holidayTiming == 'plus three weeks'){
    holidayStart <- 111
    exportFilename <- paste0(exportPath, sprintf("/holidayNational%s_avg_late3wks_prevalence.png", age))
  } else if (holidayTiming == 'plus six weeks'){
    holidayStart <- 132
    exportFilename <- paste0(exportPath, sprintf("/holidayNational%s_avg_late6wks_prevalence.png", age))
  }
  interventionTimes <- c(holidayStart-7, holidayStart+7)
  holidayPeriodTimes <- c(holidayStart-30, holidayStart+30)

  dat2 <- dat %>%
    mutate(time_step = time_step-epiStart) %>%
    filter(time_step >= 0)
  
  exportPlot <- ggplot(dat2, aes(x = time_step, y = infPer10K)) +
    geom_line(aes(colour = intervention)) + 
    geom_vline(xintercept = interventionTimes, colour = "black") +
    scale_colour_brewer(name = "intervention", palette = "Set1") + 
    ylab("Current Infections per 10,000") +
    scale_x_continuous("Time Step", limits = holidayPeriodTimes) +
    theme_bw() +
    theme(axis.text=element_text(size=12), legend.position = "bottom", legend.title = element_blank())
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
fullPop <- sum(popDat$pop)

#### full data - national by age group ####
fullDat2 <- fullDat %>% 
  group_by(intervention, timing, age, time_step) %>%
  summarise(currentInfected_mn = sum(currentInfected_mn)) %>%
  ungroup %>%
  mutate(infPer10K = currentInfected_mn/fullPop*10000)

#### export plots ####
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
dir.create(sprintf('../plot_outputs'), showWarnings=FALSE) 
setwd("../plot_outputs")
path_export <- getwd()

fullDat3 <- fullDat2 %>% 
  mutate(combo = paste(intervention, timing, sep = "_")) %>% 
  mutate(intervention = factor(intervention, levels = c("baseline", "travel", "contact", "holiday"))) 
cDat <- fullDat3 %>%
  filter(age == "C")
aDat <- fullDat3 %>%
  filter(age == "A")

actualCDat <- cDat %>% 
  filter(timing == "actual")
threeCDat <- cDat %>% 
  filter(timing == "plus three weeks" | intervention == "baseline") 
sixCDat <- cDat %>% 
  filter(timing == "plus six weeks" | intervention == "baseline") 
actualADat <- aDat %>% 
  filter(timing == "actual")
threeADat <- aDat %>% 
  filter(timing == "plus three weeks" | intervention == "baseline") 
sixADat <- aDat %>% 
  filter(timing == "plus six weeks" | intervention == "baseline") 

actualCPlot <- plotNatTS(actualCDat, "actual", "Child", path_export)
threeCPlot <- plotNatTS(threeCDat, "plus three weeks", "Child", path_export)
sixCPlot <- plotNatTS(sixCDat, "plus six weeks", "Child", path_export)
actualAPlot <- plotNatTS(actualADat, "actual", "Adult", path_export)
threeAPlot <- plotNatTS(threeADat, "plus three weeks", "Adult", path_export)
sixAPlot <- plotNatTS(sixADat, "plus six weeks", "Adult", path_export)

