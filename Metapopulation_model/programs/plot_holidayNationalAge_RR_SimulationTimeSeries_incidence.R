
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
  
  dummyDat <- read_csv(infileName, col_types = "iicd") %>%
    mutate(intervention = ifelse(codes$contact+codes$travel == 2, "holiday", 
                                 ifelse(!(codes$contact+codes$travel), "baseline", 
                                        ifelse(codes$contact, "school closure", "travel")))) %>%
    mutate(timing = ifelse(codes$actual, "actual", ifelse(codes$sixweeks, "plus six weeks", "plus three weeks")))
  
  return(dummyDat)
}
################################
plotNatTS_RR <- function(dat, holidayTiming, age, exportPath){
  print(match.call())
  # plot national time series relative risk, averaged
  
  if (holidayTiming == 'actual'){
    holidayStart <- 90
    exportFilename <- paste0(exportPath, sprintf("/holidayNational%s_avg_actual_incidence.png", age))
  } else if (holidayTiming == 'plus three weeks'){
    holidayStart <- 111
    exportFilename <- paste0(exportPath, sprintf("/holidayNational%s_avg_late3wks_incidence.png", age))
  } else if (holidayTiming == 'plus six weeks'){
    holidayStart <- 132
    exportFilename <- paste0(exportPath, sprintf("/holidayNational%s_avg_late6wks_incidence.png", age))
  }
  interventionTimes <- c(holidayStart-7, holidayStart+8)
  holidayPeriodTimes <- c(holidayStart-30, holidayStart+30)
  
  dat2 <- dat %>%
    mutate(time_step = time_step-epiStart) %>%
    filter(time_step >= 0)

  exportPlot <- ggplot(dat2, aes(x = time_step, y = RR)) +
    geom_line(aes(colour = intervention)) + 
    geom_vline(xintercept = interventionTimes, colour = "black", linetype = 2) +
    scale_colour_brewer(name = "intervention", palette = "Set1") + 
    scale_y_continuous("relative risk (children:adults)", limits = c(0,5)) +
    scale_x_continuous("time step", limits = holidayPeriodTimes) +
    theme_bw() +
    theme(axis.text=element_text(size=14), legend.position = c(1,1), legend.justification = c(1,1), legend.title = element_blank())
  ggsave(exportFilename, exportPlot, units = "in", width = w, height = h, dpi = dp)
      
  return(exportPlot)
}
################################

#### program ################################
#### import simulation data ####
setwd('../R_export') # example with relative file paths
path_infile <- getwd()
infiles <- paste(path_infile, grep("avgOutputIncidence_", list.files(), value=TRUE), sep="/")

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
cPop <- cProp*fullPop
aPop <- (1-cProp)*fullPop

#### full data - national RR ####
fullDat2 <- fullDat %>% 
  group_by(intervention, timing, age, time_step) %>%
  summarise(newInfected = sum(newInfected)) %>%
  ungroup %>%
  spread(age, newInfected) %>%
  mutate(cInfPer10K = C/cPop*10000, aInfPer10K = A/aPop*10000) %>%
  mutate(RR = cInfPer10K/aInfPer10K) %>%
  filter(time_step != 0) # rm timesteps where RR is Inf
  
#### export plots ####
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
dir.create(sprintf('../plot_outputs'), showWarnings=FALSE) 
setwd("../plot_outputs")
path_export <- getwd()

fullDat3 <- fullDat2 %>% 
  mutate(combo = paste(intervention, timing, sep = "_")) %>% 
  mutate(intervention = factor(intervention, levels = c("baseline", "travel", "school closure", "holiday"))) 

actualDat <- fullDat3 %>% 
  filter(timing == "actual")
threeDat <- fullDat3 %>% 
  filter(timing == "plus three weeks" | intervention == "baseline") 
sixDat <- fullDat3 %>% 
  filter(timing == "plus six weeks" | intervention == "baseline") 

actualCPlot <- plotNatTS_RR(actualDat, "actual", "RR", path_export)
threeCPlot <- plotNatTS_RR(threeDat, "plus three weeks", "RR", path_export)
sixCPlot <- plotNatTS_RR(sixDat, "plus six weeks", "RR", path_export)


