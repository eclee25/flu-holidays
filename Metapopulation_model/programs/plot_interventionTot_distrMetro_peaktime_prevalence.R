
## Name: Elizabeth Lee
## Date: 8/4/16
## Function: Boxplot: distribution of time to peak across averaged metro IDs, total population
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
  
  # spread to calculate total pop
  dummyDat2 <- dummyDat %>%
    spread(age, currentInfected_mn) %>%
    mutate(currentInfected_mn = A+C) %>%
    select(-A,-C) 
  
  # filter out metros that were infected only when they were the seed (these are very short)
  rmMetros <- dummyDat2 %>% 
    filter(time_step==0 & currentInfected_mn==1) %>%
    distinct(metro_id) %>%
    unlist
  dummyDat3 <- dummyDat2 %>%
    filter(!(metro_id %in% rmMetros))
  
  return(dummyDat3)
}
################################
plotBxp_tot <- function(dat, holidayTiming, exportPath){
  print(match.call())
  # plot national peak timing, distribution across peak time in average of all metro ids
  
  if (holidayTiming == 'actual'){
    holidayStart <- 90
    exportFilename <- paste0(exportPath, "/peaktime_total_actual.png")
  } else if (holidayTiming == 'plus three weeks'){
    holidayStart <- 111
    exportFilename <- paste0(exportPath, "/peaktime_total_late3wks.png")
  } else if (holidayTiming == 'plus six weeks'){
    holidayStart <- 132
    exportFilename <- paste0(exportPath, "/peaktime_total_late6wks.png")
  }
  interventionTimes <- c(holidayStart-7, holidayStart+7)

  dat2 <- dat %>%
    mutate(peak_time = peak_time-epiStart)
  
  exportPlot <- ggplot(dat2, aes(x = intervention, y = peak_time)) +
    geom_violin() + 
    geom_hline(yintercept = interventionTimes, colour = "black") +
    ylab("time steps to peak") +
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

#### total pop data by metro ID ####
mergDat <- left_join(fullDat, popDat, by = c("metro_id")) %>%
  mutate(infPer10K = currentInfected_mn/pop*10000) 

#### create plot data ####
pltDat <- mergDat %>% 
  mutate(combo = paste(intervention, timing, sep = "_")) %>% 
  mutate(intervention = factor(intervention, levels = c("baseline", "travel", "contact", "holiday"))) %>%
  group_by(combo, metro_id) %>%
  filter(infPer10K == max(infPer10K)) %>%
  rename(peak_time = time_step)

#### export plots ####
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
dir.create(sprintf('../plot_outputs'), showWarnings=FALSE) 
setwd("../plot_outputs")
path_export <- getwd()

actualDat <- pltDat %>% filter(timing == "actual")
threeDat <- pltDat %>% filter(timing == "plus three weeks" | intervention == "baseline")
sixDat <- pltDat %>% filter(timing == "plus six weeks" | intervention == "baseline")

actualPlot <- plotBxp_tot(actualDat, "actual", path_export)
threePlot <- plotBxp_tot(threeDat, "plus three weeks", path_export)
sixPlot <- plotBxp_tot(sixDat, "plus six weeks", path_export)


#### write to file ####
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
setwd("../R_export")
write_csv(mergDat, "metroTot_avg_allCombos_prevalence.csv")
# 8/4/16
