
## Name: Elizabeth Lee
## Date: 10/5/16
## Function: Boxplot: distribution of time to peak across averaged metro IDs, total population. Include red_C_ageROnly_less, red_C_ageROnly and red_C_ageROnly_more. Supplement figure.
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
processDat <- function(importDat){
  print(match.call())
  # read a single data file, adding columns for each code
  
  holidayIndicDat <- data.frame(timing = c("actual", "plus three weeks", "plus six weeks"), 
                                holidayStart = c(90 + epiStart, 111 + epiStart, 132 + epiStart),
                                stringsAsFactors = FALSE)
  
  dummyDat <- left_join(importDat, holidayIndicDat, by = c("timing"))

  # indicate before, during, after timesteps
  dummyDat2 <- dummyDat %>%
    mutate(period = ifelse((time_step >= holidayStart & time_step < holidayStart+14), "during", ifelse((time_step >= (holidayStart-21) & time_step < holidayStart+7), "before", ifelse((time_step >= (holidayStart+21) & time_step < (holidayStart+35)), "after", NA)))) %>%
    filter(!is.na(period)) %>%
    group_by(metro_id, intervention, timing, period) %>%
    summarise(infPer10K = mean(infPer10K)) %>%
    ungroup

  dummyDat3 <- dummyDat2 %>%
    spread(period, infPer10K) %>%
    mutate(PRR_db = during/before, PRR_ab = after/before) %>% 
    mutate(combo = paste(intervention, timing, sep = "_")) %>%
    mutate(intervention = factor(intervention, levels = c("-10% contact rate", "partial school closure", "+10% contact rate")))

  return(dummyDat3)
}

################################
plotBxp_totBDA_contact <- function(dat, varname, varString, timingString, exportPath){
  print(match.call())
  # plot incidence before/during/after (grouped), distribution across peak time in average of all metro ids
  
  exportFilename <- paste0(exportPath, sprintf("/bdaIncidence_contactrate_%s.png", timingString))
  dat2 <- dat %>% rename_(interestVar = varname)
  
  exportPlot <- ggplot(dat2, aes(x = intervention, y = interestVar)) +
    geom_boxplot(aes(colour = period)) + 
    ylab(varString) +
    theme_bw() +
    theme(text=element_text(size=14), axis.title.x = element_blank(), legend.position = c(1,1), legend.justification = c(1,1), legend.title = element_blank())
  ggsave(exportFilename, exportPlot, units = "in", width = w, height = h, dpi = dp)
  
  return(exportPlot)
}

################################

#### program ################################
#### import simulation data ####
setwd('../R_export') # example with relative file paths
importDat <- read_csv("metroContactrateTot_avg_allCombos_incidence.csv") %>%
  select(-newInfected, -pop)

#### process data for two sets of figures ####
# data for PRR by intervention, two plots per timing (during/before, after/before)
pltDat <- processDat(importDat)
actualDat <- pltDat %>% filter(timing == "actual")

# data for BDA prevalence, one plot per timing
actualBDA <- pltDat %>% 
  select(-PRR_db, -PRR_ab) %>%
  gather(period, infPer10K, before, during, after) %>%
  mutate(period = factor(period, levels = c("before", "during", "after"))) 


#### export plots ####
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
dir.create(sprintf('../plot_outputs'), showWarnings=FALSE) 
setwd("../plot_outputs")
path_export <- getwd()

# plot for BDA incidence, one plot per timing
actualPlot <- plotBxp_totBDA_contact(actualBDA, "infPer10K", "flu incidence per 10,000", "actual", path_export)

#### write to file ####
setwd(dirname(sys.frame(1)$ofile))
setwd("../R_export")
exportDat <- pltDat 

write_csv(exportDat, "metroContactrateTot_avg_allCombos_bda_IRR.csv")
# exported 10/5/16

