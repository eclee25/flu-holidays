
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
    mutate(intervention = factor(intervention, levels = c("baseline", "travel", "contact", "holiday")))

  return(dummyDat3)
}

################################
plotBxp_totPRR <- function(dat, varname, varString, timingString, exportPath){
  print(match.call())
  # plot IRR, distribution across peak time in average of all metro ids

  exportFilename <- paste0(exportPath, sprintf("/%s_total_%s.png", varname, timingString))
  
  dat2 <- dat %>% rename_(interestVar = varname)
  
  exportPlot <- ggplot(dat2, aes(x = intervention, y = interestVar)) +
    geom_boxplot() + 
    ylab(varString) +
    theme_bw() +
    theme(text=element_text(size=12), axis.title.x = element_blank(), legend.position = c(1,1), legend.justification = c(1,1), legend.title = element_blank())
  ggsave(exportFilename, exportPlot, units = "in", width = w, height = h, dpi = dp)
      
  return(exportPlot)
}

################################
plotBxp_totBDA <- function(dat, varname, varString, timingString, exportPath){
  print(match.call())
  # plot incidence before/during/after (grouped), distribution across peak time in average of all metro ids
  
  exportFilename <- paste0(exportPath, sprintf("/bdaPrevalence_total_%s.png", timingString))
  dat2 <- dat %>% rename_(interestVar = varname)
  
  exportPlot <- ggplot(dat2, aes(x = intervention, y = interestVar)) +
    geom_boxplot(aes(colour = period)) + 
    ylab(varString) +
    theme_bw() +
    theme(text=element_text(size=12), axis.title.x = element_blank(), legend.position = c(1,1), legend.justification = c(1,1), legend.title = element_blank())
  ggsave(exportFilename, exportPlot, units = "in", width = w, height = h, dpi = dp)
  
  return(exportPlot)
}

################################
plotBxp_totBDA_holidays <- function(dat, varname, varString, timingString, exportPath){
  print(match.call())
  # plot incidence before/during/after (grouped), distribution across peak time in average of all metro ids, holiday interventions only
  
  exportFilename <- paste0(exportPath, sprintf("/bdaPrevalence_total_%s.png", timingString))
  dat2 <- dat %>% rename_(interestVar = varname)
  
  exportPlot <- ggplot(dat2, aes(x = timing, y = interestVar)) +
    geom_boxplot(aes(colour = period)) + 
    ylab(varString) +
    theme_bw() +
    theme(text=element_text(size=12), axis.title.x = element_blank(), legend.position = c(1,1), legend.justification = c(1,1), legend.title = element_blank())
  ggsave(exportFilename, exportPlot, units = "in", width = w, height = h, dpi = dp)
  
  return(exportPlot)
}


################################
plotBxp_totPRRSumm <- function(dat, varname, varString, exportFilename){
  print(match.call())
  # plot PRR during/before & after/before (grouped), distribution across peak time in average of all metro ids
  
  dat2 <- dat %>% rename_(interestVar = varname)
  
  exportPlot <- ggplot(dat2, aes(x = combo, y = interestVar)) +
    geom_boxplot(aes(colour = period)) + 
    geom_hline(yintercept = 1) +
    ylab(varString) +
    theme_bw() +
    theme(text=element_text(size=12), axis.title.x = element_blank(), legend.position = c(1,1), legend.justification = c(1,1), legend.title = element_blank())
  ggsave(exportFilename, exportPlot, units = "in", width = w, height = h, dpi = dp)
  
  return(exportPlot)
}

################################

#### program ################################
#### import simulation data ####
setwd('../R_export') # example with relative file paths
importDat <- read_csv("metroTot_avg_allCombos_prevalence.csv") %>%
  select(-currentInfected_mn, -pop)

#### process data for two sets of figures ####
# data for PRR by intervention, two plots per timing (during/before, after/before)
pltDat <- processDat(importDat)
actualDat <- pltDat %>% filter(timing == "actual")
threeDat <- pltDat %>% filter(timing == "plus three weeks" | intervention == "baseline")
sixDat <- pltDat %>% filter(timing == "plus six weeks" | intervention == "baseline")

# data for BDA prevalence, one plot per timing
bdaDat <- pltDat %>% 
  select(-PRR_db, -PRR_ab) %>%
  gather(period, infPer10K, before, during, after) %>%
  mutate(period = factor(period, levels = c("before", "during", "after"))) 
actualBDA <- bdaDat %>% filter(timing == "actual")
threeBDA <- bdaDat %>% filter(timing == "plus three weeks")
sixBDA <- bdaDat %>% filter(timing == "plus six weeks")
holBDA <- bdaDat %>%
  mutate(timing = factor(timing, levels = c("actual", "plus three weeks", "plus six weeks"), labels = c("actual", "+3 weeks", "+6 weeks"))) %>%
  filter(combo == "holiday_actual" | combo == "holiday_plus three weeks" | combo == "holiday_plus six weeks")

# data for baseline/holiday summary fig of PRR
summDat <- pltDat %>%
  select(-before, -during, -after) %>%
  mutate(intervention = as.character(intervention)) %>%
  filter(intervention == "baseline" | intervention == "holiday") %>%
  gather(period, PRR, PRR_db, PRR_ab) %>%
  mutate(period = factor(period, levels = c("PRR_db", "PRR_ab"), labels = c("during/before", "after/before"))) %>%
  mutate(timing = ifelse(timing == "plus six weeks", "+6", ifelse(timing == "plus three weeks", "+3", timing))) %>%
  mutate(combo = ifelse(timing == "actual", intervention, paste(intervention, timing))) %>%
  mutate(intervention = factor(intervention, levels = c("baseline", "holiday"))) %>%
  mutate(combo = factor(combo, levels = c("baseline", "holiday", "holiday +3", "holiday +6")))

# data for holiday only summary fig of PRR
summDat2 <- summDat %>%
  mutate(intervention = as.character(intervention)) %>%
  filter(intervention == "holiday")
  
#### export plots ####
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
dir.create(sprintf('../plot_outputs'), showWarnings=FALSE) 
setwd("../plot_outputs")
path_export <- getwd()

# plot for PRR by intervention
actualPlot_db <- plotBxp_totPRR(actualDat, "PRR_db", "PRR during/before", "actual", path_export)
threePlot_db <- plotBxp_totPRR(threeDat, "PRR_db", "PRR during/before", "late3wks", path_export)
sixPlot_db <- plotBxp_totPRR(sixDat, "PRR_db", "PRR during/before", "late6wks", path_export)
actualPlot_ab <- plotBxp_totPRR(actualDat, "PRR_ab", "PRR after/before", "actual", path_export)
threePlot_ab <- plotBxp_totPRR(threeDat, "PRR_ab", "PRR after/before", "late3wks", path_export)
sixPlot_ab <- plotBxp_totPRR(sixDat, "PRR_ab", "PRR after/before", "late6wks", path_export)

# plot for BDA prevalence, one plot per timing
actualPlot <- plotBxp_totBDA(actualBDA, "infPer10K", "flu prevalence per 10,000", "actual", path_export)
threePlot <- plotBxp_totBDA(threeBDA, "infPer10K", "flu prevalence per 10,000", "late3wks", path_export)
sixPlot <- plotBxp_totBDA(sixBDA, "infPer10K", "flu prevalence per 10,000", "late6wks", path_export)
holPlot <- plotBxp_totBDA_holidays(holBDA, "infPer10K", "flu prevalence per 10,000", "holidays", path_export)

# plot for PRR baseline/holiday timings - summarized
filename1 <- paste0(path_export, sprintf("/PRR_total_summary.png"))
summPlot <- plotBxp_totPRRSumm(summDat, "PRR", "prevalence ratio", filename1)

# plot for PRR holiday timing only - summarized
filename2 <- paste0(path_export, sprintf("/PRR_total_summary_holidays.png"))
summPlot2 <- plotBxp_totPRRSumm(summDat2, "PRR", "prevalence ratio", filename2)

#### write to file ####
setwd(dirname(sys.frame(1)$ofile))
setwd("../R_export")
exportDat <- pltDat %>% select(-combo)

write_csv(exportDat, "metroTot_avg_allCombos_bda_PRR.csv")
# exported 8/5/16

