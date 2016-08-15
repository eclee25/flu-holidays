
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
plotScatter_prr_before <- function(dat, beforePrevString, exportFilename){
  print(match.call())
  # scatterplot of metroIds PRR vs before prevalence (4 lines: holiday/baseline, db/ab)
  
  dat2 <- dat %>% mutate_(beforePrev_choice = beforePrevString)
  scatterPlot <- ggplot(dat2, aes(x = beforePrev_actual, y = baselinePRR)) +
    geom_segment(aes(xend = beforePrev_choice, yend = holidayPRR), 
                 colour = "grey", alpha = 0.25, arrow = arrow(length = unit(0.05, "inches"))) +
    geom_point() +
    geom_hline(yintercept = 1) +
    scale_x_continuous("Infections per 10,000 (before holiday)") +
    scale_y_continuous("Prevalence Ratio") +
    theme_bw() +
    theme(text=element_text(size=12), legend.position = "bottom", legend.title = element_blank()) +
    facet_wrap(~variable)
  ggsave(exportFilename, scatterPlot, units = "in", width = w, height = h, dpi = dp)
  
  return(scatterPlot)
}

################################
plotScatter_prr_before_log <- function(dat, beforePrevString, exportFilename){
  print(match.call())
  # scatterplot of metroIds PRR vs before prevalence (4 lines: holiday/baseline, db/ab)
  
  dat2 <- dat %>% mutate_(beforePrev_choice = beforePrevString)
  scatterPlot <- ggplot(dat2, aes(x = beforePrev_actual, y = baselinePRR)) +
    geom_segment(aes(xend = beforePrev_choice, yend = holidayPRR), 
                 colour = "grey", alpha = 0.25, arrow = arrow(length = unit(0.05, "inches"))) +
    geom_point() +
    geom_hline(yintercept = 1) +
    scale_x_log10("Infections per 10,000 (before holiday)") +
    scale_y_log10("Prevalence Ratio") +
    theme_bw() +
    theme(text=element_text(size=12), legend.position = "bottom", legend.title = element_blank()) +
    facet_wrap(~variable)
  ggsave(exportFilename, scatterPlot, units = "in", width = w, height = h, dpi = dp)
  
  return(scatterPlot)
}

################################

#### program ################################
#### import pPRR data ####
setwd('../R_export') # example with relative file paths
importDat <- read_csv("metroTot_avg_allCombos_bda_PRR.csv") 

#### process data for plotting ####
pltDat <- importDat %>%
  filter(intervention == "baseline" | intervention == "holiday") %>%
  select(-during, -after) %>%
  gather(variable, value, before, PRR_db, PRR_ab) 
beforeDat <- pltDat %>% 
  filter(variable == "before") %>%
  spread(intervention, value) %>%
  select(-variable, -baseline) %>%
  spread(timing, holiday) 
names(beforeDat) <- c("metro_id", "beforePrev_actual", "beforePrev_late6", "beforePrev_late3")
prrDat <- pltDat %>%
  filter(variable != "before") %>%
  spread(intervention, value) %>%
  rename(baselinePRR = baseline, holidayPRR = holiday) %>%
  mutate(variable = ifelse(variable == "PRR_ab", "ab", "db"))

# repeat baseline PRR data for the same metro_id across different holiday timings 
blPRRDat <- prrDat %>%
  filter(!is.na(baselinePRR)) 
fillin <- prrDat %>% 
  filter(is.na(baselinePRR)) %>%
  select(-baselinePRR) %>%
  left_join(blPRRDat %>% select(-timing, -holidayPRR), by = c("metro_id", "variable")) %>%
  select(metro_id, timing, variable, baselinePRR, holidayPRR)

# create full baseline and holiday PRR data
fullPRRDat <- bind_rows(blPRRDat, fillin)

mergDat <- full_join(beforeDat, fullPRRDat, by = c("metro_id")) %>%
  mutate(variable = factor(variable, levels = c("db", "ab"), labels = c("during/before", "after/before"))) %>%
  mutate(beforePrev_late6 = ifelse(timing == "plus six weeks", beforePrev_late6, NA)) %>%
  mutate(beforePrev_late3 = ifelse(timing == "plus three weeks", beforePrev_late3, NA))

actualDat <- mergDat %>% filter(timing == "actual")
threeDat <- mergDat %>% filter(timing == "plus three weeks")
sixDat <- mergDat %>% filter(timing == "plus six weeks")

#### export plots ####
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
dir.create(sprintf('../plot_outputs'), showWarnings=FALSE) 
setwd("../plot_outputs")
path_export <- getwd()

# scatterplot prevalence ratio vs infections per 10K before holiday (actual/+3/+6)
filenameActual <- paste0(path_export, sprintf("/scatterMetro_PRR_before_actual.png"))
filenameThree <- paste0(path_export, sprintf("/scatterMetro_PRR_before_late3wks.png"))
filenameSix <- paste0(path_export, sprintf("/scatterMetro_PRR_before_late6wks.png"))

actualPlot <- plotScatter_prr_before(actualDat, "beforePrev_actual", filenameActual)
threePlot <- plotScatter_prr_before(threeDat, "beforePrev_late3", filenameThree)
sixPlot <- plotScatter_prr_before(sixDat, "beforePrev_late6", filenameSix)

# scatterplot prevalence ratio vs infections per 10K before holiday (actual/+3/+6)
filenameActual_log <- paste0(path_export, sprintf("/scatterMetro_PRR_before_actual_log.png"))
filenameThree_log <- paste0(path_export, sprintf("/scatterMetro_PRR_before_late3wks_log.png"))
filenameSix_log <- paste0(path_export, sprintf("/scatterMetro_PRR_before_late6wks_log.png"))

actualPlot_log <- plotScatter_prr_before_log(actualDat, "beforePrev_actual", filenameActual_log)
threePlot_log <- plotScatter_prr_before_log(threeDat, "beforePrev_late3", filenameThree_log)
sixPlot_log <- plotScatter_prr_before_log(sixDat, "beforePrev_late6", filenameSix_log)



