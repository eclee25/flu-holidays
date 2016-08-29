
## Name: Elizabeth Lee
## Date: 8/13/16
## Function: explore BDA output for ILI incidence (ili/pop*100000) data for the 2 week before, during, and after holiday periods (starting -3, 0 and +3 weeks relative to Xmas)

## Filenames: import_data/ILIn_bdaIndicator_incidence.csv, created in write_bda_incidence.R
## Data Source: 
## Notes:
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())
#### header ####################################
require(dplyr)
require(tidyr)
require(readr)
require(ggplot2)
require(RColorBrewer)
setwd(dirname(sys.frame(1)$ofile))

#### set these! #################################
code <- "_shifted" # ""

#### plotting parameters #################################
w <- 6; h <- 4; dp <- 300

#### functions ####################################

plotBxp_bdaIncidence <- function(dat, varname, varString, defString, exportPath, maxlim){
  print(match.call())
  # plot incidence before/during/after (grouped), distribution across peak time in average of all zip3s
  
  exportFilename <- paste0(exportPath, sprintf("/bdaIncidence_bySeason_%s%s.png", defString, code))
  dat2 <- dat %>% rename_(interestVar = varname)
  
  exportPlot <- ggplot(dat2, aes(x = season, y = interestVar)) +
    geom_boxplot(aes(colour = period)) + 
    scale_y_continuous(varString, limits = c(0,maxlim)) + 
    theme_bw() +
    theme(text=element_text(size=14), axis.title.x = element_blank(), legend.position = c(1,1), legend.justification = c(1,1), legend.title = element_blank())
  ggsave(exportFilename, exportPlot, units = "in", width = w, height = h, dpi = dp)
  
  return(exportPlot)
}

#### clean new bda definition data ####################################
setwd("./import_data")
importDat <- read_csv(sprintf("ILIn_bdaIndicator_incidence%s.csv", code), col_types = "Diciiddc")

# bda - average of 2 week periods starting -3, 0, +3 weeks relative to Xmas
newDef <- importDat %>%
  filter(season != 1 & season != 10) %>%
  group_by(season, zip3, bdaIndicator) %>%
  summarise(ILI = mean(ILIn, na.rm = TRUE), IR = mean(IR, na.rm = TRUE)) %>%
  ungroup %>%
  rename(period = bdaIndicator) %>%
  mutate(period = factor(period, levels = c("b", "d", "a"), labels = c("before", "during", "after"))) %>%
  mutate(season = factor(as.character(season), levels = c("2", "3", "4", "5", "6", "7", "8", "9"), labels = c("2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09")))

# 152-156 ILI NAs per season; 250-530 IR NAs per season
newDef %>% group_by(season) %>% summarise(nas = sum(is.na(ILI)), nas_IR = sum(is.na(IR)))

newDef2 <- newDef %>%
  filter(!is.na(ILI))

# # 8/29/16 ANOVA for bda comparisons
# aovDat <- newDef2 %>%
#   select(-ILI) %>%
#   spread(period, IR)
# seaslist <- aovDat %>% distinct(season) %>% unlist
# for(i in 1:length(seaslist)){
#   dummyDat <- aovDat %>% filter(season == seaslist[i])
#   mod <- lm(during ~ before + after, data = dummyDat)
#   print(paste("Season", seaslist[i]))
#   print(summary.lm(mod))
# }


if (code == ""){
  #### clean old bda definition data ####################################
  # bda - one week period that is -2, 0, +2 weeks relative to Xmas
  importDat2 <- read_csv("ILIn_bdaIndicator_moransI.csv", col_types = "Diciiddc")
  
  oldDef <- importDat2 %>%
    filter(season != 1 & season != 10) %>%
    rename(period = bdaIndicator) %>%
    mutate(period = factor(period, levels = c("b", "d", "a"), labels = c("before", "during", "after"))) %>%
    mutate(season = factor(as.character(season), levels = c("2", "3", "4", "5", "6", "7", "8", "9"), labels = c("2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09")))
  
  # 152-156 ILIn NAs per season; 450 to 850 IR NAs per season
  oldDef %>% group_by(season) %>% summarise(nas = sum(is.na(ILIn), nas_IR = sum(is.na(IR))))
  
  oldDef2 <- oldDef %>%
    filter(!is.na(ILIn))
}



#### program ####################################

setwd(dirname(sys.frame(1)$ofile))
setwd("./graph_outputs")
pathname <- getwd()

bdaPlt <- plotBxp_bdaIncidence(newDef2, "ILI", "ILI Reports per 10,000", "ILIn_2wkAvg_-30+3", pathname, 50)
bdaPlt3 <- plotBxp_bdaIncidence(newDef2, "IR", "ILI Incidence Ratio", "IR_2wkAvg_-30+3", pathname, 0.5)
if (code == ""){
  bdaPlt2 <- plotBxp_bdaIncidence(oldDef2, "ILIn", "ILI Reports per 10,000", "ILIn_1wk_-20+2", pathname, 50)
  bdaPlt4 <- plotBxp_bdaIncidence(oldDef2, "IR", "ILI Incidence Ratio", "IR_1wk_-20+2", pathname, 0.5)
}

# exported 8/29/16

