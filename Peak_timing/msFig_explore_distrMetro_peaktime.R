
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

#### plotting parameters #################################
w <- 6; h <- 4; dp <- 300

#### functions ####################################

plotBxp_peaktime <- function(dat, exportPath){
  print(match.call())
  # plot peak timing, distribution across peak time across all zip3s
  
  exportFilename <- paste0(exportPath, "/peaktime_bySeason.png")

  interventionTimes <- c(13, 14) # "holiday" endures for weeks 13 & 14
  
  exportPlot <- ggplot(dat, aes(x = season, y = peak_time)) +
    geom_violin() + 
    geom_hline(yintercept = interventionTimes, colour = "black", linetype = 2) +
    scale_y_continuous("weeks to peak") +
    theme_bw() +
    theme(text=element_text(size=14), axis.title.x = element_blank(), legend.position = "bottom", legend.title = element_blank())
  ggsave(exportFilename, exportPlot, units = "in", width = w, height = h, dpi = dp)
  
  return(exportPlot)
}


#### clean data ####################################
setwd("./R_export")
importDat <- read_csv("ILIn_peaktime.csv", col_types = "iDiiciidi")

pltDat <- importDat %>%
  mutate(season = factor(as.character(season), levels = c("2", "3", "4", "5", "6", "7", "8", "9"), labels = c("2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09")))


#### export plot ####################################

setwd(dirname(sys.frame(1)$ofile))
setwd("./graph_outputs")
pathname <- getwd()

bdaPlt <- plotBxp_peaktime(pltDat, pathname)
# exported 8/25/16

