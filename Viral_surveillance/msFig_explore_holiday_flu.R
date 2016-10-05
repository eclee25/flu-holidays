
## Name: Elizabeth Lee
## Date: 9/30/16
## Function: Demonstrate that ILI activity before and during Christmas is flu and the ILI activity before and during Thanksgiving is not flu.
## Filenames: 
## Data Source: 
## Notes: CDC_Source/Import_Data --> copied to import_data
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(ggplot2); require(readr)
require(ggthemes)

setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

#### plotting parameters #################################
w <- 4; h <- 4; dp <- 300

#### plotting functions #################################

plot_holiday_fluPositive <- function(plotDat, exportPath){
  print(match.call())
  
  exportFilename <- paste0(exportPath, "/fluPositive_bySeason.png")
  xmasTime <- 8
  thxTime <- 4
  
  exportPlot <- ggplot(plotDat, aes(x = plotnum, y = perc_pos)) +
    geom_line(aes(colour = season)) +
    geom_point(aes(colour = season)) +
    geom_vline(xintercept = thxTime, linetype = 2, colour = "grey") +
    geom_vline(xintercept = xmasTime, linetype = 2, colour = "black") +
    theme_bw() +
    theme(text=element_text(size=14), legend.position = "bottom", legend.title = element_blank(), legend.text=element_text(size=11), panel.grid = element_blank()) +
    scale_x_continuous("Time (Weeks)", breaks = 1:14) +
    scale_y_continuous("Positive Flu Confirmations (%)") +
    scale_colour_manual(values = c("#FF69B4", "#FF0000", "#FFA500", "#FFD700", "#008000", "#0000FF", "#00FFFF", "#9400D3"))
  ggsave(exportFilename, exportPlot, units = "in", width = w, height = h, dpi = dp)
  
  return(exportPlot)
  
}
#################################

plot_holiday_allNREVSSSamples <- function(plotDat, exportPath){
  print(match.call())
  
  exportFilename <- paste0(exportPath, "/allNREVSSSamples_bySeason.png")
  xmasTime <- 8
  thxTime <- 4
  
  exportPlot <- ggplot(plotDat, aes(x = plotnum, y = num_samples/1000)) +
    geom_line(aes(colour = season)) +
    geom_point(aes(colour = season)) +
    geom_vline(xintercept = thxTime, linetype = 2, colour = "grey") +
    geom_vline(xintercept = xmasTime, linetype = 2, colour = "black") +
    theme_bw() +
    theme(text=element_text(size=14), legend.position = "bottom", legend.title = element_blank(), legend.text=element_text(size=10), panel.grid = element_blank()) +
    scale_x_continuous("Time (Weeks)", breaks = 1:14) +
    scale_y_continuous("Samples Tested (thousands)") +
    scale_colour_manual(values = c("#FF69B4", "#FF0000", "#FFA500", "#FFD700", "#008000", "#0000FF", "#00FFFF", "#9400D3"))
  ggsave(exportFilename, exportPlot, units = "in", width = w, height = h, dpi = dp)
  
  return(exportPlot)
  
}

#### import & clean data ################################
setwd("./import_data")

# import data
vdat <- read_csv("all_cdc_source_data.csv", col_types = c(uqid = col_character(), yr = col_integer(), wk = col_integer(), num_samples = col_integer(), perc_pos = col_double())) 
# add season
cdat <- vdat %>%
  select(uqid, yr, wk, num_samples, perc_pos) %>%
  filter(wk >= 44 | wk <= 6) %>%
  mutate(season = ifelse(wk <= 6, yr-2000, yr-2000+1)) 
# shift wk numbers back if there are 53 weeks in the first year
cdat2 <- cdat %>%
  group_by(season) %>%
  mutate(has53 = ifelse(max(wk)==53, TRUE, FALSE)) %>%
  ungroup %>%
  mutate(wknum = ifelse(has53 & wk >= 44, (wk-1), wk)) %>%
  mutate(wknum = ifelse(wknum == 0, 52, wknum)) %>%
  filter(wknum >= 44 | wknum <= 5) %>%
  group_by(season) %>%
  mutate(plotnum = seq_along(wknum)) %>%
  ungroup %>%
  filter(season >=2 & season <= 9) %>%
  mutate(season = factor(as.character(season), levels = c("2", "3", "4", "5", "6", "7", "8", "9"), labels = c("2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09")))
  
  
#### plot data ################################

setwd(dirname(sys.frame(1)$ofile))
setwd("./graph_outputs")
pathname <- getwd()
  
fluPlot <- plot_holiday_fluPositive(cdat2, pathname)
sampPlote <- plot_holiday_allNREVSSSamples(cdat2, pathname)
