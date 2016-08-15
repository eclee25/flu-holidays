
## Name: Elizabeth Lee
## Date: 7/27/16
## Function: explore Moran's I output for ILI incidence (ili/pop*100000) data for the entire flu season in reference to xmas week 0.

## Filenames: moranExport_full.csv, created in write_full_moransI.R
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
setwd(dirname(sys.frame(1)$ofile))

#### import data ####################################
setwd('./export_data')
dat <- read_csv("moranExport_full.csv") %>%
  mutate(week = factor(week)) %>%
  filter(altHypothesis == "greater")

plt <- ggplot(dat, aes(x = relativeWeeknum, y = moranEstimate, group = season)) +
  geom_point(aes(colour = as.factor(season))) + 
  geom_line(aes(colour = as.factor(season))) +
  geom_vline(xintercept = c(-8, 0, 17), colour = "grey") +
  scale_y_continuous(name = "Moran's I statistic", limits = c(0,1)) +
  scale_x_continuous(name = "Week number relative to Christmas", breaks = seq(-8,32,by=4)) +
  guides(colour = guide_legend("Season")) 

print(plt)
setwd('../graph_outputs')
ggsave("explore_full_moransI.png", width=6, height=4, dpi=200)
