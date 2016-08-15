
## Name: Elizabeth Lee
## Date: 7/26/16
## Function: explore Moran's I output for ILI incidence (ili/pop*100000) data for the before, during, and after holiday periods (-2weeks, 0week, +2 weeks in reference to the week including Christmas)

## Filenames: moranExport_bda.csv, created in write_bda_moransI.R
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
dat <- read_csv("moranExport_bda.csv") %>%
  mutate(period = factor(period, levels = c("b", "d", "a"), labels = c("before", "during", "after"))) %>%
  filter(altHypothesis == "greater")

plt <- ggplot(dat, aes(x = period, y = moranEstimate, group = season)) +
  geom_point(aes(colour = as.factor(season))) + 
  geom_line(aes(colour = as.factor(season))) +
  scale_y_continuous(name = "Moran's I statistic", limits = c(0,1)) +
  scale_x_discrete(name = "Period", labels = c("before", "during", "after")) +
  guides(colour = guide_legend("Season"))

print(plt)
setwd('../graph_outputs')
ggsave("explore_moransI.png", width=4, height=4, dpi=200)
