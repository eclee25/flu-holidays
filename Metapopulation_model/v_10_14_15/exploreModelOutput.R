
## Name: Elizabeth Lee
## Date: 
## Function: 
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
# .Rprofile sets wd 'Dropbox (Bansal Lab)/code' and sources "GeneralTools.R" in .myenv
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
require(readr)
require(tidyr)
require(dplyr)
require(ggplot2)

set.seed(10)
#### plot formatting ################################
w <- 12; h <- 12

#### import data ################################
mdata <- read_csv('chain_binomial_output_nummetrozeros_1_numchildzeros_1_numadultzeros_0_numsims_100.csv')
epi.simnums <- mdata %>% group_by(sim) %>% filter(time_step == 150) %>% summarise(episize = sum(total_infected)) %>% filter(episize > 1000)
metid.samp <- mdata %>% select(metro_id) %>% unlist %>% unique %>% sample(56, replace=F) %>% sort # 50 to plot

mdata.plt <- mdata %>% filter(sim %in% epi.simnums$sim) %>% filter(metro_id %in% metid.samp)

setwd('./graph_outputs')
for (i in epi.simnums$sim){
  dummyplot <- ggplot(mdata.plt %>% filter(sim == i), aes(x = time_step, y = currently_infected, group = age)) +
    geom_line(aes(colour = age), stat = "identity") + 
    coord_cartesian(xlim = c(0, 30)) +
    theme_minimal(base_size = 16, base_family = "") +
    facet_wrap(~metro_id)
  ggsave(dummyplot, filename = sprintf('prevalence_sim%s.png', i), width = w, height = h)
}