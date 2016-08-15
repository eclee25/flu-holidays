
## Name: Elizabeth Lee
## Date: 5/26/16
## Function: check coverage of population data in IMS Health database - How many zip3s are missing data for every year?
## Filenames: dz_burden/SQL_export/pop_allYearly_totAge_allZip.csv
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr)

#### import data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("../../SDI_Data/dz_burden/SQL_export")
zip3Pop_sdi <- read_csv("pop_allYearly_totAge_allZip.csv", col_types = "ic?", skip = 1, col_names = c("year", "zip3", "popzip3_sdi"))

#### examine data ################################
clDat <- zip3Pop_sdi %>%
  spread(year, popzip3_sdi) %>%
  gather_("year", "pop", "2000":"2010") 
atleast1NA <- clDat %>%
  mutate(pop = ifelse(pop == 0, NA, pop)) %>%
  group_by(zip3) %>% 
  summarise(atleast1NA = ifelse(is.na(sum(pop)), TRUE, FALSE)) 
allNA <- clDat %>% 
  mutate(pop = ifelse(pop == 0, NA, pop)) %>% 
  filter(is.na(pop)) %>%
  group_by(zip3) %>% 
  summarise(countNA = length(pop)) %>%
  mutate(all0NA = ifelse(countNA == 11, TRUE, FALSE))
allNAstudy <- clDat %>%
  mutate(pop = ifelse(pop == 0, NA, pop)) %>%
  filter(is.na(pop) & year >= 2002 & year <= 2009) %>%
  group_by(zip3) %>%
  summarise(countNAstudy = length(pop)) %>%
  mutate(all0NA0208 = ifelse(countNAstudy == 8, TRUE, FALSE))
# merge data
fullNA <- full_join(atleast1NA, allNA %>% select(-countNA), by = "zip3") %>%
  full_join(allNAstudy %>% select(-countNAstudy), by = "zip3")
# summary of NA counts
summNA <- fullNA %>%
  summarise(uqzip3s = length(zip3), atleast1NA = sum(atleast1NA, na.rm = TRUE), all0NA = sum(all0NA, na.rm = TRUE), all0NA0208 = sum(all0NA0208, na.rm = TRUE))

# uqzip3s atleast1NA all0NA all0NA0208
# 935        229     49         50



