
## Name: Elizabeth Lee
## Date: 8/13/16
## Function: write ILI incidence (ili/pop*100000) data for peak timing (search between 11 & 3); season begins in the first week of October

## Filenames: 
## Data Source: 
## Notes: re-export of Anne's data with added pop data and ILI = 0 when missing from the database; adapted from dz_burden/programs/write_loess_fits_ILIn.R
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())
#### header ####################################
require(dplyr)
require(tidyr)
require(readr)
setwd(dirname(sys.frame(1)$ofile))

#### import data ####################################
setwd('../../SDI_Data/dz_burden/R_export')

ilic_df <- read_csv('ilicByallZip3_allWeekly_totServ_totAge.csv', col_types = list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), cov_z.y = col_double(), alpha_z.y = col_double(), ILIc = col_double(), cov_below5 = col_logical()))

#### data cleaning ####################################
# 10/27/15 remove zip3s with missing pop data in incl.lm indicator
# 7/18/16 incl.lm is redundant with changes in write_ILIc_data.R (incl.lm is already defined as FALSE when is.na(pop)), 100,000 pop multiplier
ilic_df2 <- ilic_df %>% 
  mutate(incl.lm = ifelse(!incl.lm, FALSE, ifelse(is.na(pop), FALSE, TRUE))) %>% 
  filter(incl.lm) %>%
  select(-ILIc, -cov_below5, -flu.week, -fit.week) %>%
  mutate(ILIn = ili/pop*10000) %>%
  mutate(season = ifelse(month >= 10, year+1-2000, year-2000)) 
# need to count weeks since first week of Oct  

# rm zip3s with ILIn for fewer than 5 weeks in a given flu season (between 10 & 3)
# all locations have at least 12 weeks of data between October & March
rmZip3Seas <- ilic_df2 %>%
  filter(month >= 10 | month <= 3) %>%
  group_by(season, zip3) %>%
  count

cleanDat <- ilic_df2 %>%
  mutate(season = ifelse(month >= 10, year+1-2000, year-2000)) %>%
  filter(season != 1 & season != 10) %>%
  filter(!is.na(ILIn)) %>%
  select(season, week, year, month, zip3, ili, pop, ILIn)

# count weeks since beginning of season
weekCounts <- cleanDat %>%
  distinct(season, week) %>%
  group_by(season) %>%
  mutate(tSeas = seq_along(week)) %>%
  ungroup
  
cleanDat2 <- cleanDat %>%
  full_join(weekCounts, by = c("season", "week"))

# grab peak weeks only
exportDat <- cleanDat2 %>%
  group_by(season, zip3) %>%
  filter(month >= 10 | month <= 3) %>%
  filter(max(ILIn) > 0) %>%
  filter(ILIn == max(ILIn)) %>%
  rename(peak_time = tSeas) %>%
  ungroup

#### export data ####################################
# write new dataframe for peak timing by season figs
setwd(dirname(sys.frame(1)$ofile))
setwd("./R_export")
write_csv(exportDat, "ILIn_peaktime.csv")
# exported 8/13/16


