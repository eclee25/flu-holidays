
## Name: Elizabeth Lee
## Date: 7/27/16
## Function: write ILI incidence (ili/pop*100000) data for entire flu period

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
  select(-ILIc, -cov_below5, -flu.week, -fit.week) %>%
  mutate(ILIn = ili/pop*100000) %>%
  mutate(season = ifelse(month >= 10, year+1-2000, year-2000)) %>%
  filter(season >=2 & season <=9) %>%
  filter(month >= 11 | month <= 8)

# add indicators for before, during, and after weeks relative to Christmas
interestWeeks <- ilic_df2 %>%
  distinct(week, year, month, season) %>%
  mutate(relativeWeeknum = ifelse(as.Date(paste0(year, "-12-25")) >= week & as.Date(paste0(year, "-12-25")) < week+7, 0, NA))

# grab index of xmas week and number of weeks in season
xmasIndexes <- interestWeeks %>%
  group_by(season) %>%
  summarise(xmasIndex = which.max(relativeWeeknum), numWeeks = length(relativeWeeknum))

# use xmasIndexes information to create new variable of "week number relative to xmas", where xmasweek = 0
convertIndexInfo <- function(indexInfo){
  xmasIndex <- indexInfo[2]
  numWeeks <- indexInfo[3]
  return(((xmasIndex-1)*(-1)):(numWeeks-xmasIndex))
}
relativeWeeks <- unlist(apply(xmasIndexes, 1, convertIndexInfo))

interestWeeks2 <- interestWeeks %>%
  mutate(relativeWeeknum = relativeWeeks) %>%
  select(week, relativeWeeknum)

exportDat <- left_join(ilic_df2, interestWeeks2, by = "week")  %>%
  filter(!is.na(ILIn)) %>%
  select(week, season, zip3, ili, pop, ILIn, relativeWeeknum)
  

# write new dataframe for Moran's I  
setwd(dirname(sys.frame(1)$ofile))
setwd("./import_data")
write_csv(exportDat, "ILIn_full_relativeWeeknum_moransI.csv")
# exported 7/27/16



