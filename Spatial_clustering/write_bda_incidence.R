
## Name: Elizabeth Lee
## Date: 7/26/16
## Function: write ILI incidence (ili/pop*100000) data for the before, during, and after holiday periods (-2weeks, 0week, +2 weeks in reference to the week including Christmas)
## 8/12/16: before, during, and after periods redefined as the average across 2 weeks of data, starting 3 weeks before, week of, and 3 weeks after the week of Christmas

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

setwd("../SQL_export")
viz_df <- read_csv('ILIViz_allWeekly_totServ_totAge_allZip.csv', col_types = "Dc_i") %>%
  rename(visits = ANY_DIAG_VISIT_CT, zip3 = patient_zip3, week = WEEK)

full_df <- full_join(ilic_df, viz_df, by = c("week", "zip3"))

#### data cleaning ####################################
# 10/27/15 remove zip3s with missing pop data in incl.lm indicator
# 7/18/16 incl.lm is redundant with changes in write_ILIc_data.R (incl.lm is already defined as FALSE when is.na(pop)), 100,000 pop multiplier
ilic_df2 <- full_df %>% 
  mutate(incl.lm = ifelse(!incl.lm, FALSE, ifelse(is.na(pop), FALSE, TRUE))) %>% 
  select(-ILIc, -cov_below5, -flu.week, -fit.week) %>%
  mutate(ILIn = ili/pop*10000, IR = (ili/visits)*(pop/100000))

# add indicators for before, during, and after weeks relative to Christmas (-3, 0, +3 --> 2 week average)
interestWeeks_newDef <- ilic_df2 %>%
  distinct(week, year, month) %>%
  filter(month >= 11 | month <= 2) %>%
  mutate(bdaIndicator = ifelse(as.Date(paste0(year, "-12-25")) >= week & as.Date(paste0(year, "-12-25")) < week+7, "d", NA)) %>%
  mutate(bdaIndicator = ifelse(as.Date(paste0(year, "-12-25")) >= week+21 & as.Date(paste0(year, "-12-25")) < week+21+7, "b", bdaIndicator)) %>%
  mutate(bdaIndicator = ifelse(as.Date(paste0(year-1, "-12-25")) >= week-21 & as.Date(paste0(year-1, "-12-25")) < week-21+7, "a", bdaIndicator)) %>%
  mutate(week2 = week+7) %>%
  filter(!is.na(bdaIndicator)) %>%
  select(bdaIndicator, week, week2) %>%
  gather(period, wk, week, week2) %>%
  select(wk, bdaIndicator) %>%
  rename(week = wk) %>%
  arrange(week)

# add indicators for before, during, and after weeks relative to Christmas (-2, 0, +2 --> 1 week value)
interestWeeks_oldDef <- ilic_df2 %>%
  distinct(week, year, month) %>%
  filter(month == 12 | month == 1) %>%
  mutate(bdaIndicator = ifelse(as.Date(paste0(year, "-12-25")) >= week & as.Date(paste0(year, "-12-25")) < week+7, "d", NA)) %>%
  mutate(bdaIndicator = ifelse(as.Date(paste0(year, "-12-25")) >= week+14 & as.Date(paste0(year, "-12-25")) < week+14+7, "b", bdaIndicator)) %>%
  mutate(bdaIndicator = ifelse(as.Date(paste0(year-1, "-12-25")) >= week-14 & as.Date(paste0(year-1, "-12-25")) < week-14+7, "a", bdaIndicator)) %>%
  select(week, bdaIndicator) %>%
  arrange(week)


exportDat <- left_join(ilic_df2, interestWeeks_newDef, by = "week") %>%
  mutate(season = ifelse(month >= 10, year+1-2000, year-2000)) %>%
  filter(!is.na(bdaIndicator)) %>%
  select(week, season, zip3, ili, pop, ILIn, IR, bdaIndicator)
  
exportDat2 <- left_join(ilic_df2, interestWeeks_oldDef, by = "week") %>%
  mutate(season = ifelse(month >= 10, year+1-2000, year-2000)) %>%
  filter(!is.na(bdaIndicator)) %>%
  select(week, season, zip3, ili, pop, ILIn, IR, bdaIndicator)

#### export data ####################################
# write new dataframe for bda spatial synchrony figs
setwd(dirname(sys.frame(1)$ofile))
setwd("./import_data")
write_csv(exportDat, "ILIn_bdaIndicator_incidence.csv")
write_csv(exportDat2, "ILIn_bdaIndicator_moransI.csv")
# exported 8/25/16



