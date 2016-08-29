
## Name: Elizabeth Lee
## Date: 8/26/16
## Function: plot incidence ratio at the zip3-level during the holiday period, where the holidays are week 8 and the weeks of interest are weeks 1 to 13, one plot per season

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
require(ggplot2)
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
  mutate(season = ifelse(month >= 10, year+1-2000, year-2000)) %>%
  filter(season >=2 & season <=9) %>% 
  mutate(ILIn = ili/pop*10000, IR = (ili/visits)*(pop/100000))

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

# merge data with relative week nummbers
fullDat <- left_join(ilic_df2, interestWeeks2, by = "week") %>%
  mutate(holidayWeeknum = relativeWeeknum + 8) %>%
  filter(holidayWeeknum >= 1 & holidayWeeknum <= 13) %>%
  filter(!is.na(IR))


#### plot formatting ####################################
w <- 6; h <- 6; dp <- 300
setwd(dirname(sys.frame(1)$ofile))

pathname <- paste0(getwd(), "/graph_outputs/holidayZip3_IR.png")

#### plot data ####################################
plot_zip3IR <- function(plotData, exportPath){
  print(match.call())
  
  plotData2 <- plotData %>% 
    mutate(season = paste("Season", season))
  brks <- seq(1, 13, by = 3)
  
  exportPlot <- ggplot(plotData2, aes(x = holidayWeeknum, y = IR, group = zip3)) +
    scale_x_continuous("Time (weeks)", breaks = brks, labels = as.character(brks)) +
    scale_y_continuous("ILI Incidence Ratio") +
    geom_line(colour = "grey") +
    geom_vline(xintercept = 8, colour = "black", linetype = 2) +
    theme_bw() +
    theme(text=element_text(size=14), legend.position = "none") +
    facet_wrap(~season, scales = "free_y")
  
  ggsave(exportPath, exportPlot, units = "in", width = w, height = h, dpi = dp)
}


plot_zip3IR(fullDat, pathname)
# export 8/26/16

