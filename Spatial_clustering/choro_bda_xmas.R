
## Name: Elizabeth Lee
## Date: 5/16/16
## Function: Zip3 choropleths of before, during, and after xmas week, adjusted incidence by season (medical claims data)
## Filenames: VDSTech_zip3_cl/zip3_cl
## Data Source: 
## Notes: code adapted from Manuscripts/Spatial_Big_Data_Opinion/plotting_code/zip3_choro_2008.R
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(readr)
require(ggplot2)
require(dplyr)
require(tidyr)
require(RColorBrewer)
require(maptools)
require(scales)
require(broom)
require(rgeos)

#### plot formatting ################################
# colVec <- brewer.pal(length(tierVec), 'Greens')
h <- 5; w <- 8; dp <- 200
mar <- rep(1.5, 4)
par(mar = mar, oma = mar)

#### import data ################################
# shapefile
setwd(dirname(sys.frame(1)$ofile))
setwd("./VDSTech_zip3_cl")
zip.shp <- readShapeSpatial("zip3_cl", verbose = TRUE, repair = TRUE)

# flu data
# zip IR data
setwd(dirname(sys.frame(1)$ofile))
setwd('./import_data')
b_df <- read_csv('incidence_data_for_morans_i_2_weeks_before.csv', col_types = "icd") %>%
  rename(before = incid_2wks_before) %>%
  mutate(zip3 = substr.Right(paste0("0", zip3), 3)) %>%
  gather(timeperiod, value, before)
d_df <- read_csv('incidence_data_for_morans_i_xmas.csv', col_types = "icd") %>%
  rename(during = incid_xmas) %>%
  mutate(zip3 = substr.Right(paste0("0", zip3), 3)) %>%
  gather(timeperiod, value, during)
a_df <- read_csv('incidence_data_for_morans_i_2_weeks_after.csv', col_types = "icd") %>%
  rename(after = incid_2wks_after) %>%
  mutate(zip3 = substr.Right(paste0("0", zip3), 3)) %>%
  gather(timeperiod, value, after)

# merge data for all three periods
fullDat <- bind_rows(b_df, d_df, a_df) %>%
  rename(id = zip3)

numZips <- fullDat %>% group_by(season, timeperiod) %>% count(timeperiod)

#### save figures ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("./graph_outputs")

#### loop info ####
seasons <- 2:9
periods <- c("before", "during", "after")

#### loop through seasons and periods ####
for (s in seasons){
  for (period in periods){
    
    #### clean data ####
    sDat <- fullDat %>%
      filter(season == s & timeperiod == period) 
    print(dim(sDat))
    tidy.shp <- tidy(zip.shp, region = "ZIP")
    dummyDat <- merge(tidy.shp, sDat, by = "id", all.x = TRUE)
    dummyDat2 <- dummyDat[order(dummyDat$order),]
    
    #### zip3 choropleths ####
    zipPlt <- ggplot() +
      geom_polygon(data = dummyDat2, aes(x = long, y = lat, group = group, fill = value), color = "black", size = 0.25) +
      coord_map() +
      theme_minimal()
    # print(zipPlt)
    
    #### save figure ####
    ggsave(sprintf("incid_choro_%sXmas_S%s.png", period, s), zipPlt, width = w, height = h, dpi = dp)
  }
}

bTest <- fullDat %>%
  filter(season == 2 & timeperiod == "before") 
dTest <- fullDat %>%
  filter(season == 2 & timeperiod == "during")
aTest <- fullDat %>%
  filter(season == 2 & timeperiod == "after")

hist(bTest$value, xlim = c(0, 100))
hist(dTest$value, xlim = c(0, 100))
hist(aTest$value, xlim = c(0, 100))

