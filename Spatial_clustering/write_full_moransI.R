
## Name: Elizabeth Lee
## Date: 7/27/16
## Function: calculate Moran's I for ILI incidence (ili/pop*100000) data for all weeks in flu season, in reference to xmas week 0

## Filenames: 
## Data Source: write_full_incidence.R output
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
require(spdep); require(maptools)
setwd(dirname(sys.frame(1)$ofile))

#### import data ####################################
setwd('./VDSTech_zip3continental_cl')
shp <- readShapeSpatial("zip3continental_cl")
spIds <- as.numeric(as.character(shp@data$SP_ID))
Ids <- shp@data # crosswalk between SP_ID & zip3, multiple SP_IDs may belong to the same zip3
Ids$SP_ID <- as.numeric(as.character(Ids$SP_ID))
Ids$ZIP <- as.character(Ids$ZIP)
nblist <- poly2nb(shp, queen=FALSE, snap=TRUE, row.names=spIds)
listw <- nb2listw(nblist, style = "B")

setwd(dirname(sys.frame(1)$ofile))
setwd("./import_data")
wts <- read_csv('ILIn_full_relativeWeeknum_moransI.csv', col_types = "ciciidi") 

#### set these! ####################################
seasons <- 2:9
alternatives <- c("greater") # "two.sided"

#### moran's I ####################################
# null: the attribute being analyzed is randomly distributed 
# alternative: reject the null that the spatial distribution of the feature is the result of random spatial processes --> if alternative is "greater": the spatial distribution is more spatially clustered than would be expected if the underlying process was random
set.spChkOption(TRUE)

uqweeks <- unique(wts$week)

moranDat <- data.frame()
for (wk in uqweeks){
  for(alt in alternatives){
    dummyDat <- wts %>% 
      filter(week == wk)
    fullDat <- full_join(Ids, dummyDat, by = c("ZIP" = "zip3")) %>%
      filter(!is.na(SP_ID)) 
    rownames(fullDat) <- fullDat$SP_ID # order of fullDat indicated with SP_ID
    # chkIDs(fullDat, listw)
    
    set.spChkOption(FALSE)
    naCt <- fullDat %>% filter(is.na(ILIn)) %>% count %>% unlist
    
    result <- moran.test(fullDat$ILIn, listw, alternative = alt, adjust.n = TRUE, na.action = na.omit, zero.policy = TRUE)
    rowDat <- list(season=fullDat$season[1], week=wk, relativeWeeknum=fullDat$relativeWeeknum[1], altHypothesis=alt, moranEstimate=result$estimate[1], moranExpectation=result$estimate[2], moranExpVariance=result$estimate[3], moranStdDeviate=result$statistic, pvalue=result$p.value, NAcount=naCt)
    moranDat <- bind_rows(moranDat, rowDat)
    
    print(paste(wk, alt, result$data.name))
  }
}


setwd(dirname(sys.frame(1)$ofile))
setwd("./export_data")
write_csv(moranDat, "moranExport_full.csv")
# exported 7/26/16

