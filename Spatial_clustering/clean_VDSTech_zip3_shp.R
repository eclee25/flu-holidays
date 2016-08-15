
## Name: Elizabeth Lee
## Date: 5/16/16
## Function: Save cleaned zip3 shapefile - orphaned holes removed with cleangeo
## Filenames: VDSTech_ZIP3/zip3
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
require(cleangeo)
require(spdep)

#### import data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("./VDSTech_ZIP3")
zip.shp <- readShapeSpatial("zip3", verbose = TRUE, repair = TRUE) 

#### clean shp ################################
# http://gis.stackexchange.com/questions/113964/fixing-orphaned-holes-in-r
# get a report of orphaned hole or geometry validity issues for sp object
report <- clgeo_CollectionReport(zip.shp)
summary <- clgeo_SummaryReport(report)
issues <- report[report$valid == FALSE,]
# get indexes that have orphaned holes
nv <- clgeo_SuspiciousFeatures(report)
toclean <- zip.shp[nv,]
# try to clean orphaned holes
sub.shp.cl <- clgeo_Clean(toclean, print.log = TRUE)
# check if errors remain
report.clean <- clgeo_CollectionReport(sub.shp.cl)
summary.clean <- clgeo_SummaryReport(report.clean)

# clean the entire sp object, rm orphaned holes
zip.shp2 <- clgeo_Clean(zip.shp, print.log = TRUE)
report.clean <- clgeo_CollectionReport(zip.shp2)
summary.clean <- clgeo_SummaryReport(report.clean)


### examine islands in zip3 shapefile ################################
adjMx <- poly2nb(zip.shp2) # 2375 regions, 197 regions with no links; these region numbers refer to the map and not the nb list object
nolinks <- which(card(adjMx)==0)
attr(adjMx, "region.id")[which(card(adjMx)==0)]

# return shapefile with only ids with no links
zip.nolinks <- zip.shp2[nolinks,]
plot(zip.shp2, col = 'grey') # full shapefile
plot(zip.nolinks, col = 'red', add = TRUE) # zips with no neighbors

# return shapefile with only ids with links
zip.shp3 <- zip.shp2[which(card(adjMx)!=0),]
plot(zip.shp3)

# return shapefile of only continenal US data
zips.notcontinental <- c("006", "007", "008", "009", "967", "968", "969", "995", "996", "997", "998", "999") 
zip.shp4 <- zip.shp3[!(zip.shp3@data$ZIP %in% zips.notcontinental),]
plot(zip.shp4)

# 
# ### write cleaned shapefile to file ################################
# setwd(dirname(sys.frame(1)$ofile))
# dir.create("./VDSTech_zip3continental_cl", showWarnings = FALSE)
# setwd("./VDSTech_zip3continental_cl")
# 
# writeSpatialShape(zip.shp4, "zip3continental_cl") 
# # 6/28/16

# #### test exported data data ################################
# setwd(dirname(sys.frame(1)$ofile))
# setwd("./VDSTech_zip3_cl")
# zip.shp.cl <- readShapeSpatial("zip3_cl", verbose = TRUE, repair = TRUE) 
# report <- clgeo_CollectionReport(zip.shp.cl)
# summary <- clgeo_SummaryReport(report)
# issues <- report[report$valid == FALSE,]
# # 5/16/16 no errors

