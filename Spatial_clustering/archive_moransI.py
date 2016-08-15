#!/usr/bin/python

##############################################
###Python template
###Author: Elizabeth Lee
###Date: May 16, 2016
###Function: perform spatial clustering analysis before, during, and after Xmax holiday with SDI data to identify changes in spatial patterns (NOT COMPLETE)

###Import data: Dropbox/Anne_Bansal_lab/Morans_i moved to Anne/Spatial_clustering/import_data

###Command Line: python moransI.py
##############################################


### notes ###
# 5/16/16: issues with missing data make it difficult to calculate Moran's I (too many holes)

### packages/modules ###
import numpy as np
import matplotlib.pyplot as plt
import os
import pysal


### functions ###

### shapefile names ###

# # full shapefile
# shpfile = "/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/reference_data/USzip3_shapefiles_VDSTech/zip3_cl.shp"
# dbffile = "/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/reference_data/USzip3_shapefiles_VDSTech/zip3_cl.dbf"

# # continental shapefile only
shpfile = os.getcwd() + "/VDSTech_zip3continental_cl/zip3continental_cl.shp"
dbffile = os.getcwd() + "/VDSTech_zip3continental_cl/zip3continental_cl.dbf"

### incidence data ###
bfile = os.getcwd() + "/import_data/incidence_data_for_morans_i_2_weeks_before.csv"
dfile = os.getcwd() + "/import_data/incidence_data_for_morans_i_xmas.csv"
afile = os.getcwd() + "/import_data/incidence_data_for_morans_i_2_weeks_after.csv"

##############################################
### program ###
### incidence import ###
bDat = pysal.open(bfile)
dDat = pysal.open(dfile)
aDat = pysal.open(afile)

yb = np.array(bDat.by_col['incid_2wks_before'])
print bDat

### shapefile import ###
shp = pysal.open(shpfile)
db = pysal.open(dbffile, 'r')
w = pysal.rook_from_shapefile(shpfile)
print db.field_spec # type, length, precision
print db.header  # check dbf file attributes
print db[:100]
print 'neighbors 157, zip566', w.neighbors[157]
print 'neighbors 158, zip567', w.neighbors[158]


## check attributes of w ##
print "number of spatial units", w.n
print "sparseness of matrix", w.pct_nonzero
print w.histogram

# ### calculate Moran's I ###
# mi_b = pysal.Moran(yb, w, two_tailed = False)
# "%.3f"%mi_b.I
# mi_b.EI
# "%.5f"%mi_b.p_norm

# # ### testing area ###
# # node = 945
# # print "neighbors, weights", node, w.neighbors[node], w.weights[node]
# # print "tuples with cardinality of neighbor relations", w.histogram 

