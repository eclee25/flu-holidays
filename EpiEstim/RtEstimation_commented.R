
## Name: Elizabeth Lee
## Date: 2/9/15
## Function: EpiEstim package for R(t) estimation. Our hypothesis is that holiday periods have a dip in effective R. 
## Filenames: 
## Data Source: incidentCases_RtEstimation_dataExport.py; explore/Py_export/EpiEstim_totalILI_allLocs_S%s.csv
## Notes: 
## 

require(EpiEstim)

#####################
## Leave these parameters fixed ##
### Serial Interval Data (mean, sd) ###
# Cowling et al 2011: http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3057478/
Cowling2011 <-c(3.6, 1.6) # this is in days -- I think the data needs to be converted to days too

### Plotting Parameters ###
seasons <- 2:9
labels <- c('2001-02', '2002-03', '2003-04', '2004-05', '2005-06', '2006-07', '2007-08', '2008-09')
Thx <- c('2001-11-22', '2002-11-28', '2003-11-27', '2004-11-25', '2005-11-24', '2006-11-23', '2007-11-22', '2008-11-27', '2009-11-26')
######################################
## Set these parameters every time ##
index <-5 # index of seasons, labels, Thx date to run
mean.std.si <- Cowling2011
start.index <- 1:50 # starting index of the window over which R should be estimated
end.index <- 3:52 # ending index of the window over which R should be estimated 
# (in this setup, R will be estimated over indexes 1:3, 2:4, 3:5, etc...)

### Save File Parameters ###
# These should be updated whenever you change parameters so that the files you save will have the correct names (see write.csv function below)
seas <-seasons[index]

######################################
## Leave these relationships fixed ##
# these are mostly necessary for the optional plots #
seasonlab <- labels[index]
Thx.date <- Thx[index] 
#####################
## Import Data ##
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/explore/Py_export') # set to your own working directory
data <- read.csv(sprintf('EpiEstim_totalILI_allLocs_787_S%s.csv', seas), header=FALSE, colClasses='character')
names(data) <- c('season', 'week', 'cases')
incident.cases <- as.numeric(data$cases)

# Cori et al. 2013 R(t) estimation method
Cori.output <- EstimateR(incident.cases, T.Start=start.index, T.End=end.index, method="ParametricSI", Mean.SI=mean.std.si[1], Std.SI=mean.std.si[2], Mean.Prior=5, Std.Prior=5, CV.Posterior=0.3, plot=TRUE, leg.pos="topright")
# save the figure

setwd('/home/elee/Downloads') # set this to a working directory where you'll want to save the output data
# save the estimated R and quantiles
# give the output data a name that indicates which data was used (eg. outpatient only, all service places, entire US, a specific zip3, serial interval distribution, window of estimation). I've given an example here.
write.csv(Cori.output$R, file=sprintf("Restimates_US_allService_start1-50_end3-52_Cowling2011SI_S%s.csv", seas), row.names=FALSE) 

################################
# EVERYTHING BELOW THIS IS OPTIONAL (CLEANER PLOTS, ETC)
################################
## custom plotting parameters
w = 580 
h = 580
ps = 14
margin = c(6,4,4,4) + 0.1 # bottom, left, top, right margins
omargin = c(0.5, 0.5, 0.5, 0.5) # bottom, left, top, right outer margins
lab_sz = 1.5 # scaled size for x and y-axis labels
axis_sz = 1.5 # scaled size for axis tick labels
main_sz = 1.5 # scaled size for title text
linewidth = 3
un = "px"
Thx.poly <- c(which(data$week==Thx.date), which(data$week==Thx.date), which(data$week==Thx.date)+1, which(data$week==Thx.date)+1) # x-axis polygon coordinates for bar over Thx holiday
Xmas.poly <- c(12, 12, 14, 14) # x-axis polygon coordinates for bar over Xmas holiday
incid.poly <-  c(0,500,500,0) # y-axis polygon coordinates for incidence curve
Rt.poly <- c(0,5,5,0) # y-axis polygon coordinates for estimated R plot
holiday.col <- adjustcolor('gray', alpha.f=0.5) # assign grey color for bar over holiday periods

################################
## assign variable names
weeklabels <- substr(data$week[seq(1, 52, 10)], 6, 10)
meanRt <- Cori.output$R[,3]
meanRt.upper <- Cori.output$R[,11]
meanRt.lower <- Cori.output$R[,5]
################################
################################
### two-paneled figure with incidence and R effective over time ###
# these figures are custom duplicates of the default figures plotted by the "EstimateR" function.

setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/Anne/EpiEstim/figures') # reset this to the working directory where figures should be saved
png(filename=sprintf('incidentCases_estimatedR_S%s.png', seasonlab), units=un, width=w, height=h, pointsize=ps, bg = 'white')
par(mfrow=c(2,1), mar=c(0.5,4,1.5,0.5), oma=c(4,1,0,1)) # draw figure with two rows and one column with custom margins and outer margins

plot(incident.cases, main='incidence', xaxt='n', type='l', xlab='', ylab='new cases', cex.lab=lab_sz, cex.main=main_sz, cex.axis=axis_sz, lwd=linewidth)
polygon(Thx.poly, incid.poly, col=holiday.col) # grey bar over Thx dates
polygon(Xmas.poly, incid.poly, col=holiday.col) # grey bar Xmas dates

plot(meanRt, main='time-varying transmission', xaxt='n', type='l', xlab='', ylab='estimated R effective', lwd=linewidth, ylim=c(0,4), cex.lab=lab_sz, cex.axis=axis_sz, cex.main=main_sz)
lines(meanRt.upper, col='red', lwd=linewidth) # 95%CI upper bound
lines(meanRt.lower, col='red', lwd=linewidth) # 95%CI lower bound
polygon(Thx.poly, Rt.poly, col=holiday.col) # grey bar over Thx dates
polygon(Xmas.poly, Rt.poly, col=holiday.col) # grey bar over Xmas dates
axis(1, at=seq(1, 52, 10), labels=weeklabels, las=2, cex.axis=axis_sz) # draw custom x-axis
dev.off() # closes the figure object

################################
################################
## draw single panel incidence plot over time (duplicate of two-paneled figure) ##
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/Anne/EpiEstim/figures')
png(filename=sprintf('incidentCases_S%s.png', seasonlab), units=un, width=w, height=h, pointsize=ps, bg = 'white')
par(mfrow=c(1,1), mar=margin, oma=omargin)

plot(incident.cases, main=sprintf('Austin, TX - %s', seasonlab), xaxt='n', type='l', xlab='', ylab='new cases', cex.lab=lab_sz, cex.main=main_sz, cex.axis=axis_sz, lwd=linewidth)
polygon(Thx.poly, incid.poly, col=holiday.col) # Thx dates
polygon(Xmas.poly, incid.poly, col=holiday.col) # Xmas dates
axis(1, at=seq(1, 52, 10), labels=weeklabels, las=2, cex.axis=axis_sz)
dev.off()

## draw single panel R effective plot over time (duplicate of two-paneled figure) ##
png(filename=sprintf('estimatedR_S%s.png', seasonlab), units=un, width=w, height=h, pointsize=ps, bg = 'white')
par(mfrow=c(1,1), mar=margin, oma=omargin)

plot(meanRt, main=sprintf('%s', seasonlab), xaxt='n', type='l', xlab='', ylab='estimated R effective', lwd=linewidth, ylim=c(0,4), cex.lab=lab_sz, cex.axis=axis_sz, cex.main=main_sz)
lines(meanRt.upper, col='red', lwd=linewidth)
lines(meanRt.lower, col='red', lwd=linewidth)
polygon(Thx.poly, Rt.poly, col=holiday.col) # Thx dates
polygon(Xmas.poly, Rt.poly, col=holiday.col) # Xmas dates
axis(1, at=seq(1, 52, 10), labels=weeklabels, las=2, cex.axis=axis_sz)
dev.off()



