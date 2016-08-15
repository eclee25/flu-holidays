
## Name: Elizabeth Lee
## Date: 10/28/15
## Function: check metapopulation model attack rate and results
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(flumodels)
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

#### single population ################################
params <- list(population = 100000,
               R0 = 1.2, 
               latentPeriod = 1E-9, 
               infectiousPeriod = 2, 
               seedInfections = 1, 
               contactMatrix = matrix(data = 15.8),
               simulationLength = 240)

model <- do.call(SEIRModel, c(params))
plot(model, normalizeRate = TRUE, 
     incidence = TRUE)
print(model)

print('----------------------')
#### two age groups ################################
params2 <- list(population = 100000,
               populationFractions = c(0.24, 1-0.24), 
               R0 = 1.1, 
               latentPeriod = 1E-9, 
               infectiousPeriod = 2, 
               seedInfections = c(1, 0), 
               contactMatrix = matrix(data = c(18.6, 4.2, 5.6, 8.0), ncol = 2),
               simulationLength = 240)

model2 <- do.call(SEIRModel, c(params2))
plot(model2, normalizeRate = TRUE, 
     populationLabels = c('children', 'adults'), 
     incidence = TRUE)
print(model2)
# model$rawOutput

        