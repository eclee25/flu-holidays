## Name: Elizabeth Lee
## Date: 11/4/15
## Function: Check epidemic curve against known curves with R0 = 1.2  

## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list = ls())

#### header ####################################
require(ggplot2)
require(dplyr)
require(readr)
require(tidyr)
require(flumodels)
vtxt <- 'v_11_04_15'
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf('../%s', vtxt))

#### params ####################################
ae_beta <- 0.029 # 0.029, 0.037
fm_R0 <- 1.2

#### Anne's data ####################################
# AEdat <- read_csv('chain_binomial_output_nummetrozeros_1_numchildzeros_1_numadultzeros_0.csv') %>%
#   filter(metro_id == 1) %>% 
#   rename(cInf = currently_infected) %>%
#   rename(tInf = total_infected) %>%
#   gather(infType, infecteds, c(cInf, tInf))
AEdat <- read_csv(sprintf('chain_binomial_output_nummetrozeros_1_numchildzeros_1_numadultzeros_0_beta_%s.csv', ae_beta)) %>%
  filter(metro_id == 1) %>% 
  rename(cInf = currently_infected) %>%
  rename(tInf = total_infected) %>%
  gather(infType, infecteds, c(cInf, tInf))

setwd('../Model_Inputs')
popdat <- read.table("metedges.txt", col.names = c("met1", "pop1", "met2", "pop2", "samp1", "samp2")) %>%
  select(met1, pop1) %>% unique %>%
  filter(met1 == 1) %>% unlist
  

setwd(sprintf('../%s/graph_outputs', vtxt))
pltAge <- ggplot(AEdat %>% filter(infType == 'cInf'), aes(x = time_step, y = infecteds, group = age)) +
  geom_line(aes(colour = age))
print(pltAge)

AEtot <- AEdat %>% filter(infType == 'cInf') %>%
  group_by(time_step) %>%
  summarise(totInfecteds = sum(infecteds))

pltTot <- ggplot(AEtot, aes(x = time_step, y = totInfecteds/popdat[2])) +
  geom_line()
print(pltTot)

#### FLUMODELS ####################################
#### single population ################################
params <- list(population = 100000,
               R0 = fm_R0, 
               latentPeriod = 1E-9, 
               infectiousPeriod = 2, 
               seedInfections = 1, 
               contactMatrix = matrix(data = 15.8),
               simulationLength = 240)

model <- do.call(SEIRModel, c(params))
plot(model, normalizeRate = TRUE, 
     incidence = TRUE, asRate = TRUE)
print(model)

print('----------------------')
#### two age groups ################################
params2 <- list(population = 100000,
                populationFractions = c(0.24, 1-0.24), 
                R0 = fm_R0, 
                latentPeriod = 1E-9, 
                infectiousPeriod = 2, 
                seedInfections = c(1, 0), 
                contactMatrix = matrix(data = c(18.6, 4.2, 5.6, 8.0), ncol = 2),
                simulationLength = 240)

model2 <- do.call(SEIRModel, c(params2))
plot(model2, normalizeRate = TRUE, 
     populationLabels = c('children', 'adults'), 
     incidence = TRUE, asRate = TRUE)
print(model2)

#### compare ################################
modelOut <- tbl_df(as.data.frame(model$rawOutput))
modelOut2 <- tbl_df(as.data.frame(model2$rawOutput)) %>%
  mutate(I = I1 + I2)

pltTot2 <- ggplot(AEtot, aes(x = time_step, y = totInfecteds/popdat[2])) +
  geom_line(colour = 'black', size = 2) +
  geom_line(data = modelOut, aes(x = time, y = I), colour = 'blue') + 
  geom_line(data = modelOut2, aes(x = time, y = I), colour = 'red') +
  scale_y_continuous(name = 'Current infections') +
  coord_cartesian(xlim = c(0, 175)) +
  ggtitle('blue - one pop, homogeneous; red - two pops, heterogeneous')
ggsave(sprintf('metapop_flumodels_comparison_aeBeta%s_fmR0%s.png', ae_beta, fm_R0))
print(pltTot2)



