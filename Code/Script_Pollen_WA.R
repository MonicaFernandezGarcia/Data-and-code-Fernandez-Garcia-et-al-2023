
###The script below was written in  Rstudio, so it is  recommended to be run within RStudio.


rm(list = ls()) # Clear all


#Install libraries if needed:

  #install.packages(c("openxlsx", "ggplot2", "ggpubr", "analogue", "dplyr"))


# Load libraries

library(openxlsx)
library(ggplot2)
library(ggpubr)
library(analogue)
library(dplyr)


### Choose your own directory:

setwd("...")



## GENERATE PREDICTIVE FUNCTIONS------------------------------------------------


#### Temperature model - MEAN ANNUAL TEMPERATURE (MAT)

# Load the percentage of extat pollen species
PercentageSpecies <- readRDS(file = "EMPDatabase.rds")

# Load the MAT of localities with palynological records
MAT.Dataset <- readRDS(file = "EMPDatabase_MAT.rds")

# Compute the predictive model
pred.temp <-wa(PercentageSpecies, MAT.Dataset$T_ann, deshrink= "monotonic")
pred.temp

# Bootstrap
boot <- bootstrap(pred.temp, n.boot = 500)
performance(boot)



#### Precipitation model - MEAN ANNUAL PRECIPITATION (MAP)

# Load the MAP of localities with palynological records
MAP.Dataset <- readRDS(file = "EMPDatabase_MAP.rds")

# Compute the predictive model
pred.prep<-wa(PercentageSpecies, MAP.Dataset$P_ann, deshrink= "monotonic")
pred.prep

# Bootstrap
boot <- bootstrap(pred.prep, n.boot = 500)
performance(boot)



#### Estimate MAT and MAP in the Cantabrian Region

# Load Dataset
Dataset <- read.xlsx("PollenRecord.xlsx", 
                     rowNames=FALSE, colNames=TRUE, sheet="Hoja1")

Sites <-Dataset[,1]
Level <-Dataset[,2]
Culture<- Dataset[,3]


# Pollen percentages
Dataset <-Dataset[,4:50]


# Estimate the MAT
pred.MAT <- predict(pred.temp, Dataset, tol.dw=TRUE)
pred.MAT
reconPlot(pred.MAT, use.labels = TRUE, display = "bars")


# Include the site and level of each MAT estimation and save outputs
MAT <- as.data.frame(pred.MAT$pred$pred)
MAT$Site <- Sites
MAT$Level <- Level
#write.xlsx(MAT, "Pollen_WA_MAT.xlsx")


#Estimate the MAP
pred.MAP <- predict(pred.prep, Dataset, tol.dw=TRUE)
pred.MAP
reconPlot(pred.MAP, use.labels = TRUE, display = "bars")


#Include the site and level of each MAP estimation and save outputs
MAP <- as.data.frame(pred.MAP$pred$pred)
MAP
MAP$Site <- Sites
MAP$Level <- Level
#write.xlsx(MAP, "Pollen_WA_MAP.xlsx")


#To save the MAT and MAP outputs in the same file
Outputs <- MAT
Outputs$MAP <- MAP
Outputs$Sites<- Sites
Outputs$Level <-Level
Outputs$Culture <- Culture

write.csv(Outputs, "Outputs_Pollen_WA.csv")





