# R script that reads in EMF33 Supply Scenarios (R3) data
# to produce multi-model biomass emission-supply curves 
# 
# Author: Vassilis Daioglou
# Date: October-November 2020
# Reference: AR6 WGIII Ch.7.4.4

# ---- START ----
# clear memory
rm(list=ls()) 

# Load Libraries
library(reshape2);
library(ggplot2);
library(plyr);
library(dplyr)
library(data.table);
library(tidyr)
library(xlsx)

# ---- READ DATA ----
DATA=read.csv("GitHub/EMF33/data/EmissionSupply/EmissSupplyDat.csv", sep=",", dec=".", stringsAsFactors = FALSE)
DATA$X <- NULL

DATA = spread(DATA,REGION,value)
DATA = DATA %>% mutate(WORLD = ASIA + LAM + MAF + OECD90 + REF)
DATA = melt(DATA,id.vars = c("MODEL","SCENARIO","VARIABLE","UNIT","Year"), na.rm = TRUE)
colnames(DATA)[6] <- "REGION"

#
# --- MAKE RELEVANT DATASETS ----
clean.data <- function(dataframe){
  data = spread(dataframe, SCENARIO, value, fill = 0)
  data = data %>% mutate(AllFeedstocks = R5B300 - R5B0)
  data = data %>% mutate(EnergyCrops = R5B300EC - R5B0)
  
  data = subset(data, selec = -c(R5B0, R5B300, R5B300EC))
  
  data$ID = paste(data$MODEL, data$REGION, data$Year)
  data
} 

# ---- *** Land Use Emissions *** ----
Emis.temp = subset(DATA, VARIABLE == "Emissions|CO2|Land Use")
Emis.temp = clean.data(Emis.temp)

# ---- *** Primary Biomass Production *** ----
Prim = subset(DATA, VARIABLE == "Primary Energy|Biomass|Modern" | VARIABLE == "Primary Energy|Biomass|Energy Crops")
Prim = clean.data

# ---- *** Complete Dataset *** ----
EmisSupply = Emis.temp
EmissSupply