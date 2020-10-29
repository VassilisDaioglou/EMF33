# R script that reads in EMF33 Supply Scenarios (R3) database and outputs .csv files 
# Subset relevant data and pass to EmissionSupply.R to produce multi-model biomass emission-supply curves 
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
DATA=read.csv("GitHub/EMF33/data/emf33supr5r_compare_20180725-152149.csv", sep=",", dec=".", stringsAsFactors = FALSE)
DATA=melt(DATA, id.vars=c("MODEL","SCENARIO","REGION","VARIABLE","UNIT"), variable_name="Year", value.name="value", na.rm=FALSE)
colnames(DATA)[6]<-"Year"
DATA$Year = as.numeric(substr(DATA$Year, start=2, stop=5))
DATA = na.omit(DATA)

# ---- SUBSET RELEVANT DATA ----
DATA1 = subset(DATA, 
               VARIABLE=="Emissions|CO2|Land Use" |
                 VARIABLE=="Primary Energy|Biomass|Energy Crops" | VARIABLE=="Primary Energy|Biomass|Modern")

DATA1 = subset(DATA1, SCENARIO=="R5B0" | SCENARIO == "R5B300" | SCENARIO == "R5B300EC")
DATA1 = subset(DATA1, REGION=="ASIA" | REGION == "LAM" | REGION == "MAF" | REGION == "OECD90" | REGION == "REF" | REGION == "WORLD")

# ---- OUTPUT ----
write.csv(DATA1, file = "GitHub/EMF33/data/EmissionSupply/EmissSupplyDat.csv")
