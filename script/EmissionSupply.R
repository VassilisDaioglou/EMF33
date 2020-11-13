# AIM:
# R script that reads in EMF33 Supply Scenarios (R3) data to produce multi-model biomass emission-supply curves 
# 
# METHOD:
# Use EMF-33 senarios with linearly increased (modern) biomass demand from 0 EJ/yr to 100/200/300/400 EJ/yr by 2100
# For each of these scenarios we have the annual LUC emissions. By comparing these LUC emission with those of a 
# counterfactual scenario (R3B0), we can get the LUC emissions associated with increased biomass demand
#
# By determining the cumulative (2010-2100) biomass production and LUC emissions, we can get an aggregate emission 
# factor (kgCO2/GJ-prim) for biomass production for each scenario and model. If we determine these emission factors 
# for each scenario, we can then draw and emission-factor supply curve for each model
#
# x-axis: 0, 100, 200, 300, 400 EJ/yr (based on each scenario)
# y-axis: Aggregate emission factor (based on cumulative LUC emissions divided by cumulative biomass production)
#
# AUTHORSHIP:
# Author: Vassilis Daioglou
# Date: October-November 2020
# Reference: AR6 WGIII Ch.7.4.4
#
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

# ---- CONSTANTS ----
ppi <- 300
FSizeStrip = 12
FSizeAxis = 12
FSizeLeg = 9

ActYears = c(2010,2020,2030,2040,2050)
Scnearios = c(R5B0, R5B100, R5B200, R5B300, R5B400, R5B0LP, R5B300LP)

# ---- READ DATA ----
DATA=read.csv("GitHub/EMF33/data/EmissionSupply/EmissSupplyDat.csv", sep=",", dec=".", stringsAsFactors = FALSE)

DATA$X <- NULL

DATA = spread(DATA,REGION,value)
DATA = DATA %>% mutate(WORLD = ASIA + LAM + MAF + OECD90 + REF)
DATA = melt(DATA,id.vars = c("MODEL","SCENARIO","VARIABLE","UNIT","Year"), na.rm = TRUE)
colnames(DATA)[6] <- "REGION"
DATA = subset(DATA, Year %in% ActYears)
DATA = subset(DATA, !(MODEL == "NLU 1.0" | MODEL == "FARM 3.1"))
#
# --- FUNCTIONS ----
clean.data <- function(dataframe){
  data = spread(dataframe, SCENARIO, value, fill = 0)
  data = data %>% mutate(Bio100 = R5B100 - R5B0)
  data = data %>% mutate(Bio200 = R5B200 - R5B0)
  data = data %>% mutate(Bio300 = R5B300 - R5B0)
  data = data %>% mutate(Bio400 = R5B400 - R5B0)
  data = data %>% mutate(Bio300LP = R5B300LP - R5B0LP)

  data = subset(data, select = -c(R5B0, R5B100, R5B200, R5B300, R5B400, R5B0LP, R5B300LP))
  
  data = melt(data, id.vars=c("MODEL","VARIABLE","UNIT","Year","REGION"), na.rm = TRUE)
  colnames(data)[6] <- "SCENARIO"

  data
} 

get.cumulative <- function(dataframe){
  dataframe$ID <- NULL
  dataframe = spread(dataframe, Year, value)
  colnames(dataframe)[6:10] <- c("x2010","x2020","x2030","x2040","x2050")
  # dataframe = dataframe %>% mutate(Cumulative = x2010 + x2020 + x2030 + x2040 + x2050 +
  #                                    x2060 + x2070 + x2080 + x2090 + x2100)
  dataframe = dataframe %>% mutate(Cumulative = x2010 + x2020 + x2030 + x2040 + x2050)
  dataframe = subset(dataframe, select = -c(x2010,x2020,x2030,x2040,x2050))
  dataframe 
}

# --- MAKE RELEVANT DATASETS ----
# ---- *** Land Use Emissions *** ----
Emis = subset(DATA, VARIABLE == "Emissions|CO2|Land Use")
Emis.temp = clean.data(Emis)
Emis.cum = get.cumulative(Emis.temp)
Emis.cum$ID = paste(Emis.cum$MODEL, Emis.cum$REGION, Emis.cum$SCENARIO)
# 
# ---- *** Primary Biomass Production *** ----
# Get cumulative production for EF calculation
Prim = subset(DATA, VARIABLE == "Primary Energy|Biomass|Energy Crops")
Prim.temp = clean.data(Prim)
Prim.cum = get.cumulative(Prim.temp)
Prim.cum$ID = paste(Prim.cum$MODEL, Prim.cum$REGION, Prim.cum$SCENARIO)

# Get production level in 2050 for figure drawing
Prim.2050 = subset(Prim.temp, Year == 2050)
Prim.2050$ID = paste(Prim.2050$MODEL, Prim.2050$REGION, Prim.2050$SCENARIO)
# 
# ---- *** Emission Factor Dataframe *** ----
# Combine data
EmisFac = Emis.cum
EmisFac$CumulativeEC = Prim.cum[match(EmisFac$ID,Prim.cum$ID),"Cumulative"]
EmisFac$PrimEC_2050 = Prim.2050[match(EmisFac$ID,Prim.2050$ID),"value"]

# EF calculation
EmisFac = EmisFac %>% mutate(EF_PrimEC = Cumulative / CumulativeEC)
EmisFac$UNIT <- "kgCO2/GJ"

EmisFac = subset(EmisFac, select=-c(VARIABLE, Cumulative, ID, PrimBio, PrimEC))

# Ascending order 
EmisFac$EFOrder = factor(EmisFac$SCENARIO, levels = c("Bio100","Bio200","Bio300","Bio300LP","Bio400"))
EmisFac$EFOrder = gsub( "Bio", "", EmisFac$EFOrder, fixed = F)

# Categorise 2050 production
EmisFac$PrimEC_2050_bin <- round(EmisFac$PrimEC_2050/3,-1)*3
head(EmisFac)
unique(EmisFac$PrimEC_2050_bin)
# 
# ---- LITERATURE DATA ----
daioglou=read.xlsx("GitHub/EMF33/data/EmissionSupply/Lit_EF.xlsx", sheetName="Daioglou_2017", startRow=1)
kalt = read.xlsx("GitHub/EMF33/data/EmissionSupply/Lit_EF.xlsx", sheetName="Kalt_2020", startRow=1)

# ---- FIGURES ----
# ---- *** FIG: Emission Supply Curve (cumulative) ----
boxplot<-ggplot(data = subset(EmisFac, REGION == "WORLD" ),
                aes(x=factor(PrimEC_2050_bin), y = EF_PrimEC)) + 
  geom_boxplot() +
  geom_jitter(aes(colour = MODEL, shape=SCENARIO),width=0.2, alpha = 0.9) +
  # ylim(0,50) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  # ylab(expression("W/m"^2)) +
  ylab(expression(paste("kgCO"[2],"/GJ-Prim"))) + 
  xlab("EJ Primary Biomass") 
# facet_wrap(.~MODEL, nrow=2, scales = "free_y")
boxplot

line<-ggplot(data = subset(EmisFac, REGION == "WORLD" ),
                aes(x=PrimEC_2050, y = EF_PrimEC,colour = MODEL)) + 
  geom_line() +
  geom_point(aes(shape=SCENARIO)) +
  # ylim(0,50) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  # ylab(expression("W/m"^2)) +
  ylab(expression(paste("kgCO"[2],"/GJ-Prim"))) + 
  xlab("EJ Primary Biomass") 
# facet_wrap(.~MODEL, nrow=2, scales = "free_y")
line
#
# ---- OUTPUTS ----
# #
png(file = "GitHub/EMF33/output/EmissionSupply/boxplot_EF.png", width = 4*ppi, height = 3*ppi, units = "px", res = ppi)
plot(boxplot)
dev.off()
# #
png(file = "GitHub/EMF33/output/EmissionSupply/line_EF.png", width = 4*ppi, height = 3*ppi, units = "px", res = ppi)
plot(line)
dev.off()



