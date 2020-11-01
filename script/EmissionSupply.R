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

# ---- CONSTANTS ----
ppi <- 300
FSizeStrip = 12
FSizeAxis = 12
FSizeLeg = 9

ActYears = c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100)

# ---- READ DATA ----
DATA=read.csv("GitHub/EMF33/data/EmissionSupply/EmissSupplyDat.csv", sep=",", dec=".", stringsAsFactors = FALSE)
DATA$X <- NULL

DATA = spread(DATA,REGION,value)
DATA = DATA %>% mutate(WORLD = ASIA + LAM + MAF + OECD90 + REF)
DATA = melt(DATA,id.vars = c("MODEL","SCENARIO","VARIABLE","UNIT","Year"), na.rm = TRUE)
colnames(DATA)[6] <- "REGION"
DATA = subset(DATA, Year %in% ActYears)
DATA = subset(DATA, !(MODEL == "NLU 1.0"))
#
# ---- IDENTIFY RELEVANT MODELS ----
# Models which include valid data for the "Energy Crops Only" Scenario
MODELS.EnergyCrops = unique(subset(DATA, SCENARIO == "R5B300EC")$MODEL)
#
# --- FUNCTIONS ----
clean.data <- function(dataframe){
  data = spread(dataframe, SCENARIO, value, fill = 0)
  data = data %>% mutate(AllFeedstocks = R5B300 - R5B0)
  data = data %>% mutate(EnergyCrops = R5B300EC - R5B0)
  
  data = subset(data, select = -c(R5B0, R5B300, R5B300EC))
  
  data = melt(data, id.vars=c("MODEL","VARIABLE","UNIT","Year","REGION"), na.rm = TRUE)
  colnames(data)[6] <- "SCENARIO"

  data$ID = paste(data$MODEL, data$REGION, data$Year)
  data
} 

get.marginal <- function(dataframe){
  dataframe = spread(dataframe, Year, value)
  colnames(dataframe)[6:15] <- c("x2010","x2020","x2030","x2040","x2050","x2060","x2070","x2080","x2090","x2100")
  dataframe = dataframe %>% mutate(x2020marg = x2020 - x2010)
  dataframe = dataframe %>% mutate(x2030marg = x2030 - x2020)
  dataframe = dataframe %>% mutate(x2040marg = x2040 - x2030)
  dataframe = dataframe %>% mutate(x2050marg = x2050 - x2040)
  dataframe = dataframe %>% mutate(x2060marg = x2060 - x2050)
  dataframe = dataframe %>% mutate(x2070marg = x2070 - x2060)
  dataframe = dataframe %>% mutate(x2080marg = x2080 - x2070)
  dataframe = dataframe %>% mutate(x2090marg = x2090 - x2080)
  dataframe = dataframe %>% mutate(x2100marg = x2100 - x2090)
  dataframe = subset(dataframe, select = -c(x2010,x2020,x2030,x2040,x2050,x2060,x2070,x2080,x2090,x2100))
  dataframe = melt(dataframe, id.vars=c("MODEL","SCENARIO","VARIABLE","UNIT","REGION"))
  colnames(dataframe)[6] <- "Year" 
  
  dataframe$Year <- gsub("x","",dataframe$Year)
  dataframe$Year <- gsub("marg","",dataframe$Year)
  dataframe$Year = as.numeric(dataframe$Year)
  dataframe
}

# --- MAKE RELEVANT DATASETS ----
# ---- *** Land Use Emissions *** ----
Emis.temp = subset(DATA, VARIABLE == "Emissions|CO2|Land Use")
# Emis.marg = get.marginal(Emis.temp)
Emis.marg = clean.data(Emis.temp)
Emis.marg = subset(Emis.marg, SCENARIO == "AllFeedstocks")
# ---- *** Primary Biomass Production *** ----
Prim = subset(DATA, VARIABLE == "Primary Energy|Biomass|Modern")
Prim.marg = get.marginal(Prim)
Prim.marg = clean.data(Prim.marg)

Prim$ID = paste(Prim$MODEL, Prim$REGION, Prim$Year)
# ---- *** Projections of Prim and Emissions *** ----
Prim.temp = subset(Prim, SCENARIO == "R5B300")
Prim.temp$ID = paste(Prim.temp$MODEL, Prim.temp$REGION, Prim.temp$Year)

Projections = Emis.marg
Projections$PrimBio = Prim.temp[match(Projections$ID,Prim.temp$ID),"value"]
colnames(Projections)[7] <- "LUC_MtCO2"
colnames(Projections)[9] <- "PrimBio_EJ"
Projections = subset(Projections, select =-c(VARIABLE, UNIT, ID))

Projections = melt(Projections, id.vars=c("MODEL","Year","REGION","SCENARIO"), rm.na = TRUE)
# ---- *** Emission-Supply Dataset *** ----
Emis_Supply = Emis.marg
Emis_Supply$PrimBio = Prim.marg[match(Emis_Supply$ID,Prim.marg$ID),"value"]
Emis_Supply = subset(Emis_Supply, !Year == 2010)
colnames(Emis_Supply)[7] <- "LUC_MtCO2"
colnames(Emis_Supply)[9] <- "MargPrimBio_EJ"
Emis_Supply = subset(Emis_Supply, select = -c(VARIABLE, UNIT,ID))

  # Order the data according to increasing LUC emissions
  # Per MODEL, REGION, and SCENARIO
Emis_Supply.Order = Emis_Supply
Emis_Supply.Order = Emis_Supply.Order %>% mutate(EF_PrimBio = LUC_MtCO2 / MargPrimBio_EJ)

Emis_Supply.Order <- Emis_Supply.Order[with(Emis_Supply.Order, order(MODEL, REGION, SCENARIO, EF_PrimBio)),]
Emis_Supply.Order$ID = paste(Emis_Supply.Order$MODEL, Emis_Supply.Order$REGION, Emis_Supply.Order$SCENARIO)
Emis_Supply.Order$CumPrimBio_EJ <- ave(Emis_Supply.Order$MargPrimBio_EJ, Emis_Supply.Order$ID, FUN = cumsum)

Emis_Supply.Order = subset(Emis_Supply.Order, MargPrimBio_EJ > 0)

# ---- FIGURES ----
# ---- *** FIG: Emission and Biomass Projections ----
proj<-ggplot(data = subset(Projections, SCENARIO == "AllFeedstocks" & REGION == "WORLD")) + 
  geom_line(aes(x=Year, y=value, colour = REGION)) +
  ggtitle("Effect of biomass growth (B300-B0)") + 
  xlim(2010,2100) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  xlab("Year") +
  facet_grid(variable~MODEL, scales = "free_y")
proj

#
# ---- *** FIG: Emission Supply Curve ----
Scatter<-ggplot(data = subset(Emis_Supply.Order, SCENARIO == "AllFeedstocks" & REGION == "WORLD")) + 
  geom_point(aes(x=CumPrimBio_EJ, y=EF_PrimBio, shape = REGION, colour = MODEL)) +
  geom_line(aes(x=CumPrimBio_EJ, y=EF_PrimBio,  colour = MODEL)) +
  # xlim(2010,2100) +
  # ylim(-200,400) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  # ggtitle("A: Radiative Forcing") + theme(plot.title = element_text(face="bold")) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  # ylab(expression("W/m"^2)) +
  ylab("KgCO2/GJ-Prim")+ 
  xlab("EJ Primary Biomass") 
  # theme(legend.position="none", legend.text = element_text(size=6, face="plain")) +
  # scale_colour_manual(values=c("forestgreen","forestgreen","forestgreen","navy","navy","navy","firebrick","firebrick","firebrick"),
  #                     name ="",
  #                     breaks=c("SSP1","SSP1_20","SSP1_450","SSP2","SSP2_20","SSP2_450","SSP3","SSP3_450"),
  #                     labels=c("SSP1","","","SSP2","","","SSP3",""), guide=FALSE) +
  # scale_shape_manual(values=c(1,2,3,4),
  #                    name ="Climate Target",
  #                    breaks=c("20","450","550","Baseline"),
  #                    labels=c("1.9 W/m²","2.6 W/m²","3.4 W/m²","Baseline")) +
  # facet_grid(MODEL~.)
Scatter

#
# ---- OUTPUTS ----
png(file = "GitHub/EMF33/output/EmissionSupply/Projections.png", width = 9*ppi, height = 3*ppi, units = "px", res = ppi)
plot(proj)
dev.off()
# 
png(file = "GitHub/EMF33/output/EmissionSupply/Scatter.png", width = 4*ppi, height = 3*ppi, units = "px", res = ppi)
plot(Scatter)
dev.off()
# #



