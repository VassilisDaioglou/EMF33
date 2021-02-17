# ---- INFORMATION ----
# AIM
# R script to determine the displaced fossil fuel emissions due to the use of bioenergy
# To be combined with CDR from BECCS in order to understand the full mitigation potential of bioenergy with CCS. 
#
# METHOD:
# Use EMF-33 scenarios to get range of results across models and scenarios.
# 1. Determine carbon content of electrifcity and liquids use based on baseline projections
# 2. Determine extent of bioenergy use in these energy carriers across a mitigation scenario
# 3. Determine displaced CO2
#
# Calculations are done for a carbon price of approximately 100 $/tCO2, in order to be consistent with BECCS
# potential calculations. Sinnce the EMF-33 projections do not include results for a carbon price of exactly 100$/tCO2,
# we instead identify the timestep that car bon prices are projected to surpass 100$/tCO2, and use the data of that timestep.
#
# AUTHORSHIP:
# Author: Vassilis Daioglou
# Date: February 2020
# Reference: Roe et al. 2021, Global Environmental Change
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
library(stringr)
library(xlsx)
library(openxlsx)
library(ggpubr)

setwd("C:/Users/Asus/Documents/Github/EMF33/")

# ---- INPUTS: Constants ----
ppi <- 300
FSizeTitle = 10
FSizeStrip = 9
FSizeAxis = 9
FSizeLeg = 9

GJperMWh = 3.6  

# Carbon Content in kgCO2/MWh
CCPrim <- data.frame(c("Bio","Coa","Gas","Geo","Hyd","Nuc","Sol","Win","Oce","Oil"),
                     c(0,            # Biomass = Assume no LUC here as it is accounted for in the BECCS mitigation potential
                       353.8,        # Coal 
                       221.76,       # Gas 
                       0,            # Geothermal
                       0,            # Hydro
                       0,            # Nuclear
                       0,            # Solar
                       0,            # Wind
                       0,            # Ocean
                       249.5))       # Oil 

colnames(CCPrim) <- c("Tech","CC_kgCO2_MWh")
CCPrim = CCPrim %>% mutate(CC_MtCO2_EJ = CC_kgCO2_MWh / GJperMWh)

# electricity conversion efficiency, based on UNFCCC tool to calculate electricity emission factor
# https://cdm.unfccc.int/methodologies/PAmethodologies/tools/am-tool-07-v5.0.pdf
# Appendix I
Elec_Eff <- data.frame(c("Bio","Coa","Gas","Geo","Hyd","Nuc","Sol","Win","Oce","Oil"),
                       c(0,            # Biomass = Assume no LUC here as it is accounted for in the BECCS mitigation potential
                         0.40,        # Coal 
                         0.60,       # Gas 
                         0,            # Geothermal
                         0,            # Hydro
                         0,            # Nuclear
                         0,            # Solar
                         0,            # Wind
                         0,            # Ocean
                         0.46))       # Oil 
colnames(Elec_Eff) <- c("Tech","Efficiency")

CCElec = CCPrim 
CCElec$Eff = Elec_Eff[match(CCElec$Tech,Elec_Eff$Tech),"Efficiency"]  
CCElec$Elec_CC_MtCO2_EJ = CCElec$CC_MtCO2_EJ / CCElec$Eff  
CCElec[is.na(CCElec)] <- 0.0

# Liquids thermal conversion efficiency
# Coal-to-liquids: https://uu.diva-portal.org/smash/get/diva2:293610/FULLTEXT02.pdf
# Gas-to-Liquids: https://pubs-acs-org.proxy.library.uu.nl/doi/10.1021/ie402284q
Liq_Eff <- data.frame(c("Bio","Coa","Gas","Geo","Hyd","Nuc","Sol","Win","Oce","Oil"),
                      c(0,            # Biomass = Assume no LUC here as it is accounted for in the BECCS mitigation potential
                         0.5,          # Coal 
                         0.5,          # Gas 
                         0,            # Geothermal
                         0,            # Hydro
                         0,            # Nuclear
                         0,            # Solar
                         0,            # Wind
                         0,            # Ocean
                         1))       # Oil 
colnames(Liq_Eff) <- c("Tech","Efficiency")

CCLiq = CCPrim 
CCLiq$Eff = Liq_Eff[match(CCLiq$Tech,Liq_Eff$Tech),"Efficiency"]  
CCLiq$Elec_CC_MtCO2_EJ = CCElec$CC_MtCO2_EJ / CCLiq$Eff  
CCLiq[is.na(CCLiq)] <- 0.0

# ---- READ DATA ----
DATA=read.csv("data/FossilDisplacement/DisplacementData.csv", sep=",", dec=".", stringsAsFactors = FALSE)

DATA$X <- NULL
DATA = subset(DATA, !(Year=="1990"|Year=="1995"|Year=="2000"|Year=="2005"|Year=="2010"|Year=="2015"|Year=="2110"|Year=="2130"|Year=="2150"))
DATA$ID1 = paste(DATA$MODEL, DATA$SCENARIO, DATA$REGION)
DATA$ID2 = paste(DATA$MODEL, DATA$SCENARIO, DATA$REGION,DATA$Year)

cprice = subset(DATA, VARIABLE == "Price|Carbon")
cprice$Tech <- NULL
# ---- YEAR OF INTEREST ----
# Identify first year when carbon prices are above 100$/tCO2
Above100 = subset(cprice, value > 100)
Cross100 = aggregate(Above100$Year, by=list(ID1=Above100$ID1), FUN=min, na.rm=TRUE)
Cross100$ID2 = paste(Cross100$ID1, Cross100$x)

DATA.cor = subset(DATA, ID2 %in% Cross100$ID2 & !(VARIABLE=="Price|Carbon"))

rm(Above100, Cross100)
# ----  CARBON CONTENTS OF FINAL ENERGY CARRIERS ----
DATA.cor$Carrier = substr(DATA.cor$VARIABLE, start = 18, stop = 20)
DATA.cor$VARIABLE <- NULL
DATA.cor[is.na(DATA.cor)] <- 0.0

Electricity = subset(DATA.cor, Carrier=="Ele")
Liquids = subset(DATA.cor, Carrier == "Liq")

# ---- *** ELECTRICITY *** ----
# Determine share of fuels used for each carrier, ignoring biomass use
Elec.shares <- spread(Electricity, Tech, value)
Elec.shares[is.na(Elec.shares)] <- 0.0
Elec.shares = Elec.shares %>% mutate(Total_NonBio = Coal + Gas + Geothermal + Hydro + Nuclear + Ocean + Oil + Solar + Wind)

# Get shares of primary energy carriers
Elec.shares = Elec.shares %>% mutate(Coa_Share = Coal / Total_NonBio)  
Elec.shares = Elec.shares %>% mutate(Gas_Share = Gas / Total_NonBio)  
Elec.shares = Elec.shares %>% mutate(Geo_Share = Geothermal / Total_NonBio)  
Elec.shares = Elec.shares %>% mutate(Hyd_Share = Hydro / Total_NonBio)  
Elec.shares = Elec.shares %>% mutate(Nuc_Share = Nuclear / Total_NonBio)  
Elec.shares = Elec.shares %>% mutate(Oce_Share = Ocean / Total_NonBio)  
Elec.shares = Elec.shares %>% mutate(Oil_Share = Oil / Total_NonBio)  
Elec.shares = Elec.shares %>% mutate(Sol_Share = Solar / Total_NonBio)  
Elec.shares = Elec.shares %>% mutate(Win_Share = Wind / Total_NonBio)  

Elec.shares = Elec.shares[,c(1:3,5:6,20:29)]
Elec.shares = melt(Elec.shares, id.vars=c("MODEL","SCENARIO","REGION","Year","ID1"))
Elec.shares$Tech = substr(Elec.shares$variable, start = 1, stop = 3)
Elec.NonBio = subset(Elec.shares, Tech == "Tot")

# Determine contribution of each energy carrier to secondary emission factor
Elec.shares = subset(Elec.shares, !(Tech=="Tot"))
Elec.shares$CC = CCElec[match(Elec.shares$Tech,CCElec$Tech),5]
Elec.shares = Elec.shares %>% mutate(CC_factor = value * CC)

# Determine final emission factor of secondary energy carrier by summing contribution of each primary feedstock
Elec.CC = aggregate(Elec.shares$CC_factor, by=list(ID1=Elec.shares$ID1), FUN=sum, na.rm=TRUE)

