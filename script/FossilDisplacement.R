# ---- INFORMATION ----
# AIM
# R script to determine the displaced fossil fuel due to the use of bioenergy, and thus determine 
# avoided CO2 emissions due to the use of bioenergy.
# To be combined with CDR from BECCS in order to understand the full mitigation potential of bioenergy with CCS. 
#
# METHOD
# Emissions mitigation due to the use of bioenergy depends on the carbon content of the 
# energy carriers it displaces. This differs across use of bioenergy (production of liquids or electricity),
# and may vary over time as different technologies are used (i.e. movement from coal based electricity to natural gas, or renewables)
#
# Thus, a system perspective has to be taken. However, there are huge uncertainties on how 
# the energy system may develop over time. 
#
# To adress this problem, we use EMF-33 scenarios to get range of results across models and scenarios.
# 1. Determine carbon content of ELECTRICITY and LIQUIDS production, based on the primary fuels used in a baseline projection
# 2. Determine extent of bioenergy use in the production of ELECTRICITY and LIQUIDS across a set mitigation scenario
# 3. Thus, the mitigation due to bioenergy in the mitigation scenario is determined by multiplying bioenergy use 
#     in the production of ELECTRICITY and LIQUIDS with thier baseline carbon content
#
# Results are generated across a combination of models and mitigation scenarios (EMF-33 project, Bauer et al.(2018)) 
#
# KEY ASSUMPTIONS
# 1. Calculations are done for a carbon price of approximately 100 $/tCO2, in order to be consistent with BECCS 
# potential calculations. Since the EMF-33 projections do not include results for a carbon price of exactly 100$/tCO2,
# we instead identify the timestep that car bon prices are projected to surpass 100$/tCO2, and use the data of that timestep.
#
# 2. The carbon content of the baseline is used as the counterfactual carbon content of bioenergy. Since the bioenergy use
# is estimated for a carbon price of ~100$/tCO2, this is inconsistent since by this price level there is some decarbonization
# of the production of electricity and liquids. However, using the carbon contents at that carbon price would only reveal the
# MARGINAL emissions mitigation of bioenergy at the price level, while at lower price levels the mitigation would be higher. 
#
# 3. Explicity assume biomass has no emissions. AFOLU emissions are included in the BECCS potentials. Only ENERGY SYSTEM emissions are included here.
#
# AUTHORSHIP:
# Author: Vassilis Daioglou
# Date: February 2021
# Reference: Roe et al. (2021), Land-based measures to mitigate climate change: potential and feasibility by country, Global Environmental Change
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
CCPrim <- data.frame(Tech = c("Bio","Coa","Gas","Geo","Hyd","Nuc","Sol","Win","Oce","Oil"),
                      CC_kgCO2_MWh = c(0,            # Biomass = Assume no LUC here as it is accounted for in the BECCS mitigation potential
                                       353.8,        # Coal 
                                       221.76,       # Gas 
                                       0,            # Geothermal
                                       0,            # Hydro
                                       0,            # Nuclear
                                       0,            # Solar
                                       0,            # Wind
                                       0,            # Ocean
                                       249.5))       # Oil 

CCPrim = CCPrim %>% mutate(CC_MtCO2_EJ = CC_kgCO2_MWh / GJperMWh)

# electricity conversion efficiency, based on UNFCCC tool to calculate electricity emission factor
# https://cdm.unfccc.int/methodologies/PAmethodologies/tools/am-tool-07-v5.0.pdf
# Appendix I
# Biomass based on average of supplementary data of Daioglou et al. 2020 (https://link.springer.com/article/10.1007/s10584-020-02799-y)
  # Filter: Year = 2050; Primary Carrier = Biomass; Secondary Carrier = Ele; Carbon Capture = With
Elec_Eff <- data.frame(Tech = c("Bio","Coa","Gas","Geo","Hyd","Nuc","Sol","Win","Oce","Oil"),
                       Efficiency = c(0.36,         # Biomass
                                      0.40,         # Coal 
                                      0.60,         # Gas 
                                      0,            # Geothermal
                                      0,            # Hydro
                                      0,            # Nuclear
                                      0,            # Solar
                                      0,            # Wind
                                      0,            # Ocean
                                      0.46))       # Oil 

CCElec = CCPrim 
CCElec$Eff = Elec_Eff[match(CCElec$Tech,Elec_Eff$Tech),"Efficiency"]  
CCElec$Elec_CC_MtCO2_EJ = CCElec$CC_MtCO2_EJ / CCElec$Eff  
CCElec[is.na(CCElec)] <- 0.0

# Liquids thermal conversion efficiency
# Coal-to-liquids: https://uu.diva-portal.org/smash/get/diva2:293610/FULLTEXT02.pdf
# Gas-to-Liquids: https://pubs-acs-org.proxy.library.uu.nl/doi/10.1021/ie402284q
# Biomass based on average of supplementary data of Daioglou et al. 2020 (https://link.springer.com/article/10.1007/s10584-020-02799-y)
  # Filter: Year = 2050; Primary Carrier = Biomass; Secondary Carrier = Liq; Carbon Capture = With
Liq_Eff <- data.frame(Tech = c("Bio","Coa","Gas","Geo","Hyd","Nuc","Sol","Win","Oce","Oil"),
                      Efficiency = c(0.47,         # Biomass
                                     0.5,          # Coal 
                                     0.5,          # Gas 
                                     0,            # Geothermal
                                     0,            # Hydro
                                     0,            # Nuclear
                                     0,            # Solar
                                     0,            # Wind
                                     0,            # Ocean
                                     1))       # Oil 
CCLiq = CCPrim 
CCLiq$Eff = Liq_Eff[match(CCLiq$Tech,Liq_Eff$Tech),"Efficiency"]  
CCLiq$Elec_CC_MtCO2_EJ = CCElec$CC_MtCO2_EJ / CCLiq$Eff  
CCLiq[is.na(CCLiq)] <- 0.0
#
# ---- READ DATA ----
DATA=read.csv("data/FossilDisplacement/DisplacementData.csv", sep=",", dec=".", stringsAsFactors = FALSE)

DATA$X <- NULL
DATA = subset(DATA, !(Year=="1990"|Year=="1995"|Year=="2000"|Year=="2005"|Year=="2010"|Year=="2015"|Year=="2110"|Year=="2130"|Year=="2150"))
BioDem.Total = subset(DATA, VARIABLE == "Secondary Energy|Biomass")
DATA = subset(DATA, !(VARIABLE == "Secondary Energy|Biomass"))
DATA$ID1 = paste(DATA$MODEL, DATA$SCENARIO, DATA$REGION)
DATA$ID2 = paste(DATA$MODEL, DATA$SCENARIO, DATA$REGION,DATA$Year)

cprice = subset(DATA, VARIABLE == "Price|Carbon")
cprice$Tech <- NULL
# ---- YEAR OF INTEREST ----
# Determine combination of MODEL-REGION for year of interest
  # Identify first year when carbon prices are above 100$/tCO2
Above100 = subset(cprice, value > 100)
Cross100 = aggregate(Above100$Year, by=list(ID1=Above100$ID1), FUN=min, na.rm=TRUE)
Cross100$ID2 = paste(Cross100$ID1, Cross100$x)

Cross100$ID3 = Cross100$ID2
Cross100$ID3 = gsub("R3-B-hi-cost100","R3-BASE-0-full",Cross100$ID3, fixed=F)
Cross100$ID3 = gsub("R3-B-hi-full","R3-BASE-0-full",Cross100$ID3, fixed=F)
Cross100$ID3 = gsub("R3-B-hi-ready2050","R3-BASE-0-full",Cross100$ID3, fixed=F)
Cross100$ID3 = gsub("R3-B-hi-limbio","R3-BASE-0-full",Cross100$ID3, fixed=F)
Cross100$ID3 = gsub("R3-B-lo-full","R3-BASE-0-full",Cross100$ID3, fixed=F)
Cross100$ID3 = gsub("R3-B-lo-ready2050","R3-BASE-0-full",Cross100$ID3, fixed=F)
Cross100$ID3 = gsub("R3-B-lo-cost100","R3-BASE-0-full",Cross100$ID3, fixed=F)
Cross100$ID3 = gsub("R3-B-lo-limbio","R3-BASE-0-full",Cross100$ID3, fixed=F)
Cross100$ID3 = gsub("R3-B-vlo-full","R3-BASE-0-full",Cross100$ID3, fixed=F)
Cross100$ID3 = gsub("R3-B-vlo-ready2050","R3-BASE-0-full",Cross100$ID3, fixed=F)
Cross100$ID3 = gsub("R3-B-vlo-cost100","R3-BASE-0-full",Cross100$ID3, fixed=F)
Cross100$ID3 = gsub("R3-B-vlo-limbio","R3-BASE-0-full",Cross100$ID3, fixed=F)

# Correct data to include (i) Mitigation AND Baseline scenario data for year of interest 
DATA.cor = subset(DATA, (ID2 %in% Cross100$ID2 | ID2 %in% Cross100$ID3) & !(VARIABLE=="Price|Carbon"))

rm(Above100, cprice)
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

Elec.shares = Elec.shares[,c(1:3,5:7,20:29)]
Elec.shares = melt(Elec.shares, id.vars=c("MODEL","SCENARIO","REGION","Year","ID1","ID2"))
Elec.shares$Tech = substr(Elec.shares$variable, start = 1, stop = 3)
Elec.NonBio = subset(Elec.shares, Tech == "Tot")
Elec.NonBio$Tech <- NULL
Elec.NonBio$Carrier <- "Electricity"

# Determine contribution of each energy carrier to secondary emission factor
Elec.shares = subset(Elec.shares, !(Tech=="Tot"))
Elec.shares$CC = CCElec[match(Elec.shares$Tech,CCElec$Tech),5]
Elec.shares = Elec.shares %>% mutate(CC_factor = value * CC)

# Determine final emission factor of secondary energy carrier by summing contribution of each primary feedstock
Elec.CC = aggregate(Elec.shares$CC_factor, by=list(ID2=Elec.shares$ID2), FUN=sum, na.rm=TRUE)
Elec.CC$Carrier <- "Electricity"

rm(Elec.shares)
#
# ---- *** LIQUIDS *** ----
# Determine share of fuels used for each carrier, ignoring biomass use
Liq.shares <- spread(Liquids, Tech, value)
Liq.shares[is.na(Liq.shares)] <- 0.0
Liq.shares = Liq.shares %>% mutate(Total_NonBio = Coal + Gas + Oil)

# Get shares of primary energy carriers
Liq.shares = Liq.shares %>% mutate(Coa_Share = Coal / Total_NonBio)  
Liq.shares = Liq.shares %>% mutate(Gas_Share = Gas / Total_NonBio)  
Liq.shares = Liq.shares %>% mutate(Oil_Share = Oil / Total_NonBio)  

Liq.shares = Liq.shares[,c(1:3,5:7,14:17)]
Liq.shares = melt(Liq.shares, id.vars=c("MODEL","SCENARIO","REGION","Year","ID1","ID2"))
Liq.shares$Tech = substr(Liq.shares$variable, start = 1, stop = 3)
Liq.NonBio = subset(Liq.shares, Tech == "Tot")
Liq.NonBio$Tech <- NULL
Liq.NonBio$Carrier <- "Liquids"
# Determine contribution of each energy carrier to secondary emission factor
Liq.shares = subset(Liq.shares, !(Tech=="Tot"))
Liq.shares$CC = CCLiq[match(Liq.shares$Tech,CCLiq$Tech),5]
Liq.shares = Liq.shares %>% mutate(CC_factor = value * CC)

# Determine final emission factor of secondary energy carrier by summing contribution of each primary feedstock
Liq.CC = aggregate(Liq.shares$CC_factor, by=list(ID2=Liq.shares$ID2), FUN=sum, na.rm=TRUE)
Liq.CC$Carrier <- "Liquids"

rm(Liq.shares)
#
# ---- USE OF BIOMASS IN MITIGATION SCENARIOS ----
# Use of Biomass
Elec.bio = subset(Electricity, Tech == "Biomass")
Liq.bio = subset(Liquids, Tech == "Biomass")

Elec.bio$Carrier <- "Electricity"
Liq.bio$Carrier <- "Liquids"

BioDem = rbind(Elec.bio, Liq.bio)
BioDem$ID3 = paste(BioDem$MODEL, BioDem$REGION, BioDem$Year, BioDem$Carrier)

rm(Elec.bio, Liq.bio,
   Electricity, Liquids)
# 
# ---- AVOIDED EMISSIONS ----
Liq.NonBio$CC = Liq.CC[match(Liq.NonBio$ID2,Liq.CC$ID2),"x"]
Elec.NonBio$CC = Elec.CC[match(Elec.NonBio$ID2,Elec.CC$ID2),"x"]

NonBioDemand = rbind(Liq.NonBio, Elec.NonBio)
NonBioDemand.base = subset(NonBioDemand, SCENARIO=="R3-BASE-0-full")
NonBioDemand.base$ID3 = paste(NonBioDemand.base$MODEL, NonBioDemand.base$REGION, NonBioDemand.base$Year, NonBioDemand.base$Carrier) 


rm(Elec.NonBio, Liq.NonBio)
# FINAL DATASET
# Unit of Baseline_CC is MtCO2/EJ-sec
FinalData <- BioDem
FinalData$Baseline_CC = NonBioDemand.base[match(FinalData$ID3, NonBioDemand.base$ID3),"CC"]
FinalData = FinalData[,c(1:8,10,12)]
colnames(FinalData)[colnames(FinalData) == 'value'] <- 'Biomass_EJ'
colnames(FinalData)[colnames(FinalData) == 'Baseline_CC'] <- 'Baseline_CC_MtCO2perEJ'
FinalData$UNIT <- NULL
FinalData$Tech <- NULL
FinalData = FinalData %>% mutate(Avoided_Emis_MtCO2 = Biomass_EJ * Baseline_CC_MtCO2perEJ)

# Remove FARM since it does not report Coal use in electricity and thus skews the results
FinalData = subset(FinalData, !(MODEL == "FARM 3.1"))

# Only interested in Global data. 
FinalData = subset(FinalData, (REGION == "World"))

# Remove cases where there is no biomass use as this skews results
FinalData = subset(FinalData, Biomass_EJ > 1)

# Remove Basleine Data as we only want mitigation scenario results
FinalData = subset(FinalData, !(SCENARIO=="R3-BASE-0-full"))

# Summing Electricity and Liquids
FinalData.Total<- aggregate(FinalData$Avoided_Emis_MtCO2, by=list(ID1=FinalData$ID1), FUN=sum, na.rm=TRUE) 
colnames(FinalData.Total)[colnames(FinalData.Total) == "x"] <- "Bioenergy_Mitigation_MtCO2"

# Total secondary bioenergy demand
BioDem.EleLiq = subset(BioDem, ID1 %in% FinalData.Total$ID1)
BioDem.EleLiq <- aggregate(BioDem.EleLiq$value, by=list(ID1=BioDem.EleLiq$ID1), FUN=sum, na.rm=TRUE) 
plot(density(BioDem.EleLiq$x))

#
# ---- TECHNICAL POTENTIALS ----
# Determine Technical potential of bioenergy fossil fuel displacement mitigation
#
# Use two different methods:
# 1. Technical potential of bioenergy based on Ar6 Ch7.4.4 technical potential for bioenergy
# 2. Technical potential based on Hanssen et al. (2020), bioelectricity-BECCS potential offering net-negative emissions within a 30 year timframe
#
# METHOD 1:
# Determine technical potential of bioenergy based on Ar6 Ch7.4.4 technical potential for bioenergy
# Potential based on sum of:
# Energy Crops:
#   Min: 46
#   Max: 245
# Residues:
#   Min: 4
#   Max: 74

TechPot1 <- data.frame(Prim_EJ_Min = 46+4,
                      Prim_EJ_Max = 245+74)

TechPot1$SecEle_EJ_Min = TechPot1$Prim_EJ_Min * Elec_Eff$Efficiency[Elec_Eff$Tech=="Bio"]
TechPot1$SecEle_EJ_Max = TechPot1$Prim_EJ_Max * Elec_Eff$Efficiency[Elec_Eff$Tech=="Bio"]
TechPot1$SecLiq_EJ_Min = TechPot1$Prim_EJ_Min * Liq_Eff$Efficiency[Liq_Eff$Tech=="Bio"]
TechPot1$SecLiq_EJ_Max = TechPot1$Prim_EJ_Max * Liq_Eff$Efficiency[Liq_Eff$Tech=="Bio"]

# METHOD 2:
# Technical potential based on Hanssen et al. (2020), bioelectricity-BECCS potential offering net-negative emissions within a 30 year timframe
# Potential:
# 28 EJ-Elec/yr - basic restult
# 59.2 EJ-Prim/yr - Residue availability (SI Table S4)
TechPot2 <- data.frame(Sec_EJ_Hanssen1 = 28,
                       Sec_EJ_Hanssen2 = 28 + (59.2 * Elec_Eff$Efficiency[Elec_Eff$Tech=="Bio"]))


# DETERMINE TECHNICAL MITIGATION POTENTIAL
# Do this far all model-scenario-regionitimestep combinations, initially. 

  # First get baseline Electricity and Liquids Carbon Contents
BaselineCC = subset(BioDem, select = c(MODEL,SCENARIO,REGION,Year,ID2,ID3))
BaselineCC = subset(BaselineCC, SCENARIO == "R3-BASE-0-full")
BaselineCC$Ele_CC_MtCO2perEJ = Elec.CC[match(BaselineCC$ID2, Elec.CC$ID2),"x"]
BaselineCC$Liq_CC_MtCO2perEJ = Liq.CC[match(BaselineCC$ID2, Elec.CC$ID2),"x"]

  # Make DF with Baseline Carbon contants for the baseline
TechPotMitigation = subset(DATA, select = c(MODEL,SCENARIO,REGION,Year,ID2))
TechPotMitigation = subset(TechPotMitigation, SCENARIO == "R3-BASE-0-full")
TechPotMitigation = unique(TechPotMitigation) #Removes some duplicate observations
TechPotMitigation$EleBaseline_CC_MtCO2perEJ = BaselineCC[match(TechPotMitigation$ID2,BaselineCC$ID2),"Ele_CC_MtCO2perEJ"]
TechPotMitigation$LiqBaseline_CC_MtCO2perEJ = BaselineCC[match(TechPotMitigation$ID2,BaselineCC$ID2),"Liq_CC_MtCO2perEJ"]
TechPotMitigation = melt(TechPotMitigation, id.vars=c("MODEL","SCENARIO","REGION","Year","ID2"))
TechPotMitigation$Carrier = substr(TechPotMitigation$variable, start = 1, stop = 3)
colnames(TechPotMitigation)[colnames(TechPotMitigation)=="value"] <- "Baseline_CC_MtCO2perEJ"
TechPotMitigation$variable <- NULL
# Get only for years where ctax ~ 100$/tCO2
TechPotMitigation = subset(TechPotMitigation, ID2 %in% Cross100$ID3)

  # Dwetermine mitigation potential based on biomass technical potentials
TechPotMitigation$Sec_EJ_Min[TechPotMitigation$Carrier=="Ele"] <- TechPot1$SecEle_EJ_Min 
TechPotMitigation$Sec_EJ_Max[TechPotMitigation$Carrier=="Ele"] <- TechPot1$SecEle_EJ_Max 
TechPotMitigation$Sec_EJ_Min[TechPotMitigation$Carrier=="Liq"] <- TechPot1$SecLiq_EJ_Min 
TechPotMitigation$Sec_EJ_Max[TechPotMitigation$Carrier=="Liq"] <- TechPot1$SecLiq_EJ_Max 
TechPotMitigation$Sec_EJ_Hanssen1[TechPotMitigation$Carrier=="Ele"] <- TechPot2$Sec_EJ_Hanssen1
TechPotMitigation$Sec_EJ_Hanssen2[TechPotMitigation$Carrier=="Ele"] <- TechPot2$Sec_EJ_Hanssen2
TechPotMitigation$Sec_EJ_Hanssen1[TechPotMitigation$Carrier=="Liq"] <- 0
TechPotMitigation$Sec_EJ_Hanssen2[TechPotMitigation$Carrier=="Liq"] <- 0

TechPotMitigation = TechPotMitigation %>% mutate(Tech_Avoided_Emis_Min_MtCO2 = Baseline_CC_MtCO2perEJ * Sec_EJ_Min )
TechPotMitigation = TechPotMitigation %>% mutate(Tech_Avoided_Emis_Max_MtCO2 = Baseline_CC_MtCO2perEJ * Sec_EJ_Max )
TechPotMitigation = TechPotMitigation %>% mutate(Tech_Avoided_Emis_Hanssen1_MtCO2 = Baseline_CC_MtCO2perEJ * Sec_EJ_Hanssen1 )
TechPotMitigation = TechPotMitigation %>% mutate(Tech_Avoided_Emis_Hanssen2_MtCO2 = Baseline_CC_MtCO2perEJ * Sec_EJ_Hanssen2 )

  # Summing Electricity and Liquids
TechPotMitigation.TotalMin<- aggregate(TechPotMitigation$Tech_Avoided_Emis_Min_MtCO2, by=list(ID2=TechPotMitigation$ID2), FUN=sum, na.rm=TRUE) 
TechPotMitigation.TotalMax<- aggregate(TechPotMitigation$Tech_Avoided_Emis_Max_MtCO2, by=list(ID2=TechPotMitigation$ID2), FUN=sum, na.rm=TRUE) 
TechPotMitigation.Hanssen1<- aggregate(TechPotMitigation$Tech_Avoided_Emis_Hanssen1, by=list(ID2=TechPotMitigation$ID2), FUN=sum, na.rm=TRUE) 
TechPotMitigation.Hanssen2<- aggregate(TechPotMitigation$Tech_Avoided_Emis_Hanssen2, by=list(ID2=TechPotMitigation$ID2), FUN=sum, na.rm=TRUE) 

  # Final dataset with technical mitigation potential, per IAM baseline carbon contents 
TechPotMitigation.Total = subset(DATA, select = c(MODEL,SCENARIO,REGION,Year,ID2))
TechPotMitigation.Total = subset(TechPotMitigation.Total, SCENARIO == "R3-BASE-0-full")
TechPotMitigation.Total = unique(TechPotMitigation.Total) #Removes some duplicate observations
TechPotMitigation.Total = subset(TechPotMitigation.Total, ID2 %in% Cross100$ID3)

TechPotMitigation.Total$Min_TechMitigation_MtCO2perYr = TechPotMitigation.TotalMin[match(TechPotMitigation.Total$ID2, TechPotMitigation.TotalMin$ID2),2]
TechPotMitigation.Total$Max_TechMitigation_MtCO2perYr = TechPotMitigation.TotalMax[match(TechPotMitigation.Total$ID2, TechPotMitigation.TotalMax$ID2),2]
TechPotMitigation.Total$Hanssen1_TechMitigation_MtCO2perYr = TechPotMitigation.Hanssen1[match(TechPotMitigation.Total$ID2, TechPotMitigation.Hanssen1$ID2),2]
TechPotMitigation.Total$Hanssen2_TechMitigation_MtCO2perYr = TechPotMitigation.Hanssen2[match(TechPotMitigation.Total$ID2, TechPotMitigation.Hanssen2$ID2),2]
TechPotMitigation.Total = melt(TechPotMitigation.Total, id.vars=c("MODEL","SCENARIO","REGION","Year","ID2"))

# Remove FARM since it does not report Coal use in electricity and thus skews the results
TechPotMitigation.Total = subset(TechPotMitigation.Total, !(MODEL == "FARM 3.1"))

# Only interested in Global data. 
TechPotMitigation.Total = subset(TechPotMitigation.Total, (REGION == "World"))

rm(Elec.CC, Liq.CC,
   TechPot1, TechPot2,
   TechPotMitigation, 
   TechPotMitigation.TotalMax, TechPotMitigation.TotalMin,
   TechPotMitigation.Hanssen1, TechPotMitigation.Hanssen2)
#
# ---- SUMMARY STATISTICS ----
# Total Biomass Mitigation 
  # Economic Potential
png(file = "output/FossilDisplacement/density_economic_pot.png", width = 5*ppi, height = 5*ppi, units = "px", res = ppi)
plot(density(FinalData.Total$Bioenergy_Mitigation_MtCO2), 
     main = "Mitigation Bioenergy Use @ 100$/tCO2",
     xlab = "Mt-CO2",
     cex.lab = 1,
     cex.axis = 1,
     cex.main = 1)
dev.off()

summary_stats_economic <- data.frame(Percentile = c("5th perc.","10th perc.","25th perc.","50th perc.","75th perc.","90th perc.","95th perc."),
                                  "Bioenergy_Mitigation_MtCO2peryr" = quantile(FinalData.Total$Bioenergy_Mitigation_MtCO2, probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)))

  # Technical Potential
TechPotMitigation.Final = subset(TechPotMitigation.Total, variable=="Hanssen2_TechMitigation_MtCO2perYr") # Biomass potential from Hanssen et al. 2020, including residues

png(file = "output/FossilDisplacement/density_technical_pot.png", width = 5*ppi, height = 5*ppi, units = "px", res = ppi)
plot(density(TechPotMitigation.Final$value),
     main = "Technical Potnetial of Bioenergy Mitigation",
     xlab = "Mt-CO2",
     cex.lab = 1,
     cex.axis = 1,
     cex.main = 1)
dev.off()

summary_stats_technical <- data.frame(Percentile = c("5th perc.","10th perc.","25th perc.","50th perc.","75th perc.","90th perc.","95th perc."),
                                 "Bioenergy_Technical_Mitigation_Potential_MtCO2peryr" = quantile(TechPotMitigation.Final$value, probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)))
#
# ---- PRIMARY BIOMASS ----
# Some biomass is used for secondary energy other than electricity and liquids (i.e. gasses, hydrogen, heat, etc.). 
# Have to correct for this.
# Determine share of secondary bioenergy which is used for ELECTRICITY or LIQUIDS only, scale primary biomass demand to that
BioDem.Total$ID1 = paste(BioDem.Total$MODEL, BioDem.Total$SCENARIO, BioDem.Total$REGION)
BioDem.Total$ID2 = paste(BioDem.Total$MODEL, BioDem.Total$SCENARIO, BioDem.Total$REGION, BioDem.Total$Year)
BioDem.Total = subset(BioDem.Total, ID2 %in% Cross100$ID2)

# Share of bio-based electricity and liquids in total bioenergy
BioDem.EleLiqShare = BioDem.EleLiq
BioDem.EleLiqShare$Total_SecBio = BioDem.Total[match(BioDem.EleLiqShare$ID1, BioDem.Total$ID1),"value"]
BioDem.EleLiqShare = BioDem.EleLiqShare %>% mutate(Share_EleLiq = x / Total_SecBio)
BioDem.EleLiqShare$Share_EleLiq[BioDem.EleLiqShare$Share_EleLiq > 1] <- 1

# Scale primary biomass to this share
BioPrim = subset(DATA, VARIABLE == "Primary Energy|Biomass|Modern")
BioPrim.cor = subset(BioPrim, ID2 %in% Cross100$ID2)
BioPrim.cor = subset(BioPrim.cor, ID1 %in% BioDem.EleLiqShare$ID1)
BioPrim.cor = subset(BioPrim.cor, select = c ("MODEL","SCENARIO","REGION","VARIABLE","UNIT","Year","value","ID1"))
BioPrim.cor$Share_EleLiq = BioDem.EleLiqShare[match(BioPrim.cor$ID1,BioDem.EleLiqShare$ID1),"Share_EleLiq"]
BioPrim.cor = BioPrim.cor %>% mutate(BioPrim_cor = value * Share_EleLiq)
#
# ---- FINAL DATASET ----
BioMitigation = DATA.cor
BioMitigation = subset(BioMitigation, ID1 %in% FinalData.Total$ID1)
BioMitigation$Mitigation_MtCO2perYr = FinalData.Total[match(BioMitigation$ID1, FinalData.Total$ID1),2]
BioMitigation$SecBioenergy_EJperYr = BioDem.EleLiq[match(BioMitigation$ID1, BioDem.EleLiq$ID1),2]
BioMitigation$PrimBiomass_EJperYr = BioPrim.cor[match(BioMitigation$ID1, BioPrim.cor$ID1),"BioPrim_cor"]
BioMitigation = subset(BioMitigation, select=c("MODEL","SCENARIO","REGION","Mitigation_MtCO2perYr","SecBioenergy_EJperYr","PrimBiomass_EJperYr"))
BioMitigation = unique(BioMitigation) #Removes some duplicate observations

# ---- FIGURES ----
# ---- Economic Potential Boxplot + Jitter per Carrier  ----
boxplot.c<-ggplot(data = subset(FinalData, !(REGION == "WORLD")),
                aes(x = Carrier, y = Avoided_Emis_MtCO2)) + 
  geom_boxplot() +
  geom_jitter(width=0.1, alpha = 0.75) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("Avoided MtCO"[2],"/GJ-Prim"))) + 
  xlab("") 
boxplot.c

# ---- Economic Potential Boxplot + Jitter TOTAL  ----
boxplot.t<-ggplot(data = BioMitigation,
                aes(x = factor(0), y = Mitigation_MtCO2perYr)) + 
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(aes(colour = MODEL),width=0.2, alpha = 0.75, size=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  xlab("") + ylab(expression(paste("Avoided MtCO"[2])))
boxplot.t

# ---- Technical Potential Boxplot + Jitter TOTAL  ----
boxplot.tech<-ggplot(data = TechPotMitigation.Total,
                  aes(x = variable, y = value)) + 
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(aes(colour = MODEL), width=0.2, alpha = 0.75, size=0.5) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab(expression(paste("Avoided MtCO"[2])))
boxplot.tech

# ---- OUTPUTS ----
# wb <- createWorkbook()
# 
# addWorksheet(wb, "Avoided Emissions")
# writeDataTable(wb, sheet = "Avoided Emissions", x = BioMitigation)
# 
# addWorksheet(wb, "Economic Potential Percentiles")
# writeDataTable(wb, sheet = "Economic Potential Percentiles", x = summary_stats_economic)
# 
# addWorksheet(wb, "Technical Potential Percentiles")
# writeDataTable(wb, sheet = "Technical Potential Percentiles", x = summary_stats_technical)
# 
# saveWorkbook(wb, "output/FossilDisplacement/EMF-33_AvoidedEmissions.xlsx", overwrite = TRUE)

# #
# png(file = "output/FossilDisplacement/Boxplot_carriers.png", width = 3*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(boxplot.c)
# dev.off()
# # # #
# png(file = "output/FossilDisplacement/Boxplot_total.png", width = 3*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(boxplot.t)
# dev.off()
# 
# png(file = "output/FossilDisplacement/TechnicalPot_Boxplot_total.png", width = 5*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(boxplot.tech)
# dev.off()
# 
# 
