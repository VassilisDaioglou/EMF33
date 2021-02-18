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

rm(Above100)
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

#
# ---- USE OF BIOMASS IN MITIGATION SCENARIOS ----
# Use of Biomass
Elec.bio = subset(Electricity, Tech == "Biomass")
Liq.bio = subset(Liquids, Tech == "Biomass")

Elec.bio$Carrier <- "Electricity"
Liq.bio$Carrier <- "Liquids"

BioDem = rbind(Elec.bio, Liq.bio)
BioDem$ID3 = paste(BioDem$MODEL, BioDem$REGION, BioDem$Year, BioDem$Carrier)

# 
# ---- AVOIDED EMISSIONS ----
Liq.NonBio$CC = Liq.CC[match(Liq.NonBio$ID2,Liq.CC$ID2),"x"]
Elec.NonBio$CC = Elec.CC[match(Elec.NonBio$ID2,Elec.CC$ID2),"x"]

NonBioDemand = rbind(Liq.NonBio, Elec.NonBio)
NonBioDemand.base = subset(NonBioDemand, SCENARIO=="R3-BASE-0-full")
NonBioDemand.base$ID3 = paste(NonBioDemand.base$MODEL, NonBioDemand.base$REGION, NonBioDemand.base$Year, NonBioDemand.base$Carrier) 

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

# Remove FARM since it does not report Coal use in China and thus skew the results
FinalData = subset(FinalData, !(MODEL == "FARM 3.1"))

FinalData = subset(FinalData, (REGION == "World"))

# Remove cases where there is no biomass use as this skews results
FinalData = subset(FinalData, Biomass_EJ > 1)

# Remove Basleine Data as we only want mitigation scenario results
FinalData = subset(FinalData, !(SCENARIO=="R3-BASE-0-full"))

# Summing Electricity and Liquids
FinalData.Total<- aggregate(FinalData$Avoided_Emis_MtCO2, by=list(ID1=FinalData$ID1), FUN=sum, na.rm=TRUE) 
colnames(FinalData.Total)[colnames(FinalData.Total) == "x"] <- "Bioenergy_Mitigation_MtCO2"
#
# ---- SUMMARY STATISTICS ----
# Separated for Electricity and liquids
meds <- aggregate(FinalData$Avoided_Emis_MtCO2, by=list(Carrier=FinalData$Carrier), FUN=median, na.rm=TRUE) 
P5 <- aggregate(FinalData$Avoided_Emis_MtCO2, by=list(Carrier=FinalData$Carrier), FUN=quantile, probs=0.05, na.rm=TRUE) 
P25 <- aggregate(FinalData$Avoided_Emis_MtCO2, by=list(Carrier=FinalData$Carrier), FUN=quantile, probs=0.25, na.rm=TRUE) 
P75 <- aggregate(FinalData$Avoided_Emis_MtCO2, by=list(Carrier=FinalData$Carrier), FUN=quantile, probs=0.75, na.rm=TRUE) 
P95 <- aggregate(FinalData$Avoided_Emis_MtCO2, by=list(Carrier=FinalData$Carrier), FUN=quantile, probs=0.95, na.rm=TRUE) 

colnames(meds)[2] <- "Median"
colnames(P5)[2] <- "5th Percentile"
colnames(P25)[2] <- "25th Percentile"
colnames(P75)[2] <- "75th Percentile"
colnames(P95)[2] <- "95th Percentile"

summary_stats_carrier = merge(meds,P5,by=c("Carrier"))
summary_stats_carrier = merge(summary_stats,P25,by=c("Carrier"))
summary_stats_carrier = merge(summary_stats,P75,by=c("Carrier"))
summary_stats_carrier = merge(summary_stats,P95,by=c("Carrier"))

plot(density(FinalData$Avoided_Emis_MtCO2))

# Total Biomass Mitigation 
quantiles = quantile(FinalData.Total$Bioenergy_Mitigation_MtCO2)
plot(density(FinalData.Total$Bioenergy_Mitigation_MtCO2))

summary_stats_total <- data.frame(c("5th perc.","10th perc.","25th perc.","50th perc.","75th perc.","90th perc.","95th perc."),
                      quantile(FinalData.Total$Bioenergy_Mitigation_MtCO2, probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)))
colnames(summary_stats_total) <- c("Percentile","Bioenergy Mitigation: MtCO2/yr")

#
# ---- FINAL DATASET ----
BioMitigation = DATA.cor
BioMitigation = subset(BioMitigation, ID1 %in% FinalData.Total$ID1)
BioMitigation$Bioenergy_Mitigation = FinalData.Total[match(BioMitigation$ID1, FinalData.Total$ID1),2]
BioMitigation = subset(BioMitigation, select=c("MODEL","SCENARIO","REGION","Bioenergy_Mitigation"))
BioMitigation = unique(BioMitigation) #Removes some duplicate observations
BioMitigation$UNIT <- "Mitigated MtCO2/yr"

# ---- FIGURES ----
# ---- Boxplot + Jitter per Carrier  ----
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

# ---- Boxplot + Jitter TOTAL  ----
boxplot.t<-ggplot(data = BioMitigation,
                aes(x = factor(0), y = Bioenergy_Mitigation)) + 
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
  xlab("") + ylab(expression(paste("Avoided MtCO"[2]))) + 
  xlab("") 
boxplot.t

# ---- OUTPUTS ----
# # ---- SUPPLEMENTARY DATA OUTPUT ----
# wb <- createWorkbook()
# 
# addWorksheet(wb, "Avoided Emissions")
# writeDataTable(wb, sheet = "Avoided Emissions", x = BioMitigation)
# 
# addWorksheet(wb, "Percentiles")
# writeDataTable(wb, sheet = "Percentiles", x = summary_stats_total)
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
# png(file = "output/FossilDisplacement/distribution.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(density(FinalData.Total$Bioenergy_Mitigation_MtCO2), cex.main = 0.75, cex.axis=0.75, cex.lab=0.75)
# dev.off()
