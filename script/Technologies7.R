# R script for EMF33 Bio-Technologies Paper
# ----START----
# clear memory
rm(list=ls()) 

# Load Libraries
library(reshape2);
library(ggplot2);
library(data.table);
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(ggpubr)
library(gridExtra)
library(xlsx)

# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# ---- INPUTS ----
ppi <- 600
# ACTIVE SCENARIO FOR THIS SCRIPT
ActScen = "R3-B-lo-full"

# set directory path for csv file
#setwd("~/disks/y/ontwapps/Timer/Users/Vassilis/Projects - Documents/EMF33/Scenario results/R-Scripts")
setwd("C:/Users/Asus/Documents/GitHub/EMF33")

# Read Data Files
TechData2=read.csv("data/Technology/TechDATA.csv", sep=",", dec=".", stringsAsFactors = FALSE)
PriceData=read.csv("data/Technology/PriceDATA.csv", sep=",", dec=".", stringsAsFactors = FALSE)
SecEnTot=read.csv("data/Technology/SecEnTot.csv", sep=",", dec=".", stringsAsFactors = FALSE)
BakerDat=read.csv("data/Technology/BakerElicitation.csv", sep=",", dec=".", stringsAsFactors = FALSE)
TechData2$X <-NULL
PriceData$X <-NULL
SecEnTot$X <- NULL

# ---- CONSTANTS ----
# For figures
fontsize1 = 7 # For Panels, legends and axes (labels)
fontsize2 = 6 # For axes (scale)
fontsize3 = 8 #For chart titles
arrowsize = 0.15
#Capital recovery Factor assumes a rate of 2% for 20 years
DiscountRate = 0.05
# Convert per-GJ to per-MWh
GJ2MWh = 3.6

# Default Capture rates
TechData2$CaptID = paste(TechData2$CarrierID,TechData2$Capt)
CapRate <- data.frame(c("Gas woCCS","Hea woCCS","Hyd woCCS","Hyd wCCS","Liq woCCS","Liq wCCS","Ele woCCS","Ele wCCS","Hea wCCS" ),
                      c(0,0,0,0.95,0,0.45,0,0.95,0.95))
colnames(CapRate) <- c("CaptID","CaptureRate")

# Feedstock price in $/MWh (ONLY FOR MODELS THAT DO NOT SUBMIT RESULTS)
# first get rid of negative values
TechData2$FeedCost[TechData2$FeedCost < 0] <-NA
# From remaining costs, get mean price across models and scenarios
FeedPriceMed = subset(TechData2, !(FeedCost=="NA"))
FeedPriceMed <- aggregate(FeedPriceMed$FeedCost, by=list(SCENARIO=FeedPriceMed$SCENARIO,
                                                         REGION=FeedPriceMed$REGION,
                                                         Year=FeedPriceMed$Year,
                                                         Prim=FeedPriceMed$Prim), FUN=mean, na.rm=F)
# Assign to missing FeedCost
TechData2$PrimPriceID=paste(TechData2$SCENARIO, TechData2$REGION, TechData2$Year, TechData2$Prim)
FeedPriceMed$PrimPriceID=paste(FeedPriceMed$SCENARIO, FeedPriceMed$REGION, FeedPriceMed$Year, FeedPriceMed$Prim)

TechData2.temp <- TechData2[is.na(TechData2$FeedCost),]
TechData2.temp2 <- TechData2[!is.na(TechData2$FeedCost),]

TechData2.temp$FeedCost <- FeedPriceMed[match(TechData2.temp$PrimPriceID, FeedPriceMed$PrimPriceID),5]
TechData2 = rbind(TechData2.temp,TechData2.temp2)
rm(FeedPriceMed)
# Convert all $/GJ to $/MWh
TechData2 = TechData2 %>% mutate(FeedCost2 = FeedCost * GJ2MWh)
TechData2 = subset(TechData2, select=-c(PrimPriceID,FeedCost))

# Carbon Content in kgCO2/MWh
CCPrim <- data.frame(c("Biomass","BiomasswCCS","Coal","CoalwCCS","Gas","GaswCCS","Geothermal","Hydro","Nuclear","Solar","Wind","Ocean","Electricity","Oil"),
                                               c(0, 0,           # Biomass (& wCCS) = Assume no LUC here
                                                 353.8, 353.8,   # Coal (& wCCS)
                                                 221.76, 221.76, # Gas (& wCCS)
                                                 0,              # Geothermal
                                                 0,              # Hydro
                                                 0,              # Nuclear
                                                 0,              # Solar
                                                 0,              # Wind
                                                 0,              # Ocean
                                                 770,            # Electricity
                                                 249.5))         # Oil 
colnames(CCPrim) <- c("Prim","CC")
# Carbon dioxide removal potential in kgCO2/MWh
CDRPrim <- data.frame(c("Biomass","BiomasswCCS","Coal","CoalwCCS","Gas","GaswCCS","Geothermal","Hydro","Nuclear","Solar","Wind","Ocean","Electricity","Oil"),
                                                c(0, 394.68,     # Biomass (& wCCS) = Assume no LUC here
                                                 0, 0,           # Coal (& wCCS)
                                                 0, 0,           # Gas (& wCCS)
                                                 0,              # Geothermal
                                                 0,              # Hydro
                                                 0,              # Nuclear
                                                 0,              # Solar
                                                 0,              # Wind
                                                 0,              # Ocean
                                                 0,              # Electricity
                                                 0))             # Oil 
colnames(CDRPrim) <- c("Prim","CC")
# Capacity Factor
CF <- data.frame(c("Biomass","BiomasswCCS","Coal","CoalwCCS","Gas","GaswCCS","Geothermal","Hydro","Nuclear","Solar","Wind","Ocean","Electricity","Oil"),
                                           c(    0.85, 0.85, # Biomass (& wCCS)
                                                 0.85, 0.85, # Coal (& wCCS)
                                                 0.85, 0.85, # Gas (& wCCS)
                                                 0.9,        # Geothermal
                                                 0.5,        # Hydro
                                                 0.85,       # Nuclear
                                                 0.15,       # Solar
                                                 0.3,        # Wind
                                                 0.85,       # Electricity
                                                 0.5,       # Ocean
                                                 0.85))      # Oil 
colnames(CF) <- c("Prim","CF")

# LifeTime
Lifetime <- data.frame(c("Gas","Hea","Hyd","Liq","Ele"),
                      c(25,25,25,25,30))
colnames(Lifetime) <- c("CarrierID","Lifetime")

#Add new columns
TechData2$CaptureRate <- CapRate[match(TechData2$CaptID, CapRate$CaptID),2]
TechData2$CCPrim <- CCPrim[match(TechData2$Prim, CCPrim$Prim),2]
TechData2$CDRPrim <- CDRPrim[match(TechData2$Prim, CDRPrim$Prim),2]
TechData2$CapFac <- CF[match(TechData2$Prim, CF$Prim),2]
TechData2$Lifetime <- Lifetime[match(TechData2$CarrierID, Lifetime$CarrierID),2]

rm(TechData2.temp,TechData2.temp2,CCPrim,CDRPrim,CapRate,CF,Lifetime)

# Add (overwrite) constants that were provided in questionnaires
TechData3 = TechData2
TechData3$QID <- gsub("2|3|4","",TechData3$VARIABLE,fixed=F)
TechData3$QID2 = paste(TechData3$MODEL,TechData3$QID)
TechDataQuest=read.xlsx("data/Technology/QuestionnaireDATA.xlsx", sheetIndex=1)
TechDataQuest$X <-NULL
TechDataQuest$CaptureRate=pmax(TechDataQuest$CapRate,TechDataQuest$CapRateCor)
TechDataQuest = melt(TechDataQuest, id.vars=c("MODEL","CARRIER","CarrierID","Capt","Prim"), na.rm=TRUE)
TechDataQuest$QID2 = paste(TechDataQuest$MODEL, TechDataQuest$CARRIER)

CaptureRates = subset(TechDataQuest, (Prim=="Biomass"|Prim=="BiomasswCCS")&variable=="CapRate"&Capt=="wCCS"&value>0&(!CarrierID=="Hea")&(!MODEL=="COFFEE"))

l=0
for (i in unique(TechDataQuest$QID2)){
  l=l+1
  if(length(TechDataQuest$value[TechDataQuest$QID2==i & TechDataQuest$variable=="CCPrim"])>0) # only accept combinations that work
  { 
    TechData3$CCPrim[TechData3$QID2==i] <- TechDataQuest$value[TechDataQuest$QID2==i & TechDataQuest$variable=="CCPrim"]
  }
  if(length(TechDataQuest$value[TechDataQuest$QID2==i & TechDataQuest$variable=="CDRPrim"])>0) # only accept combinations that work
  { 
    TechData3$CDRPrim[TechData3$QID2==i] <- TechDataQuest$value[TechDataQuest$QID2==i & TechDataQuest$variable=="CDRPrim"]
  }
    if(length(TechDataQuest$value[TechDataQuest$QID2==i & TechDataQuest$variable=="CaptureRate"])>0) # only accept combinations that work
  { 
    TechData3$CaptureRate[TechData3$QID2==i] <- TechDataQuest$value[TechDataQuest$QID2==i & TechDataQuest$variable=="CaptureRate"]
  }
    if(length(TechDataQuest$value[TechDataQuest$QID2==i & TechDataQuest$variable=="Lifetime"])>0) # only accept combinations that work
  { 
    TechData3$Lifetime[TechData3$QID2==i] <- TechDataQuest$value[TechDataQuest$QID2==i & TechDataQuest$variable=="Lifetime"]
  }
}

#TechData
rm(TechDataQuest,i,l)
TechData3 = subset(TechData3, select=-c(CaptID,QID,QID2))

# ---- CORRECT VARS ----
TechData4 = TechData3

# Assign "Liquids|Biomass|Other for specific models
# For MESSAGE: Ethanol synthesis via biomass gasification (email Matt Gidden 26/03/2018)
# For GRAPE: That is biomass part of CBTL (coal-biomass to liquids),it could be "advanced biofuel" using FT processes (emails Etsushi Kato 26&27/03/2018)
# For GRAPE, LiquidsBiomassOtherwCCS is not used, and in the questionnaires no liquids+CCS option is listed. So will remove from dataset
# For IMACLIM: "Other" includes a mix of 1st generation ethanol and biodiesel, as those are not separated in the model (email Florian Leblanc 27/03/2018)
# For DNE21+: "Other" includes a mix of 1st generation and advanced biofuels, as those are not separated in the model (email Fuminori Sano 26/09/2018)
# For IMAGE: It includes FT diesel
TechData4$VARIABLE2 = TechData4$VARIABLE
TechData4$VARIABLE2[TechData4$MODEL=="MESSAGE-GLOBIOM"&TechData4$VARIABLE=="LiquidsBiomassOtherwCCS"] <- "LiquidsBiomassCellulosicNondieselwCCS"
TechData4$VARIABLE2[TechData4$MODEL=="MESSAGE-GLOBIOM"&TechData4$VARIABLE=="LiquidsBiomassOtherwoCCS"] <- "LiquidsBiomassCellulosicNondieselwoCCS"

TechData4$VARIABLE2[TechData4$MODEL=="GRAPE-15"&TechData4$VARIABLE=="LiquidsBiomassOtherwCCS"] <- "LiquidsBiomassCellulosicNondieselwCCS2"
TechData4$VARIABLE2[TechData4$MODEL=="GRAPE-15"&TechData4$VARIABLE=="LiquidsBiomassOtherwoCCS"] <- "LiquidsBiomassCellulosicNondieselwoCCS2"
TechData4 = subset(TechData4, !(MODEL=="GRAPE-15"&VARIABLE2=="LiquidsBiomassCellulosicNondieselwCCS2"))

TechData4$VARIABLE2[TechData4$MODEL=="IMACLIM-NLU"&TechData4$VARIABLE=="LiquidsBiomassOtherwoCCS"] <- "LiquidsBiomassBiodieselwoCCS"

TechData4$VARIABLE2[TechData4$MODEL=="DNE21+ V.14"&TechData4$VARIABLE=="LiquidsBiomassOtherwoCCS"] <- "LiquidsBiomassCellulosicNondieselwoCCS2"

TechData4$VARIABLE2[TechData4$MODEL=="IMAGE"&TechData4$VARIABLE=="LiquidsBiomassOtherwCCS"] <- "LiquidsBiomassCellulosicNondieselwCCS5"
TechData4$VARIABLE2[TechData4$MODEL=="IMAGE"&TechData4$VARIABLE=="LiquidsBiomassOtherwoCCS"] <- "LiquidsBiomassCellulosicNondieselwoCCS5"
TechData4$VARIABLE2[TechData4$MODEL=="IMAGE"&TechData4$VARIABLE=="LiquidsBiomassOtherwCCS2"] <- "LiquidsBiomassCellulosicNondieselwCCS6"
TechData4$VARIABLE2[TechData4$MODEL=="IMAGE"&TechData4$VARIABLE=="LiquidsBiomassOtherwoCCS2"] <- "LiquidsBiomassCellulosicNondieselwoCCS6"

TechData4$VARIABLE <- NULL
names(TechData4)[names(TechData4)=="VARIABLE2"] <- "VARIABLE"

# For BET and GRAPE, Liquids, There is no explicit capital cost, so OMcostVa is used as a proxy
# $/GJ * GJ/MWh * h * MW/kW * CapFac = $/kW
TechData4$CapitalCo[TechData4$MODEL=="BET"&TechData4$CarrierID=="Liq"] = TechData4$OMCostVa[TechData4$MODEL=="BET"&TechData4$CarrierID=="Liq"] * GJ2MWh * 8760 * (1/1000) * TechData4$CapFac[TechData4$MODEL=="BET"&TechData4$CarrierID=="Liq"]
TechData4$OMCostVa[TechData4$MODEL=="BET"&TechData4$CarrierID=="Liq"] = 0.0

TechData4$CapitalCo[TechData4$MODEL=="GRAPE-15"&TechData4$CarrierID=="Liq"] = TechData4$OMCostVa[TechData4$MODEL=="GRAPE-15"&TechData4$CarrierID=="Liq"] * GJ2MWh * 8760 * (1/1000) * TechData4$CapFac[TechData4$MODEL=="GRAPE-15"&TechData4$CarrierID=="Liq"]
TechData4$OMCostVa[TechData4$MODEL=="GRAPE-15"&TechData4$CarrierID=="Liq"] = 0.0

#Correct Efficiency of renewables (=1)
TechData4$Efficiency[TechData4$Prim=="Geothermal"|TechData4$Prim=="Hydro"|TechData4$Prim=="Nuclear"|TechData4$Prim=="Solar"|TechData4$Prim=="Wind"|TechData4$Prim=="Ocean"] <- 1

#Correct efficiency for oil based liquids which are NA (set to 1). This is important for IMAGE and IMACLIM which do not report efficiencies
TechData4$Efficiency[TechData4$Prim=="Oil" & TechData4$CarrierID=="Liq" & is.na(TechData4$Efficiency)] <- 1

# Remove Observations that either make no sense 
TechData5 = TechData4
TechData5=subset(TechData5, !(Efficiency=="NA"))
TechData5=subset(TechData5, !(CapitalCo=="NA"))
TechData5=subset(TechData5, Efficiency>0.03)

# Years
TechData5 = subset(TechData5, Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2080"|Year=="2090"|Year=="2100")

# Scenarios
TechData5 = subset(TechData5, SCENARIO =="R3-BASE-0-full"|
                         SCENARIO=="R3-B-hi-full"|
                         SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-lo-limbio"|SCENARIO=="R3-B-lo-cost100"|SCENARIO=="R3-B-lo-ready2050"|
                         SCENARIO=="R3-B-vlo-full")

# Replace NAs with 0
TechData5$OMCostFi[is.na(TechData5$OMCostFi)] <- 0
TechData5$OMCostVa[is.na(TechData5$OMCostVa)] <- 0
TechData5$SecEn[is.na(TechData5$SecEn)] <- 0
TechData5$Ctax[is.na(TechData5$Ctax)] <- 0

# Do not allow negative values of OMVar (relevant for IMAGE) and Capacity
TechData5$OMCostVa[TechData5$OMCostVa<0]<-0
TechData5$SecEn[TechData5$SecEn<0]<-0

# Make sure relevant columns are numeric
TechData5$CapitalCo = as.numeric(substr(TechData5$CapitalCo, start=1, stop=5))
TechData5$Efficiency = as.numeric(substr(TechData5$Efficiency, start=1, stop=5))
TechData5$OMCostFi = as.numeric(substr(TechData5$OMCostFi, start=1, stop=5))
TechData5$OMCostVa = as.numeric(substr(TechData5$OMCostVa, start=1, stop=5))
TechData5$Ctax = as.numeric(substr(TechData5$Ctax, start=1, stop=5))
TechData5$FeedCost2 = as.numeric(substr(TechData5$FeedCost2, start=1, stop=5))
TechData5$CaptureRate = as.numeric(substr(TechData5$CaptureRate, start=1, stop=5))
TechData5$Lifetime= as.numeric(substr(TechData5$Lifetime, start=1, stop=5))

# ---- CALC: LCOE ----
# CALCULATION: LEVELISED COST OF ELECTRICITY (LCOE - $/MWh)
# LCOE = Var1 + Var2 + Var3 + Var 4 + Var 5 + Var 6
# Var 1 = Feedstock Cost = BiomassCost / Eff[tech,mod,scen] [$/MWh]
# Var 2 = Capital Cost = (1000*CapitalCost)/(8760*CF) [$/MWh]
# Var 3 = OMFixed Cost [$/MWh]
# Var 4 = OMVar Cost * GJ2MWh [$/MWh]
# Var 5 = Ctax * ((CC(Prim)/Eff * (1-CapRate)) + CC(Bio-conv))
# Var 6 = -1 * Ctax * CC(bio)/Eff * (CapRate)
TechData5 = TechData5 %>% mutate(alpha = DiscountRate/(1-(1+DiscountRate)^-Lifetime))
TechData5 = TechData5 %>% mutate(LCOE_Feed = FeedCost2/Efficiency)
TechData5 = TechData5 %>% mutate(LCOE_Cap  = ((1000*CapitalCo)/(8760 * CapFac)) * alpha)
TechData5 = TechData5 %>% mutate(LCOE_OM   = ((1000*OMCostFi)/(8760 * CapFac)) + (OMCostVa * GJ2MWh))
TechData5 = TechData5 %>% mutate(LCOE_ctax = (Ctax/1000) * (CCPrim/Efficiency) * (1-CaptureRate))
TechData5 = TechData5 %>% mutate(LCOE_CDR  = -1 * (Ctax/1000) * (CDRPrim/Efficiency) * (CaptureRate))
TechData5 = TechData5 %>% mutate(LCOE = LCOE_Feed + LCOE_Cap + LCOE_OM + LCOE_ctax + LCOE_CDR)

TechData5$LCOE = as.numeric(substr(TechData5$LCOE, start=1, stop=5))
TechData5$LCOE_Cor = paste(TechData5$LCOE)
TechData5$LCOE_Cor[TechData5$LCOE_Cor<0]<-0
TechData5$LCOE_Cor = as.numeric(substr(TechData5$LCOE_Cor, start=1, stop=5))
rm(DiscountRate)
#
# ---- LABELS ----
# Model labels in plots
uniqueInitials <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
initialShapes <- unlist(lapply(uniqueInitials, utf8ToInt))

scen_labels <- c("R3-B-hi-full"="hi-full","R3-B-hi-ready2050"="hi-ready2050","R3-B-hi-cost100"="hi-cost100","R3-B-hi-limbio"="hi-limbio","R3-B-lo-full"="lo-full","R3-B-lo-ready2050"="lo-ready2050","R3-B-lo-cost100"="lo-cost100","R3-B-lo-limbio"="lo-limbio")
#Model labels with text wraps                
model_labels <- c("AIM/CGE"="AIM/CGE","BET"="BET","COFFEE"="COFFEE","DNE21+ V.14"="DNE21","FARM 3.1"="FARM","MESSAGE-GLOBIOM"="MESSAGEix-\nGLOBIOM","GCAM_EMF33"="GCAM","GRAPE-15"="GRAPE","IMACLIM-NLU"="IMACLIM","IMAGE"="IMAGE","POLES EMF33"="POLES","REMIND-MAGPIE"="REMIND-\nMAgPIE")
#Model labels without text wraps                
model_labels2 <- c("AIM/CGE"="AIM/CGE","BET"="BET","COFFEE"="COFFEE","DNE21+ V.14"="DNE21","FARM 3.1"="FARM","MESSAGE-GLOBIOM"="MESSAGEix-GLOBIOM","GCAM_EMF33"="GCAM","GRAPE-15"="GRAPE","IMACLIM-NLU"="IMACLIM","IMAGE"="IMAGE","POLES EMF33"="POLES","REMIND-MAGPIE"="REMIND-MAgPIE")

eletech_labeler <- c("Geothermal"="Geothermal", "Hydro"="Hydro","Nuclear"="Nuclear","Solar"="Solar (PV&CSP)","Wind"="Wind (On/off-shore)","BiomasswCCS"="Biomass w/ CCS","Biomass"="Biomass","CoalwCCS"="Coal w/ CCS","Coal"="Coal","GaswCCS"="Gas w/ CCS","Gas"="Gas")

carrier_labels <- c("Ele"="Bio-Electricity","Liq"="Bio-Liquids","Hyd"="Bio-Hydrogen","Gas"="Bio-Gas","Ele woCCS"="Bio-Electricity","Liq woCCS"="Bio-Liquids","Hyd woCCS"="Bio-Hydrogen","Gas woCCS"="Bio-Gas","Ele wCCS"="Bio-Electricity w/CCS","Liq wCCS"="Bio-Liquids w/CCS","Hyd wCCS"="Bio-Hydrogen w/CCS")

scen_labels <- c("hi-"="High","lo-"="Low","vlo"="Very Low","SE-"="Baseline")

TechData6=TechData5
TechData6$Mitig <- substr(TechData6$SCENARIO, start=6, stop=8)

TechData6$ScenOrder = factor(TechData6$Mitig, levels=c("SE-","hi-","lo-","vlo"))

Biotech_labeler <- c("1st gen. ethanol" = "1st Gen. Eth.",
                     "Biodeisel"        = "Biodiesel",
                     "Lignocellulosic"  = "Adv. Biofuel",
                     "Other biomass"    = "Other Biofuel",
                     "Electricity"      = "Electricity",
                     "Hydrogen"         = "Hydrogen",
                     "1st gen. ethanolwCCS" = "1st Gen. Eth \nw/CCS",
                     "BiodeiselwCCS"        = "Biodiesel \nw/CCS",
                     "LignocellulosicwCCS"  = "Adv. Biofuel \nw/CCS",
                     "Other biomasswCCS"    = "Other Biofuel \nw/CCS",
                     "ElectricitywCCS"      = "Electricity \nw/CCS",
                     "HydrogenwCCS"         = "Hydrogen \nw/CCS",
                     "Gas"              = "Bio-Gas")

dummy_labeler <- c("2010"= "Model Assumption","2020"= "Model Assumption","2030"= "Model Assumption","Literature" = "Literature")

var_labeler <- c("CapitalCo"  = "Capital Costs [$/kW]",
                 "Efficiency" = "Efficiency [-]",
                 "LCOE1"      = "Non-Energy Costs [$/MWh]",
                 "FeedFrac"   = "Feedstock Fraction of LCOE [-]")

var_labeler2 <- c("CapitalCo"  = "Capital Costs Penatly [$/kW]","Efficiency" = "Efficiency Penalty [% points]")

#
# ---- FIGURE DFs GLOBAL ----
TechData6 = subset(TechData6, SCENARIO==ActScen & !(CarrierID=="Hea"))
TechData6 = subset(TechData6, select=-c(OMCostFi,OMCostVa,Lifetime,alpha,
                                            CarrierID2,CaptureRate,FeedCost2,CCPrim,CDRPrim,CapFac))

PriceData = subset(PriceData, SCENARIO==ActScen)
PriceData.Glob = subset(PriceData, REGION=="World")
PriceData = subset(PriceData, REGION=="ASIA"|REGION=="LAM"|REGION=="OECD90"|REGION=="MAF"|REGION=="REF")
PriceData = PriceData %>% mutate(PricePerMWh = value * GJ2MWh) 

# CostEffData = subset(TechData6, Year=="2020"|Year=="2030"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2100")
CostEffData = TechData6
CostEffData$Tech = paste(CostEffData$VARIABLE) 
CostEffData$Tech <- gsub("Biomass","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("Coal","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("Gas","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("Oil","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("Geothermal","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("Wind","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("Hydro","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("Nuclear","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("SolarPV","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("SolarCSP","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("Onshore","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("Offshore","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("2","",CostEffData$Tech,fixed=F)
CostEffData$Tech <- gsub("3","",CostEffData$Tech,fixed=F)
CostEffData$Tech <- gsub("4","",CostEffData$Tech,fixed=F)
CostEffData$Tech <- gsub("5","",CostEffData$Tech,fixed=F)
CostEffData$Tech <- gsub("6","",CostEffData$Tech,fixed=F)
CostEffData$Tech <- gsub("woCCS","",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("gen","Hydrogen",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("HydrogenElectricity","Hydrogen",CostEffData$Tech,fixed=F) 
CostEffData$Tech <- gsub("es","Gas",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("LiquidsBiodiGasel","Biodeisel",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("LiquidsCellulosicNondiGasel","Lignocellulosic",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("LiquidsConventionalthanol","1st gen. ethanol",CostEffData$Tech,fixed=F)  
CostEffData$Tech <- gsub("LiquidsOther","Other biomass",CostEffData$Tech,fixed=F)  

CostEffData = CostEffData %>% mutate(LCOE1 = LCOE_Cap+LCOE_OM)
CostEffData = CostEffData %>% mutate(LCOE2 = LCOE1+LCOE_Feed)
CostEffData = CostEffData %>% mutate(LCOE3 = LCOE2+LCOE_ctax)
CostEffDataR=subset(CostEffData, !(REGION=="World"))

# Global Data
CostEffDataR$GlobID = paste(CostEffDataR$MODEL, CostEffDataR$SCENARIO, CostEffDataR$Year, CostEffDataR$VARIABLE)
EffGlob <- aggregate(CostEffDataR$Efficiency, by=list(MODEL=CostEffDataR$MODEL, SCENARIO=CostEffDataR$SCENARIO, VARIABLE=CostEffDataR$VARIABLE, Year=CostEffDataR$Year, CarrierID=CostEffDataR$CarrierID, Prim=CostEffDataR$Prim, Tech=CostEffDataR$Tech, GlobID=CostEffDataR$GlobID, Capt=CostEffDataR$Capt), FUN=mean, na.rm=TRUE) 
CostGlob <- aggregate(CostEffDataR$CapitalCo, by=list(MODEL=CostEffDataR$MODEL, SCENARIO=CostEffDataR$SCENARIO, VARIABLE=CostEffDataR$VARIABLE, Year=CostEffDataR$Year, CarrierID=CostEffDataR$CarrierID, Prim=CostEffDataR$Prim, Tech=CostEffDataR$Tech, GlobID=CostEffDataR$GlobID, Capt=CostEffDataR$Capt), FUN=mean, na.rm=TRUE)
LCOE_CapGlob <- aggregate(CostEffDataR$LCOE_Cap, by=list(MODEL=CostEffDataR$MODEL, SCENARIO=CostEffDataR$SCENARIO, VARIABLE=CostEffDataR$VARIABLE, Year=CostEffDataR$Year, CarrierID=CostEffDataR$CarrierID, Prim=CostEffDataR$Prim, Tech=CostEffDataR$Tech, GlobID=CostEffDataR$GlobID, Capt=CostEffDataR$Capt), FUN=mean, na.rm=TRUE)
LCOE1Glob <- aggregate(CostEffDataR$LCOE1, by=list(MODEL=CostEffDataR$MODEL, SCENARIO=CostEffDataR$SCENARIO, VARIABLE=CostEffDataR$VARIABLE, Year=CostEffDataR$Year, CarrierID=CostEffDataR$CarrierID, Prim=CostEffDataR$Prim, Tech=CostEffDataR$Tech, GlobID=CostEffDataR$GlobID, Capt=CostEffDataR$Capt), FUN=mean, na.rm=TRUE)
LCOE2Glob <- aggregate(CostEffDataR$LCOE2, by=list(MODEL=CostEffDataR$MODEL, SCENARIO=CostEffDataR$SCENARIO, VARIABLE=CostEffDataR$VARIABLE, Year=CostEffDataR$Year, CarrierID=CostEffDataR$CarrierID, Prim=CostEffDataR$Prim, Tech=CostEffDataR$Tech, GlobID=CostEffDataR$GlobID, Capt=CostEffDataR$Capt), FUN=mean, na.rm=TRUE)
LCOE3Glob <- aggregate(CostEffDataR$LCOE3, by=list(MODEL=CostEffDataR$MODEL, SCENARIO=CostEffDataR$SCENARIO, VARIABLE=CostEffDataR$VARIABLE, Year=CostEffDataR$Year, CarrierID=CostEffDataR$CarrierID, Prim=CostEffDataR$Prim, Tech=CostEffDataR$Tech, GlobID=CostEffDataR$GlobID, Capt=CostEffDataR$Capt), FUN=mean, na.rm=TRUE)
LCOEGlob <- aggregate(CostEffDataR$LCOE, by=list(MODEL=CostEffDataR$MODEL, SCENARIO=CostEffDataR$SCENARIO, VARIABLE=CostEffDataR$VARIABLE, Year=CostEffDataR$Year, CarrierID=CostEffDataR$CarrierID, Prim=CostEffDataR$Prim, Tech=CostEffDataR$Tech, GlobID=CostEffDataR$GlobID, Capt=CostEffDataR$Capt), FUN=mean, na.rm=TRUE)
LCOE_CorGlob <- aggregate(CostEffDataR$LCOE_Cor, by=list(MODEL=CostEffDataR$MODEL, SCENARIO=CostEffDataR$SCENARIO, VARIABLE=CostEffDataR$VARIABLE, Year=CostEffDataR$Year, CarrierID=CostEffDataR$CarrierID, Prim=CostEffDataR$Prim, Tech=CostEffDataR$Tech, GlobID=CostEffDataR$GlobID, Capt=CostEffDataR$Capt), FUN=mean, na.rm=TRUE)
CtaxGlob <- aggregate(CostEffDataR$Ctax, by=list(MODEL=CostEffDataR$MODEL, SCENARIO=CostEffDataR$SCENARIO, VARIABLE=CostEffDataR$VARIABLE, Year=CostEffDataR$Year, CarrierID=CostEffDataR$CarrierID, Prim=CostEffDataR$Prim, Tech=CostEffDataR$Tech, GlobID=CostEffDataR$GlobID, Capt=CostEffDataR$Capt), FUN=mean, na.rm=TRUE)

# In order to get global totals of secondary energy have to sum across RCP regions. Cannot use "world" region because some models do not report results there
TechDATA.RCP=read.csv("data/Technology/TechDATA_RCP.csv", sep=",", dec=".", stringsAsFactors = FALSE)
TechDATA.RCP$X <- NULL
TechDATA.RCP$GlobID = paste(TechDATA.RCP$MODEL,TechDATA.RCP$SCENARIO,TechDATA.RCP$Year,TechDATA.RCP$VARIABLE)
TechDATA.RCP$SecEn[is.na(TechDATA.RCP$SecEn)] <- 0
TechDATA.RCP$SecEn[TechDATA.RCP$SecEn<0]<-0
# For MESSAGE, corrections done previously lead to double counting of liquids SecEn, have to correct
TechDATA.RCP=subset(TechDATA.RCP, !(MODEL=="MESSAGE-GLOBIOM"&is.na(Efficiency)&is.na(CapitalCo)&is.na(OMCostFi)&is.na(OMCostVa)))
# As done above, have to streamline technologies
TechDATA.RCP$VARIABLE2 = TechDATA.RCP$VARIABLE
TechDATA.RCP$VARIABLE2[TechDATA.RCP$MODEL=="MESSAGE-GLOBIOM"&TechDATA.RCP$VARIABLE=="LiquidsBiomassOtherwCCS"] <- "LiquidsBiomassCellulosicNondieselwCCS"
TechDATA.RCP$VARIABLE2[TechDATA.RCP$MODEL=="MESSAGE-GLOBIOM"&TechDATA.RCP$VARIABLE=="LiquidsBiomassOtherwoCCS"] <- "LiquidsBiomassCellulosicNondieselwoCCS"

TechDATA.RCP$VARIABLE2[TechDATA.RCP$MODEL=="GRAPE-15"&TechDATA.RCP$VARIABLE=="LiquidsBiomassOtherwCCS"] <- "LiquidsBiomassCellulosicNondieselwCCS2"
TechDATA.RCP$VARIABLE2[TechDATA.RCP$MODEL=="GRAPE-15"&TechDATA.RCP$VARIABLE=="LiquidsBiomassOtherwoCCS"] <- "LiquidsBiomassCellulosicNondieselwoCCS2"

TechDATA.RCP$VARIABLE2[TechDATA.RCP$MODEL=="IMACLIM-NLU"&TechDATA.RCP$VARIABLE=="LiquidsBiomassOtherwoCCS"] <- "LiquidsBiomassBiodieselwoCCS"

TechDATA.RCP$VARIABLE2[TechDATA.RCP$MODEL=="DNE21+ V.14"&TechDATA.RCP$VARIABLE=="LiquidsBiomassOtherwoCCS"] <- "LiquidsBiomassCellulosicNondieselwoCCS2"

TechDATA.RCP$VARIABLE2[TechDATA.RCP$MODEL=="IMAGE"&TechDATA.RCP$VARIABLE=="LiquidsBiomassOtherwCCS"] <- "LiquidsBiomassCellulosicNondieselwCCS5"
TechDATA.RCP$VARIABLE2[TechDATA.RCP$MODEL=="IMAGE"&TechDATA.RCP$VARIABLE=="LiquidsBiomassOtherwoCCS"] <- "LiquidsBiomassCellulosicNondieselwoCCS5"
TechDATA.RCP$VARIABLE2[TechDATA.RCP$MODEL=="IMAGE"&TechDATA.RCP$VARIABLE=="LiquidsBiomassOtherwCCS2"] <- "LiquidsBiomassCellulosicNondieselwCCS6"
TechDATA.RCP$VARIABLE2[TechDATA.RCP$MODEL=="IMAGE"&TechDATA.RCP$VARIABLE=="LiquidsBiomassOtherwoCCS2"] <- "LiquidsBiomassCellulosicNondieselwoCCS6"

TechDATA.RCP$VARIABLE <- NULL
names(TechDATA.RCP)[names(TechDATA.RCP)=="VARIABLE2"] <- "VARIABLE"

# Aggregate Globally
SecEnGlob <- aggregate(TechDATA.RCP$SecEn, by=list(MODEL=TechDATA.RCP$MODEL, SCENARIO=TechDATA.RCP$SCENARIO, VARIABLE=TechDATA.RCP$VARIABLE, Year=TechDATA.RCP$Year, CarrierID=TechDATA.RCP$CarrierID, Prim=TechDATA.RCP$Prim, GlobID=TechDATA.RCP$GlobID, Capt=TechDATA.RCP$Capt), FUN=sum, na.rm=TRUE)
colnames(SecEnGlob)[9] <- "SecEn"
rm(TechDATA.RCP)

# Determine decadal increase in SecEn
SecEnGlob$GlobID <- NULL
SecEnGlob=spread(SecEnGlob,Year,SecEn)
SecEnGlob=subset(SecEnGlob, select=c("MODEL","SCENARIO","VARIABLE","CarrierID","Prim","Capt","2000","2010","2020","2030","2040","2050","2060","2070","2080","2090","2100"))
SecEnGlob.Dif=matrix(ncol=17, nrow=nrow(SecEnGlob))
YearCount=8:17
l=0
for(i in unique(SecEnGlob$MODEL)){
  for(j in unique(SecEnGlob$SCENARIO)){
    for(k in unique(SecEnGlob$VARIABLE)){
      if(length(subset(SecEnGlob, MODEL==i & SCENARIO==j & VARIABLE==k)[,1]) >0)
      {
        l=l+1
        for(m in YearCount){
          temp=subset(SecEnGlob, MODEL==i & SCENARIO==j & VARIABLE==k)
          Diff=temp[,m] - temp[,m-1]
          SecEnGlob.Dif[l,1:3] <- c(i,j,k)
          SecEnGlob.Dif[l,4:6] <- c(temp$CarrierID,temp$Prim,temp$Capt)
          SecEnGlob.Dif[l,7] <- NA
          SecEnGlob.Dif[l,m] <-Diff
        }
      }
    }
  }
}
SecEnGlob.Dif <- as.data.frame(SecEnGlob.Dif)
colnames(SecEnGlob.Dif) <- c("MODEL","SCENARIO","VARIABLE","CarrierID","Prim","Capt",
                         "2000","2010","2020","2030","2040","2050","2060","2070","2080","2090","2100")
for(i in 7:17) {SecEnGlob.Dif[,i] <- as.numeric(as.character(SecEnGlob.Dif[,i]))}

SecEnGlob=melt(SecEnGlob, id.vars=c("MODEL","SCENARIO","VARIABLE","CarrierID","Prim","Capt"))
colnames(SecEnGlob) <- c("MODEL","SCENARIO","VARIABLE","CarrierID","Prim","Capt","Year","SecEn")
SecEnGlob$GlobID = paste(SecEnGlob$MODEL,SecEnGlob$SCENARIO,SecEnGlob$Year,SecEnGlob$VARIABLE)

SecEnGlob.Dif=melt(SecEnGlob.Dif, id.vars=c("MODEL","SCENARIO","VARIABLE","CarrierID","Prim","Capt"))
colnames(SecEnGlob.Dif) <- c("MODEL","SCENARIO","VARIABLE","CarrierID","Prim","Capt","Year","SecEnDif")
SecEnGlob.Dif$GlobID = paste(SecEnGlob.Dif$MODEL,SecEnGlob.Dif$SCENARIO,SecEnGlob.Dif$Year,SecEnGlob.Dif$VARIABLE)

GlobObs=unique(EffGlob$GlobID)
SecEnGlob.Extra=SecEnGlob
SecEnGlob = subset(SecEnGlob, GlobID %in% GlobObs)
SecEnGlob.Dif = subset(SecEnGlob.Dif, GlobID %in% GlobObs)

GlobalData = EffGlob 
colnames(GlobalData)[10] <- "Efficiency"
GlobalData$CapitalCo <- CostGlob[match(GlobalData$GlobID, CostGlob$GlobID),"x"]
GlobalData$SecEn <- SecEnGlob[match(GlobalData$GlobID, SecEnGlob$GlobID),"SecEn"]
    # Add LiquidsOil for MESSAGE-GLOBIOM + BET + GRAPE-15 + POLES  because they lack the relevant identifier
    SecEnGlob.Extra=subset(SecEnGlob.Extra, (MODEL=="MESSAGE-GLOBIOM"|MODEL=="BET"|MODEL=="GRAPE-15"|MODEL=="POLES EMF33")&CarrierID=="Liq"&Prim=="Oil"&SCENARIO==ActScen&(Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100"))
    SecEnGlob.Extra$Tech <- "Liquids"
    SecEnGlob.Extra$Efficiency <- NA
    SecEnGlob.Extra$CapitalCo <- NA
    SecEnGlob.Extra=SecEnGlob.Extra[,c("MODEL","SCENARIO","VARIABLE","Year","CarrierID","Prim","Tech","GlobID","Capt","Efficiency","CapitalCo","SecEn")]
    GlobalData=rbind(GlobalData,SecEnGlob.Extra)
    rm(SecEnGlob.Extra)
GlobalData$SecEnDif <- SecEnGlob.Dif[match(GlobalData$GlobID, SecEnGlob.Dif$GlobID),"SecEnDif"]
GlobalData$Ctax <- CtaxGlob[match(GlobalData$GlobID, CtaxGlob$GlobID),"x"]
GlobalData$LCOE_Cap <- LCOE_CapGlob[match(GlobalData$GlobID, LCOE_CapGlob$GlobID),"x"]
GlobalData$LCOE1 <- LCOE1Glob[match(GlobalData$GlobID, LCOE1Glob$GlobID),"x"]
GlobalData$LCOE2 <- LCOE2Glob[match(GlobalData$GlobID, LCOE2Glob$GlobID),"x"]
GlobalData$LCOE3 <- LCOE3Glob[match(GlobalData$GlobID, LCOE3Glob$GlobID),"x"]
GlobalData$LCOE <- LCOEGlob[match(GlobalData$GlobID, LCOEGlob$GlobID),"x"]
# Add LiquidsOil for MESSAGE-GLOBIOM + BET + GRAPE-15 + POLES EMF33 because they lack the relevant identifier
    PriceData.Glob=subset(PriceData.Glob, (MODEL=="MESSAGE-GLOBIOM"|MODEL=="BET"|MODEL=="GRAPE-15"|MODEL=="POLES EMF33")&SCENARIO==ActScen&VARIABLE=="LiquidsOil"&(Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100"))
    PriceData.Glob = PriceData.Glob %>% mutate(PricePerMWh = value * GJ2MWh) 
    PriceData.Glob$GlobID = paste(PriceData.Glob$MODEL,PriceData.Glob$SCENARIO,PriceData.Glob$Year,PriceData.Glob$VARIABLE)
    temp1=subset(GlobalData, (MODEL=="MESSAGE-GLOBIOM"|MODEL=="BET"|MODEL=="GRAPE-15"|MODEL=="POLES EMF33")&VARIABLE=="LiquidsOil")
    temp1$LCOE <-PriceData.Glob[match(temp1$GlobID, PriceData.Glob$GlobID),"PricePerMWh"]
    GlobalData=subset(GlobalData, !((MODEL=="MESSAGE-GLOBIOM"|MODEL=="BET"|MODEL=="GRAPE-15")&VARIABLE=="LiquidsOil"))
    GlobalData=rbind(GlobalData,temp1)
    rm(temp1)
GlobalData$GlobID <- NULL
GlobalData$Prim <- gsub("wCCS","",GlobalData$Prim,fixed=F)

GlobalData$Tech2 <- gsub("wCCS","",GlobalData$Tech,fixed=F)  
GlobalData$TechOrder = factor(GlobalData$Tech, levels=c("Electricity","ElectricitywCCS",
                                                        "1st gen. ethanol","1st gen. ethanolwCCS","Lignocellulosic","LignocellulosicwCCS","Biodeisel","BiodeiselwCCS","Other biomass","Other biomasswCCS","Liquids","LiquidswCCS",
                                                     "Hydrogen","HydrogenwCCS",
                                                     "Gas"))
GlobalData$Tech2 <- gsub("wCCS","",GlobalData$Tech,fixed=F)  
GlobalData$TechOrder2 = factor(GlobalData$Tech2, levels=c("Electricity","1st gen. ethanol","Lignocellulosic","Biodeisel","Other biomass","Liquids","Hydrogen","Gas"))

GlobalData.Bio = subset(GlobalData, Prim=="Biomass"|Prim=="BiomasswCCS")
GlobalData.Bio1 = subset(GlobalData.Bio, select=-c(SCENARIO,VARIABLE,Efficiency,CapitalCo,SecEn,Ctax))

# For figures only: Add variable for cases where LCOE2 & LCOE (ie.e + feedstock and TOTAL) > 500
GlobalData.Bio1cor = GlobalData.Bio1
GlobalData.Bio1cor$LCOE2cor = pmin(GlobalData.Bio1cor$LCOE2,500)
GlobalData.Bio1cor$LCOE2[GlobalData.Bio1cor$LCOE2cor==500] <- NA
GlobalData.Bio1cor$LCOE2cor[!GlobalData.Bio1cor$LCOE2cor==500] <- NA

GlobalData.Bio1cor$LCOE3cor = pmin(GlobalData.Bio1cor$LCOE3,500)
GlobalData.Bio1cor$LCOE3[GlobalData.Bio1cor$LCOE3cor==500] <- NA
GlobalData.Bio1cor$LCOE3cor[!GlobalData.Bio1cor$LCOE3cor==500] <- NA

GlobalData.Bio1cor$LCOEcor_min = pmax(GlobalData.Bio1cor$LCOE,-500)
GlobalData.Bio1cor$LCOE[GlobalData.Bio1cor$LCOEcor_min==-500] <- NA
GlobalData.Bio1cor$LCOEcor_min[!GlobalData.Bio1cor$LCOEcor_min==-500] <- NA

GlobalData.Bio1cor$LCOEcor_max = pmin(GlobalData.Bio1cor$LCOE,500)
GlobalData.Bio1cor$LCOE[GlobalData.Bio1cor$LCOEcor_max==500] <- NA
GlobalData.Bio1cor$LCOEcor_max[!GlobalData.Bio1cor$LCOEcor_max==500] <- NA

# TradDATA[is.na(TradDATA)]<-0
GlobalData.Bio1cor$LCOEcor_min[is.na(GlobalData.Bio1cor$LCOEcor_min)] <- 0
GlobalData.Bio1cor$LCOEcor_max[is.na(GlobalData.Bio1cor$LCOEcor_max)] <- 0
GlobalData.Bio1cor = GlobalData.Bio1cor %>% mutate(LCOEcor = LCOEcor_min+LCOEcor_max)
GlobalData.Bio1cor$LCOEcor[GlobalData.Bio1cor$LCOEcor==0] <- NA
GlobalData.Bio1cor$LCOEcor[GlobalData.Bio1cor$Capt=="woCCS"] <- NA

GlobalData.Bio1cor$LCOEcor_max <- NULL
GlobalData.Bio1cor$LCOEcor_min <- NULL

GlobalData.Bio1cor <- melt(GlobalData.Bio1cor, measure.vars=c("LCOE","LCOE_Cap","LCOE1","LCOE2","LCOE3","LCOEcor","LCOE2cor","LCOE3cor"), na.rm=FALSE)
GlobalData.Bio1cor$TechYr = paste(GlobalData.Bio1cor$TechOrder,GlobalData.Bio1cor$Year,sep="_")

# Determine further dataframes
GlobalData.Bio1 <- melt(GlobalData.Bio1, measure.vars=c("LCOE","LCOE_Cap","LCOE1","LCOE2","LCOE3"), na.rm=FALSE)
GlobalData.Bio1$TechYr = paste(GlobalData.Bio1$TechOrder,GlobalData.Bio1$Year,sep="_")
GlobalData.Bio2 = subset(GlobalData.Bio1, variable=="LCOE1"|variable=="LCOE_Cap")
GlobalData.Bio3 = subset(GlobalData.Bio1, variable=="LCOE1"|variable=="LCOE_Cap"|variable=="LCOE2")
GlobalData.Ele = subset(GlobalData, CarrierID=="Ele")
GlobalData.Liq = subset(GlobalData, CarrierID=="Liq")

GlobPriceUse.Bio = subset(GlobalData.Bio, select=-c(SCENARIO,VARIABLE,Efficiency,CapitalCo,LCOE_Cap,LCOE1,LCOE2,Ctax))

# Literature data
LitData=read.xlsx("data/Technology/LitData.xlsx", sheetIndex=3)
LitData$X <-NULL
LitData$Tech2 <- gsub("wCCS","",LitData$Tech,fixed=F)  
LitData$TechOrder2 = factor(LitData$Tech2, levels=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Other biomass","Liquids","Electricity","Hydrogen","Gas"))
LitData$Year <- "Literature"

# Data on Secondary Energy Only
SecEnTot= subset(SecEnTot, REGION=="World"&(Year=="2020"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2080"|Year=="2100"))
SecEnTot$Tech3 = paste(SecEnTot$CarrierID,SecEnTot$Capt)
SecEnTot$TechOrder3 = factor(SecEnTot$Tech3, levels=c('Ele woCCS','Ele wCCS','Ele Total','Liq woCCS','Liq wCCS','Liq Total','Hyd woCCS','Hyd wCCS','Hyd Total','Gas Total','Hea Total'))
SecEnTot$Year = as.character(SecEnTot$Year)

SecEnTot2=subset(SecEnTot, select=-c(VARIABLE,UNIT,VARID))
SecEnTot2.Bio = subset(SecEnTot2, Prim=="Biomass")
SecEnTot2.Bio = subset(SecEnTot2.Bio, select=-c(Tech3,TechOrder3,Prim))
SecEnTot2.Bio=spread(SecEnTot2.Bio,Capt,value)
SecEnTot2.Bio[is.na(SecEnTot2.Bio)]<-0
SecEnTot2.Bio = SecEnTot2.Bio %>% mutate(Total2 = Total+wCCS+woCCS)
SecEnTot2.Bio$ID=paste(SecEnTot2.Bio$MODEL,SecEnTot2.Bio$SCENARIO,SecEnTot2.Bio$REGION,SecEnTot2.Bio$Year,SecEnTot2.Bio$CarrierID)

SecEnTot2=subset(SecEnTot2, Capt=="Total"&Prim=="Total")
colnames(SecEnTot2)[colnames(SecEnTot2) == 'value'] <- 'Total'
SecEnTot2$Prim <- NULL
SecEnTot2$ID=paste(SecEnTot2$MODEL,SecEnTot2$SCENARIO,SecEnTot2$REGION,SecEnTot2$Year,SecEnTot2$CarrierID)
SecEnTot2$Biomass <- SecEnTot2.Bio[match(SecEnTot2$ID,SecEnTot2.Bio$ID),9]
SecEnTot2$ID <- NULL
SecEnTot2$Capt <- NULL
SecEnTot2=SecEnTot2 %>% mutate(BioFrac=Biomass/Total)
SecEnTot2$BioFrac[SecEnTot2$Total<1] <- 0 #If total use of secondary energy carrier is negligible, set as zero

SecEnTot3=subset(SecEnTot, CarrierID=="Ele"|CarrierID=="Liq")
SecEnTot3=subset(SecEnTot3, SCENARIO==ActScen)
SecEnTot3=subset(SecEnTot3, select=-c(SCENARIO, REGION, UNIT, VARID, VARIABLE, Tech3, TechOrder3))
SecEnTot3=spread(SecEnTot3,Capt,value)
SecEnTot3 = SecEnTot3 %>% mutate(TotalBio = wCCS+woCCS)
SecEnTot3=subset(SecEnTot3, select=-c(wCCS, woCCS, Prim))
SecEnTot3<-melt(SecEnTot3, id.vars=c("MODEL","Year","CarrierID"), na.rm=TRUE)
SecEnTot3$VARID=paste(SecEnTot3$variable, SecEnTot3$CarrierID)
SecEnTot3$VARID <-gsub( "[[:space:]]","",SecEnTot3$VARID,fixed=F)
SecEnTot3=subset(SecEnTot3, select=-c(variable, CarrierID))
SecEnTot3=spread(SecEnTot3,VARID,value)
SecEnTot3 = SecEnTot3 %>% mutate(BioFracLiq = TotalBioLiq/(TotalBioLiq + TotalBioEle))
SecEnTot3 = SecEnTot3 %>% mutate(BioFracEle = 1-BioFracLiq)

EleVSBioFrac <- ggplot(subset(SecEnTot3, Year=="2100")) + 
  geom_point(aes(x=TotalEle, y=BioFracLiq, shape=MODEL), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab("BioFrac Liq") + xlab("Total Ele") +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=6), legend.title=element_text(face="bold")) +
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10,11,12),name="",breaks=unique(SecEnTot3$MODEL),labels=unique(SecEnTot3$MODEL)) 
EleVSBioFrac
#
# ---- FIGURE DFs REGIONAL ----
RegData = subset(CostEffDataR, !(Year=="2070"))
RegData = subset(RegData, select=-c(Mitig,ScenOrder, GlobID))
RegData$Tech2 <- gsub("wCCS","",RegData$Tech,fixed=F)  
RegData$TechOrder = factor(RegData$Tech, levels=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Other biomass","Liquids",
                                                  "1st gen. ethanolwCCS","BiodeiselwCCS","LignocellulosicwCCS","Other biomasswCCS","LiquidswCCS",
                                                  "Electricity","Hydrogen","ElectricitywCCS","HydrogenwCCS",
                                                  "Gas"))
RegData$Tech2 <- gsub("wCCS","",RegData$Tech,fixed=F)  
RegData$TechOrder2 = factor(RegData$Tech2, levels=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Other biomass","Liquids","Electricity","Hydrogen","Gas"))
RegData$Prim2 <- gsub("wCCS","",RegData$Prim,fixed=F)

RegData.Bio = subset(RegData, Prim=="Biomass"|Prim=="BiomasswCCS")
RegData.Bio1 = subset(RegData.Bio, select=-c(SCENARIO,VARIABLE,Efficiency,CapitalCo,SecEn,Ctax))
RegData.Bio1 <- melt(RegData.Bio1, measure.vars=c("LCOE","LCOE_Cap","LCOE_OM","LCOE_Feed","LCOE_ctax","LCOE_CDR","LCOE_Cor","LCOE1","LCOE2"), na.rm=FALSE)
RegData.Bio1$TechYr = paste(RegData.Bio1$TechOrder,RegData.Bio1$Year,sep="_")
RegData.Bio2 = subset(RegData.Bio1, variable=="LCOE1"|variable=="LCOE_Cap")
RegData.Bio3 = subset(RegData.Bio1, variable=="LCOE1"|variable=="LCOE_Cap"|variable=="LCOE2")
RegData.Bio4 = subset(RegData.Bio1, variable=="LCOE1"|variable=="LCOE_Cap"|variable=="LCOE2"|variable=="LCOE")
RegData.Ele = subset(RegData, CarrierID=="Ele")
RegData.Liq = subset(RegData, CarrierID=="Liq")

# ---- CALCULATIONS FOR PAPER ----
# ---- *** General *** ----
Calcs = subset(RegData.Bio, select=c(MODEL,SCENARIO,REGION,Year,
                                         VARIABLE,Prim,Capt,Efficiency,CapitalCo,
                                         LCOE_Cap,LCOE_Feed,LCOE1,LCOE2,LCOE3,LCOE,
                                         TechOrder))

Calcs.CapCo <- aggregate(Calcs$CapitalCo, by=list(MODEL=Calcs$MODEL, SCENARIO=Calcs$SCENARIO, REGION=Calcs$REGION, Year=Calcs$Year,VARIABLE=Calcs$VARIABLE, Capt=Calcs$Capt, TechOrder=Calcs$TechOrder), FUN=mean, na.rm=TRUE)
colnames(Calcs.CapCo)[colnames(Calcs.CapCo) == 'x'] <- 'CapitalCo'
Calcs.CapCo$ID = paste(Calcs.CapCo$MODEL,Calcs.CapCo$REGION,Calcs.CapCo$Year,Calcs.CapCo$VARIABLE)

Calcs.Eff <- aggregate(Calcs$Efficiency, by=list(MODEL=Calcs$MODEL, SCENARIO=Calcs$SCENARIO, REGION=Calcs$REGION, Year=Calcs$Year,VARIABLE=Calcs$VARIABLE, Capt=Calcs$Capt, TechOrder=Calcs$TechOrder), FUN=mean, na.rm=TRUE)
colnames(Calcs.Eff)[colnames(Calcs.Eff) == 'x'] <- 'Efficiency'
Calcs.Eff$ID = paste(Calcs.Eff$MODEL,Calcs.Eff$REGION,Calcs.Eff$Year,Calcs.Eff$VARIABLE)

Calcs.LCOE_Cap <- aggregate(Calcs$LCOE_Cap, by=list(MODEL=Calcs$MODEL, SCENARIO=Calcs$SCENARIO, REGION=Calcs$REGION, Year=Calcs$Year,VARIABLE=Calcs$VARIABLE, Capt=Calcs$Capt, TechOrder=Calcs$TechOrder), FUN=mean, na.rm=TRUE)
colnames(Calcs.LCOE_Cap)[colnames(Calcs.LCOE_Cap) == 'x'] <- 'LCOE_Cap'
Calcs.LCOE_Cap$ID = paste(Calcs.LCOE_Cap$MODEL,Calcs.LCOE_Cap$REGION,Calcs.LCOE_Cap$Year,Calcs.LCOE_Cap$VARIABLE)

Calcs.LCOE_Feed <- aggregate(Calcs$LCOE_Feed, by=list(MODEL=Calcs$MODEL, SCENARIO=Calcs$SCENARIO, REGION=Calcs$REGION, Year=Calcs$Year,VARIABLE=Calcs$VARIABLE, Capt=Calcs$Capt, TechOrder=Calcs$TechOrder), FUN=mean, na.rm=TRUE)
colnames(Calcs.LCOE_Feed)[colnames(Calcs.LCOE_Feed) == 'x'] <- 'LCOE_Feed'
Calcs.LCOE_Feed$ID = paste(Calcs.LCOE_Feed$MODEL,Calcs.LCOE_Feed$REGION,Calcs.LCOE_Feed$Year,Calcs.LCOE_Feed$VARIABLE)

Calcs.LCOE1 <- aggregate(Calcs$LCOE1, by=list(MODEL=Calcs$MODEL, SCENARIO=Calcs$SCENARIO, REGION=Calcs$REGION, Year=Calcs$Year,VARIABLE=Calcs$VARIABLE, Capt=Calcs$Capt, TechOrder=Calcs$TechOrder), FUN=mean, na.rm=TRUE)
colnames(Calcs.LCOE1)[colnames(Calcs.LCOE1) == 'x'] <- 'LCOE1'
Calcs.LCOE1$ID = paste(Calcs.LCOE1$MODEL,Calcs.LCOE1$REGION,Calcs.LCOE1$Year,Calcs.LCOE1$VARIABLE)

Calcs.LCOE2 <- aggregate(Calcs$LCOE2, by=list(MODEL=Calcs$MODEL, SCENARIO=Calcs$SCENARIO, REGION=Calcs$REGION, Year=Calcs$Year,VARIABLE=Calcs$VARIABLE, Capt=Calcs$Capt, TechOrder=Calcs$TechOrder), FUN=mean, na.rm=TRUE)
colnames(Calcs.LCOE2)[colnames(Calcs.LCOE2) == 'x'] <- 'LCOE2'
Calcs.LCOE2$ID = paste(Calcs.LCOE2$MODEL,Calcs.LCOE2$REGION,Calcs.LCOE2$Year,Calcs.LCOE2$VARIABLE)

Calcs.LCOE3 <- aggregate(Calcs$LCOE3, by=list(MODEL=Calcs$MODEL, SCENARIO=Calcs$SCENARIO, REGION=Calcs$REGION, Year=Calcs$Year,VARIABLE=Calcs$VARIABLE, Capt=Calcs$Capt, TechOrder=Calcs$TechOrder), FUN=mean, na.rm=TRUE)
colnames(Calcs.LCOE3)[colnames(Calcs.LCOE3) == 'x'] <- 'LCOE3'
Calcs.LCOE3$ID = paste(Calcs.LCOE3$MODEL,Calcs.LCOE3$REGION,Calcs.LCOE3$Year,Calcs.LCOE3$VARIABLE)

Calcs.LCOE <- aggregate(Calcs$LCOE, by=list(MODEL=Calcs$MODEL, SCENARIO=Calcs$SCENARIO, REGION=Calcs$REGION, Year=Calcs$Year,VARIABLE=Calcs$VARIABLE, Capt=Calcs$Capt, TechOrder=Calcs$TechOrder), FUN=mean, na.rm=TRUE)
colnames(Calcs.LCOE)[colnames(Calcs.LCOE) == 'x'] <- 'LCOE'
Calcs.LCOE$ID = paste(Calcs.LCOE$MODEL,Calcs.LCOE$REGION,Calcs.LCOE$Year,Calcs.LCOE$VARIABLE)

Calcs1=Calcs.CapCo
Calcs1$Efficiency <- Calcs.Eff[match(Calcs1$ID,Calcs.Eff$ID),8]
Calcs1$LCOE_Cap <- Calcs.LCOE_Cap[match(Calcs1$ID,Calcs.LCOE_Cap$ID),8]
Calcs1$LCOE_Feed <- Calcs.LCOE_Feed[match(Calcs1$ID,Calcs.LCOE_Feed$ID),8]
Calcs1$LCOE1 <- Calcs.LCOE1[match(Calcs1$ID,Calcs.LCOE1$ID),8]
Calcs1$LCOE2 <- Calcs.LCOE2[match(Calcs1$ID,Calcs.LCOE2$ID),8]
Calcs1$LCOE3 <- Calcs.LCOE3[match(Calcs1$ID,Calcs.LCOE3$ID),8]
Calcs1$LCOE <- Calcs.LCOE[match(Calcs1$ID,Calcs.LCOE$ID),8]
Calcs1$ID <- NULL

# ---- *** Cost Components *** ----
#  Difference between wCCS and woCCS
Calcs.CCS = Calcs1
Calcs.CCS <- melt(Calcs.CCS, measure.vars=c("CapitalCo","Efficiency","LCOE_Cap","LCOE_Feed","LCOE1","LCOE2","LCOE3","LCOE"))
Calcs.CCS$VARIABLE <-gsub( "wCCS","",Calcs.CCS$VARIABLE,fixed=F)
Calcs.CCS$VARIABLE <-gsub( "woCCS","",Calcs.CCS$VARIABLE,fixed=F)
Calcs.CCS$TechOrder <-NULL
Calcs.CCS <- spread(Calcs.CCS, Capt,value)
Calcs.CCS$wCCS[is.na(Calcs.CCS$wCCS)] <- 0
Calcs.CCS = Calcs.CCS %>% mutate(CCS_Diff = wCCS - woCCS)
Calcs.CCS = Calcs.CCS %>% mutate(Perc_Chang = (CCS_Diff/woCCS)*100)
Calcs.CCS$CarrierID <-substr(Calcs.CCS$VARIABLE, start=1, stop=3)
Calcs.CCS$CCS_Diff[Calcs.CCS$variable=="Efficiency"] <- Calcs.CCS$CCS_Diff[Calcs.CCS$variable=="Efficiency"] * 100

Calcs.CCSMed=subset(Calcs.CCS, wCCS>0)
Calcs.CCSMed$CarrierID <-NULL
Calcs.CCSMed=subset(Calcs.CCSMed, Perc_Chang<1.5)
Calcs.CCSMed$SecID <- substr(Calcs.CCSMed$VARIABLE, start=1, stop=3)
Calcs.CCSMed1 <- aggregate(Calcs.CCSMed$Perc_Chang, by=list(SecID=Calcs.CCSMed$SecID,variable=Calcs.CCSMed$variable, Year=Calcs.CCSMed$Year), FUN=mean, na.rm=TRUE) 
Calcs.CCSMed1 = subset(Calcs.CCSMed1, !variable=="Efficiency")
Calcs.CCSMed2 <- aggregate(Calcs.CCSMed$CCS_Diff, by=list(SecID=Calcs.CCSMed$SecID,variable=Calcs.CCSMed$variable, Year=Calcs.CCSMed$Year), FUN=mean, na.rm=TRUE) 
Calcs.CCSMed2 = subset(Calcs.CCSMed2, variable=="Efficiency")
rm(Calcs.CCSMed)
Calcs.CCSMed = rbind(Calcs.CCSMed1,Calcs.CCSMed2)
rm(Calcs.CCSMed1,Calcs.CCSMed2)

# Cost Components
Calcs.Costs = Calcs1
Calcs.Costs = Calcs.Costs %>% mutate(OM_Cap = (LCOE1-LCOE_Cap)/LCOE_Cap)
Calcs.Costs = Calcs.Costs %>% mutate(CapFrac = LCOE_Cap/LCOE)
Calcs.Costs = Calcs.Costs %>% mutate(OMFrac = (LCOE1-LCOE_Cap)/LCOE)
Calcs.Costs = Calcs.Costs %>% mutate(FeedFrac = (LCOE_Feed)/LCOE)
Calcs.Costs = Calcs.Costs %>% mutate(CDRFrac = (LCOE-LCOE3)/LCOE)
Calcs.Costs$SecID <- substr(Calcs.Costs$VARIABLE, start=1, stop=3)
Calcs.Costs$MedID = paste(Calcs.Costs$SecID,Calcs.Costs$Capt,Calcs.Costs$Year)
Calcs.CostsMed1 <- aggregate(Calcs.Costs$OM_Cap, by=list(MedID=Calcs.Costs$MedID,SecID=Calcs.Costs$SecID,Capt=Calcs.Costs$Capt,Year=Calcs.Costs$Year), FUN=mean, na.rm=TRUE) 
colnames(Calcs.CostsMed1)[colnames(Calcs.CostsMed1) == 'x'] <- 'OM_Cap'
Calcs.CostsMed2 <- aggregate(Calcs.Costs$CapFrac, by=list(MedID=Calcs.Costs$MedID,SecID=Calcs.Costs$SecID,Capt=Calcs.Costs$Capt,Year=Calcs.Costs$Year), FUN=mean, na.rm=TRUE) 
colnames(Calcs.CostsMed2)[colnames(Calcs.CostsMed2) == 'x'] <- 'CapFrac'
Calcs.CostsMed3 <- aggregate(Calcs.Costs$OMFrac, by=list(MedID=Calcs.Costs$MedID,SecID=Calcs.Costs$SecID,Capt=Calcs.Costs$Capt,Year=Calcs.Costs$Year), FUN=mean, na.rm=TRUE) 
colnames(Calcs.CostsMed3)[colnames(Calcs.CostsMed3) == 'x'] <- 'OMFrac'
Calcs.CostsMed4 <- aggregate(Calcs.Costs$FeedFrac, by=list(MedID=Calcs.Costs$MedID,SecID=Calcs.Costs$SecID,Capt=Calcs.Costs$Capt,Year=Calcs.Costs$Year), FUN=mean, na.rm=TRUE) 
colnames(Calcs.CostsMed4)[colnames(Calcs.CostsMed4) == 'x'] <- 'FeedFrac'
Calcs.CostsMed5 <- aggregate(Calcs.Costs$CDRFrac, by=list(MedID=Calcs.Costs$MedID,SecID=Calcs.Costs$SecID,Capt=Calcs.Costs$Capt,Year=Calcs.Costs$Year), FUN=mean, na.rm=TRUE) 
colnames(Calcs.CostsMed5)[colnames(Calcs.CostsMed5) == 'x'] <- 'CDRFrac'

Calcs.CostsMed=Calcs.CostsMed1
Calcs.CostsMed$CapFrac <- Calcs.CostsMed2[match(Calcs.CostsMed$MedID, Calcs.CostsMed2$MedID),5]
Calcs.CostsMed$OMFrac <- Calcs.CostsMed3[match(Calcs.CostsMed$MedID, Calcs.CostsMed3$MedID),5]
Calcs.CostsMed$FeedFrac <- Calcs.CostsMed4[match(Calcs.CostsMed$MedID, Calcs.CostsMed4$MedID),5]
Calcs.CostsMed$CDRFrac <- Calcs.CostsMed5[match(Calcs.CostsMed$MedID, Calcs.CostsMed5$MedID),5]
rm(Calcs.CostsMed1,Calcs.CostsMed2,Calcs.CostsMed3,Calcs.CostsMed4,Calcs.CostsMed5)

# ---- *** Regional Variation *** ----
# Regional variation
Calcs.Reg = Calcs1
Calcs.Reg = melt(Calcs.Reg, id.vars=c("MODEL","SCENARIO","REGION","Year","VARIABLE","Capt","TechOrder"), variable_name="variable")
Calcs.Reg$RegID=paste(Calcs.Reg$MODEL,Calcs.Reg$SCENARIO,Calcs.Reg$Year,Calcs.Reg$VARIABLE)
Calcs.Reg1 <- aggregate(Calcs.Reg$value, by=list(RegID=Calcs.Reg$RegID,MODEL=Calcs.Reg$MODEL,SCENARIO=Calcs.Reg$SCENARIO,Year=Calcs.Reg$Year,VARIABLE=Calcs.Reg$VARIABLE, variable2=Calcs.Reg$variable), 
                        function(x) sd=sd(x))
colnames(Calcs.Reg1)[7] <- 'SD'
Calcs.Reg2 <- aggregate(Calcs.Reg$value, by=list(RegID=Calcs.Reg$RegID,MODEL=Calcs.Reg$MODEL,SCENARIO=Calcs.Reg$SCENARIO,Year=Calcs.Reg$Year,VARIABLE=Calcs.Reg$VARIABLE, variable2=Calcs.Reg$variable), 
                        function(x) mean=mean(x))
Calcs.Reg1$Mean <- Calcs.Reg2[match(Calcs.Reg1$RegID,Calcs.Reg2$RegID),7]
Calcs.Reg1$RegID <- NULL
rm(Calcs.Reg2)
Calcs.Reg1 = Calcs.Reg1 %>% mutate(CV_perc = (SD/Mean)*100)
Calcs.Reg1$CarrierID = substr(Calcs.Reg1$VARIABLE,1,3)

# ---- *** Ranges & Percentiles *** ----
# Determine percentile ranges of results
  #First, dissagregzte 1st gen and 2nd gen liquids
  Calcs.Ranges = Calcs.Costs
  Calcs.Ranges$SecID[Calcs.Ranges$TechOrder=="Lignocellulosic"|Calcs.Ranges$TechOrder=="LignocellulosicwCCS"] <- "Liq2"
  Calcs.Ranges$MedID = paste(Calcs.Ranges$SecID,Calcs.Ranges$Capt,Calcs.Ranges$Year)

Calcs.RangeC <- by(Calcs.Ranges$CapitalCo,Calcs.Ranges$MedID,quantile,c(0,0.1,0.9,1))
Calcs.RangeC=as.data.frame.list(Calcs.RangeC)
Calcs.RangeC= setDT(Calcs.RangeC, keep.rownames = TRUE)[]
Calcs.RangeC = setNames(data.frame(t(Calcs.RangeC[,-1])), Calcs.RangeC[,1])
colnames(Calcs.RangeC)[1:4] <- c("0%","10%","90%","100%")
Calcs.RangeC= setDT(Calcs.RangeC, keep.rownames = TRUE)[]
Calcs.RangeC$VARIABLE <- "CapitalCo" 

Calcs.RangeE <- by(Calcs.Ranges$Efficiency,Calcs.Ranges$MedID,quantile,c(0,0.1,0.9,1))
Calcs.RangeE=as.data.frame.list(Calcs.RangeE)
Calcs.RangeE= setDT(Calcs.RangeE, keep.rownames = TRUE)[]
Calcs.RangeE = setNames(data.frame(t(Calcs.RangeE[,-1])), Calcs.RangeE[,1])
colnames(Calcs.RangeE)[1:4] <- c("0%","10%","90%","100%")
Calcs.RangeE= setDT(Calcs.RangeE, keep.rownames = TRUE)[]
Calcs.RangeE$VARIABLE <- "Efficiency" 

Calcs.RangeN <- by(Calcs.Ranges$LCOE1,Calcs.Ranges$MedID,quantile,c(0,0.1,0.9,1))
Calcs.RangeN=as.data.frame.list(Calcs.RangeN)
Calcs.RangeN= setDT(Calcs.RangeN, keep.rownames = TRUE)[]
Calcs.RangeN = setNames(data.frame(t(Calcs.RangeN[,-1])), Calcs.RangeN[,1])
colnames(Calcs.RangeN)[1:4] <- c("0%","10%","90%","100%")
Calcs.RangeN= setDT(Calcs.RangeN, keep.rownames = TRUE)[]
Calcs.RangeN$VARIABLE <- "Non-Energy Costs" 

Calcs.RangeFF <- by(Calcs.Ranges$FeedFrac,Calcs.Ranges$MedID,quantile,c(0,0.1,0.9,1))
Calcs.RangeFF=as.data.frame.list(Calcs.RangeFF)
Calcs.RangeFF= setDT(Calcs.RangeFF, keep.rownames = TRUE)[]
Calcs.RangeFF = setNames(data.frame(t(Calcs.RangeFF[,-1])), Calcs.RangeFF[,1])
colnames(Calcs.RangeFF)[1:4] <- c("0%","10%","90%","100%")
Calcs.RangeFF= setDT(Calcs.RangeFF, keep.rownames = TRUE)[]
Calcs.RangeFF$VARIABLE <- "Feedstock Fraction" 

Calcs.RangeCCS = Calcs.CCS
Calcs.RangeCCS = subset(Calcs.RangeCCS, CCS_Diff>0&variable=="CapitalCo")
Calcs.RangeCCS$CarrierID[Calcs.RangeCCS$VARIBALE=="LiquidsBiomassCellulosicNondiesel"|
                       Calcs.RangeCCS$VARIBALE=="LiquidsBiomassCellulosicNondiesel2"|
                       Calcs.RangeCCS$VARIBALE=="LiquidsBiomassCellulosicNondiesel3"|
                       Calcs.RangeCCS$VARIBALE=="LiquidsBiomassCellulosicNondiesel4"|
                       Calcs.RangeCCS$VARIBALE=="LiquidsBiomassCellulosicNondiesel5"|
                       Calcs.RangeCCS$VARIBALE=="LiquidsBiomassCellulosicNondiesel6"] <- "Liq2"
Calcs.RangeCCS$TechID = paste(Calcs.RangeCCS$CarrierID,"wCCS",Calcs.RangeCCS$Year)
Calcs.RangeCCS <- by(Calcs.RangeCCS$CCS_Diff,Calcs.RangeCCS$TechID,quantile,c(0,0.1,0.9,1))
Calcs.RangeCCS=as.data.frame.list(Calcs.RangeCCS)
Calcs.RangeCCS= setDT(Calcs.RangeCCS, keep.rownames = TRUE)[]
Calcs.RangeCCS = setNames(data.frame(t(Calcs.RangeCCS[,-1])), Calcs.RangeCCS[,1])
colnames(Calcs.RangeCCS)[1:4] <- c("0%","10%","90%","100%")
Calcs.RangeCCS= setDT(Calcs.RangeCCS, keep.rownames = TRUE)[]
Calcs.RangeCCS$VARIABLE <- "CCS CapitalCo" 

Calcs.Range = rbind(Calcs.RangeC, Calcs.RangeE, Calcs.RangeN, Calcs.RangeCCS, Calcs.RangeFF)

# Compare withBaker et al. information to categorise models
Calcs.Costs2 = Calcs.Costs
Calcs.Costs2 = subset(Calcs.Costs2, select = c(MODEL,SCENARIO,REGION,Year,Efficiency,LCOE1,MedID))
#Calcs.Costs2 = subset(Calcs.Costs2, Year==2030&REGION=="USA") # Have to use a single region to avoid over-counting
Calcs.Costs2 = melt(Calcs.Costs2, id.vars=c("MODEL","SCENARIO","REGION","Year","MedID"), na.rm=TRUE)

Calcs.CCS2 = Calcs.CCS
Calcs.CCS2 = subset(Calcs.CCS2, select = c(MODEL,SCENARIO,REGION,Year,variable,CCS_Diff,CarrierID))
#Calcs.CCS2 = subset(Calcs.CCS2, Year==2030&REGION=="USA"&!(CarrierID=="Gas")) # Have to use a single region to avoid over-counting
Calcs.CCS2$TechID = paste(Calcs.CCS2$CarrierID,"wCCS",Calcs.CCS2$Year)
Calcs.CCS2 = na.omit(Calcs.CCS2)

BakerDat1 = subset(BakerDat, variable=="Efficiency"|variable=="LCOE1") # Non-CCS Parameters
BakerDat2 = subset(BakerDat, variable=="CapitalCo") # CCS Parameters

# Non-CCS parameters
l=0
for(i in 1:nrow(BakerDat1)){
  l=l+1
  # Get counts per row
  BakerSample=BakerDat1[l,]
  test = subset(Calcs.Costs2, MedID==BakerSample$TechID&variable==BakerSample$variable)
  count = sum(test$value >= BakerSample$value.low &
                test$value <= BakerSample$value.high)
  NumObs = nrow(test)
  # Fill in counts in original DF
  BakerDat1[l,6]<-count
  BakerDat1[l,7]<-NumObs
}

# CCS parameters
l=0
for(i in 1:nrow(BakerDat2)){
  l=l+1
  # Get counts per row
  BakerSample=BakerDat2[l,]
  test = subset(Calcs.CCS2, TechID==BakerSample$TechID&variable==BakerSample$variable&CCS_Diff>0)
  count = sum(test$CCS_Diff >= BakerSample$value.low &
                test$CCS_Diff <= BakerSample$value.high)
  NumObs = nrow(test)
  # Fill in counts in original DF
  BakerDat2[l,6]<-count
  BakerDat2[l,7]<-NumObs
}

BakerDat=rbind(BakerDat1,BakerDat2)
colnames(BakerDat)[6] <- "Count"
colnames(BakerDat)[7] <- "Observations"

rm(Calcs.Costs2,Calcs.CCS2, BakerDat1, BakerDat2, BakerSample,test,count, NumObs)
rm(Calcs.RangeC, Calcs.RangeE, Calcs.RangeN, Calcs.RangeCCS, Calcs.RangeFF, Calcs.Ranges)

# write.xlsx(Calcs.CCS, file="output/BioTech/Diagnostic/TechDiagnostics.xlsx", sheetName="Effect of CCS", append=FALSE, row.names=TRUE, showNA = TRUE)
# write.xlsx(Calcs.CCSMed, file="output/BioTech/Diagnostic/TechDiagnostics.xlsx", sheetName="Mean Effect of CCS", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(Calcs.Costs, file="output/BioTech/Diagnostic/TechDiagnostics.xlsx", sheetName="Cost Components", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(Calcs.CostsMed, file="output/BioTech/Diagnostic/TechDiagnostics.xlsx", sheetName="Mean Cost Components", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(Calcs.Reg1, file="output/BioTech/Diagnostic/TechDiagnostics.xlsx", sheetName="Regional Variation", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(Calcs.Range, file="output/BioTech/Diagnostic/TechDiagnostics.xlsx", sheetName="Percentile Ranges", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(BakerDat, file="output/BioTech/Diagnostic/TechDiagnostics.xlsx", sheetName="R&D category Count2", append=TRUE, row.names=FALSE, showNA = TRUE)

# Range of CCS penalties
RangeCCSPen.CapCo = Calcs.CCS
RangeCCSPen.CapCo = subset(RangeCCSPen.CapCo, CCS_Diff>0 & SCENARIO==ActScen & Year=="2030" & variable=="CapitalCo")
RangeCCSPen.CapCo <- by(RangeCCSPen.CapCo$Perc_Chang,
                        RangeCCSPen.CapCo$CarrierID,
                        quantile,c(0,0.1,0.5,0.9,1))
RangeCCSPen.CapCo=as.data.frame.list(RangeCCSPen.CapCo)
RangeCCSPen.CapCo <- as.data.frame(t(RangeCCSPen.CapCo))
RangeCCSPen.CapCo$variable <-"CapitalCo"

RangeCCSPen.Eff = Calcs.CCS
RangeCCSPen.Eff = subset(RangeCCSPen.Eff, SCENARIO==ActScen & Year=="2030" & variable=="Efficiency" & wCCS>0)
RangeCCSPen.Eff <- by(RangeCCSPen.Eff$CCS_Diff,
                        RangeCCSPen.Eff$CarrierID,
                        quantile,c(0,0.1,0.5,0.9,1),
                      na.rm=TRUE)
RangeCCSPen.Eff=as.data.frame.list(RangeCCSPen.Eff)
RangeCCSPen.Eff <- as.data.frame(t(RangeCCSPen.Eff))
RangeCCSPen.Eff$variable <-"Efficiency (%pts)"

RangeCCSPen = rbind(RangeCCSPen.CapCo,RangeCCSPen.Eff)
rm(RangeCCSPen.CapCo,RangeCCSPen.Eff)
# write.xlsx(RangeCCSPen, file="output/BioTech/Diagnostic/CCSPenalties.xlsx", sheetName="CCS Penalty Percentiles", append=FALSE, row.names=TRUE, showNA = TRUE)

#
# **** FIGURES FOR DRAFT *****
# ---- FIG 1: Liq+Ele Use ----
SecEnTot1 = subset(SecEnTot, !(Tech3=="Ele Total"|Tech3=="Liq Total"|Tech3=="Hyd Total")&!(MODEL=="FARM 3.1"|MODEL=="COFFEE")&SCENARIO==ActScen)

GBioLiqEleSec <- ggplot(subset(SecEnTot1, Prim=="Biomass"&!(Year=="2000"|Year=="2050")&!(Tech3=="Hea Total")), mapping=aes(x=Year, y=value, fill=TechOrder3)) + 
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("A: DEPLOYMENT OF BIOENERGY TECHNOLOGIES") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Secondary Energy EJ/yr") + xlab("") +
  theme_bw() +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold")) +
  scale_fill_manual(values=c("grey47","grey67","forestgreen","green2","navy","dodgerblue","darkmagenta"),
                    name ="Bio-Technology:",
                    breaks=c('Ele woCCS','Ele wCCS','Liq woCCS','Liq wCCS','Hyd woCCS','Hyd wCCS','Gas Total'),
                    labels=c('Ele woCCS','Ele wCCS','Liq woCCS','Liq wCCS','Hyd woCCS','Hyd wCCS','Gas Total')
  ) +
  facet_wrap(~MODEL, nrow=1, labeller=labeller(MODEL=model_labels, TechOrder3=carrier_labels))
GBioLiqEleSec

GBioSecFrac <- ggplot(subset(SecEnTot2, SCENARIO==ActScen&REGION=="World"&!(CarrierID=="Hea")&!(Year=="2050"))) + 
  geom_point(aes(x=Year, y=BioFrac, colour=TechOrder3, shape=TechOrder3), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("B: FRACTION OF BIOENERGY TECHNOLOGIES IN RESPECTIVE ENERGY CARRIER") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Bioenergy Fraction [-]") +
  theme_bw() +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold")) +
  scale_colour_manual(values=c("grey47","forestgreen","navy","magenta"),
                      name ="Energy Carrier:",
                      breaks=c("Ele Total","Liq Total","Hyd Total","Gas Total"),
                      labels=c("Electricity","Liqudis","Hydrogen","Gas")
  ) +
  scale_shape_manual(values=c(1,2,3,4),
                     name ="Energy Carrier:",
                     breaks=c("Ele Total","Liq Total","Hyd Total","Gas Total"),
                     labels=c("Electricity","Liqudis","Hydrogen","Gas")
  ) +
  facet_grid(~MODEL, labeller=labeller(MODEL=model_labels))
GBioSecFrac

lay<-rbind(1,1,1,1,1,1,1,1,1,
             2,2,2,2,2,2,2,2) 
GBioSec <- grid.arrange(GBioLiqEleSec,GBioSecFrac, layout_matrix=lay)

#
# ---- FIG 2: Bio Cost v. Eff ----
GlobalData.BioCor=as.data.table(GlobalData.Bio)
GlobalData.BioCor=subset(GlobalData.BioCor, Year==2030|Year==2020)
GlobalData.BioCor$x_min <- 0
GlobalData.BioCor[TechOrder2 =="1st gen. ethanol", x_max :=1.1 * max(GlobalData.BioCor$CapitalCo[GlobalData.BioCor$TechOrder2=="1st gen. ethanol"])]
GlobalData.BioCor[TechOrder2 =="Biodeisel", x_max :=1.1 * max(GlobalData.BioCor$CapitalCo[GlobalData.BioCor$TechOrder2=="Biodeisel"])]
GlobalData.BioCor[TechOrder2 =="Lignocellulosic", x_max :=1.1 * max(GlobalData.BioCor$CapitalCo[GlobalData.BioCor$TechOrder2=="Lignocellulosic"])]
GlobalData.BioCor[TechOrder2 =="Other biomass", x_max :=1.1 * max(GlobalData.BioCor$CapitalCo[GlobalData.BioCor$TechOrder2=="Other biomass"])]
GlobalData.BioCor[TechOrder2 =="Liquids", x_max :=1.1 * max(GlobalData.BioCor$CapitalCo[GlobalData.BioCor$TechOrder2=="Liquids"])]
GlobalData.BioCor[TechOrder2 =="Electricity", x_max :=1.1 * max(GlobalData.BioCor$CapitalCo[GlobalData.BioCor$TechOrder2=="Electricity"])]
GlobalData.BioCor[TechOrder2 =="Hydrogen", x_max :=1.1 * max(GlobalData.BioCor$CapitalCo[GlobalData.BioCor$TechOrder2=="Hydrogen"])]
GlobalData.BioCor[TechOrder2 =="Gas", x_max :=1.1 * max(GlobalData.BioCor$CapitalCo[GlobalData.BioCor$TechOrder2=="Gas"])]

# Make correction for 2020 since some models display dummy data for that timestep
GlobalData.BioCor$Efficiency[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="ElectricityBiomasswCCS"&GlobalData.BioCor$Year==2020] <- GlobalData.BioCor$Efficiency[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="ElectricityBiomasswCCS"&GlobalData.BioCor$Year==2030]
GlobalData.BioCor$CapitalCo[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="ElectricityBiomasswCCS"&GlobalData.BioCor$Year==2020] <- GlobalData.BioCor$CapitalCo[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="ElectricityBiomasswCCS"&GlobalData.BioCor$Year==2030]

GlobalData.BioCor$Efficiency[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&GlobalData.BioCor$Year==2020] <- GlobalData.BioCor$Efficiency[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&GlobalData.BioCor$Year==2030]
GlobalData.BioCor$CapitalCo[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&GlobalData.BioCor$Year==2020] <- GlobalData.BioCor$CapitalCo[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&GlobalData.BioCor$Year==2030]

GlobalData.BioCor$Efficiency[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="HydrogenBiomasswCCS"&GlobalData.BioCor$Year==2020] <- GlobalData.BioCor$Efficiency[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="HydrogenBiomasswCCS"&GlobalData.BioCor$Year==2030]
GlobalData.BioCor$CapitalCo[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="HydrogenBiomasswCCS"&GlobalData.BioCor$Year==2020] <- GlobalData.BioCor$CapitalCo[GlobalData.BioCor$MODEL=="MESSAGE-GLOBIOM"&GlobalData.BioCor$VARIABLE=="HydrogenBiomasswCCS"&GlobalData.BioCor$Year==2030]

# For REMIND-MAGPIE there is no CCS data for 2020, use 2030 data instead
# In order to avoid inconsistency between 2020 (noCCS) data and 2030 (CCS) data, ignore the 2020 values 
RM2020Data = subset(GlobalData.BioCor, MODEL=="REMIND-MAGPIE"&Year==2030)
RM2020Data$Year <- 2020

GlobalData.BioCor1 = subset(GlobalData.BioCor, !(MODEL=="REMIND-MAGPIE"&Year==2020))
GlobalData.BioCor1 = rbind(GlobalData.BioCor1,RM2020Data)
rm(RM2020Data)

# FIGURE: Comparison of Efficiencies vs. Capital Costs per model
Data <- unique(GlobalData.BioCor1[,c("TechOrder2")])
Data$Year <- "Literature"

BioEffCost <- ggplot(subset(GlobalData.BioCor1, Year=="2020"&!(CarrierID=="Gas")&!(TechOrder2=="Other biomass")))+
  geom_point(data=subset(LitData, !(Efficiency=="NA"|CapitalCo=="NA"|CarrierID=="Gas")), aes(x=CapitalCo, y=Efficiency, fill=Capt, colour=Capt), alpha=0.5, shape=21, size=1.5) +
  geom_point(data=, aes(x=CapitalCo, y=Efficiency, shape=MODEL, colour=Capt), alpha=0.9, size=1.5) +
  geom_rect(data=subset(Data, !(TechOrder2=="Gas"|TechOrder2=="Other biomass")), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ylim(0,1) + xlab(expression("Capital Costs [US$"[2005]*"/kW"[Out]*"]")) + ylab("Conversion Efficiency [-]") +
  theme_bw() +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1)) +
  scale_colour_manual(values=c("green4","black"),name="",breaks=c("woCCS","wCCS"),labels=c("No CCS", "With CCS")) +
  scale_fill_manual(values=c("green4","black"),name ="",breaks=c("woCCS","wCCS"),labels=c("No CCS", "With CCS")) +
  scale_shape_manual(values=initialShapes,
                     name="",
                     breaks=c("AIM/CGE","BET","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE", "MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM/CGE","BET","DNE21+","GCAM","GRAPE-15","IMACLIM","IMAGE","MESSAGEix-GLOBIOM","POLES","REMIND-MAgPIE")
  ) +
  geom_blank(aes(x=x_max)) + geom_blank(aes(x=x_min)) +
  guides(col = guide_legend(nrow = 2)) +
  facet_grid( Year~TechOrder2, scales="free_x", labeller=labeller(Year = dummy_labeler,TechOrder2=Biotech_labeler)) 
BioEffCost
#
# ---- FIG 3: G. Bio All cost ALL ----
GlobalData.Bio4 = subset(GlobalData.Bio1cor, variable=="LCOE1"|variable=="LCOE3"|variable=="LCOE"|variable=="LCOE3cor"|variable=="LCOEcor")
GlobalData.Bio4 = subset(GlobalData.Bio4, Year=="2030"|Year=="2050"| Year=="2100")
GlobalData.Bio4$Year = as.character(GlobalData.Bio4$Year)
GlobalData.Bio4$value[GlobalData.Bio4$variable=="LCOE"&GlobalData.Bio4$Capt=="woCCS"] <-NA

GBioAllCost4 <- ggplot(subset(GlobalData.Bio4, (CarrierID=="Liq"|CarrierID=="Ele"|CarrierID=="Hyd")&!(Tech2=="Other biomass"|Tech2=="1st gen. ethanol"))) +
  geom_jitter(aes(x=Year, y=value, colour=variable, shape=variable),size=1.25, width=0.2, alpha=0.8) +
  geom_rect(data=Bkgrd, aes(fill = CarrierID), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.03) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  geom_vline(xintercept=c(1.5,2.5), size=0.1, colour="gray10") +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1)) +
  scale_shape_manual(values=c(19,19,19,5,5),
                     name="",
                     breaks=c("LCOE1","LCOE3","LCOE3cor","LCOE","LCOEcor"),
                     labels=c("Capital + O&M","+ Feedstock","+ Feedstock (out of scale)","+ Feedstock & CDR","+ Feedstock & CDR (out of scale)")
  ) +
  scale_colour_manual(values=c("firebrick","black","green4","firebrick","green4"),
                      name="",
                      breaks=c("LCOE1","LCOE3","LCOE3cor","LCOE","LCOEcor"),
                      labels=c("Capital + O&M","+ Feedstock","+ Feedstock (out of scale)","+ Feedstock & CDR","+ Feedstock & CDR (out of scale)")
  ) +
  scale_fill_manual(values=c("grey","dodgerblue","purple"),
                    name="",
                    breaks=c("Ele","Liq","Hyd"),
                    labels=c("Electricity","Liquids","Hydrogen")
                    ,guide=FALSE
  ) +  guides(shape=guide_legend(nrow=2,byrow=TRUE)) +
  facet_grid(TechOrder~MODEL, scales="free", labeller=labeller(MODEL= model_labels, TechOrder=Biotech_labeler)) +
  theme(strip.text.x = element_text(size=fontsize1), strip.text.y = element_text(size=fontsize1))
GBioAllCost4

#
# ---- ***FIG 3: G. Bio All cost ALL ----
GlobalData.Bio5 = subset(GlobalData.Bio1cor, variable=="LCOE1"|variable=="LCOE3"|variable=="LCOE"|variable=="LCOE3cor"|variable=="LCOEcor")
GlobalData.Bio5 = subset(GlobalData.Bio4, Year=="2030"|Year=="2050")
GlobalData.Bio5$Year = as.character(GlobalData.Bio5$Year)
GlobalData.Bio5$value[GlobalData.Bio5$variable=="LCOE"&GlobalData.Bio5$Capt=="woCCS"] <-NA

GBioCapOMCost <- ggplot(subset(GlobalData.Bio5, 
                               (CarrierID=="Liq"|CarrierID=="Ele"|CarrierID=="Hyd")
                               &!(Tech2=="Other biomass"|Tech2=="1st gen. ethanol")
                               &(variable=="LCOE1"))) +
  geom_jitter(aes(x=Year, y=value, colour=variable, shape=MODEL),size=2, width=0.25, alpha=0.8) +
  geom_rect(data=Bkgrd, aes(fill = CarrierID), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.03) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  geom_vline(xintercept=1.5, size=0.1, colour="gray10") +
  ggtitle("Capital and O&M Costs") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +  
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="none", legend.text=element_text(size=fontsize1)) +
  scale_shape_manual(values=initialShapes,
                     name="",
                     breaks=c("AIM/CGE","BET","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE", "MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM/CGE","BET","DNE21+","GCAM","GRAPE-15","IMACLIM","IMAGE","MESSAGEix-GLOBIOM","POLES","REMIND-MAgPIE")
  ) +
  scale_color_manual(values="black") +
  scale_fill_manual(values=c("grey","dodgerblue","purple"),
                    name="",
                    breaks=c("Ele","Liq","Hyd"),
                    labels=c("Electricity","Liquids","Hydrogen")
                    ,guide=FALSE
  ) +  guides(shape=guide_legend(nrow=2,byrow=TRUE)) +
  facet_grid(.~TechOrder, scales="free", labeller=labeller(MODEL= model_labels, TechOrder=Biotech_labeler)) +
  theme(strip.text.x = element_text(size=fontsize1), strip.text.y = element_text(size=fontsize1))
GBioCapOMCost

GBioFeedCost <- ggplot(subset(GlobalData.Bio5, 
                               (CarrierID=="Liq"|CarrierID=="Ele"|CarrierID=="Hyd")
                               &!(Tech2=="Other biomass"|Tech2=="1st gen. ethanol")
                               &(variable=="LCOE3"|variable=="LCOE3cor"))) +
  geom_jitter(aes(x=Year, y=value, colour=variable, shape=MODEL),size=2, width=0.25, alpha=0.8) +
  geom_rect(data=Bkgrd, aes(fill = CarrierID), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.03) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  geom_vline(xintercept=1.5, size=0.1, colour="gray10") +
  ggtitle("+ Feedstock Costs") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +  
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="none", legend.text=element_text(size=fontsize1)) +
  scale_shape_manual(values=initialShapes,
                     name="",
                     breaks=c("AIM/CGE","BET","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE", "MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM/CGE","BET","DNE21+","GCAM","GRAPE-15","IMACLIM","IMAGE","MESSAGEix-GLOBIOM","POLES","REMIND-MAgPIE")
  ) +
  scale_color_manual(values=c("green4","green4")) +
  scale_fill_manual(values=c("grey","dodgerblue","purple"),
                    name="",
                    breaks=c("Ele","Liq","Hyd"),
                    labels=c("Electricity","Liquids","Hydrogen")
                    ,guide=FALSE
  ) +  guides(shape=guide_legend(nrow=2,byrow=TRUE)) +
  facet_grid(.~TechOrder, scales="free", labeller=labeller(MODEL= model_labels, TechOrder=Biotech_labeler)) +
  theme(strip.text.x = element_text(size=fontsize1), strip.text.y = element_text(size=fontsize1))
GBioFeedCost

GBioCDRCost <- ggplot(subset(GlobalData.Bio5, 
                              (CarrierID=="Liq"|CarrierID=="Ele"|CarrierID=="Hyd")
                              &!(Tech2=="Other biomass"|Tech2=="1st gen. ethanol")
                              &(variable=="LCOE"|variable=="LCOEcor")
                              &(Capt=="wCCS"))) +
  geom_jitter(aes(x=Year, y=value, colour=variable, shape=MODEL),size=2, width=0.25, alpha=0.8) +
  geom_rect(data=subset(Bkgrd, TechOrder=="ElectricitywCCS"|TechOrder=="LignocellulosicwCCS"|TechOrder=="HydrogenwCCS"|TechOrder=="BiodeiselwCCS")
            , aes(fill = CarrierID), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.03) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  geom_vline(xintercept=1.5, size=0.1, colour="gray10") +
  ggtitle("+ CDR") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() + 
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.text=element_text(size=fontsize1)) +
  scale_shape_manual(values=initialShapes,
                     name="",
                     breaks=c("AIM/CGE","BET","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE", "MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM/CGE","BET","DNE21+","GCAM","GRAPE-15","IMACLIM","IMAGE","MESSAGEix-GLOBIOM","POLES","REMIND-MAgPIE")
  ) +
  scale_color_manual(values=c("firebrick","firebrick"),
                     guide=FALSE) +
  scale_fill_manual(values=c("grey","dodgerblue","purple"),
                    name="",
                    breaks=c("Ele","Liq","Hyd"),
                    labels=c("Electricity","Liquids","Hydrogen"),
                    guide=FALSE) +  
  guides(shape=guide_legend(ncol=2,byrow=TRUE)) +
  facet_grid(.~TechOrder, scales="free", labeller=labeller(MODEL= model_labels, TechOrder=Biotech_labeler)) +
  theme(strip.text.x = element_text(size=fontsize1), strip.text.y = element_text(size=fontsize1))
GBioCDRCost

lay<-rbind(1,2,3) 
GBioAllCostPanel <- grid.arrange(GBioCapOMCost,GBioFeedCost,GBioCDRCost,
                                 layout_matrix=lay)

#
# ---- FIG 4: G. Cost vs. Use Bio+Fossil 2050 ----
GlobalData2=GlobalData

# Have to determine fraction of Secondary energy each technology accounts for
SecEnTot2.temp = SecEnTot2
SecEnTot2.temp$ID = paste(SecEnTot2.temp$MODEL,SecEnTot2.temp$SCENARIO,SecEnTot2.temp$Year,SecEnTot2.temp$CarrierID)
SecEnTot2.temp = subset(SecEnTot2.temp, REGION=="World")
GlobalData2$ID = paste(GlobalData2$MODEL,GlobalData2$SCENARIO, GlobalData2$Year,GlobalData2$CarrierID)
GlobalData2$TotSecEn <- SecEnTot2.temp[match(GlobalData2$ID,SecEnTot2.temp$ID),5]
GlobalData2$ID <- NULL
GlobalData2 = GlobalData2 %>% mutate(SecEnFrac = (SecEn/TotSecEn)*100)

GBioLiqSecCost2 <- ggplot(subset(GlobalData2, CarrierID=="Liq"&Year=="2050"&SecEn>0.1)) + 
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=TechOrder2, shape=Capt), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("A: DEPLOYMENT OF LIQUID TECHNOLOGIES") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  ylim(0,100) + theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("chocolate","purple","forestgreen","black"),
                      name ="CONVERSION TECHNOLOGY:",
                      breaks=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Liquids"),
                      labels=c("1st Gen. Eth.","Biodiesel","Adv. Biofuel","Fossil")
  ) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No CSS","With CCS")) +
  facet_wrap(~MODEL, scales="free", ncol=5, labeller=labeller(MODEL= model_labels2))
GBioLiqSecCost2

GBioOthSecCost2Dat = subset(GlobalData2, CarrierID=="Ele"&Year=="2050"&SecEn>0.1)
GBioOthSecCost2Dat$Year = as.numeric(substr(GBioOthSecCost2Dat$Year, start=1, stop=4))
GBioOthSecCost2 <- ggplot(GBioOthSecCost2Dat) + 
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=Prim, shape=Capt), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("B: DEPLOYMENT OF ELECTRICITY TECHNOLOGIES") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  scale_y_continuous(breaks=c(0,10,20,30,40,50), limits=c(0,50))+
  theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("green3", "black","purple","brown","blue","pink2","orange","skyblue3"),
                      name ="PRMARY ENERGY CARRIER:", 
                      breaks=c("Biomass","Coal","Gas","Geothermal","Hydro","Nuclear","Solar","Wind"),
                      labels=c("Biomass","Coal","Nat. Gas","Geothermal","Hydro","Nuclear","Solar","Wind")) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No CSS","With CCS")) +
  facet_wrap(~MODEL, scales="free", ncol=5, labeller=labeller(MODEL= model_labels2))
GBioOthSecCost2

lay<-rbind(1,1,1,1,1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2,2,2,2,2,2) 
SecCostFinal2 <- grid.arrange(GBioLiqSecCost2,GBioOthSecCost2, layout_matrix=lay)

#
# ---- ...fix data ----
GlobalData2=GlobalData

# Have to determine fraction of Secondary energy each technology accounts for
SecEnTot2.temp = SecEnTot2
SecEnTot2.temp$ID = paste(SecEnTot2.temp$MODEL,SecEnTot2.temp$SCENARIO,SecEnTot2.temp$Year,SecEnTot2.temp$CarrierID)
SecEnTot2.temp = subset(SecEnTot2.temp, REGION=="World")
GlobalData2$ID = paste(GlobalData2$MODEL,GlobalData2$SCENARIO, GlobalData2$Year,GlobalData2$CarrierID)
GlobalData2$TotSecEn <- SecEnTot2.temp[match(GlobalData2$ID,SecEnTot2.temp$ID),5]
GlobalData2$ID <- NULL
GlobalData2 = GlobalData2 %>% mutate(SecEnFrac = (SecEn/TotSecEn)*100)

# Make new variables which indicate the movement of the LCOE and SecEnFrac over time. Used to draw arrows
GlobalData2$ID <- paste(GlobalData2$MODEL, GlobalData2$Year, GlobalData2$VARIABLE)
GlobalData2 = unique(GlobalData2) #Removes some duplicate observations
# FIRST DO FOR LIQUIDS
  # Calculate change in SecEnFrac (y-axis)
Diff_Frac=subset(GlobalData2, (Year==2050|Year==2100)&CarrierID=="Liq"&SecEnFrac>0.01)
Diff_Frac=subset(Diff_Frac, select=c(MODEL,SCENARIO,VARIABLE,Year,CarrierID,SecEnFrac,TechOrder2))
Diff_Frac = unique(Diff_Frac) #Removes some duplicate observations
Diff_Frac = na.omit(Diff_Frac)
Diff_Frac=spread(Diff_Frac,Year,SecEnFrac)
colnames(Diff_Frac)[6:7]<-c("x2050","x2100")
Diff_Frac=Diff_Frac %>% mutate(Diff=x2100-x2050)
Diff_Frac$ID <- paste(Diff_Frac$MODEL, "2050", Diff_Frac$VARIABLE)
  # Calculate maximumlength of axis (to help determine arrow length)
Diff_Frac = melt(Diff_Frac, id.vars=c("MODEL","SCENARIO","VARIABLE","CarrierID","TechOrder2","Diff","ID"))
Diff_Frac$ylen <- 100 # Lenth of y-axis is always 100 since scale is a percentage
  # Calculate change in LCOE (x-axis)
Diff_LCOE=subset(GlobalData2, (Year==2050|Year==2100)&CarrierID=="Liq"&SecEnFrac>0.01)
Diff_LCOE=subset(Diff_LCOE, select=c(MODEL,SCENARIO,VARIABLE,Year,CarrierID,LCOE,TechOrder2))
Diff_LCOE = unique(Diff_LCOE) #Removes some duplicate observations
Diff_LCOE = na.omit(Diff_LCOE)
Diff_LCOE=spread(Diff_LCOE,Year,LCOE)
colnames(Diff_LCOE)[6:7]<-c("x2050","x2100")
Diff_LCOE=Diff_LCOE %>% mutate(Diff=x2100-x2050)
Diff_LCOE$ID <- paste(Diff_LCOE$MODEL, "2050", Diff_LCOE$VARIABLE)
  # Calculate maximumlength of axis (to help determine arrow length)
x_max = aggregate(Diff_LCOE$x2050, by=list(MODEL=Diff_LCOE$MODEL), FUN=max, na.rm=TRUE)
x_min = aggregate(Diff_LCOE$x2050, by=list(MODEL=Diff_LCOE$MODEL), FUN=min, na.rm=TRUE)
Diff_LCOE$x_max = x_max[match(Diff_LCOE$MODEL,x_max$MODEL),"x"]
Diff_LCOE$x_min = x_min[match(Diff_LCOE$MODEL,x_min$MODEL),"x"]
Diff_LCOE = Diff_LCOE %>% mutate(xlen=((x_min>=0)*x_max)+((x_min<0)*(x_max-x_min)))
  # Merge x&y calculations into dataframe
GlobalData2_Liq = subset(GlobalData2, (Year==2050|Year==2100)&CarrierID=="Liq")
GlobalData2_Liq$x_diff = Diff_LCOE[match(GlobalData2_Liq$ID,Diff_LCOE$ID),"Diff"]
GlobalData2_Liq$y_diff = Diff_Frac[match(GlobalData2_Liq$ID,Diff_Frac$ID),"Diff"]
GlobalData2_Liq$x_len = Diff_LCOE[match(GlobalData2_Liq$ID,Diff_LCOE$ID),"xlen"]
GlobalData2_Liq$y_len = Diff_Frac[match(GlobalData2_Liq$ID,Diff_Frac$ID),"ylen"]

# SAME BUT FOR ELECTRICITY
  # Calculate change in SecEnFrac
Diff_Frac=subset(GlobalData2, (Year==2050|Year==2100)&CarrierID=="Ele"&SecEnFrac>0.01)
Diff_Frac=subset(Diff_Frac, select=c(MODEL,SCENARIO,VARIABLE,Year,CarrierID,SecEnFrac,TechOrder2))
Diff_Frac = unique(Diff_Frac) #Removes some duplicate observations
Diff_Frac = na.omit(Diff_Frac)
Diff_Frac=spread(Diff_Frac,Year,SecEnFrac)
colnames(Diff_Frac)[6:7]<-c("x2050","x2100")
Diff_Frac=Diff_Frac %>% mutate(Diff=x2100-x2050)
Diff_Frac$ID <- paste(Diff_Frac$MODEL, "2050", Diff_Frac$VARIABLE)
  # Calculate maximumlength of axis (to help determine arrow length)
Diff_Frac = melt(Diff_Frac, id.vars=c("MODEL","SCENARIO","VARIABLE","CarrierID","TechOrder2","Diff","ID"))
Diff_Frac$ylen <- 100 # Lenth of y-axis is always 100 since scale is a percentage
  # Calculate change in LCOE
Diff_LCOE=subset(GlobalData2, (Year==2050|Year==2100)&CarrierID=="Ele"&SecEnFrac>0.01)
Diff_LCOE=subset(Diff_LCOE, select=c(MODEL,SCENARIO,VARIABLE,Year,CarrierID,LCOE,TechOrder2))
Diff_LCOE = unique(Diff_LCOE) #Removes some duplicate observations
Diff_LCOE = na.omit(Diff_LCOE)
Diff_LCOE=spread(Diff_LCOE,Year,LCOE)
colnames(Diff_LCOE)[6:7]<-c("x2050","x2100")
Diff_LCOE=Diff_LCOE %>% mutate(Diff=x2100-x2050)
Diff_LCOE$ID <- paste(Diff_LCOE$MODEL, "2050", Diff_LCOE$VARIABLE)
  # Calculate maximumlength of axis (to help determine arrow length)
x_max = aggregate(Diff_LCOE$x2050, by=list(MODEL=Diff_LCOE$MODEL), FUN=max, na.rm=TRUE)
x_min = aggregate(Diff_LCOE$x2050, by=list(MODEL=Diff_LCOE$MODEL), FUN=min, na.rm=TRUE)
Diff_LCOE$x_max = x_max[match(Diff_LCOE$MODEL,x_max$MODEL),"x"]
Diff_LCOE$x_min = x_min[match(Diff_LCOE$MODEL,x_min$MODEL),"x"]
Diff_LCOE = Diff_LCOE %>% mutate(xlen=((x_min>=0)*x_max)+((x_min<0)*(x_max-x_min)))
  # Merge into dataframe
GlobalData2_Ele = subset(GlobalData2, (Year==2050|Year==2100)&CarrierID=="Ele")
GlobalData2_Ele$y_diff = Diff_Frac[match(GlobalData2_Ele$ID,Diff_Frac$ID),"Diff"]
GlobalData2_Ele$x_diff = Diff_LCOE[match(GlobalData2_Ele$ID,Diff_LCOE$ID),"Diff"]
GlobalData2_Ele$x_len = Diff_LCOE[match(GlobalData2_Ele$ID,Diff_LCOE$ID),"xlen"]
GlobalData2_Ele$y_len = Diff_Frac[match(GlobalData2_Ele$ID,Diff_Frac$ID),"ylen"]

# Final Dataframe for figure
GlobalData3 = rbind(GlobalData2_Liq, GlobalData2_Ele)
# Have to make sure arrow lengths are constant. Draw arrows based on congruent triangles
GlobalData3 = subset(GlobalData3, select = c(MODEL,SCENARIO,Year,CarrierID,Prim,Capt,SecEnFrac,LCOE,TechOrder2,y_diff,x_diff,x_len,y_len))
GlobalData3 = subset(GlobalData3, Year==2050|Year==2100)
GlobalData3 = GlobalData3 %>% mutate(tantheta = y_diff/x_diff)
GlobalData3 = GlobalData3 %>% mutate(theta = atan(tantheta))
# Due to different x&y scales, have to make an "arrow length multiplier" dependent on its angle
GlobalData3 = GlobalData3 %>% mutate(len_c = x_len/y_len) # The length multiplier at theta=0, i.e. a horizontal line
GlobalData3 = GlobalData3 %>% mutate(len_mult = (len_c-1)*exp(-4*abs(theta))+1) #multiplier based on exponential decay: The smaller the angle, the greater the multiplier
GlobalData3$linlen <- 20 #base  arrow length, @ theta = 90degrees

GlobalData3 = GlobalData3 %>% mutate(dx = ((x_diff < 0)
                                           * -1 * linlen * len_mult * cos(abs(theta)) 
                                           ) +
                                       ((x_diff >=0)
                                        * linlen * len_mult * cos(abs(theta))
                                        )
                                     )
GlobalData3 = GlobalData3 %>% mutate(dy = ((y_diff < 0)
                                           * -1 * linlen * len_mult * sin(abs(theta))
                                           ) +
                                       ((y_diff >=0)
                                        * linlen * len_mult * sin(abs(theta))
                                        )
                                     )

GlobalData3 = GlobalData3 %>% mutate(xend = LCOE+dx)
GlobalData3 = GlobalData3 %>% mutate(yend = SecEnFrac+dy)
#
# ---- ...fig4b----
GBioLiqSecCost2b <- ggplot(subset(GlobalData3, CarrierID=="Liq"&Year=="2050"&SecEnFrac>0.05)) + 
  geom_segment(aes(x=LCOE, xend=xend, y = SecEnFrac, yend=yend, colour=TechOrder2), alpha=0.3,
              arrow=arrow(angle=30, length=unit(arrowsize,"cm"), ends="last", type="open")) +
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=TechOrder2, shape=Capt), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("A: DEPLOYMENT OF LIQUID TECHNOLOGIES") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  ylim(0,100) + theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("chocolate","purple","forestgreen","black"),
                      name ="CONVERSION TECHNOLOGY:",
                      breaks=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Liquids"),
                      labels=c("1st Gen. Eth.","Biodiesel","Adv. Biofuel","Fossil")
  ) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No CSS","With CCS")) +
  facet_wrap(~MODEL, scales="free", ncol=5, labeller=labeller(MODEL= model_labels2))
GBioLiqSecCost2b

GBioOthSecCost3Dat = subset(GlobalData3, CarrierID=="Ele"&Year=="2050"&SecEnFrac>0.05)
GBioOthSecCost3Dat$Year = as.numeric(substr(GBioOthSecCost3Dat$Year, start=1, stop=4))
GBioOthSecCost2b <- ggplot(GBioOthSecCost3Dat) + 
  geom_segment(aes(x=LCOE, xend=xend, y = SecEnFrac, yend=yend, colour=Prim), alpha=0.3,
              arrow=arrow(angle=30, length=unit(arrowsize,"cm"), ends="last", type="open")) +
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=Prim, shape=Capt), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("B: DEPLOYMENT OF ELECTRICITY TECHNOLOGIES") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60), limits=c(0,65))+
  theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("green3", "black","purple","brown","blue","pink2","orange","skyblue3"),
                      name ="PRMARY ENERGY CARRIER:", 
                      breaks=c("Biomass","Coal","Gas","Geothermal","Hydro","Nuclear","Solar","Wind"),
                      labels=c("Biomass","Coal","Nat. Gas","Geothermal","Hydro","Nuclear","Solar","Wind")) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No CSS","With CCS")) +
  facet_wrap(~MODEL, scales="free_x", ncol=5, labeller=labeller(MODEL= model_labels2))
GBioOthSecCost2b

lay<-rbind(1,1,1,1,1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2,2,2,2,2,2) 
SecCostFinal2b <- grid.arrange(GBioLiqSecCost2b,GBioOthSecCost2b, layout_matrix=lay)

#
# ---- ...fig4c----
GBioLiqSecCost2c <- ggplot(subset(GlobalData3, CarrierID=="Liq"&Year=="2050"&SecEnFrac>0.05)) + 
  geom_segment(aes(x=LCOE, xend=LCOE+x_diff, y = SecEnFrac, yend=SecEnFrac+y_diff, colour=TechOrder2), alpha=0.3,
               arrow=arrow(angle=30, length=unit(arrowsize,"cm"), ends="last", type="open")) +
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=TechOrder2, shape=Capt), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("A: DEPLOYMENT OF LIQUID TECHNOLOGIES") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  ylim(0,100) + theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("chocolate","purple","forestgreen","black"),
                      name ="CONVERSION TECHNOLOGY:",
                      breaks=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Liquids"),
                      labels=c("1st Gen. Eth.","Biodiesel","Adv. Biofuel","Fossil")
  ) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No CSS","With CCS")) +
  facet_wrap(~MODEL, scales="free", ncol=5, labeller=labeller(MODEL= model_labels2))
GBioLiqSecCost2c

GBioOthSecCost3Dat = subset(GlobalData3, CarrierID=="Ele"&Year=="2050"&SecEnFrac>0.05)
GBioOthSecCost3Dat$Year = as.numeric(substr(GBioOthSecCost3Dat$Year, start=1, stop=4))
GBioOthSecCost2c <- ggplot(GBioOthSecCost3Dat) + 
  geom_segment(aes(x=LCOE, xend=LCOE+x_diff, y = SecEnFrac, yend=SecEnFrac+y_diff, colour=Prim), alpha=0.3,
               arrow=arrow(angle=30, length=unit(arrowsize,"cm"), ends="last", type="open")) +
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=Prim, shape=Capt), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("B: DEPLOYMENT OF ELECTRICITY TECHNOLOGIES") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  scale_y_continuous(breaks=c(10,20,30,40,50))+
  theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("green3", "black","purple","brown","blue","pink2","orange","skyblue3"),
                      name ="PRMARY ENERGY CARRIER:", 
                      breaks=c("Biomass","Coal","Gas","Geothermal","Hydro","Nuclear","Solar","Wind"),
                      labels=c("Biomass","Coal","Nat. Gas","Geothermal","Hydro","Nuclear","Solar","Wind")) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No CSS","With CCS")) +
  facet_wrap(~MODEL, scales="free_x", ncol=5, labeller=labeller(MODEL= model_labels2))
GBioOthSecCost2c

lay<-rbind(1,1,1,1,1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2,2,2,2,2,2) 
SecCostFinal2c <- grid.arrange(GBioLiqSecCost2c,GBioOthSecCost2c, layout_matrix=lay)

#
# ---- ...fig4d----
GlobalData4 = subset(GlobalData2, select=c(MODEL,SCENARIO,VARIABLE,Year,CarrierID,Prim,Capt,SecEnFrac,TechOrder2,LCOE))
GlobalData4 = subset(GlobalData4, Year==2050|Year==2060|Year==2070|Year==2080|Year==2090|Year==2100)
GlobalData4$Year <- as.numeric(GlobalData4$Year)

GBioLiqSecCost2d <- ggplot(subset(GlobalData4, CarrierID=="Liq"&Year=="2050"&SecEnFrac>0.1)) + 
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=TechOrder2, shape=Capt), size=2) +
  geom_path(data=subset(GlobalData4, CarrierID=="Liq"&!(Year==2030)&SecEnFrac>0.1), 
            aes(x=LCOE, y = SecEnFrac, group = VARIABLE, colour = TechOrder2), alpha=0.3,
            arrow=arrow(angle=30, length=unit(arrowsize,"cm"), ends="last", type="open")) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("A: DEPLOYMENT OF LIQUID TECHNOLOGIES") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  ylim(0,100) + theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("chocolate","purple","forestgreen","black"),
                      name ="CONVERSION TECHNOLOGY:",
                      breaks=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Liquids"),
                      labels=c("1st Gen. Eth.","Biodiesel","Adv. Biofuel","Fossil")) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No CSS","With CCS")) +
  facet_wrap(~MODEL, scales="free", ncol=5, labeller=labeller(MODEL= model_labels2))
GBioLiqSecCost2d

GBioOthSecCost3Dat = subset(GlobalData3, CarrierID=="Ele"&Year=="2050"&SecEnFrac>0.1)
GBioOthSecCost3Dat$Year = as.numeric(substr(GBioOthSecCost3Dat$Year, start=1, stop=4))
GBioOthSecCost2d <- ggplot(GBioOthSecCost3Dat) + 
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=Prim, shape=Capt), size=2) +
  geom_path(data=subset(GlobalData4, CarrierID=="Ele"&!(Year==2030)&SecEnFrac>0.1), 
            aes(x=LCOE, y = SecEnFrac, group = VARIABLE, colour = Prim), alpha=0.3,
            arrow=arrow(angle=30, length=unit(arrowsize,"cm"), ends="last", type="open")) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("B: DEPLOYMENT OF ELECTRICITY TECHNOLOGIES") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  scale_y_continuous(breaks=c(10,20,30,40,50))+
  theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("green3", "black","purple","brown","blue","pink2","orange","skyblue3"),
                      name ="PRIMARY ENERGY CARRIER:", 
                      breaks=c("Biomass","Coal","Gas","Geothermal","Hydro","Nuclear","Solar","Wind"),
                      labels=c("Biomass","Coal","Nat. Gas","Geothermal","Hydro","Nuclear","Solar","Wind")) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No CSS","With CCS")) +
  facet_wrap(~MODEL, scales="free_x", ncol=5, labeller=labeller(MODEL= model_labels2))
GBioOthSecCost2d

lay<-rbind(1,1,1,1,1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2,2,2,2,2,2) 
SecCostFinal2d <- grid.arrange(GBioLiqSecCost2d,GBioOthSecCost2d, layout_matrix=lay)

#
# ---- FIG S1: Regional Parameters ----
# Get relevant dataset
TechDataR = Calcs1
TechDataR$TechOrder2 <-gsub("wCCS","",TechDataR$TechOrder)
# Make correction for 2020 since some models display dummy data for that timestep
TechDataR$Efficiency[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="ElectricityBiomasswCCS"&TechDataR$Year==2020] <- TechDataR$Efficiency[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="ElectricityBiomasswCCS"&TechDataR$Year==2030]
TechDataR$CapitalCo[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="ElectricityBiomasswCCS"&TechDataR$Year==2020] <- TechDataR$CapitalCo[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="ElectricityBiomasswCCS"&TechDataR$Year==2030]
TechDataR$Efficiency[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&TechDataR$Year==2020] <- TechDataR$Efficiency[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&TechDataR$Year==2030]
TechDataR$CapitalCo[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&TechDataR$Year==2020] <- TechDataR$CapitalCo[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&TechDataR$Year==2030]
TechDataR$Efficiency[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="HydrogenBiomasswCCS"&TechDataR$Year==2020] <- TechDataR$Efficiency[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="HydrogenBiomasswCCS"&TechDataR$Year==2030]
TechDataR$CapitalCo[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="HydrogenBiomasswCCS"&TechDataR$Year==2020] <- TechDataR$CapitalCo[TechDataR$MODEL=="MESSAGE-GLOBIOM"&TechDataR$VARIABLE=="HydrogenBiomasswCCS"&TechDataR$Year==2030]
TechDataR2 = melt(TechDataR, id.vars=c("MODEL","SCENARIO","REGION","Year","VARIABLE","Capt","TechOrder","TechOrder2"))
# Literature data
LitData2 = subset(LitData, select=-c(Ref,TechType,CapitalCoPerIn))
LitData2 = melt(LitData2, id.vars=c("VARIABLE","CarrierID","Prim","Tech","Capt","Tech2","TechOrder2","Year"))
LitData2$VarID = paste(LitData2$Tech,LitData2$Capt,LitData2$variable)
LitData2.max <- aggregate(LitData2$value, by=list(VarID=LitData2$VarID), max, na.rm=TRUE)
LitData2.min <- aggregate(LitData2$value, by=list(VarID=LitData2$VarID), min, na.rm=TRUE)
# Hydrogen has identical min-max efficiency (0.35). Set them appart
LitData2.max$x[LitData2.max$VarID=="Hydrogen woCCS Efficiency"] <- 0.37
LitData2.min$x[LitData2.min$VarID=="Hydrogen woCCS Efficiency"] <- 0.33
# Ligno Liquids wCCS have identical min-max efficiency (0.465) and CapitalCo (362.82) . Set them appart
LitData2.max$x[LitData2.max$VarID=="Lignocellulosic wCCS Efficiency"] <- 0.475
LitData2.min$x[LitData2.min$VarID=="Lignocellulosic wCCS Efficiency"] <- 0.435
LitData2.max$x[LitData2.max$VarID=="Lignocellulosic wCCS CapitalCo"] <- 402
LitData2.min$x[LitData2.min$VarID=="Lignocellulosic wCCS CapitalCo"] <- 322

LitData2$Max <- LitData2.max[match(LitData2$VarID, LitData2.max$VarID),2]
LitData2$Min <- LitData2.min[match(LitData2$VarID, LitData2.min$VarID),2]
# Remove duplicates
LitData2$VARIABLE <- NULL
LitData2 =LitData2[!duplicated(LitData2),]

# Have to repeat literature data across models in order to add them to the panels
LitData2$MODEL <- NA
LitData2.temp = LitData2
for(i in unique(TechDataR2$MODEL)){
  LitData2.model = LitData2.temp
  LitData2.model$MODEL <- i
  LitData2 = rbind(LitData2,LitData2.model)
}
LitData2=subset(LitData2, !MODEL=="NA")
rm(LitData2.max,LitData2.min,LitData2.model,LitData2.temp)
# Remove specific datapoints in orde rto avoid over-shading of the area (have 1 set of points per technology, the min-max)
LitData2$value <- NULL
LitData2 =LitData2[!duplicated(LitData2),]

BioCostR2 <- ggplot(subset(TechDataR2, Year=="2020"&(variable=="CapitalCo")&!TechOrder=="Gas"))+
  geom_point(data=, aes(x=REGION, y=value, colour=Capt, fill=Capt, shape=Capt), alpha=0.4, size=1.5) +
  geom_rect(data=subset(LitData2, !(TechOrder2=="Gas")), aes(xmin=-Inf, xmax=Inf, ymin=Min, ymax=Max, fill=Capt), alpha = 0.2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("A: Capital Costs") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab(expression("Capital Costs [US$"[2005]*"/kW"[Out]*"]")) + xlab("") + #ylab("Conversion Efficiency [-]") +
  theme_bw() +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="none", legend.text=element_text(size=fontsize1)) +
  scale_colour_manual(values=c("green4","black"),name="",breaks=c("woCCS","wCCS"),labels=c("No CCS", "With CCS")) +
  scale_fill_manual(values=c("green4","black"),name ="",breaks=c("woCCS","wCCS"),labels=c("No CCS", "With CCS")) +
  scale_shape_manual(values=c(2,1),name="",breaks=c("woCCS","wCCS"),labels=c("No CCS","With CCS")) +
  facet_grid(TechOrder2~MODEL, labeller=labeller(TechOrder2=Biotech_labeler, MODEL=model_labels), scales="free_y") 
BioCostR2

BioEffR2 <- ggplot(subset(TechDataR2, Year=="2020"&(variable=="Efficiency")&!TechOrder=="Gas"))+
  geom_point(data=, aes(x=REGION, y=value, colour=Capt, fill=Capt, shape=Capt), alpha=0.4, size=1.5) +
  geom_rect(data=subset(LitData2, !(TechOrder2=="Gas")), aes(xmin=-Inf, xmax=Inf, ymin=Min, ymax=Max, fill=Capt), alpha = 0.2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("B: Conversion Efficiency") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylim(0,1) + 
  xlab("") + ylab("Conversion Efficiency [-]") +
  theme_bw() +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1)) +
  scale_colour_manual(values=c("green4","black"),name="",breaks=c("woCCS","wCCS"),labels=c("No CCS", "With CCS")) +
  scale_fill_manual(values=c("green4","black"),name ="",breaks=c("woCCS","wCCS"),labels=c("No CCS", "With CCS")) +
  scale_shape_manual(values=c(2,1),name="",breaks=c("woCCS","wCCS"),labels=c("No CCS","With CCS")) +
  facet_grid(TechOrder2~MODEL, labeller=labeller(TechOrder2=Biotech_labeler, MODEL=model_labels), scales="free_y") 
BioEffR2

lay<-rbind(1,1,1,1,1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2,2,2,2,2,2) 
BioEffCostR2 <- grid.arrange(BioCostR2,BioEffR2, layout_matrix=lay)

#
# ---- FIG S2: Histograms of techno-economic parameters ----
Calcs.Costs1=melt(Calcs.Costs, id.vars=c("MODEL","SCENARIO","REGION","Year","VARIABLE","Capt","TechOrder","MedID"), variable_name="variable")
Calcs.Costs1=subset(Calcs.Costs1, Year=="2030")
Calcs.Costs1=subset(Calcs.Costs1, variable=="CapitalCo"|variable=="Efficiency"|variable=="LCOE1"|variable=="FeedFrac")
Calcs.Costs1$value = as.numeric(substr(Calcs.Costs1$value,start=1,stop=5))

TechDistr <- ggplot(data=subset(Calcs.Costs1, MedID=="Liq woCCS 2030"|MedID=="Ele woCCS 2030"),aes(value, fill=MedID)) +
  geom_histogram(bins = "15",position="dodge") +   geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("A: Histogram of 2030 model values for key techno-economic parameters") + theme(plot.title = element_text(lineheight=20, face="bold", size=fontsize3)) +
  xlab("") + ylab("Count") +  theme_bw() +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  scale_fill_manual(values=c("forestgreen","chocolate"),name ="Parameter",breaks=c("Liq woCCS","Ele woCCS"),labels=c("Liquids","Electricity"),guide=FALSE) +
  facet_grid(.~variable, scales="free_x", labeller=labeller(variable=var_labeler))
TechDistr

CCSPen <- ggplot(data=subset(Calcs.CCS, (variable=="CapitalCo"|variable=="Efficiency")&Year=="2030"&!(CarrierID=="Hyd"|CarrierID=="Gas")&!(CCS_Diff<0&variable=="CapitalCo")&!(CCS_Diff>0&variable=="Efficiency")&!(wCCS==0)),
                 aes(CCS_Diff, fill=CarrierID)) +  geom_histogram(bins = "30",position="dodge") + geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("B: Histogram of 2030 model values for cost and efficiency penalty of CCS") + theme(plot.title = element_text(lineheight=20, face="bold", size=fontsize3)) +
  xlab("") +  ylab("Count") +  theme_bw() +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  scale_fill_manual(values=c("forestgreen","chocolate"),name ="Energy Carrier",breaks=c("Liq","Ele"),labels=c("Liquids","Electricity")) +
  facet_grid(.~variable, scales="free_x", labeller=labeller(variable=var_labeler2))
CCSPen

AssumpHist <- grid.arrange(TechDistr,CCSPen)
#
# ---- FIG S3: G. Bio Cap+OM Cost ALL ----
GlobalData.Bio2 = subset(GlobalData.Bio2, Year=="2030"|Year=="2050"| Year=="2100")
GlobalData.Bio2$Year = as.character(GlobalData.Bio2$Year)
# For some models OM and Capital Costs are not dissagregated. For these only present Cap costs
GlobalData.Bio2$value[GlobalData.Bio2$MODEL=="BET"&GlobalData.Bio2$CarrierID=="Liq"&GlobalData.Bio2$variable=="LCOE1"] <- NA
GlobalData.Bio2$value[GlobalData.Bio2$MODEL=="GRAPE-15"&GlobalData.Bio2$variable=="LCOE1"] <- NA
GlobalData.Bio2$value[GlobalData.Bio2$MODEL=="GCAM_EMF33"&GlobalData.Bio2$variable=="LCOE1"&GlobalData.Bio2$CarrierID=="Liq"] <- NA

# All Carriers Combined
Bkgrd <- unique(GlobalData.Bio2[,c("CarrierID","TechOrder","Year")])
Bkgrd =subset(Bkgrd, (CarrierID=="Liq"|CarrierID=="Ele"|CarrierID=="Hyd")&!(TechOrder=="Other biomass"|TechOrder=="1st gen. ethanol"|TechOrder=="1st gen. ethanolwCCS"))

GBioAllCost <- ggplot(subset(GlobalData.Bio2, (CarrierID=="Liq"|CarrierID=="Ele"|CarrierID=="Hyd")&!(TechOrder=="Other biomass"|TechOrder=="1st gen. ethanol"|TechOrder=="1st gen. ethanolwCCS"))) +
  geom_jitter(aes(x=Year, y=value, colour=variable, shape=variable), width=0.2, alpha=0.8) +
  geom_rect(data=Bkgrd, aes(fill = CarrierID), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.03) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=c(1.5,2.5), size=0.1, colour="gray10") +
  xlab("") + ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_shape_manual(values=c(0,4),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1"),
                     labels=c("CapEx", "+ O&M")) +
  scale_colour_manual(values=c("black","red"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1"),
                      labels=c("CapEx", "+ O&M")) +
  scale_fill_manual(values=c("grey","dodgerblue","purple"),
                    name="",
                    breaks=c("Ele","Liq","Hyd"),
                    labels=c("Electricity","Liquids","Hydrogen")
                    ,guide=FALSE) +
  facet_grid(TechOrder~MODEL, scales="free", labeller=labeller(MODEL= model_labels, TechOrder=Biotech_labeler)) +
  theme(strip.text.x = element_text(size=fontsize1), strip.text.y = element_text(size=fontsize1))
GBioAllCost

#
# ---- FIG S4: LCOE vs Ctax ----
LCOEvCtax <- ggplot(subset(GlobalData, Capt=="wCCS"&(Year=="2050"|Year=="2100")&Prim=="Biomass"&SecEn>0.1)) + 
  geom_point(aes(x=LCOE, y=Ctax, colour=MODEL, shape=MODEL), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab(expression("Carbon tax, US$"[2005]*"/tCO"[2])) + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  ylim(0,3000) +  theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +  theme(strip.text.y = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_color_manual(values=c("burlywood4","red","darkgoldenrod1","blue","azure4","limegreen","coral2","burlywood","darkolivegreen","darkorchid1"),
                     name="Model",
                     breaks=c("AIM/CGE","BET","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE", "MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM/CGE","BET","DNE21+","GCAM","GRAPE-15","IMACLIM","IMAGE","MESSAGEix-GLOBIOM","POLES","REMIND-MAgPIE")) +
  scale_shape_manual(values=initialShapes,
                     name="Model",
                     breaks=c("AIM/CGE","BET","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE", "MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM/CGE","BET","DNE21+","GCAM","GRAPE-15","IMACLIM","IMAGE","MESSAGEix-GLOBIOM","POLES","REMIND-MAgPIE")) +
  facet_grid(Year~CarrierID, scales="free", labeller=labeller(MODEL= model_labels, CarrierID = carrier_labels))
LCOEvCtax

#
# ---- FIG S5: Caputre Rate vs Deployment ----
CaptureRates$Tech = paste(CaptureRates$CARRIER) 
CaptureRates$Tech <- gsub("Biomass","",CaptureRates$Tech,fixed=F)  
CaptureRates$Tech <- gsub("woCCS","",CaptureRates$Tech,fixed=F)  
CaptureRates$Tech <- gsub("wCCS","",CaptureRates$Tech,fixed=F)  
CaptureRates$Tech <- gsub("LiquidsBiodiesel","Biodeisel",CaptureRates$Tech,fixed=F)  
CaptureRates$Tech <- gsub("LiquidsCellulosicNondiesel","Lignocellulosic",CaptureRates$Tech,fixed=F)  
CaptureRates$Tech <- gsub("LiquidsConventionalthanol","1st gen. ethanol",CaptureRates$Tech,fixed=F)  
CaptureRates$Tech <- gsub("LiquidsOther","Other biomass",CaptureRates$Tech,fixed=F)  
CaptureRates = subset(CaptureRates, select=c(MODEL,CarrierID,Capt,variable,value,Tech))
CaptureRates=spread(CaptureRates,variable,value)
CaptureRates$ID = paste(CaptureRates$MODEL,CaptureRates$Tech)


GlobalData.CR = subset(GlobalData3, (Year=="2050"|Year=="2100") & Prim=="Biomass" & Capt=="wCCS")
GlobalData.CR = subset(GlobalData.CR, select=c(MODEL,Year,CarrierID,Prim,Capt,SecEnFrac,TechOrder2))
GlobalData.CR$ID = paste(GlobalData.CR$MODEL,GlobalData.CR$TechOrder2)

GlobalData.CR$CaptRate = CaptureRates[match(GlobalData.CR$ID,CaptureRates$ID),5]

CaptureCorr <- ggplot(GlobalData.CR) + 
  geom_point(aes(x=CaptRate, y=SecEnFrac, colour=MODEL, shape=MODEL), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab("Fraction of Secondary Energy (%)") + xlab("Capture Rate [-]") +
  ylim(0,100) + xlim(0,1) + theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_color_manual(values=c("burlywood4","red","darkgoldenrod1","blue","azure4","limegreen","coral2","burlywood","darkolivegreen","darkorchid1"),
                     name="Model",
                     breaks=c("AIM/CGE","BET","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE", "MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM/CGE","BET","DNE21+","GCAM","GRAPE-15","IMACLIM","IMAGE","MESSAGEix-GLOBIOM","POLES","REMIND-MAgPIE")) +
  scale_shape_manual(values=initialShapes,
                     name="Model",
                     breaks=c("AIM/CGE","BET","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE", "MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM/CGE","BET","DNE21+","GCAM","GRAPE-15","IMACLIM","IMAGE","MESSAGEix-GLOBIOM","POLES","REMIND-MAgPIE")) +
  facet_grid(Year~CarrierID, scales="free", labeller=labeller(MODEL= model_labels2, CarrierID=carrier_labels))
CaptureCorr

# 
# ---- FIG S6: G. Cost vs. Use Bio+Fossil 2100----
GBioLiqSecCost2100 <- ggplot(subset(GlobalData2, CarrierID=="Liq"&Year=="2100"&SecEnFrac>0.01)) + 
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=TechOrder2, shape=Capt), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("A: DEPLOYMENT OF LIQUID TECHNOLOGIES") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  ylim(0,100) + theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("chocolate","purple","forestgreen","cornflowerblue","black"),
                      name ="CONVERSION TECHNOLOGY:",
                      breaks=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Other biomass","Liquids"),
                      labels=c("1st Gen. Eth.","Biodiesel","Adv. Biofuel","Other Biomass","Fossil")) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No CSS","With CCS")) +
  facet_wrap(~MODEL, scales="free", ncol=5, labeller=labeller(MODEL= model_labels2))
GBioLiqSecCost2100

GBioOthSecCost2100Dat = subset(GlobalData2, CarrierID=="Ele"&Year=="2100"&SecEnFrac>0.01)
GBioOthSecCost2100 <- ggplot(GBioOthSecCost2100Dat) +  
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=Prim, shape=Capt), size=2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("B: DEPLOYMENT OF ELECTRICITY TECHNOLOGIES") + theme(plot.title = element_text(face="bold", size=fontsize3)) +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  ylim(0,100) + theme_bw() +
  theme(strip.text.x = element_text(size = fontsize1, face="plain")) +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize1), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("green3", "black","purple","brown","blue","pink2","orange","skyblue3"),
                      name ="PRIMARY ENERGY CARRIER:", 
                      breaks=c("Biomass","Coal","Gas","Geothermal","Hydro","Nuclear","Solar","Wind"),
                      labels=c("Biomass","Coal","Nat. Gas","Geothermal","Hydro","Nuclear","Solar","Wind")) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No CSS","With CCS")) +
  facet_wrap(~MODEL, scales="free", ncol=5, labeller=labeller(MODEL= model_labels2))
GBioOthSecCost2100

lay<-rbind(1,1,1,1,1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2,2,2,2,2,2) 
SecCostFinal2100 <- grid.arrange(GBioLiqSecCost2100,GBioOthSecCost2100, layout_matrix=lay)
rm(lay)
#
# # ---- OUTPUT: GLOBAL FOR DRAFT----
# png("output/BioTech/Fig1.png", width=10*ppi, height=5.5*ppi, res=ppi)
# print(plot(GBioSec))
# dev.off()
# 
# png("output/BioTech/Fig2.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(BioEffCost))
# dev.off()
# 
# png("output/BioTech/Fig3.png", width=7*ppi, height=7*ppi, res=ppi)
# print(plot(GBioAllCost4))
# dev.off()
# 
# png("output/BioTech/Fig3_Revised.png", width=7*ppi, height=7*ppi, res=ppi)
# print(plot(GBioAllCostPanel))
# dev.off()
# 
# png("output/BioTech/Fig4.png", width=8*ppi, height=9*ppi, res=ppi)
# print(plot(SecCostFinal2))
# dev.off()
# 
# png("output/BioTech/Fig4b.png", width=8*ppi, height=9*ppi, res=ppi)
# print(plot(SecCostFinal2b))
# dev.off()
# 
# png("output/BioTech/Fig4c.png", width=8*ppi, height=9*ppi, res=ppi)
# print(plot(SecCostFinal2c))
# dev.off()
# 
# png("output/BioTech/Fig4d.png", width=8*ppi, height=9*ppi, res=ppi)
# print(plot(SecCostFinal2d))
# dev.off()
# #
# # ---- OUTPUT: SUPPLEMENTARY INFORMATION ----
# png("output/BioTech/FigS1.png", width=7*ppi, height=10*ppi, res=ppi)
# print(plot(BioEffCostR2))
# dev.off()
# 
# png("output/BioTech/FigS2.png", width=7*ppi, height=5*ppi, res=ppi)
# print(plot(AssumpHist))
# dev.off()
# 
# png("output/BioTech/FigS3.png", width=7*ppi, height=7*ppi, res=ppi)
# print(plot(GBioAllCost))
# dev.off()
# #
# png("output/BioTech/FigS4.png", width=6*ppi, height=5*ppi, res=ppi)
# print(plot(LCOEvCtax))
# dev.off()
# 
# png("output/BioTech/FigS5.png", width=5*ppi, height=5*ppi, res=ppi)
# print(plot(CaptureCorr))
# dev.off()
#
# png("output/BioTech/FigS6.png", width=8*ppi, height=9*ppi, res=ppi)
# print(plot(SecCostFinal2100))
# dev.off()
# 
# ---- OUTPUT: SUPPLEMENTARY DATA ----
SupData = subset(CostEffDataR, select=c(MODEL,SCENARIO,REGION,Year,Prim,CarrierID,Capt,SecEn,Efficiency,CapitalCo,Ctax,Tech,VARIABLE,
                                     LCOE_Feed,LCOE_Cap,LCOE_OM,LCOE_ctax,LCOE_CDR,LCOE))
SupData = subset(SupData, !Year==2010)
SupData$Prim <- gsub("wCCS","",SupData$Prim,fixed=F)  
SupData$Tech <- gsub("wCCS","",SupData$Tech,fixed=F)  
SupData$Capt <- gsub("wCCS","With",SupData$Capt,fixed=F)  
SupData$Capt <- gsub("woCCS","Without",SupData$Capt,fixed=F)  

# Make correction for 2020 since some models display dummy data for that timestep
SupData$Efficiency[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="ElectricityBiomasswCCS"&SupData$Year==2020] <- SupData$Efficiency[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="ElectricityBiomasswCCS"&SupData$Year==2030]
SupData$CapitalCo[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="ElectricityBiomasswCCS"&SupData$Year==2020] <- SupData$CapitalCo[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="ElectricityBiomasswCCS"&SupData$Year==2030]

SupData$Efficiency[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&SupData$Year==2020] <- SupData$Efficiency[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&SupData$Year==2030]
SupData$CapitalCo[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&SupData$Year==2020] <- SupData$CapitalCo[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="LiquidsBiomassCellulosicNondieselwCCS"&SupData$Year==2030]

SupData$Efficiency[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="HydrogenBiomasswCCS"&SupData$Year==2020] <- SupData$Efficiency[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="HydrogenBiomasswCCS"&SupData$Year==2030]
SupData$CapitalCo[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="HydrogenBiomasswCCS"&SupData$Year==2020] <- SupData$CapitalCo[SupData$MODEL=="MESSAGE-GLOBIOM"&SupData$VARIABLE=="HydrogenBiomasswCCS"&SupData$Year==2030]

SupData$VARIABLE <- NULL
# For REMIND-MAGPIE there is no CCS data for 2020, use 2030 data instead
# In order to avoid inconsistency between 2020 (noCCS) data and 2030 (CCS) data, ignore the 2020 values 
RM2020Data = subset(SupData, MODEL=="REMIND-MAGPIE"&Year==2030)
RM2020Data$Year <- 2020

SupData = subset(SupData, !(MODEL=="REMIND-MAGPIE"&Year==2020))
SupData = rbind(SupData,RM2020Data)
rm(RM2020Data)

SupData = SupData[,c(1:5,12,6:11,13:18)]
colnames(SupData)[1:18] <-c("Model","Scenario","Region","Year","Primary Carrier","Technology","Secondary Carrier","Carbon Capture",
                            "Secondary Energy [EJ/yr]","Efficiency [-]","Capital Costs [$/kW(out)]","Carbon Tax [$/tCO2]",
                            "Feedstock LC [$/MWh]","Capital Cost LC [$/MWh]","O&M LC [$/MWh]","Carbon Tax LC [$/MWh]","Carbon Dioxide Removal LC [$/MWh]",
                            "Levelised Cost of Energy [$/MWh]")

SupData.defs <- data.frame(Variables = c("Secondary Carrier",
                                        "Secondary Energy",
                                        "Feedstock LC",
                                        "Capital Cost LC",
                                        "O&M LC",
                                        "Carbon Tax LC",
                                        "Carbon Dioxide Removal LC",
                                        "Levelised Cost of Energy"),
                          Definition = c("Ele = Electricity; Gas = Gasseous fuel, Hyd = Hydrogen, Liq = Liquid fuel",
                                         "Deployment of secondary energy carrier",
                                         "Levelised Costs of feedstock",
                                         "Levelised Costs of capital",
                                         "Levelised Costs of operation and maintenance",
                                         "Levelised Costs of Carbon emissions",
                                         "Levelised Costs of Carbon Dioxide removal (benefits)",
                                         "Overall Levelised Costs of Energy"))

# write.xlsx(SupData, file="output/BioTech/Supplementary_Data.xlsx", sheetName="Supplementary Data", append=FALSE, row.names=FALSE, showNA = TRUE)
# write.xlsx(SupData.defs, file="output/BioTech/Supplementary_Data.xlsx", sheetName="Notes", append=TRUE, row.names=FALSE, showNA = TRUE)

#
# **** FIGURES DISSAGREGATED BETWEEN LIQUIDS/OTHER****
# ---- OUTPUT: DATA FOR EXTERNAL USE ----
RegSecEn.Liq = subset(RegData.Liq, select=c(MODEL,SCENARIO,REGION,Year,Prim,Capt,SecEn,Tech2))
RegSecEn.BioLiqIMAGE = subset(RegSecEn.Liq, MODEL=="IMAGE"&(Prim=="Biomass"|Prim=="BiomasswCCS"))
RegSecEn.BioLiqIMAGE = subset(RegSecEn.BioLiqIMAGE, select=-c(Prim))
RegSecEn.BioLiqIMAGE = subset(RegSecEn.BioLiqIMAGE, !Year<=2010)                           

# Definitions of primary biomass categories
SecBioLiqDefs <- data.frame(Variables = c("Tech2 = Biodeisel",
                                        "Tech2 = Lignocellulosic",
                                        "Tech2 = 1st Gen. Ethanol",
                                        "Capt = woCCS",
                                        "Capt = wCCS",
                                        "SCENARIO = R2-B-lo-full",
                                        "SecEn"),
                          Definition = c("Biodiesel produced from oil-crops)",
                                         "Advanced biofuels made from lignocellulosic crops (ethanol, methanol, FT deisel)",
                                         "1st generation ethanol from sugarcane or maize",
                                         "Technologies applied WITHOUT carbon capture and storage",
                                         "Technologies applied WITH carbon capture and storage",
                                         "Scenario with a global 1000GtCO2 emission budget, consistent with a 2degree climate target. Model default settings on biomass supply and technology availability ",
                                         "Secondary Energy (biofuel) in EJ/yr"))

# write.xlsx(RegSecEn.BioLiqIMAGE, file="output/BioTech/Other/IMAGE_Secondary_Bioliquids_EMF33.xlsx", sheetName="Secondary Biofuels Deployment", row.names=FALSE, showNA = TRUE)
# write.xlsx(SecBioLiqDefs, file="output/BioTech/Other/IMAGE_Secondary_Bioliquids_EMF33.xlsx", sheetName="Definitions", append=TRUE, row.names=FALSE, showNA = TRUE)

#
# ---- FIG: G. Bio Cap+OM Cost----
# Bio Liquids
GBioLiqCost1 <- ggplot(subset(GlobalData.Bio2, CarrierID=="Liq")) +
  geom_jitter(aes(x=Year, y=value, colour=variable, shape=variable), width=0.2, alpha=0.8) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=c(1.5,2.5), size=0.1, colour="gray10") +
  ggtitle("A: Capital and O&M Costs of Biofuel Technologies") +
  xlab("") +
  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=66, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(0,4),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1"),
                     labels=c("CapEx", "+ O&M")
  ) +
  scale_colour_manual(values=c("black","red"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1"),
                      labels=c("CapEx", "+ O&M")
  ) +
  facet_grid(TechOrder~MODEL, scales="free", labeller=labeller(MODEL= model_labels))
GBioLiqCost1

# Non-Liquids
GBioOthCost1 <- ggplot(subset(GlobalData.Bio2, !(CarrierID=="Liq"))) +
  geom_jitter(aes(x=Year, y=value, colour=variable, shape=variable), width=0.2, alpha=0.8) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=c(1.5,2.5), size=0.1, colour="gray10") +
  ggtitle("B: Capital and O&M Costs of Non-Liquid Technologies") +
  xlab("") +
  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=66, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(0,4),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1"),
                     labels=c("CapEx", "+ O&M"),
                     guide=FALSE) +
  scale_colour_manual(values=c("black","red"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1"),
                      labels=c("CapEx", "+ O&M"),
                      guide=FALSE) +
  facet_grid(TechOrder~MODEL, scales="free_y", labeller=labeller(MODEL= model_labels))
GBioOthCost1

GBioCost1Final <- grid.arrange(GBioLiqCost1, GBioOthCost1, layout_matrix=rbind(c(1,1,1,1),c(1,1,1,1),c(2,2,2,2),c(2,2,2,2)))
GBioCost1Final
#
# ---- FIG: G. Bio Cap+OM+Feed Cost ----
# Bio Liquids
GlobalData.Bio3Liq = subset(GlobalData.Bio3, CarrierID=="Liq")
GBioLiqCost2 <- ggplot(GlobalData.Bio3Liq) +
  geom_point(aes(x=Year, y=value, colour=variable, shape=variable)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Capital, O&M and Feedstock Costs of Biofuel Technologies") +
  xlab("") +
  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=66, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(0,4,20),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1","LCOE2"),
                     labels=c("CapEx", "+ O&M","+ Feedstock")) +
  scale_colour_manual(values=c("black","red","green4"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1","LCOE2"),
                      labels=c("CapEx", "+ O&M","+ Feedstock")) +
  facet_grid(MODEL~TechOrder, scales="free_y", labeller=labeller(MODEL= model_labels))
GBioLiqCost2

# Non-Liquids
GlobalData.Bio3NLiq = subset(GlobalData.Bio3, !(CarrierID=="Liq"))
GBioOthCost2 <- ggplot(GlobalData.Bio3NLiq) +
  geom_point(aes(x=Year, y=value, colour=variable, shape=variable)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Capital, O&M and Feedstock Costs of Non-Liquid Technologies") +
  xlab("") +
  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=66, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(0,4,20),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1","LCOE2"),
                     labels=c("CapEx", "+ O&M","+ Feedstock")) +
  scale_colour_manual(values=c("black","red","green4"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1","LCOE2"),
                      labels=c("CapEx", "+ O&M","+ Feedstock")) +
  facet_grid(MODEL~TechOrder, scales="free_y", labeller=labeller(MODEL= model_labels))
GBioOthCost2

#
# ---- FIG: G. Bio Cap+OM+Feed+Ctax+CDR Cost ----
# Bio Liquids
GlobalData.Bio4Liq = subset(GlobalData.Bio4, CarrierID=="Liq")
GBioLiqCost4 <- ggplot(GlobalData.Bio4Liq) +
  geom_point(aes(x=Year, y=value, colour=variable, shape=variable)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Capital, O&M, Feedstock, Ctax and CDR Costs of Biofuel Technologies") +
  xlab("") +
  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(3,0,4,20),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1","LCOE2","LCOE"),
                     labels=c("CapEx", "+ O&M","+ Feedstock","+ CDR")) +
  scale_colour_manual(values=c("orange","black","red","green4"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1","LCOE2","LCOE"),
                      labels=c("CapEx", "+ O&M","+ Feedstock","+ CDR")) +
  facet_grid(MODEL~TechOrder, scales="free_y", labeller=labeller(MODEL= model_labels))
GBioLiqCost4

# Non-Liquids
GlobalData.Bio4NLiq = subset(GlobalData.Bio4, !(CarrierID=="Liq"))
GBioOthCost4 <- ggplot(GlobalData.Bio4NLiq) +
  geom_point(aes(x=Year, y=value, colour=variable, shape=variable)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Capital, O&M, Feedstock, Ctax and CDR Costs of Non-Liquid Technologies") +
  xlab("") +
  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(3,0,4,20),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1","LCOE2","LCOE"),
                     labels=c("CapEx", "+ O&M","+ Feedstock","+ CDR")) +
  scale_colour_manual(values=c("orange","black","red","green4"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1","LCOE2","LCOE"),
                      labels=c("CapEx", "+ O&M","+ Feedstock","+ CDR")) +
  facet_grid(MODEL~TechOrder, scales="free_y", labeller=labeller(MODEL= model_labels))
GBioOthCost4
#
# ---- FIG: G. Cost vs. Use Bio Only ----
GBioLiqSecCost <- ggplot(subset(GlobalData, CarrierID=="Liq"&Year=="2050"&SecEn>0.1)) + 
  geom_point(aes(x=LCOE, y=SecEn, colour=MODEL, shape=TechOrder), size=3) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("A: Deployment of Biofuel Technologies (2050)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
  ylab("Secondary Energy EJ/yr") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.text=element_text(size=6)) +
  scale_colour_manual(values=c("black","blue","red","cyan","forestgreen","green3","lightblue3","chocolate","grey","burlywood4"),
                      name="MODEL",
                      breaks=c("AIM/CGE","BET","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"),
                      labels=c("AIM","BET","DNE21","GCAM","GRAPE","IMACLIM","IMAGE","MESSAGE-GLOBIOM","POLES","REMIND-MAGPIE")) +
  scale_shape_manual(values=c(0,15,1,20,16,2,17,5,21,18),
                     name ="Conversion Technology",
                     breaks=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Other biomass","Liquids","1st gen. ethanolwCCS","BiodeiselwCCS","LignocellulosicwCCS","Other biomasswCCS","LiquidswCCS"),
                     labels=c("1st Gen. Eth.","Biodiesel","Adv. Biofuel","Other Biomass","Fossil","1st Gen. Eth. wCCS","Biodeisel wCCS","Adv. Biofuel wCCS","Other Biomass wCCS","Fossil wCCS"))
GBioLiqSecCost

GBioOthSecCost <- ggplot(subset(GlobalData.Bio, CarrierID=="Ele"&Year=="2050"&SecEn>0.1)) + 
  geom_point(aes(x=LCOE, y=SecEn, colour=MODEL, shape=Capt), size=3, alpha=0.75) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("B: Deployment of Bioelectricity Technologies (2050)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
  ylab("Secondary Energy EJ/yr") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=6)) +
  scale_colour_manual(values=c("black","blue","red","black","forestgreen","purple","green3","lightblue3","grey","burlywood4"),
                      name="MODEL",
                      breaks=c("AIM/CGE","BET","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"),
                      labels=c("AIM","BET","DNE21","GCAM","GRAPE","IMACLIM","IMAGE","MESSAGE-GLOBIOM","POLES","REMIND-MAGPIE")) +
  scale_shape_manual(values=c(16,4),name="CCS",breaks=c("woCCS","wCCS"),labels=c("No CSS","With CCS"),guide=FALSE)
GBioOthSecCost

GBioAllSecCost <- ggplot(subset(GlobalData.Bio, Year=="2050"&SecEn>0.1)) + 
  geom_point(aes(x=LCOE, y=SecEn, colour=MODEL, shape=TechOrder), size=2, alpha=0.75) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("A: Deployment of Bioenergy Technologies across models (2050)") + theme(plot.title = element_text(lineheight=20, face="bold")) +
  ylab("Secondary Energy EJ/yr") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=6), legend.title=element_text(face="bold")) +
  scale_colour_manual(values=c("forestgreen","blue","red","black","chocolate","purple","green3","lightblue3","grey","burlywood4"),
                      name="Model", 
                      breaks=c("AIM/CGE","BET","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"),
                      labels=c("AIM","BET","DNE21","GCAM","GRAPE","IMACLIM","IMAGE","MESSAGE-GLOBIOM","POLES","REMIND-MAGPIE")) +
  scale_shape_manual(values=c(0,15, # 1st Gen 
                              1,16,    # biodiesel
                              2,17, # Ligno
                              20,21, #other biomass
                              0,15, # Elec
                              16),   # Hydrogen 
                     name ="Conversion Technology",
                     breaks=c("1st gen. ethanol",
                              "1st gen. ethanolwCCS",
                              "Biodeisel",
                              "BiodeiselwCCS",
                              "Lignocellulosic",
                              "LignocellulosicwCCS",
                              "Other biomass",
                              "Other biomasswCCS",
                              "Electricity",
                              "ElectricitywCCS",
                              "HydrogenwCCS"),
                     labels=c("1st Gen. Eth.",
                              "1st Gen. Eth. wCCS",
                              "Biodiesel",
                              "Biodiesel wCCS",
                              "Adv. Biofuel",
                              "Adv. Biofuel wCCS",
                              "Other Biomass",
                              "Other Biomass wCCS",
                              "Electricity",
                              "Electricity wCCS",
                              "Hydrogen wCCS"))+
  facet_wrap(~CarrierID, scales="free", labeller=labeller(CarrierID=carrier_labels))
GBioAllSecCost

#
# ---- OUTPUT: OTHER GLOBAL ----
# png("output/BioTech/GBioLiqCost1.png", width=8*ppi, height=5*ppi, res=ppi)
# print(plot(GBioLiqCost1))
# dev.off()
# 
# png("output/BioTech/GBioLiqCost2.png", width=8*ppi, height=5*ppi, res=ppi)
# print(plot(GBioLiqCost2))
# dev.off()
# 
# png("output/BioTech/GBioLiqCost4.png", width=8*ppi, height=5*ppi, res=ppi)
# print(plot(GBioLiqCost4))
# dev.off()
# 
# # Global - Non-liquids - Cost Components
# png("output/BioTech/GBioOthCost1.png", width=8*ppi, height=7*ppi, res=ppi)
# print(plot(GBioOthCost1))
# dev.off()
# 
# png("output/BioTech/GBioOthCost2.png", width=8*ppi, height=7*ppi, res=ppi)
# print(plot(GBioOthCost2))
# dev.off()
# 
# png("output/BioTech/GBioOthCost4.png", width=8*ppi, height=7*ppi, res=ppi)
# print(plot(GBioOthCost4))
# dev.off()
# 
# **** REGIONAL FIGURES ****
# ---- FIG: R. Bio Cap+OM Cost ----
# Bio Liquids
RegData.Bio2Liq = subset(RegData.Bio2, CarrierID=="Liq")
RBioLiqCost1 <- ggplot(RegData.Bio2Liq) +
  geom_point(aes(x=TechOrder, y=value, colour=variable, shape=variable, group=Year), size=0.8, alpha=0.8, position=position_dodge(0.6)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Capital and O&M Costs of Biofuel Technologies") +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(0,4),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1"),
                     labels=c("CapEx", "+ O&M")) +
  scale_colour_manual(values=c("black","red"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1"),
                      labels=c("CapEx", "+ O&M")) +
  facet_grid(MODEL~REGION, scales="free_y", labeller=labeller(MODEL= model_labels))
RBioLiqCost1

# Non-Liquids
RegData.Bio2NLiq = subset(RegData.Bio2, !(CarrierID=="Liq"))
RBioOthCost1 <- ggplot(RegData.Bio2NLiq) +
  geom_point(aes(x=TechOrder, y=value, colour=variable, shape=variable, group=Year), size=0.8, alpha=0.8, position=position_dodge(0.6)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Capital and O&M Costs of Non-Liquid Technologies") +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(0,4),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1"),
                     labels=c("CapEx", "+ O&M")) +
  scale_colour_manual(values=c("black","red"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1"),
                      labels=c("CapEx", "+ O&M")) +
  facet_grid(MODEL~REGION, scales="free_y", labeller=labeller(MODEL= model_labels))
RBioOthCost1

#
# ---- FIG: R. Bio Cap+OM+Feed Cost ----
# Bio Liquids
RegData.Bio3Liq = subset(RegData.Bio3, CarrierID=="Liq")
RBioLiqCost2 <- ggplot(RegData.Bio3Liq) +
  geom_point(aes(x=TechOrder, y=value, colour=variable, shape=variable, group=Year), size=0.8, alpha=0.8, position=position_dodge(0.6)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Capital, O&M and Feedstock Costs of Biofuel Technologies") +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(0,4,20),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1","LCOE2"),
                     labels=c("CapEx", "+ O&M","+ Feedstock")) +
  scale_colour_manual(values=c("black","red","green4"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1","LCOE2"),
                      labels=c("CapEx", "+ O&M","+ Feedstock")) +
  facet_grid(MODEL~REGION, scales="free_y", labeller=labeller(MODEL= model_labels))
RBioLiqCost2

# Non-Liquids
RegData.Bio3NLiq = subset(RegData.Bio3, !(CarrierID=="Liq"))
RBioOthCost2 <- ggplot(RegData.Bio3NLiq) +
  geom_point(aes(x=TechOrder, y=value, colour=variable, shape=variable, group=Year), size=0.8, alpha=0.8, position=position_dodge(0.6)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Capital, O&M and Feedstock Costs of Non-Liquid Technologies") +
  xlab("") +
  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(0,4,20),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1","LCOE2"),
                     labels=c("CapEx", "+ O&M","+ Feedstock")) +
  scale_colour_manual(values=c("black","red","green4"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1","LCOE2"),
                      labels=c("CapEx", "+ O&M","+ Feedstock")) +
  facet_grid(MODEL~REGION, scales="free_y", labeller=labeller(MODEL= model_labels))
RBioOthCost2

#
# ---- FIG: R. Bio Cap+OM+Feed+Ctax+CDR Cost ----
# Bio Liquids
RegData.Bio4Liq = subset(RegData.Bio4, CarrierID=="Liq")
RBioLiqCost4 <- ggplot(RegData.Bio4Liq) +
  geom_point(aes(x=TechOrder, y=value, colour=variable, shape=variable, group=Year), size=0.8, alpha=0.8, position=position_dodge(0.6)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Capital, O&M, Feedstock, Ctax and CDR Costs of Biofuel Technologies") +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(3,0,4,20),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1","LCOE2","LCOE"),
                     labels=c("CapEx", "+ O&M","+ Feedstock","+ CDR")) +
  scale_colour_manual(values=c("orange","black","red","green4"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1","LCOE2","LCOE"),
                      labels=c("CapEx", "+ O&M","+ Feedstock","+ CDR")) +
  facet_grid(MODEL~REGION, scales="free_y", labeller=labeller(MODEL= model_labels))
RBioLiqCost4

# Non-Liquids
RegData.Bio4NLiq = subset(RegData.Bio4, !(CarrierID=="Liq"))
RBioOthCost4 <- ggplot(RegData.Bio4NLiq) +
  geom_point(aes(x=TechOrder, y=value, colour=variable, shape=variable, group=Year), size=0.8, alpha=0.8, position=position_dodge(0.6)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Capital, O&M, Feedstock, Ctax and CDR Costs of Non-Liquid Technologies") +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(3,0,4,20),
                     name="",
                     breaks=c("LCOE_Cap","LCOE1","LCOE2","LCOE"),
                     labels=c("CapEx", "+ O&M","+ Feedstock","+ CDR")) +
  scale_colour_manual(values=c("orange","black","red","green4"),
                      name="",
                      breaks=c("LCOE_Cap","LCOE1","LCOE2","LCOE"),
                      labels=c("CapEx", "+ O&M","+ Feedstock","+ CDR")) +
  facet_grid(MODEL~REGION, scales="free_y", labeller=labeller(MODEL= model_labels))
RBioOthCost4
#
# ---- FIG: REGIONAL Cost vs. Use ----
RegData.Ele2100 = subset(RegData.Ele, Year=="2100")
PriceData.Ele2100 = subset(PriceData, Year=="2100")
PriceData.Ele2100 = subset(PriceData.Ele2100, VARIABLE=="Electricity")
PriceData.Ele2100 = subset(PriceData.Ele2100, !(MODEL=="COFFEE"))

BioSecEleCost <- ggplot(RegData.Ele2100) +
  geom_point(aes(x=LCOE, y=SecEn, colour=Prim2, shape=Capt), size=0.8) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("Deployment of Electricity Technologies (2100)") +
  ylab("Secondary Energy EJ/yr") +
  xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("green3", "black","purple","brown","blue","pink2","orange","lightblue2"),
                      name ="Primary Energy Carrier",
                      breaks=c("Biomass","Coal","Gas","Geothermal","Hydro","Nuclear","Solar","Wind"),
                      labels=c("Biomass","Coal","Nat. Gas","Geothermal","Hydro","Nuclear","Solar","Wind")) +
  scale_shape_manual(values=c(16,3),
                     name="Carbon Capture",
                     breaks=c("wCCS","woCCS"),
                     labels=c("with CCS","no CCS")) +
  facet_grid(REGION~MODEL, scales="free", labeller=labeller(MODEL= model_labels))
BioSecEleCost

# Liquids
RegData.Liq2100 = subset(RegData.Liq, Year=="2100")
BioSecLiqCost <- ggplot(RegData.Liq2100) +
  geom_point(aes(x=LCOE, y=SecEn, colour=TechOrder2, shape=Capt), size=0.8, alpha=0.7) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Deployment of Liquids Technologies (2100)") +
  ylab("Secondary Energy EJ/yr") +
  xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("red","purple","green3","lightblue3","black"),
                      name ="Conversion Technology",
                      breaks=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Other biomass","Liquids"),
                      labels=c("1st Gen. Eth.","Biodiesel","Adv. Biofuel","Other Biofuel","Fossil")) +
  scale_shape_manual(values=c(16,3),name="Carbon Capture",breaks=c("wCCS","woCCS"),labels=c("with CCS","no CCS")) +
  facet_grid(REGION~MODEL, scales="free", labeller=labeller(MODEL= model_labels))
BioSecLiqCost

# ---- FIG: REGIONAL Cost vs. Use2 ----
RegData.Ele2100a = subset(RegData.Ele2100, SecEn>1)
BioSecEleCosta <- ggplot(RegData.Ele2100a) +
  geom_point(aes(x=LCOE, y=SecEn, colour=Prim2, shape=Capt), size=0.8) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  ggtitle("Deployment of Electricity Technologies (2100)") +
  ylab("Secondary Energy EJ/yr") +  xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("green3", "black","purple","brown","blue","pink2","orange","lightblue2"),
                      name ="Primary Energy Carrier",
                      breaks=c("Biomass","Coal","Gas","Geothermal","Hydro","Nuclear","Solar","Wind"),
                      labels=c("Biomass","Coal","Nat. Gas","Geothermal","Hydro","Nuclear","Solar","Wind")) +
  scale_shape_manual(values=c(16,3),
                     name="Carbon Capture",
                     breaks=c("wCCS","woCCS"),
                     labels=c("with CCS","no CCS")) +
  facet_grid(REGION~MODEL, scales="free", labeller=labeller(MODEL= model_labels))
BioSecEleCosta

# Liquids
RegData.Liq2100a = subset(RegData.Liq2100, SecEn>1)
BioSecLiqCosta <- ggplot(RegData.Liq2100a) +
  geom_point(aes(x=LCOE, y=SecEn, colour=TechOrder2, shape=Capt), size=0.8, alpha=0.7) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Deployment of Liquids Technologies (2100)") +
  ylab("Secondary Energy EJ/yr") +  xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("red","purple","green3","lightblue3","black"),
                      name ="Conversion Technology",
                      breaks=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Other biomass","Liquids"),
                      labels=c("1st Gen. Eth.","Biodiesel","Adv. Biofuel","Other Biofuel","Fossil")) +
  scale_shape_manual(values=c(16,3),name="Carbon Capture",breaks=c("wCCS","woCCS"),labels=c("with CCS","no CCS")) +
  facet_grid(REGION~MODEL, scales="free", labeller=labeller(MODEL= model_labels))
BioSecLiqCosta

#
# ---- FIG: Regional CV ----
RegVar <- ggplot(data=subset(Calcs.Reg1, !(CarrierID=="Gas")&(variable2=="CapitalCo"|variable2=="Efficiency"|variable2=="LCOE_Feed")&Year=="2050"),aes(CV_perc, fill=variable2)) +
  geom_histogram(bins = "20",position="dodge") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlab(expression("Coefficient of Variation [%]")) +  ylab("Count") +
  theme_bw() +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize2, hjust=2, vjust=1), axis.text.y = element_text(size=fontsize2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("black","grey60","forestgreen"),
                    name ="",
                    breaks=c("CapitalCo","Efficiency","LCOE_Feed"),
                    labels=c("Capital Costs","Efficiency","Feedstock Cost")) +
  facet_grid(MODEL~CarrierID, scales="free", labeller=labeller(MODEL=model_labels, CarrierID=carrier_labels))
RegVar
#

# ---- OUTPUT: OTHER REGIONAL ----
# png("output/BioTech/Regional/RBioLiqCost1.png", width=8*ppi, height=5*ppi, res=ppi)
# print(plot(RBioLiqCost1))
# dev.off()
# 
# png("output/BioTech/Regional/RBioOthCost1.png", width=8*ppi, height=7*ppi, res=ppi)
# print(plot(RBioOthCost1))
# dev.off()
# 
# png("output/BioTech/Regional/RBioLiqCost2.png", width=8*ppi, height=8*ppi, res=ppi)
# print(plot(RBioLiqCost2))
# dev.off()
# 
# png("output/BioTech/Regional/RBioOthCost2.png", width=8*ppi, height=8*ppi, res=ppi)
# print(plot(RBioOthCost2))
# dev.off()
# 
# png("output/BioTech/Regional/RBioLiqCost3.png", width=8*ppi, height=5*ppi, res=ppi)
# print(plot(RBioLiqCost3))
# dev.off()
# 
# png("output/BioTech/Regional/RBioOthCost3.png", width=8*ppi, height=7*ppi, res=ppi)
# print(plot(RBioOthCost3))
# dev.off()
# 
# png("output/BioTech/Regional/RBioLiqCost4.png", width=8*ppi, height=5*ppi, res=ppi)
# print(plot(RBioLiqCost4))
# dev.off()
# 
# png("output/BioTech/Regional/RBioOthCost4.png", width=8*ppi, height=7*ppi, res=ppi)
# print(plot(RBioOthCost4))
# dev.off()
# 
# png("output/BioTech/Regional/BioSecEleCost.png", width=9*ppi, height=5*ppi, res=ppi)
# print(plot(BioSecEleCost))
# dev.off()
# 
# png("output/BioTech/Regional/BioSecLiqCost.png", width=7*ppi, height=5*ppi, res=ppi)
# print(plot(BioSecLiqCost))
# dev.off()
# 
# png("output/BioTech/Regional/BioSecEleCosta.png", width=9*ppi, height=5*ppi, res=ppi)
# print(plot(BioSecEleCosta))
# dev.off()
# 
# png("output/BioTech/Regional/BioSecLiqCosta.png", width=7*ppi, height=5*ppi, res=ppi)
# print(plot(BioSecLiqCosta))
# dev.off()
#
# png("output/BioTech/Regional/RegionalVariation.png", width=6*ppi, height=8*ppi, res=ppi)
# print(plot(RegVar))
# dev.off()
#
# ---- FOR PRESENTATION ----
# For figures
fontsize4 = 5 # For Panels, legends and axes (labels)
fontsize5 = 5 # For axes (scale)
fontsize6 = 6 #For chart titles
fontsize7 = 6
arrowsize = 0.15
#
# ---- FIG: Bioenergy Strategies ----
BioStrat <- ggplot(subset(SecEnTot1, Prim=="Biomass"&!(Year=="2000"|Year=="2050")&!(Tech3=="Hea Total")), mapping=aes(x=Year, y=value, fill=TechOrder3)) + 
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab("Secondary Energy EJ/yr") + xlab("") +
  theme_bw() +
  theme(text= element_text(size=fontsize7, face="plain"), axis.text.x = element_text(angle=66, size=fontsize5, hjust=1), axis.text.y = element_text(size=fontsize5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize4), legend.title=element_text(face="bold")) +
  scale_fill_manual(values=c("grey47","grey67","forestgreen","green2","navy","dodgerblue","darkmagenta"),
                    name ="Bio-Technology:",
                    breaks=c('Ele woCCS','Ele wCCS','Liq woCCS','Liq wCCS','Hyd woCCS','Hyd wCCS','Gas Total'),
                    labels=c('Ele woCCS','Ele wCCS','Liq woCCS','Liq wCCS','Hyd woCCS','Hyd wCCS','Gas Total')
  ) +
  facet_wrap(~MODEL, nrow=2, labeller=labeller(MODEL=model_labels, TechOrder3=carrier_labels))
BioStrat

BioStrat2 <- ggplot(subset(SecEnTot1, Prim=="Biomass"&!(Year=="2000"|Year=="2050")&!(Tech3=="Hea Total")), mapping=aes(x=Year, y=value, fill=TechOrder3)) + 
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab("Secondary Energy EJ/yr") + xlab("") +
  theme_bw() +
  theme(text= element_text(size=fontsize7, face="plain"), axis.text.x = element_text(angle=66, size=fontsize5, hjust=1), axis.text.y = element_text(size=fontsize5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize4), legend.title=element_text(face="bold")) +
  scale_fill_manual(values=c("grey47","grey67","forestgreen","green2","navy","dodgerblue","darkmagenta"),
                    name ="Bio-Technology:",
                    breaks=c('Ele woCCS','Ele wCCS','Liq woCCS','Liq wCCS','Hyd woCCS','Hyd wCCS','Gas Total'),
                    labels=c('Ele woCCS','Ele wCCS','Liq woCCS','Liq wCCS','Hyd woCCS','Hyd wCCS','Gas Total')
  ) +
  facet_wrap(~MODEL, nrow=1, labeller=labeller(MODEL=model_labels, TechOrder3=carrier_labels))
BioStrat2

#
# ----- FIG: Parameters ----
BioEffCostPre <- ggplot(subset(GlobalData.BioCor1, Year=="2020"&!(CarrierID=="Gas")&!(TechOrder2=="Other biomass")))+
  geom_point(data=subset(LitData, !(Efficiency=="NA"|CapitalCo=="NA"|CarrierID=="Gas"|TechOrder2=="Other biomass")), aes(x=CapitalCo, y=Efficiency, fill=Capt, colour=Capt), alpha=0.5, shape=21, size=1.5) +
  geom_point(data=, aes(x=CapitalCo, y=Efficiency, shape=MODEL, colour=Capt), alpha=0.8, size=1.5) +
  geom_rect(data=subset(Data, !(TechOrder2=="Gas"|TechOrder2=="Other biomass")), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ylim(0,1) + xlab(expression("Capital Costs [US$"[2005]*"/kW"[Out]*"]")) + ylab("Conversion Efficiency [-]") +
  theme_bw() +
  theme(text= element_text(size=fontsize1, face="plain"), axis.text.x = element_text(angle=66, size=fontsize1, hjust=1), axis.text.y = element_text(size=fontsize1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.text=element_text(size=fontsize3)) +
  scale_colour_manual(values=c("green4","black"),name="",breaks=c("woCCS","wCCS"),labels=c("No CCS", "With CCS")) +
  scale_fill_manual(values=c("green4","black"),name ="",breaks=c("woCCS","wCCS"),labels=c("No CCS", "With CCS")) +
  scale_shape_manual(values=c(1,2,3,4,6,8,9,10,11,12),
                     name="",
                     breaks=c("AIM/CGE","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE", "MESSAGE-GLOBIOM","BET","DNE21+ v.14"),
                     labels=c("AIM/CGE","DNE21+","GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","REMIND-MAgPIE","MESSAGE-GLOBIOM","BET","DNE21+")
  ) +
  geom_blank(aes(x=x_max)) + geom_blank(aes(x=x_min)) +
  guides(col = guide_legend(nrow = 2)) +
  facet_grid( Year~TechOrder2, scales="free_x", labeller=labeller(Year = dummy_labeler,TechOrder2=Biotech_labeler)) +
  theme(strip.text.x = element_text(size = fontsize1), strip.text.y = element_text(size = fontsize1))
BioEffCostPre

BioEffCostPre2 <- ggplot(subset(GlobalData.BioCor1, Year=="2020"&!(CarrierID=="Gas")&!(TechOrder2=="Other biomass")))+
  geom_point(data=subset(LitData, !(Efficiency=="NA"|CapitalCo=="NA"|CarrierID=="Gas"|TechOrder2=="Other biomass")), aes(x=CapitalCo, y=Efficiency, fill=Capt, shape=Capt, colour=Capt), alpha=0.5, size=2.5) +
  geom_point(data=, aes(x=CapitalCo, y=Efficiency, shape=Capt, colour=Capt), alpha=0.7, size=2.5) +
  geom_rect(data=subset(Data, !(TechOrder2=="Gas"|TechOrder2=="Other biomass")), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ylim(0,1) + xlab(expression("Capital Costs [US$"[2005]*"/kW"[Out]*"]")) + ylab("Conversion Efficiency [-]") +
  theme_bw() +
  theme(text= element_text(size=11, face="plain"), axis.text.x = element_text(angle=66, size=10, hjust=1), axis.text.y = element_text(size=11)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=9)) +
  scale_colour_manual(values=c("green4","black"),name="",breaks=c("woCCS","wCCS"),labels=c("No CCS", "With CCS")) +
  scale_fill_manual(values=c("green4","black"),name ="",breaks=c("woCCS","wCCS"),labels=c("No CCS", "With CCS")) +
  scale_shape_manual(values=c(10,16),name ="",breaks=c("woCCS","wCCS"),labels=c("No CCS", "With CCS")) +
  geom_blank(aes(x=x_max)) + geom_blank(aes(x=x_min)) +
  guides(col = guide_legend(nrow = 2)) +
  facet_grid( Year~TechOrder2, scales="free_x", labeller=labeller(Year = dummy_labeler,TechOrder2=Biotech_labeler)) +
  theme(strip.text.x = element_text(size = 11), strip.text.y = element_text(size = 11))
BioEffCostPre2

#
# ---- FIG: Fig 2 components----
EleCost1 <- ggplot(subset(GlobalData.Bio4, CarrierID=="Ele"&!(Tech2=="Other biomass"|Tech2=="1st gen. ethanol")&variable=="LCOE1")) +
  geom_point(aes(x=Year, y=value, colour=variable, shape=variable),size=1.25, alpha=0.8) +
  geom_rect(data=subset(Bkgrd, CarrierID=="Ele"), aes(fill = CarrierID), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.03) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  geom_vline(xintercept=c(1.5,2.5), size=0.1, colour="gray10") +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=fontsize4, face="plain"), axis.text.x = element_text(angle=66, size=fontsize5, hjust=1), axis.text.y = element_text(size=fontsize5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize4)) +
  scale_shape_manual(values=c(19),name="",breaks=c("LCOE1"),labels=c("Capital + O&M")) +
  scale_colour_manual(values=c("black"),name="",breaks=c("LCOE1"),labels=c("Capital + O&M")) +
  scale_fill_manual(values=c("grey"),name="",breaks=c("Ele"),labels=c("Electricity"),guide=FALSE) +
  guides(shape=guide_legend(nrow=2,byrow=TRUE)) +
  facet_grid(TechOrder~MODEL, scales="free", labeller=labeller(MODEL= model_labels, TechOrder=Biotech_labeler)) +
  theme(strip.text.x = element_text(size=fontsize4), strip.text.y = element_text(size=fontsize4))
EleCost1

EleCost2 <- ggplot(subset(GlobalData.Bio4, CarrierID=="Ele"&!(Tech2=="Other biomass"|Tech2=="1st gen. ethanol")&(variable=="LCOE1"|variable=="LCOE3"|variable=="LCOE3cor"))) +
  geom_point(aes(x=Year, y=value, colour=variable, shape=variable),size=1.25, alpha=0.8) +
  geom_rect(data=subset(Bkgrd, CarrierID=="Ele"), aes(fill = CarrierID), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.03) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  geom_vline(xintercept=c(1.5,2.5), size=0.1, colour="gray10") +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=fontsize4, face="plain"), axis.text.x = element_text(angle=66, size=fontsize5, hjust=1), axis.text.y = element_text(size=fontsize5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize4)) +
  scale_shape_manual(values=c(19,19,5),name="",breaks=c("LCOE1","LCOE3","LCOE3cor"),labels=c("Capital + O&M","+ Feedstock","+ Feedstock (out of scale)")) +
  scale_colour_manual(values=c("black", "green4","green4"),name="",breaks=c("LCOE1","LCOE3","LCOE3cor"),labels=c("Capital + O&M","+ Feedstock","+ Feedstock (out of scale)")) +
  scale_fill_manual(values=c("grey"),name="",breaks=c("Ele"),labels=c("Electricity"),guide=FALSE) +
  guides(shape=guide_legend(nrow=2,byrow=TRUE)) +
  facet_grid(TechOrder~MODEL, scales="free", labeller=labeller(MODEL= model_labels, TechOrder=Biotech_labeler)) +
  theme(strip.text.x = element_text(size=fontsize4), strip.text.y = element_text(size=fontsize4))
EleCost2

EleCost3 <- ggplot(subset(GlobalData.Bio4, CarrierID=="Ele"&!(Tech2=="Other biomass"|Tech2=="1st gen. ethanol"))) +
  geom_jitter(aes(x=Year, y=value, colour=variable, shape=variable),size=1.25, width=0.2, alpha=0.8) +
  geom_rect(data=subset(Bkgrd, CarrierID=="Ele"), aes(fill = CarrierID), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.03) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  geom_vline(xintercept=c(1.5,2.5), size=0.1, colour="gray10") +
  xlab("") +  ylab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=fontsize4, face="plain"), axis.text.x = element_text(angle=66, size=fontsize5, hjust=1), axis.text.y = element_text(size=fontsize5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize4)) +
  scale_shape_manual(values=c(19,19,19,5,5),name="",breaks=c("LCOE1","LCOE3","LCOE3cor","LCOE","LCOEcor"),labels=c("Capital + O&M","+ Feedstock","+ Feedstock (out of scale)","+ Feedstock & CDR","+ Feedstock & CDR (out of scale)")) +
  scale_colour_manual(values=c("firebrick","black","green4","firebrick","green4"),name="",breaks=c("LCOE1","LCOE3","LCOE3cor","LCOE","LCOEcor"),labels=c("Capital + O&M","+ Feedstock","+ Feedstock (out of scale)","+ Feedstock & CDR","+ Feedstock & CDR (out of scale)")) +
  scale_fill_manual(values=c("grey","dodgerblue","purple"),name="",breaks=c("Ele","Liq","Hyd"),labels=c("Electricity","Liquids","Hydrogen"),guide=FALSE) +  guides(shape=guide_legend(nrow=2,byrow=TRUE)) +
  facet_grid(TechOrder~MODEL, scales="free", labeller=labeller(MODEL= model_labels, TechOrder=Biotech_labeler)) +
  theme(strip.text.x = element_text(size=fontsize4), strip.text.y = element_text(size=fontsize4))
EleCost3

#
# ---- FIG: Deploy-LCOE Liq ----
# Use version of figure including technology trajectory thorugh to 2100
# for LiqDepCost1, sel <alpha> for geom_path as 0 in order to make sure panel axes are identical to allow for easy presentation
LiqDepCost1 <- ggplot(subset(GlobalData4, CarrierID=="Liq"&Year=="2050"&SecEnFrac>0.1)) + 
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=TechOrder2, shape=Capt), size=2) +
  geom_path(data=subset(GlobalData4, CarrierID=="Liq"&!(Year==2030)&SecEnFrac>0.1), 
            aes(x=LCOE, y = SecEnFrac, group = VARIABLE, colour = TechOrder2), alpha=0.0,
            arrow=arrow(angle=30, length=unit(arrowsize,"cm"), ends="last", type="open")) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  ylim(0,100) + theme_bw() +
  theme(strip.text.x = element_text(size = fontsize4, face="plain")) +
  theme(text= element_text(size=fontsize7, face="plain"), axis.text.x = element_text(angle=66, size=fontsize5, hjust=1), axis.text.y = element_text(size=fontsize5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize4), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("chocolate","purple","forestgreen","black"),
                      name ="CONVERSION \nTECHNOLOGY:",
                      breaks=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Liquids"),
                      labels=c("1st Gen. Eth.","Biodiesel","Adv. Biofuel","Fossil")) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No \nCSS","With \nCCS")) +
  facet_wrap(~MODEL, scales="free_x", ncol=5, labeller=labeller(MODEL= model_labels2))
LiqDepCost1

LiqDepCost2 <- ggplot(subset(GlobalData4, CarrierID=="Liq"&Year=="2050"&SecEnFrac>0.1)) + 
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=TechOrder2, shape=Capt), size=2) +
  geom_path(data=subset(GlobalData4, CarrierID=="Liq"&!(Year==2030)&SecEnFrac>0.1), 
            aes(x=LCOE, y = SecEnFrac, group = VARIABLE, colour = TechOrder2), alpha=0.3,
            arrow=arrow(angle=30, length=unit(arrowsize,"cm"), ends="last", type="open")) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) +
  ylim(0,100) + theme_bw() +
  theme(strip.text.x = element_text(size = fontsize4, face="plain")) +
  theme(text= element_text(size=fontsize7, face="plain"), axis.text.x = element_text(angle=66, size=fontsize5, hjust=1), axis.text.y = element_text(size=fontsize5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text=element_text(size=fontsize4), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("chocolate","purple","forestgreen","black"),
                      name ="CONVERSION \nTECHNOLOGY:",
                      breaks=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Liquids"),
                      labels=c("1st Gen. Eth.","Biodiesel","Adv. Biofuel","Fossil")) +
  scale_shape_manual(values=c(16,1),name="CCS",breaks=c("woCCS","wCCS"),labels=c("No \nCSS","With \nCCS")) +
  facet_wrap(~MODEL, scales="free_x", ncol=5, labeller=labeller(MODEL= model_labels2))
LiqDepCost2

#
# ---- FIG: Deploy-LCOE Ele ----
# Use version of figure including technology trajectory thorugh to 2100
# for EleDepCost1, sel <alpha> for geom_path as 0 in order to make sure panel axes are identical to allow for easy presentation
GBioOthSecCost3Dat.Agr=GBioOthSecCost3Dat
GBioOthSecCost3Dat.Agr$Agr[GBioOthSecCost3Dat.Agr$Prim=="Biomass"]<-"Biomass"
GBioOthSecCost3Dat.Agr$Agr[GBioOthSecCost3Dat.Agr$Prim=="Coal"|GBioOthSecCost3Dat.Agr$Prim=="Gas"]<-"Fossil"
GBioOthSecCost3Dat.Agr$Agr[GBioOthSecCost3Dat.Agr$Prim=="Geothermal"|GBioOthSecCost3Dat.Agr$Prim=="Hydro"|GBioOthSecCost3Dat.Agr$Prim=="Nuclear"]<-"NonVar"
GBioOthSecCost3Dat.Agr$Agr[GBioOthSecCost3Dat.Agr$Prim=="Wind"|GBioOthSecCost3Dat.Agr$Prim=="Solar"]<-"VarREN"

GBioOthSecCost3Dat.Agr2=GlobalData4
GBioOthSecCost3Dat.Agr2$Agr[GBioOthSecCost3Dat.Agr2$Prim=="Biomass"]<-"Biomass"
GBioOthSecCost3Dat.Agr2$Agr[GBioOthSecCost3Dat.Agr2$Prim=="Coal"|GBioOthSecCost3Dat.Agr2$Prim=="Gas"]<-"Fossil"
GBioOthSecCost3Dat.Agr2$Agr[GBioOthSecCost3Dat.Agr2$Prim=="Geothermal"|GBioOthSecCost3Dat.Agr2$Prim=="Hydro"|GBioOthSecCost3Dat.Agr2$Prim=="Nuclear"]<-"NonVar"
GBioOthSecCost3Dat.Agr2$Agr[GBioOthSecCost3Dat.Agr2$Prim=="Wind"|GBioOthSecCost3Dat.Agr2$Prim=="Solar"]<-"VarREN"


EleDepCost1 <- ggplot(GBioOthSecCost3Dat.Agr) + 
  geom_point(aes(x=LCOE, y=SecEnFrac, colour=Agr, shape=Capt), size=2) +
  geom_path(data=subset(GBioOthSecCost3Dat.Agr2, CarrierID=="Ele"&!(Year==2030)&SecEnFrac>0.1), 
            aes(x=LCOE, y = SecEnFrac, group = VARIABLE, colour = Agr), alpha=0.0,
            arrow=arrow(angle=30, length=unit(arrowsize,"cm"), ends="last", type="open")) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  scale_y_continuous(breaks=c(10,20,30,40,50))+
  theme_bw() +
  theme(strip.text.x = element_text(size = fontsize4, face="plain")) +
  theme(text= element_text(size=fontsize7, face="plain"), axis.text.x = element_text(angle=66, size=fontsize5, hjust=1), axis.text.y = element_text(size=fontsize5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.text=element_text(size=fontsize4), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("green3", "black","purple","orange"),
                      name ="PRIMARY \nENERGY CARRIER:", 
                      breaks=c("Biomass","Fossil","NonVar","VarREN"),
                      labels=c("Biomass","Fossil","Hydro/Nuclear","Solar/Wind")) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No \nCSS","With \nCCS")) +
  facet_wrap(~MODEL, scales="free_x", ncol=5, labeller=labeller(MODEL= model_labels2))
EleDepCost1

EleDepCost2 <- ggplot(GBioOthSecCost3Dat.Agr) + 
  geom_point(data= subset(GBioOthSecCost3Dat.Agr, SecEnFrac>1),
             aes(x=LCOE, y=SecEnFrac, colour=Agr, shape=Capt), alpha=0.5, size=2) +
  geom_path(data=subset(GBioOthSecCost3Dat.Agr2, CarrierID=="Ele"&!(Year==2030)&SecEnFrac>1), 
            aes(x=LCOE, y = SecEnFrac, group = VARIABLE, colour = Agr), alpha=1,
            arrow=arrow(angle=30, length=unit(arrowsize,"cm"), ends="last", type="closed")) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab("Fraction of Secondary Energy (%)") + xlab(expression("Levelised Cost of Energy, US$"[2005]*"/MWh")) + 
  scale_y_continuous(breaks=c(10,20,30,40,50))+
  theme_bw() +
  theme(strip.text.x = element_text(size = fontsize4, face="plain")) +
  theme(text= element_text(size=fontsize7, face="plain"), axis.text.x = element_text(angle=66, size=fontsize5, hjust=1), axis.text.y = element_text(size=fontsize5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.text=element_text(size=fontsize4), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("green3", "black","purple","orange"),
                      name ="PRIMARY \nENERGY CARRIER:", 
                      breaks=c("Biomass","Fossil","NonVar","VarREN"),
                      labels=c("Biomass","Fossil","Hydro/Nuclear","Solar/Wind")) +
  scale_shape_manual(values=c(16,1),name="CCS:",breaks=c("woCCS","wCCS"),labels=c("No \nCSS","With \nCCS")) +
  facet_wrap(~MODEL, scales="free_x", ncol=5, labeller=labeller(MODEL= model_labels2))
EleDepCost2

#
# ---- OUTPUT: PRESENTATION ----
# png("output/BioTech/Presentation/BioStrategy.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(BioStrat))
# dev.off()
# 
# png("output/BioTech/Presentation/BioStrategy2.png", width=9*ppi, height=2.5*ppi, res=ppi)
# print(plot(BioStrat2))
# dev.off()
# 
# png("output/BioTech/Presentation/BioEffCost.png", width=9*ppi, height=4*ppi, res=ppi)
# print(plot(BioEffCostPre))
# dev.off()
#
# png("output/BioTech/Presentation/BioEffCost2.png", width=9*ppi, height=5*ppi, res=ppi)
# print(plot(BioEffCostPre2))
# dev.off()
#
# png("output/BioTech/Presentation/EleCost1.png", width=6*ppi, height=3*ppi, res=ppi)
# print(plot(EleCost1))
# dev.off()
# 
# png("output/BioTech/Presentation/EleCost2.png", width=6*ppi, height=3*ppi, res=ppi)
# print(plot(EleCost2))
# dev.off()
# 
# png("output/BioTech/Presentation/EleCost3.png", width=6*ppi, height=3*ppi, res=ppi)
# print(plot(EleCost3))
# dev.off()
#
# png("output/BioTech/Presentation/LiqDepCost1.png", width=6*ppi, height=3.5*ppi, res=ppi)
# print(plot(LiqDepCost1))
# dev.off()
# #
# png("output/BioTech/Presentation/LiqDepCost2.png", width=6*ppi, height=3.5*ppi, res=ppi)
# print(plot(LiqDepCost2))
# dev.off()
# # 
# png("output/BioTech/Presentation/EleDepCost1.png", width=7*ppi, height=3*ppi, res=ppi)
# print(plot(EleDepCost1))
# dev.off()
# #
# png("output/BioTech/Presentation/EleDepCost2.png", width=7*ppi, height=3*ppi, res=ppi)
# print(plot(EleDepCost2))
# dev.off()


# ---- END ---- 
