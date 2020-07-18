# R script that reads in EMF33 Demand Scenario (R3) database and outputs .csv files with relevant data for Bioenergy "Trade" and "Technology" papers
# These .csv files used in relevant R-scripts
# Purpose: Save computing time
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

# set directory path for csv file
setwd("C:/Users/Asus/Documents/GitHub/EMF33")

# ---- READ DATA ----
DATA=read.csv("data/emf33demr3r_compare_20180219-164947.csv", sep=",", dec=".", stringsAsFactors = FALSE)
DATA=melt(DATA, id.vars=c("MODEL","SCENARIO","REGION","VARIABLE","UNIT"), variable_name="Year", value.name="value", na.rm=FALSE)
colnames(DATA)[6]<-"Year"
DATA$Year = as.numeric(substr(DATA$Year, start=2, stop=5))
DATA = na.omit(DATA)

# ---- TRADE DATAFRAME ----
# Produce dataframe to be used in the processing of "Bioenergy trade" results

# Read in and add extra data from GRAPE-15 (secondary bioliquids trade to primary equivalent)
NewGRAPE=read.csv("data/Trade/GRAPE_trade_secondary_energy_liquids_biomass_primary_eq.csv", sep=",", dec=".", stringsAsFactors = FALSE)
NewGRAPE=melt(NewGRAPE, id.vars=c("MODEL","SCENARIO","REGION","VARIABLE","UNIT"), variable.name="Year", value.name="value", na.rm=FALSE)
colnames(NewGRAPE)[6]<-"Year"
NewGRAPE$Year = as.numeric(substr(NewGRAPE$Year, start=2, stop=5))
NewGRAPE = na.omit(NewGRAPE)

# Read in and add extra data from IMACLIM (Trade|Primary Energy|Biomass|Volume is prim+sec)
NewIMACLIM=read.csv("data/Trade/Imaclim_emf33_trade_biom_20112017.csv", sep=",", dec=".", stringsAsFactors=FALSE)
NewIMACLIM=melt(NewIMACLIM, id.vars=c("MODEL","SCENARIO","REGION","VARIABLE","UNIT"), variable.name="Year", value.name="value", na.rm=FALSE)
colnames(NewIMACLIM)[6]<-"Year"
NewIMACLIM$Year = as.numeric(substr(NewIMACLIM$Year, start=2, stop=5))
NewIMACLIM = na.omit(NewIMACLIM)

DATA = rbind(DATA,NewGRAPE)

# Regions
TradDATA1.R = subset(DATA, REGION=="ASIA"|
                            REGION=="LAM"|
                            REGION=="MAF"|
                            REGION=="OECD90"|
                            REGION=="REF"|
                            REGION=="Brazil"|REGION=="BRA"|
                            REGION=="China"|REGION=="CHN"|
                            REGION=="Japan"|REGION=="JPN"|
                            REGION=="EU"|REGION=="WEU"|REGION=="CEU"|REGION=="EEU"|
                            REGION=="USA")
# Years
TradDATA1.RY = subset(TradDATA1.R, Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2080"|Year=="2090"|Year=="2100")
# Variables
TradDATA1.RYV = subset(TradDATA1.RY, VARIABLE=="Trade|Primary Energy|Biomass|Volume"|
                                      VARIABLE=="Trade|Primary Energy|Coal|Volume"|
                                      VARIABLE=="Trade|Primary Energy|Gas|Volume"|
                                      VARIABLE=="Trade|Primary Energy|Oil|Volume"|
                                      VARIABLE=="Agricultural Production"|
                                      VARIABLE=="Agricultural Production|Energy Crops"|
                                      VARIABLE=="Primary Energy"|
                                      VARIABLE=="Primary Energy|Biomass|Modern"|
                                      VARIABLE=="Primary Energy|Fossil"|
                                      VARIABLE=="Trade|Primary Energy|Biomass|Value"|
                                      VARIABLE=="Trade|Primary Energy|Coal|Value"|
                                      VARIABLE=="Trade|Primary Energy|Gas|Value"|
                                      VARIABLE=="Trade|Primary Energy|Oil|Value"|
                                      VARIABLE=="Trade|Secondary Energy|Liquids|Biomass|Volume"|
                                      VARIABLE=="Trade|Secondary Energy|Solids|Biomass|Volume"|
                                      VARIABLE=="Trade|Secondary Energy|Liquids|Biomass|Primary Equivalent")  # UNIQUE FOR GRAPE

# Scenarios
TradDATA1.RYVS = subset(TradDATA1.RYV, SCENARIO=="R3-BASE-0-full"|
                                      SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-hi-limbio"|SCENARIO=="R3-B-hi-nofuel"|SCENARIO=="R3-B-hi-none"|SCENARIO=="R3-B-hi-nobeccs"|
                                      SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-lo-limbio"|SCENARIO=="R3-B-lo-nofuel"|SCENARIO=="R3-B-lo-none"|SCENARIO=="R3-B-lo-nobeccs"|
                                      SCENARIO=="R3-B-vlo-full")

# Specifically for COFFEE: does not report Modern primary biomass, use "Total" primary biomass and assume equal to modern
TradData1.COF = subset(TradDATA1.RY, VARIABLE=="Primary Energy|Biomass"&MODEL=="COFFEE")
TradData1.COF = subset(TradData1.COF, SCENARIO=="R3-BASE-0-full"|
                          SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-hi-limbio"|SCENARIO=="R3-B-hi-nofuel"|SCENARIO=="R3-B-hi-none"|SCENARIO=="R3-B-hi-nobeccs"|
                          SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-lo-limbio"|SCENARIO=="R3-B-lo-nofuel"|SCENARIO=="R3-B-lo-none"|SCENARIO=="R3-B-lo-nobeccs"|
                          SCENARIO=="R3-B-vlo-full")
TradData1.COF$VARIABLE <- "Primary Energy|Biomass|Modern"

# Specifically for IMACLIM: Remove "Trade|Primary Energy|Biomass|Volume" from snapshot, and use one from new dataset.
NewIMACLIM1 =  subset(NewIMACLIM, SCENARIO=="R3-BASE-0-full"|
                       SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-hi-limbio"|SCENARIO=="R3-B-hi-nofuel"|SCENARIO=="R3-B-hi-none"|SCENARIO=="R3-B-hi-nobeccs"|
                       SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-lo-limbio"|SCENARIO=="R3-B-lo-nofuel"|SCENARIO=="R3-B-lo-none"|SCENARIO=="R3-B-lo-nobeccs"|
                       SCENARIO=="R3-B-vlo-full")

NewIMACLIM1 = subset(NewIMACLIM1, VARIABLE=="Trade|Primary Energy|Biomass|Volume")
NewIMACLIM1 = subset(NewIMACLIM1, Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2080"|Year=="2090"|Year=="2100")
  
#Biomass Prices
BioPrice = subset(TradDATA1.RY, VARIABLE=="Price|Primary Energy|Biomass|Delivered"|VARIABLE=="Price|Primary Energy|Biomass|Market")
BioPrice = subset(BioPrice, SCENARIO=="R3-BASE-0-full"|SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-vlo-full")

# Final dataframe for "Trade Paper"
TradDATA1.RYVS = subset(TradDATA1.RYVS, !(MODEL=="COFFEE"&VARIABLE=="Primary Energy|Biomass|Modern"))
TradDATA1.RYVS = subset(TradDATA1.RYVS, !(MODEL=="IMACLIM-NLU"&VARIABLE=="Trade|Primary Energy|Biomass|Volume"))
TradDATA = rbind(TradDATA1.RYVS,TradData1.COF,NewIMACLIM1)
# write.csv(TradDATA, file = "data/Trade/TradDATA.csv")
# write.csv(BioPrice, file = "data/Trade/TradPrice.csv")

# ---- TECHNOLOGIES DATAFRAME ----
# Produce dataframe to be used in the processing of "Bioenergy Technologies" results

DATA$VARID <- substr(DATA$VARIABLE, start=1, stop=10)

# For DNE21+ there is a technology producing liquid fuels reported under "Secondary Energy|Liquids|Other".
# This is a bio-synthetic fuel (see email fuminori Sano 26/09/2018)
# Technoeconomic parameters are not however reported for this technology and so it is not included in the results.
# Thus, we add the secondary energy production from this technology to "Secondary Energy|Liquids|Other|w/o CCS"
# First for specific technology (*|Biomass|Other|w/o CCS)
DNEcor=subset(DATA, MODEL=="DNE21+ V.14"&
                (VARIABLE=="Secondary Energy|Liquids|Biomass|Other|w/o CCS"|VARIABLE=="Secondary Energy|Liquids|Other"))

DNEcor = spread(DNEcor,VARIABLE,value)
colnames(DNEcor)[7:8] <- c("A","B")
DNEcor = DNEcor %>% mutate(Total = A + B)
DNEcor$A <- NULL
DNEcor$B <- NULL
colnames(DNEcor)[length(DNEcor)] <-"value"
DNEcor$VARIABLE <- "Secondary Energy|Liquids|Biomass|Other|w/o CCS"

# Second for overall biofuels (*|Biomass|w/o CCS)
DNEcor2=subset(DATA, MODEL=="DNE21+ V.14"&
                (VARIABLE=="Secondary Energy|Liquids|Biomass|w/o CCS"|VARIABLE=="Secondary Energy|Liquids|Other"))

DNEcor2 = spread(DNEcor2,VARIABLE,value)
colnames(DNEcor2)[7:8] <- c("A","B")
DNEcor2 = DNEcor2 %>% mutate(Total = A + B)
DNEcor2$A <- NULL
DNEcor2$B <- NULL
colnames(DNEcor2)[length(DNEcor2)] <-"value"
DNEcor2$VARIABLE <- "Secondary Energy|Liquids|Biomass|w/o CCS"

# Add to main DATA dataframe
DATA = subset(DATA, !(MODEL=="DNE21+ V.14"&
                (VARIABLE=="Secondary Energy|Liquids|Biomass|Other|w/o CCS"|
                   VARIABLE=="Secondary Energy|Liquids|Other"|
                   VARIABLE=="Secondary Energy|Liquids|Biomass|w/o CCS")))

DATA = rbind(DATA,DNEcor,DNEcor2)
rm(DNEcor,DNEcor2)

# ---- Secondary Energy Demand ----
# First get total Secondary Energy Deamand DF
SecEnTot = subset(DATA, VARID=="Secondary ")
SecEnTot = subset(SecEnTot,
                  VARIABLE=="Secondary Energy|Electricity"|
                  VARIABLE=="Secondary Energy|Gases"|
                  VARIABLE=="Secondary Energy|Heat"|
                  VARIABLE=="Secondary Energy|Hydrogen"|
                  VARIABLE=="Secondary Energy|Liquids"|
                  VARIABLE=="Secondary Energy|Electricity|Biomass|w/ CCS"|
                  VARIABLE=="Secondary Energy|Electricity|Biomass|w/o CCS"|
                  VARIABLE=="Secondary Energy|Gases|Biomass"|
                  VARIABLE=="Secondary Energy|Heat|Biomass"|
                  VARIABLE=="Secondary Energy|Hydrogen|Biomass|w/ CCS"|
                  VARIABLE=="Secondary Energy|Hydrogen|Biomass|w/o CCS"|
                  VARIABLE=="Secondary Energy|Liquids|Biomass|w/ CCS"|
                  VARIABLE=="Secondary Energy|Liquids|Biomass|w/o CCS")
                    
SecEnTot$CarrierID <-gsub( "[[:punct:]]","",SecEnTot$VARIABLE,fixed=F)
SecEnTot$CarrierID <-gsub( "Secondary Energy","",SecEnTot$CarrierID,fixed=F)
SecEnTot$CarrierID <-gsub( "Electricity","Ele",SecEnTot$CarrierID,fixed=F)
SecEnTot$CarrierID <-gsub( "Gases","Gas",SecEnTot$CarrierID,fixed=F)
SecEnTot$CarrierID <-gsub( "Heat","Hea",SecEnTot$CarrierID,fixed=F)
SecEnTot$CarrierID <-gsub( "Hydrogen","Hyd",SecEnTot$CarrierID,fixed=F)
SecEnTot$CarrierID <-gsub( "Liquids","Liq",SecEnTot$CarrierID,fixed=F)

SecEnTot$Capt[SecEnTot$CarrierID=="EleBiomassw CCS"|SecEnTot$CarrierID=="HydBiomassw CCS"|SecEnTot$CarrierID=="LiqBiomassw CCS"] <- "wCCS"
SecEnTot$Capt[SecEnTot$CarrierID=="EleBiomasswo CCS"|SecEnTot$CarrierID=="HydBiomasswo CCS"|SecEnTot$CarrierID=="LiqBiomasswo CCS"] <- "woCCS"
SecEnTot$Capt[is.na(SecEnTot$Capt)] <- "Total"

SecEnTot$CarrierID <-gsub( "CCS","",SecEnTot$CarrierID,fixed=F)
SecEnTot$CarrierID <-gsub( "wo","",SecEnTot$CarrierID,fixed=F)
SecEnTot$CarrierID <-gsub( "w","",SecEnTot$CarrierID,fixed=F)
SecEnTot$CarrierID <-gsub( "[[:space:]]","",SecEnTot$CarrierID,fixed=F)

SecEnTot$Prim[SecEnTot$CarrierID=="EleBiomass"|SecEnTot$CarrierID=="HydBiomass"|SecEnTot$CarrierID=="LiqBiomass"|SecEnTot$CarrierID=="GasBiomass"|SecEnTot$CarrierID=="HeaBiomass"] <- "Biomass"
SecEnTot$Prim[is.na(SecEnTot$Prim)] <- "Total"

SecEnTot$CarrierID <-gsub( "Biomass","",SecEnTot$CarrierID,fixed=F)

# ---- Techno-economic Parameters ----
# Spread Data to creat columns of "Captial Cost","Efficiency","OM fixed" and "OM Variabled"
DATA$VARID <- substr(DATA$VARIABLE, start=1, stop=10)
TechData = subset(DATA, VARID=="Capital Co"|VARID=="Efficiency"|VARID=="OM Cost|Fi"|VARID=="OM Cost|Va"|VARID=="Secondary "|VARID=="Price|Carb")
TechData1=data.table(TechData)
TechData1=TechData1[VARID %in% c("Capital Co", "Efficiency", "OM Cost|Fi","OM Cost|Va","Secondary ","Price|Carb")] 

TechData1$VARIABLE <-gsub("Capital Cost|","",TechData1$VARIABLE,fixed=F)
TechData1$VARIABLE <-gsub( "Efficiency|","",TechData1$VARIABLE,fixed=F)
TechData1$VARIABLE <-gsub( "OM Cost|Fixed|","",TechData1$VARIABLE,fixed=F)
TechData1$VARIABLE <-gsub( "OM Cost|Variable|","",TechData1$VARIABLE,fixed=F)
TechData1$VARIABLE <-gsub( "Capacity|E","",TechData1$VARIABLE,fixed=F)
TechData1$VARIABLE <-gsub( "lectricity","Electricity",TechData1$VARIABLE,fixed=F)
TechData1$VARIABLE <-gsub( "[[:punct:]]"," ",TechData1$VARIABLE,fixed=F)
TechData1$VARIABLE <-gsub( "[[:space:]]","",TechData1$VARIABLE,fixed=F)
TechData1$VARID <-gsub( "[[:punct:]]","",TechData1$VARID,fixed=F)
TechData1$VARID <-gsub( "[[:space:]]","",TechData1$VARID,fixed=F)
TechData1$UNIT <- NULL
TechData1$VARIABLE2 <- paste(TechData1$VARIABLE)
TechData1$VARIABLE2 <-gsub( "Secondarynergy","",TechData1$VARIABLE2,fixed=F)

# Add External GCAM DATA
# read in Data
GCAMDAT = read.xlsx("data/Technology/GCAM/GCAM_Data.xlsx", sheetIndex=4)
GCAMDAT = na.omit(GCAMDAT)
GCAMDAT = melt(GCAMDAT, id.vars=c("MODEL","SCENARIO","REGION","VARIABLE","VARID","VARIABLE2"), na.rm=TRUE)
colnames(GCAMDAT)[7] <- "Year"
GCAMDAT$Year = as.numeric(substr(GCAMDAT$Year, start=2, stop=5))
GCAMDAT$value = as.numeric(substr(GCAMDAT$value, start=1, stop=6))

#Use for selected scenarios
GCAMDAT.Scen=c("R3-BASE-0-full",
               "R3-B-hi-full","R3-B-lo-full","R3-B-vlo-full")
# Allocate to Regions and Scenarios 
GCAMDAT1=matrix(ncol=8, nrow=length(unique(TechData1$REGION[TechData1$MODEL=="GCAM_EMF33"])) * length(GCAMDAT.Scen) * nrow(GCAMDAT))
l=0
for(i in unique(TechData1$REGION[TechData1$MODEL=="GCAM_EMF33"])){
  for(j in unique(GCAMDAT.Scen)){
    for(m in 1:nrow(GCAMDAT)){
      l=l+1  
      GCAMDAT1[l,1:8] <-c("GCAM_EMF33",
                          j,
                          i,
                          as.character(GCAMDAT[m,4]),
                          as.numeric(GCAMDAT[m,7]),
                          as.character(GCAMDAT[m,8]),
                          as.character(GCAMDAT[m,5]),
                          as.character(GCAMDAT[m,6]))
    }
  }
}
#
rm(l,i,j,m)
GCAMDAT1 <- as.data.frame(GCAMDAT1)
colnames(GCAMDAT1) <- c("MODEL","SCENARIO","REGION","VARIABLE","Year","value","VARID","VARIABLE2")
GCAMDAT1$Year=as.numeric(substr(GCAMDAT1$Year, start=1, stop=4))
GCAMDAT1$value=as.numeric(substr(GCAMDAT1$value, start=1, stop=6))

TechData1a=rbind(TechData1,GCAMDAT1)
rm(GCAMDAT.Scen,GCAMDAT,GCAMDAT1)

# Fix Data Set
TechData1a=spread(TechData1a, VARID, value, drop=TRUE)

TechData2=TechData1a
TechData2$OBSID = paste(TechData2$MODEL,TechData2$SCENARIO,TechData2$REGION,TechData2$Year,TechData2$VARIABLE2)
TechData2$CtaxID = paste(TechData2$MODEL,TechData2$SCENARIO,TechData2$REGION,TechData2$Year)
TechData2$SpecID = paste(TechData2$MODEL,TechData2$SCENARIO,TechData2$REGION,TechData2$Year,TechData2$VARIABLE)

# Separate  "price of carbon" to be attached to energy variables later (base on CtaxID)
TechData2.ctax = subset(TechData2, VARIABLE=="PriceCarbon")
TechData2.ctax = subset(TechData2.ctax, select=-c(Efficiency,CapitalCo,OMCostFi,OMCostVa,Secondary))
TechData2 = subset(TechData2, !(VARIABLE=="PriceCarbon"))
TechData2$PriceCarb <-NULL

# Separate  "secondary energy", to be merged and attached to energy variables later (based on OBSID)
TechData2$SecID <- substr(TechData2$VARIABLE, start=1, stop=14)
TechData2.sec = subset(TechData2, SecID=="Secondarynergy")
TechData2.sec = subset(TechData2.sec, !(VARIABLE2==""))
TechData2.sec = subset(TechData2.sec, select=-c(Efficiency,CapitalCo,OMCostFi,OMCostVa,SecID,CtaxID))
TechData2.sec = subset(TechData2.sec, Secondary>0.0)
DuplicatesOK <- length(unique(TechData2.sec$OBSID)) == nrow(TechData2.sec)

# For REMIND: "Secondary Energy|Liquids|Biomass|*" = "Secondary Energy|Liquids|Biomass|Biodiesel|*"
TechData2.secREM=subset(TechData2.sec, MODEL=="REMIND-MAGPIE"&
                        (VARIABLE=="SecondarynergyLiquidsBiomass"|VARIABLE=="SecondarynergyLiquidsBiomasswCCS"|VARIABLE=="SecondarynergyLiquidsBiomasswoCCS"))
TechData2.secREM$VARIABLE[TechData2.secREM$VARIABLE=="SecondarynergyLiquidsBiomass"] <- "SecondarynergyLiquidsBiomassBiodiesel"
TechData2.secREM$VARIABLE[TechData2.secREM$VARIABLE=="SecondarynergyLiquidsBiomasswCCS"] <- "SecondarynergyLiquidsBiomassBiodieselwCCS"
TechData2.secREM$VARIABLE[TechData2.secREM$VARIABLE=="SecondarynergyLiquidsBiomasswoCCS"] <- "SecondarynergyLiquidsBiomassBiodieselwoCCS"
TechData2.secREM2=subset(TechData2.sec, !(MODEL=="REMIND-MAGPIE"&
                                     (VARIABLE=="SecondarynergyLiquidsBiomass"|VARIABLE=="SecondarynergyLiquidsBiomasswCCS"|VARIABLE=="SecondaryEnergyLiquidsBiomasswoCCS"|
                                        VARIABLE=="SecondarynergyLiquidsBiomassBiodiesel"|VARIABLE=="SecondarynergyLiquidsBiomassBiodieselwCCS"|VARIABLE=="SecondarynergyLiquidsBiomassBiodieselwoCCS")))

TechData2.sec=rbind(TechData2.secREM2,TechData2.secREM)

# For MESSAGE: "Secondary Energy|Liquids|Biomass|*" = "Secondary Energy|Liquids|Biomass|Other|*"
TechData2.secMES=subset(TechData2.sec, MODEL=="MESSAGE-GLOBIOM"&
                          (VARIABLE=="SecondarynergyLiquidsBiomass"|VARIABLE=="SecondarynergyLiquidsBiomasswCCS"|VARIABLE=="SecondarynergyLiquidsBiomasswoCCS"))
TechData2.secMES$VARIABLE[TechData2.secMES$VARIABLE=="SecondarynergyLiquidsBiomass"] <- "SecondarynergyLiquidsBiomassOther"
TechData2.secMES$VARIABLE[TechData2.secMES$VARIABLE=="SecondarynergyLiquidsBiomasswCCS"] <- "SecondarynergyLiquidsBiomassOtherwCCS"
TechData2.secMES$VARIABLE[TechData2.secMES$VARIABLE=="SecondarynergyLiquidsBiomasswoCCS"] <- "SecondarynergyLiquidsBiomassOtherwoCCS"
TechData2.secMES2=subset(TechData2.sec, !(MODEL=="MESSAGE-GLOBIOM"&
                                            (VARIABLE=="SecondarynergyLiquidsBiomass"|VARIABLE=="SecondarynergyLiquidsBiomasswCCS"|VARIABLE=="SecondaryEnergyLiquidsBiomasswoCCS"|
                                               VARIABLE=="SecondarynergyLiquidsBiomassOther"|VARIABLE=="SecondarynergyLiquidsBiomassOtherwCCS"|VARIABLE=="SecondarynergyLiquidsBiomassOtherwoCCS")))

TechData2.sec=rbind(TechData2.secMES2,TechData2.secMES)

# Continue Fixing
TechData2.sec$SpecID <- NULL
TechData2.sec$VARIABLE <- sub("Secondarynergy","",TechData2.sec$VARIABLE,fixed=F)
TechData2.sec$SpecID = paste(TechData2.sec$MODEL,TechData2.sec$SCENARIO,TechData2.sec$REGION,TechData2.sec$Year,TechData2.sec$VARIABLE)

TechData2 = subset(TechData2, !(SecID=="Secondarynergy"))
TechData2$SecID <- NULL
TechData2$Secondary <- NULL
TechData2 = melt(TechData2, measure.vars=c("CapitalCo","Efficiency","OMCostFi","OMCostVa"), variable.name="VARID", value.name="value")
TechData2 =na.omit(TechData2)
TechData2 = spread(TechData2, VARID, value, drop=TRUE)

# Get columns with Energy Carrier and CCS technolopgy Identifiers
TechData2$CarrierID=substr(TechData2$VARIABLE2,1,3)
TechData2$CarrierID2 <-paste(TechData2$MODEL,TechData2$SCENARIO,TechData2$REGION,TechData2$Year,TechData2$CarrierID)
TechData2$Prim <- sub("Electricity|Gases|Heat|Hydrogen|Liquids","",TechData2$VARIABLE2,fixed=F)
TechData2$Prim <- sub("2|3|4|5","",TechData2$Prim,fixed=F)
TechData2$Prim <- sub("CellulosicNondiesel|ConventionalEthanol|Biodiesel|Other|Conventionalthanol","",TechData2$Prim,fixed=F)
TechData2$Prim <- sub("Onshore|Offshore","",TechData2$Prim,fixed=F)
TechData2$Prim <- sub("SolarPV|CSP|PV|SolarCSP|Solar","Solar",TechData2$Prim,fixed=F)
TechData2 = subset(TechData2, !(Prim==""))
TechData2$Capt <- TechData2$Prim
TechData2$Prim <- sub("woCCS","",TechData2$Prim,fixed=F)
TechData2$Capt <- sub("Biomass|Coal|Gas|Geothermal|Hydro|Nuclear|Solar|Wind|Electricity|Oil|Ocean","",TechData2$Capt,fixed=F)
TechData2$Capt[TechData2$Capt==""]<-"woCCS"

TechData3 = TechData2
# Attach "Secondary Energy" as a single column
TechData3$SecEn <- TechData2.sec[match(TechData3$SpecID, TechData2.sec$SpecID),7]

# Separately add "Secondary Energy|Liquids|Oil" for:
# MESSAGE-GLOBIOM, BET, GRAPE-15, POLES EMF33
# These have to be added separately since they do not have techo-economic parameters andthus get lost when data is matched through "SpecID"
TechData3.Extra=subset(TechData2.sec, (MODEL=="MESSAGE-GLOBIOM"|MODEL=="BET"|MODEL=="GRAPE-15"|MODEL=="POLES EMF33")&VARIABLE=="LiquidsOil")
TechData3.Extra$CtaxID = paste(TechData3.Extra$MODEL,TechData3.Extra$SCENARIO,TechData3.Extra$REGION,TechData3.Extra$Year)
TechData3.Extra$CapitalCo <- NA
TechData3.Extra$Efficiency <- NA
TechData3.Extra$OMCostFi <- NA
TechData3.Extra$OMCostVa <- NA
TechData3.Extra$CarrierID=substr(TechData3.Extra$VARIABLE2,1,3)
TechData3.Extra$CarrierID2 <-paste(TechData3.Extra$MODEL,TechData3.Extra$SCENARIO,TechData3.Extra$REGION,TechData3.Extra$Year,TechData3.Extra$CarrierID)
TechData3.Extra$Prim <- "Oil"
TechData3.Extra$Capt <- "woCCS"
colnames(TechData3.Extra)[7] <- "SecEn"
TechData3.Extra=TechData3.Extra[,c("MODEL","SCENARIO","REGION","VARIABLE","Year","VARIABLE2","OBSID","CtaxID","SpecID","CapitalCo","Efficiency","OMCostFi","OMCostVa","CarrierID","CarrierID2","Prim","Capt","SecEn")]

TechData3=rbind(TechData3,TechData3.Extra)
rm(TechData3.Extra)

# For IMACLIM correct CapCosts and OMCost for Bio-Hydrogen Technology
TechData3$CapitalCo[TechData3$MODEL=="IMACLIM-NLU"&TechData3$CarrierID=="Hyd"&TechData3$Prim=="BiomasswCCS"] <- 2857
TechData3$OMCostFi[TechData3$MODEL=="IMACLIM-NLU"&TechData3$CarrierID=="Hyd"&TechData3$Prim=="BiomasswCCS"] <- 258.6
TechData3$CapitalCo=as.numeric(substr(TechData3$CapitalCo, start=1, stop=6))
TechData3$OMCostFi=as.numeric(substr(TechData3$OMCostFi, start=1, stop=6))

TechData4 = TechData3
# Attach Carbon taxes which were separated earlier
TechData4$Ctax <- TechData2.ctax[match(TechData3$CtaxID, TechData2.ctax$CtaxID),7]
TechData4 = subset(TechData4, select=-c(OBSID,CtaxID,VARIABLE2))

# Get price of prim energy carriers and attach (based on PrimPriceID)
PrimPrice = subset(DATA, VARID=="Price|Prim")
PrimPrice = subset(PrimPrice, VARIABLE=="Price|Primary Energy|Biomass|Market"| 
                              VARIABLE=="Price|Primary Energy|Biomass|Delivered"|
                              VARIABLE=="Price|Primary Energy|Coal"|
                              VARIABLE=="Price|Primary Energy|Gas"|
                              VARIABLE=="Price|Primary Energy|Oil")
PrimPrice$Prim <- sub("[[:punct:]]","",PrimPrice$VARIABLE,fixed=F) 
PrimPrice$Prim <- sub("[[:space:]]","",PrimPrice$Prim,fixed=F) 
PrimPrice$Prim <- sub("PricePrimaryEnergy","",PrimPrice$Prim,fixed=F) 
PrimPrice$Prim <- sub("[[:punct:]]","",PrimPrice$Prim,fixed=F) 
PrimPrice$Prim <- sub("[[:punct:]]","",PrimPrice$Prim,fixed=F) 
PrimPrice$VARIABLE <- sub("[[:punct:]]","",PrimPrice$VARIABLE,fixed=F) 
PrimPrice$VARIABLE <- sub("[[:punct:]]","",PrimPrice$VARIABLE,fixed=F) 
PrimPrice$VARIABLE <- sub("[[:punct:]]","",PrimPrice$VARIABLE,fixed=F) 
PrimPrice$VARIABLE <- sub("[[:space:]]","",PrimPrice$VARIABLE,fixed=F) 
PrimPrice$VARIABLE <- sub("PrimaryEnergy","Prim",PrimPrice$VARIABLE,fixed=F) 

# For cases where "Biomass|Market" is missing, use "Biomass|Delivered"
PrimPrice.Bio=subset(PrimPrice, VARIABLE=="PricePrimBiomassMarket"|VARIABLE=="PricePrimBiomassDelivered")
PrimPrice.Bio=subset(PrimPrice.Bio, select=-c(VARID,Prim))
PrimPrice.Bio2=spread(PrimPrice.Bio,VARIABLE,value)
PrimPrice.Bio2$value <- ifelse(is.na(PrimPrice.Bio2$PricePrimBiomassMarket), PrimPrice.Bio2$PricePrimBiomassDelivered, PrimPrice.Bio2$PricePrimBiomassMarket)
PrimPrice.Bio2$VARID <- "Price|Prim"
PrimPrice.Bio2$Prim <- "Biomass"
PrimPrice.Bio2$VARIABLE <- "PrimPriceBiomass"
PrimPrice.Bio2=subset(PrimPrice.Bio2, select=-c(PricePrimBiomassDelivered, PricePrimBiomassMarket))

PrimPrice.Fos=subset(PrimPrice, !(VARIABLE=="PricePrimBiomassMarket"|VARIABLE=="PricePrimBiomassDelivered"))
PrimPrice=rbind(PrimPrice.Fos,PrimPrice.Bio2)

ElecPrice = subset(DATA, VARIABLE=="Price|Secondary Energy|Electricity")
ElecPrice$Prim <-"Electricity"

PrimPrice = rbind(PrimPrice,ElecPrice)

PrimPrice$PrimPriceID = paste(PrimPrice$MODEL,PrimPrice$SCENARIO,PrimPrice$REGION,PrimPrice$Year,PrimPrice$Prim)
PrimPrice = subset(PrimPrice, select=-c(VARIABLE,VARID))
DuplicatesOK2 <- length(unique(PrimPrice$PrimPriceID)) == nrow(PrimPrice)

TechData4$Prim2 <- sub("wCCS","",TechData4$Prim,fixed=F)
TechData4$PrimPriceID = paste(TechData4$MODEL,TechData4$SCENARIO,TechData4$REGION,TechData4$Year,TechData4$Prim2)

TechData4$FeedCost <- PrimPrice[match(TechData4$PrimPriceID, PrimPrice$PrimPriceID),6]
TechData4 = subset(TechData4, select=-c(PrimPriceID,Prim2))
TechData4$FeedCost[TechData4$Prim=="Geothermal"|
                     TechData4$Prim=="Hydro"|
                     TechData4$Prim=="Solar"|
                     TechData4$Prim=="Wind"] <- 0.0
TechData4$FeedCost[TechData4$Prim=="Nuclear"] <- 9.33

TechData4=data.table(TechData4)

# Correct Column Order
TechData4 <- TechData4[,c("MODEL","SCENARIO","REGION","Year","VARIABLE","Prim","CarrierID","Capt","SecEn","Efficiency","CapitalCo","OMCostFi","OMCostVa","Ctax","FeedCost","CarrierID2")]

# Final energy prices
PriceData = subset(DATA, VARID=="Price|Seco")
PriceData = subset(PriceData, VARIABLE=="Price|Secondary Energy|Electricity"|
                              VARIABLE=="Price|Secondary Energy|Liquids"|
                              VARIABLE=="Price|Secondary Energy|Liquids|Oil")
      # For BET, use "Price|Secondary Energy|Liquids" as "Price|Secondary Energy|Liquids|Oil"
      temp=subset(DATA, VARIABLE=="Price|Secondary Energy|Liquids"&MODEL=="BET")      
      temp$VARIABLE <- "Price|Secondary Energy|Liquids|Oil"
      PriceData=rbind(PriceData,temp)
      rm(temp)
PriceData$VARIABLE <-gsub("Price|Secondary Energy|","",PriceData$VARIABLE,fixed=F)
PriceData$VARIABLE <-gsub( "[[:punct:]]","",PriceData$VARIABLE,fixed=F)
PriceData$VARID <-gsub("[[:punct:]]","",PriceData$VARID, fixed=F)

# ---- REGIONAL CORRECTIONS ----
# Subset relevant regions
# Limit to following regions: US, China, EU, India, Brazil
# Make corrections: 
# REMIND: LAM=Brazil
# MESSAGE: LAM=Brazil
# BET: Only gives RCP regions
TechData4 = subset(TechData4,  REGION=="ASIA"|REGION=="LAM"|REGION=="MAF"|REGION=="OECD90"|REGION=="REF"|REGION=="Russia"|
                                REGION=="USA"|REGION=="EU"|REGION=="Brazil"|REGION=="China"|REGION=="Japan"|REGION=="India"|
                                REGION=="World")

# CORRECT EFFICIENCIES and regional aggregation issues
#Correct Efficiency For REMINd (/100)
TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE"] <- (TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE"]/100)

# For DNE21: 
#ASIA has to be divided by 4
TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="ASIA"] <- (TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="ASIA"]/4)
TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="ASIA"] <- (TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="ASIA"]/4)
TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="ASIA"]/4)
TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="ASIA"]/4)
#LAM has to be divided by 3
TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="LAM"] <- (TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="LAM"]/3)
TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="LAM"] <- (TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="LAM"]/3)
TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="LAM"] <- (TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="LAM"]/3)
TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="LAM"] <- (TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="LAM"]/3)
#MAF has to be divided by 2
TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="MAF"] <- (TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="MAF"]/2)
TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="MAF"] <- (TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="MAF"]/2)
TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="MAF"] <- (TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="MAF"]/2)
TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="MAF"] <- (TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="MAF"]/2)
#oecd has to be divided by 8
TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="OECD90"] <- (TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="OECD90"]/8)
TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="OECD90"] <- (TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="OECD90"]/8)
TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="OECD90"]/8)
TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="OECD90"]/8)
#REF has to be divided by 3
TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="REF"] <- (TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="REF"]/3)
TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="REF"] <- (TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="REF"]/3)
TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="REF"] <- (TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="REF"]/3)
TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="REF"] <- (TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="REF"]/3)
#EU has to be divided by 2
TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="EU"] <- (TechData4$Efficiency[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="EU"]/2)
TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="EU"] <- (TechData4$CapitalCo[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="EU"]/2)
TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="EU"] <- (TechData4$OMCostFi[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="EU"]/2)
TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="EU"] <- (TechData4$OMCostVa[TechData4$MODEL=="DNE21+ V.14" & TechData4$REGION=="EU"]/2)

# For GCAM: 
#ASIA has to be divided by 9
TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="ASIA"] <- (TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="ASIA"]/9)
TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="ASIA"] <- (TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="ASIA"]/9)
TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="ASIA"]/9)
TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="ASIA"]/9)
#LAM has to be divided by 7
TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="LAM"] <- (TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="LAM"]/7)
TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="LAM"] <- (TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="LAM"]/7)
TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="LAM"] <- (TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="LAM"]/7)
TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="LAM"] <- (TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="LAM"]/7)
#MAF has to be divided by 6
TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="MAF"] <- (TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="MAF"]/6)
TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="MAF"] <- (TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="MAF"]/6)
TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="MAF"] <- (TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="MAF"]/6)
TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="MAF"] <- (TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="MAF"]/6)
#oecd has to be divided by 7
TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="OECD90"] <- (TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="OECD90"]/7)
TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="OECD90"] <- (TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="OECD90"]/7)
TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="OECD90"]/7)
TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="OECD90"]/7)
#REF has to be divided by 3
TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="REF"] <- (TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="REF"]/3)
TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="REF"] <- (TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="REF"]/3)
TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="REF"] <- (TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="REF"]/3)
TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="REF"] <- (TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="REF"]/3)
#EU has to be divided by 2
TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU"] <- (TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU"]/2)
TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU"] <- (TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU"]/2)
TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU"] <- (TechData4$OMCostFi[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU"]/2)
TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU"] <- (TechData4$OMCostVa[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU"]/2)
#Specifically for Liquid Biofuels in the EU, this has to be undone (these data are not taken from database but added exogenously)
TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU" & TechData4$CarrierID=="Liq"] <- (TechData4$Efficiency[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU" & TechData4$CarrierID=="Liq"]*2)
TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU"  & TechData4$CarrierID=="Liq"] <- (TechData4$CapitalCo[TechData4$MODEL=="GCAM_EMF33" & TechData4$REGION=="EU" & TechData4$CarrierID=="Liq"]*2)

# For GRAPE-15: 
#EU has to be divided by 2
TechData4$Efficiency[TechData4$MODEL=="GRAPE-15" & TechData4$REGION=="EU"] <- (TechData4$Efficiency[TechData4$MODEL=="GRAPE-15" & TechData4$REGION=="EU"]/2)
TechData4$CapitalCo[TechData4$MODEL=="GRAPE-15" & TechData4$REGION=="EU"] <- (TechData4$CapitalCo[TechData4$MODEL=="GRAPE-15" & TechData4$REGION=="EU"]/2)
TechData4$OMCostFi[TechData4$MODEL=="GRAPE-15" & TechData4$REGION=="EU"] <- (TechData4$OMCostFi[TechData4$MODEL=="GRAPE-15" & TechData4$REGION=="EU"]/2)
TechData4$OMCostVa[TechData4$MODEL=="GRAPE-15" & TechData4$REGION=="EU"] <- (TechData4$OMCostVa[TechData4$MODEL=="GRAPE-15" & TechData4$REGION=="EU"]/2)

# For IMACLIM: 
#ASIA has to be divided by 3
TechData4$Efficiency[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="ASIA"] <- (TechData4$Efficiency[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="ASIA"]/3)
TechData4$CapitalCo[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="ASIA"] <- (TechData4$CapitalCo[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="ASIA"]/3)
TechData4$OMCostFi[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostFi[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="ASIA"]/3)
TechData4$OMCostVa[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostVa[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="ASIA"]/3)
#LAM has to be divided by 2
TechData4$Efficiency[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="LAM"] <- (TechData4$Efficiency[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="LAM"]/2)
TechData4$CapitalCo[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="LAM"] <- (TechData4$CapitalCo[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="LAM"]/2)
TechData4$OMCostFi[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="LAM"] <- (TechData4$OMCostFi[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="LAM"]/2)
TechData4$OMCostVa[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="LAM"] <- (TechData4$OMCostVa[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="LAM"]/2)
#MAF has to be divided by 2
TechData4$Efficiency[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="MAF"] <- (TechData4$Efficiency[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="MAF"]/2)
TechData4$CapitalCo[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="MAF"] <- (TechData4$CapitalCo[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="MAF"]/2)
TechData4$OMCostFi[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="MAF"] <- (TechData4$OMCostFi[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="MAF"]/2)
TechData4$OMCostVa[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="MAF"] <- (TechData4$OMCostVa[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="MAF"]/2)
#oecd has to be divided by 4
TechData4$Efficiency[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="OECD90"] <- (TechData4$Efficiency[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="OECD90"]/4)
TechData4$CapitalCo[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="OECD90"] <- (TechData4$CapitalCo[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="OECD90"]/4)
TechData4$OMCostFi[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostFi[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="OECD90"]/4)
TechData4$OMCostVa[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostVa[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="OECD90"]/4)
#REF has to be divided by 1
TechData4$Efficiency[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="REF"] <- (TechData4$Efficiency[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="REF"]/1)
TechData4$CapitalCo[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="REF"] <- (TechData4$CapitalCo[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="REF"]/1)
TechData4$OMCostFi[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="REF"] <- (TechData4$OMCostFi[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="REF"]/1)
TechData4$OMCostVa[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="REF"] <- (TechData4$OMCostVa[TechData4$MODEL=="IMACLIM-NLU" & TechData4$REGION=="REF"]/1)

# For IMAGE: 
#ASIA has to be divided by 6
TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="ASIA"] <- (TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="ASIA"]/6)
TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="ASIA"] <- (TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="ASIA"]/6)
TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="ASIA"]/6)
TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="ASIA"]/6)
#LAM has to be divided by 4
TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="LAM"] <- (TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="LAM"]/4)
TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="LAM"] <- (TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="LAM"]/4)
TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="LAM"] <- (TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="LAM"]/4)
TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="LAM"] <- (TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="LAM"]/4)
#MAF has to be divided by 6
TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="MAF"] <- (TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="MAF"]/6)
TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="MAF"] <- (TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="MAF"]/6)
TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="MAF"] <- (TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="MAF"]/6)
TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="MAF"] <- (TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="MAF"]/6)
#oecd has to be divided by 6
TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="OECD90"] <- (TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="OECD90"]/6)
TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="OECD90"] <- (TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="OECD90"]/6)
TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="OECD90"]/6)
TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="OECD90"]/6)
#REF has to be divided by 4
TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="REF"] <- (TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="REF"]/4)
TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="REF"] <- (TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="REF"]/4)
TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="REF"] <- (TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="REF"]/4)
TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="REF"] <- (TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="REF"]/4)
#EU has to be divided by 2
TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="EU"] <- (TechData4$Efficiency[TechData4$MODEL=="IMAGE" & TechData4$REGION=="EU"]/2)
TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="EU"] <- (TechData4$CapitalCo[TechData4$MODEL=="IMAGE" & TechData4$REGION=="EU"]/2)
TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="EU"] <- (TechData4$OMCostFi[TechData4$MODEL=="IMAGE" & TechData4$REGION=="EU"]/2)
TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="EU"] <- (TechData4$OMCostVa[TechData4$MODEL=="IMAGE" & TechData4$REGION=="EU"]/2)

# For MESSAGE-GLOBIOM: 
#ASIA has to be divided by 3
TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="ASIA"] <- (TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="ASIA"]/3)
TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="ASIA"] <- (TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="ASIA"]/3)
TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="ASIA"]/3)
TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="ASIA"]/3)
#LAM has to be divided by 1
TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="LAM"] <- (TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="LAM"]/1)
TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="LAM"] <- (TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="LAM"]/1)
TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="LAM"] <- (TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="LAM"]/1)
TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="LAM"] <- (TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="LAM"]/1)
#MAF has to be divided by 2
TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="MAF"] <- (TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="MAF"]/2)
TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="MAF"] <- (TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="MAF"]/2)
TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="MAF"] <- (TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="MAF"]/2)
TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="MAF"] <- (TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="MAF"]/2)
#oecd has to be divided by 3
TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="OECD90"] <- (TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="OECD90"]/3)
TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="OECD90"] <- (TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="OECD90"]/3)
TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="OECD90"]/3)
TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="OECD90"]/3)
#REF has to be divided by 2
TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="REF"] <- (TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="REF"]/2)
TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="REF"] <- (TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="REF"]/2)
TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="REF"] <- (TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="REF"]/2)
TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="REF"] <- (TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="REF"]/2)
#EU has to be divided by 2
TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="EU"] <- (TechData4$Efficiency[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="EU"]/2)
TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="EU"] <- (TechData4$CapitalCo[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="EU"]/2)
TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="EU"] <- (TechData4$OMCostFi[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="EU"]/2)
TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="EU"] <- (TechData4$OMCostVa[TechData4$MODEL=="MESSAGE-GLOBIOM" & TechData4$REGION=="EU"]/2)

# For REMIND-MAGPIE: 
#ASIA has to be divided by 3
TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="ASIA"] <- (TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="ASIA"]/3)
TechData4$CapitalCo[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="ASIA"] <- (TechData4$CapitalCo[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="ASIA"]/3)
TechData4$OMCostFi[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostFi[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="ASIA"]/3)
TechData4$OMCostVa[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="ASIA"] <- (TechData4$OMCostVa[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="ASIA"]/3)
#LAM has to be divided by 1
TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="LAM"] <- (TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="LAM"]/1)
TechData4$CapitalCo[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="LAM"] <- (TechData4$CapitalCo[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="LAM"]/1)
TechData4$OMCostFi[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="LAM"] <- (TechData4$OMCostFi[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="LAM"]/1)
TechData4$OMCostVa[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="LAM"] <- (TechData4$OMCostVa[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="LAM"]/1)
#MAF has to be divided by 2
TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="MAF"] <- (TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="MAF"]/2)
TechData4$CapitalCo[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="MAF"] <- (TechData4$CapitalCo[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="MAF"]/2)
TechData4$OMCostFi[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="MAF"] <- (TechData4$OMCostFi[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="MAF"]/2)
TechData4$OMCostVa[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="MAF"] <- (TechData4$OMCostVa[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="MAF"]/2)
#oecd has to be divided by 4
TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="OECD90"] <- (TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="OECD90"]/4)
TechData4$CapitalCo[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="OECD90"] <- (TechData4$CapitalCo[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="OECD90"]/4)
TechData4$OMCostFi[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostFi[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="OECD90"]/4)
TechData4$OMCostVa[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="OECD90"] <- (TechData4$OMCostVa[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="OECD90"]/4)
#REF has to be divided by 1
TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="REF"] <- (TechData4$Efficiency[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="REF"]/1)
TechData4$CapitalCo[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="REF"] <- (TechData4$CapitalCo[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="REF"]/1)
TechData4$OMCostFi[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="REF"] <- (TechData4$OMCostFi[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="REF"]/1)
TechData4$OMCostVa[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="REF"] <- (TechData4$OMCostVa[TechData4$MODEL=="REMIND-MAGPIE" & TechData4$REGION=="REF"]/1)

# ---- OUTPUT ----
TechData5 = subset(TechData4,  !(REGION=="ASIA"|REGION=="LAM"|REGION=="MAF"|REGION=="OECD90"|REGION=="REF"))
TechData5.RCP = subset(TechData4,  REGION=="ASIA"|REGION=="LAM"|REGION=="MAF"|REGION=="OECD90"|REGION=="REF")

#TechData5=TechData4
TechData5.Diag = subset(TechData4,  !(REGION=="MAF"|REGION=="REF"))

# write.csv(TechData5, file = "data/Technology/TechDATA.csv")
# write.csv(TechData5.RCP, file = "data/Technology/TechDATA_RCP.csv")
# write.csv(TechData5.Diag, file = "data/Technology/TechDATA_Reg.csv")
# write.csv(PriceData, file = "data/Technology/PriceDATA.csv")
# write.csv(SecEnTot, file = "data/Technology/SecEnTot.csv")

# ---- EMISSIONS ----
Emis = subset(DATA, VARID=="Emissions|") 
Emis$VARID <- substr(Emis$VARIABLE, start=1, stop=13)
Emis = subset(Emis, VARID=="Emissions|CO2")
Emis$VARIABLE <- sub("Carbon Capture and Storage","CCS",Emis$VARIABLE,fixed=F)
Emis$VARIABLE <- sub("Fossil Fuels and Industry","FFI",Emis$VARIABLE,fixed=F)
Emis$VARIABLE <- sub("Land Use","Land",Emis$VARIABLE,fixed=F)
Emis$VARIABLE <- sub("Emissions","Emis",Emis$VARIABLE,fixed=F)

Emis = subset(Emis, SCENARIO=="R3-BASE-0-full"|SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-vlo-full")

# write.csv(Emis, file = "data/Emissions/EmisCO2.csv")

#
# ---- DIAGNOSTIC ----
TechDiag = subset(DATA, SCENARIO=="R3-B-lo-full"&Year=="2050"&REGION=="OECD90")
TechDiag = subset(TechDiag, VARID=="Capital Co"|VARID=="Efficiency"|VARID=="OM Cost|Fi"|VARID=="OM Cost|Va"|
                           (VARIABLE=="Secondary Energy|Liquids|Biomass"|
                            VARIABLE=="Secondary Energy|Liquids|Biomass|wCCS"|
                            VARIABLE=="Secondary Energy|Liquids|Biomass|woCCS"|
                            VARIABLE=="Secondary Energy|Liquids|Biomass|Biodiesel"|
                            VARIABLE=="Secondary Energy|Liquids|Biomass|Biodiesel|wCCS"|
                            VARIABLE=="Secondary Energy|Liquids|Biomass|Biodiesel|woCCS"|
                            VARIABLE=="Secondary Energy|Liquids|Biomass|Cellulosic Nondiesel|w/ CCS"|
                            VARIABLE=="Secondary Energy|Liquids|Biomass|Cellulosic Nondiesel|w/o CCS"|
                            VARIABLE=="Secondary Energy|Liquids|Biomass|Conventional Ethanol|w/ CCS"|
                            VARIABLE=="Secondary Energy|Liquids|Biomass|Conventional Ethanol|w/o CCS"|
                            VARIABLE=="Secondary Energy|Liquids|Biomass|Other|w/ CCS"|
                            VARIABLE=="Secondary Energy|Liquids|Biomass|Other|w/o CCS"|
                            VARIABLE=="Price|Primary Energy|Biomass|Delivered"|
                            VARIABLE=="Price|Primary Energy|Biomass|Market"|
                            VARIABLE=="Price|Primary Energy|Coal"|
                            VARIABLE=="Price|Primary Energy|Gas"|
                            VARIABLE=="Price|Primary Energy|Oil")
                  )
TechDiag$ID=TechDiag$VARIABLE
TechDiag$ID <-gsub("Capital Cost|","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Efficiency|","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "OM Cost|Fixed|","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "OM Cost|Variable|","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Liquids","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Biomass","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Coal","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Gas","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Oil","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Hydro","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Solar","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Geothermal","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Wind","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Onshore","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Offshore","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "CSP","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "PV","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Nuclear","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Biodiesel","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Cellulosic Nondiesel","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Conventional Ethanol","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Other","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Electricity","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "Heat","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "gen","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "w/o CCS","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "w/ CCS","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "es","",TechDiag$ID,fixed=F)
TechDiag$ID <-gsub( "[[:punct:]]","",TechDiag$ID,fixed=F)
TechDiag = subset(TechDiag, !(ID=="2"|ID=="3"|ID=="4"))

TechDiag = subset(TechDiag, select=c(MODEL,VARIABLE,value))
TechDiag = spread(TechDiag,VARIABLE,value)
TechDiag = melt(TechDiag, id.vars="MODEL", na.rm=F)

TechDiag$value[is.na(TechDiag$value)] <- "MISSING"
TechDiag$value[!(TechDiag$value=="MISSING")] <- "SUBMITTED"

colnames(TechDiag)[2] <- "VARIABLE"
colnames(TechDiag)[3] <- "SUBMISSION STATUS"

TechDiag=spread(TechDiag,MODEL,"SUBMISSION STATUS")

# write.csv(TechDiag, file = "output/BioTech/Diagnostic/SubmissionStatus.csv")