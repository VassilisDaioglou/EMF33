# R script to make figures for EMF33 Bioenergy Trade crosscut
# clear memory
# ---- START ----
rm(list=ls()) 

#install.packages(c("maps", "mapdata"))

# Load Libraries
library(reshape);
library(ggplot2);
library(data.table);
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(xlsx)
library(ggmap)
library(maps)
library(mapdata)
library(gridExtra)
library(scales)

# set directory path for csv file
setwd("~/disks/y/ontwapps/Timer/Users/Vassilis/Projects - Documents/EMF33/Scenario results/R-Scripts")
#setwd("C:/Users/vassi/Documents/Work/EMF33/R-Scripts")

# ---- READ DATA FILE ----
ppi <- 600
NewReg=read.csv("data/Trade/TradeRegData.csv", sep=",", dec=".", stringsAsFactors = FALSE)
BioPrice=read.csv("data/Trade/TradeRegPrice.csv", sep=",", dec=".", stringsAsFactors = FALSE)
# Delete extra column created when data in imported
NewReg$X <-NULL
NewReg$Year = as.numeric(substr(NewReg$Year, start=1, stop=4))
BioPrice$X <- NULL
# Spread Data to creat columns for each of the variables
Trade = NewReg
Trade1=spread(Trade, variable, value, drop=TRUE)

# ---- PROCESS DATA ----
# Identify biomass and fossil exporters
Trade1$BioExporter = ifelse(Trade1$TradePrimBiomassVol>0,1,0)
Trade1$FFExporter = ifelse((Trade1$TradePrimCoalVol+Trade1$TradePrimOilVol+Trade1$TradePrimGasVol)>0,1,0)
Trade1 = Trade1 %>% mutate(TradeFF=(TradePrimCoalVol+TradePrimOilVol+TradePrimGasVol))
# Biomass Imports / Biomass demand
Trade1= Trade1 %>% mutate(BioImpDep=((1-BioExporter)*((TradePrimBiomassVol*-1)/PrimBiomass)))
# Biomass Demand / TPES
Trade1 = Trade1 %>% mutate(BioDep=(PrimBiomass/Prim))
# Fossil Imports / Fossil demand
Trade1= Trade1 %>% mutate(FFImpDep=((1-FFExporter)*((TradeFF*-1)/PrimFossil)))
# Fossil Demand / TPES
Trade1 = Trade1 %>% mutate(FFDep=(PrimFossil/Prim))
Trade2 = Trade1
# Get median of FFImpDep for 2010
FossilDepMedx <- aggregate(Trade2$FFImpDep, by=list(REGION=Trade2$REGION, Year=Trade2$Year), FUN=median, na.rm=TRUE)
FossilDepMedx =subset(FossilDepMedx, Year=="2010")
FossilDepMedx=subset(FossilDepMedx, REGION=="Brazil"|REGION=="RLAM"|REGION=="USA"|REGION=="EU"|REGION=="ROECD90"|REGION=="MAF"|REGION=="EAsia"|REGION=="RAsia"|REGION=="REF")
# FossilDepMedx1 = FossilDepMedx
# FossilDepMedx1$Year <- 2050
# FossilDepMedx2 = FossilDepMedx
# FossilDepMedx2$Year <- 2100
# FossilDepMedx=rbind(FossilDepMedx1,FossilDepMedx2)

# Get median of FFDep for 2010
FossilDepMedy <- aggregate(Trade2$FFDep, by=list(REGION=Trade2$REGION, Year=Trade2$Year), FUN=median, na.rm=TRUE)
FossilDepMedy =subset(FossilDepMedy, Year=="2010")
FossilDepMedy=subset(FossilDepMedy, REGION=="Brazil"|REGION=="RLAM"|REGION=="USA"|REGION=="EU"|REGION=="ROECD90"|REGION=="MAF"|REGION=="EAsia"|REGION=="RAsia"|REGION=="REF")
# FossilDepMedy1 = FossilDepMedy
# FossilDepMedy1$Year <- 2050
# FossilDepMedy2 = FossilDepMedy
# FossilDepMedy2$Year <- 2100
# FossilDepMedy=rbind(FossilDepMedy1,FossilDepMedy2)
# Exporters: Fraction of produced biomass destined for export
Trade2= Trade2 %>% mutate(BioExpFrac=(BioExporter*(TradePrimBiomassVol/(PrimBiomass+TradePrimBiomassVol))))
# Importers: Fraction of consumed biomass imported
Trade2= Trade2 %>% mutate(BioImpFrac=((1-BioExporter)*(TradePrimBiomassVol/PrimBiomass)))

# ---- RESULTS FOR PAPER ----
# Global Net Trade of different energy carriers
EneTrade=subset(Trade2, REGION=="Brazil"|REGION=="RLAM"|REGION=="USA"|REGION=="EU"|REGION=="ROECD90"|REGION=="MAF"|REGION=="EAsia"|REGION=="RAsia"|REGION=="REF")
EneTrade[,7:8]<- list(NULL)
EneTrade$TradePrimCoalVal <- NULL
EneTrade$TradePrimGasVal <- NULL
EneTrade$TradePrimOilVal <- NULL
EneTrade[,13:21]<- list(NULL)
EneTrade <- melt(EneTrade, measure.vars=c('Prim','PrimBiomass','TradePrimBiomassVol', 'TradePrimCoalVol','TradePrimOilVol','TradePrimGasVol','TradeSecLiquidsBiomassVol','TradeSecSolidsBiomassVol'))  
EneTrade$value[EneTrade$value<0] <-0
EneTrade <- spread(EneTrade,REGION,value)
EneTrade[is.na(EneTrade)]<-0
EneTrade= EneTrade %>% mutate(Global=(Brazil+EAsia+EU+MAF+RAsia+REF+RLAM+ROECD90+USA)) # Total Global Exports
BiomassTrade = subset(EneTrade, variable=="PrimBiomass"|variable=="TradePrimBiomassVol"|variable=="TradeSecLiquidsBiomassVol"|variable=="TradeSecSolidsBiomassVol")
EneTrade[,5:13]<- list(NULL)
EneTrade = subset(EneTrade, !(EneTrade$Global==0))
EneTradeMed <- aggregate(EneTrade$Global, by=list(Year=EneTrade$Year,Variable=EneTrade$variable), FUN=median, na.rm=TRUE)
EneTradeMed$SCENARIO <- "Aggregate"
EneTradeMed <- EneTradeMed[,c(1,2,4,3)]
EneTradeMed.Scen <- aggregate(EneTrade$Global, by=list(Year=EneTrade$Year,Variable=EneTrade$variable,SCENARIO=EneTrade$SCENARIO), FUN=median, na.rm=TRUE)
EneTradeMed = rbind(EneTradeMed,EneTradeMed.Scen)

# Cross-Model Median of Regional trade 
EneTrade2=melt(Trade2, id.vars=c("MODEL","SCENARIO","Year","REGION"))
EneTradeMed.Reg <- aggregate(EneTrade2$value, by=list(Year=EneTrade2$Year,variable=EneTrade2$variable,SCENARIO=EneTrade2$SCENARIO,REGION=EneTrade2$REGION), FUN=mean, na.rm=TRUE)

# Ranges
Ranges.max <- aggregate(BiomassTrade[,5:14], by=list(SCENARIO=BiomassTrade$SCENARIO,Year=BiomassTrade$Year,variable=BiomassTrade$variable), FUN=max, na.rm=TRUE)
Ranges <- melt(BiomassTrade, id.vars=c("MODEL","SCENARIO","Year","variable"), variable_name="REGION")
Ranges = subset(Ranges, value>1)
Ranges <- spread(Ranges,REGION,value)
Ranges.max <- aggregate(Ranges[,5:14], by=list(SCENARIO=Ranges$SCENARIO,Year=Ranges$Year,variable=Ranges$variable), FUN=max, na.rm=TRUE)
Ranges.max$MODEL <- "Maximum"
Ranges.min <- aggregate(Ranges[,5:14], by=list(SCENARIO=Ranges$SCENARIO,Year=Ranges$Year,variable=Ranges$variable), FUN=min, na.rm=TRUE)
Ranges.min$MODEL <- "Minimum"
Ranges.med <- aggregate(Ranges[,5:14], by=list(SCENARIO=Ranges$SCENARIO,Year=Ranges$Year,variable=Ranges$variable), FUN=median, na.rm=TRUE)
Ranges.med$MODEL <- "Median"

BiomassTrade= rbind(BiomassTrade,Ranges.max,Ranges.min,Ranges.med)

# Growth Rates
EneTradeGr = subset(EneTradeMed, Variable=="TradePrimBiomassVol")
EneTradeGr <- spread(EneTradeGr,Year,x)
names(EneTradeGr)[names(EneTradeGr)=="2010"] <- "yr2010"
names(EneTradeGr)[names(EneTradeGr)=="2020"] <- "yr2020"
names(EneTradeGr)[names(EneTradeGr)=="2030"] <- "yr2030"
names(EneTradeGr)[names(EneTradeGr)=="2040"] <- "yr2040"
names(EneTradeGr)[names(EneTradeGr)=="2050"] <- "yr2050"
names(EneTradeGr)[names(EneTradeGr)=="2060"] <- "yr2060"
names(EneTradeGr)[names(EneTradeGr)=="2070"] <- "yr2070"
names(EneTradeGr)[names(EneTradeGr)=="2080"] <- "yr2080"
names(EneTradeGr)[names(EneTradeGr)=="2090"] <- "yr2090"
names(EneTradeGr)[names(EneTradeGr)=="2100"] <- "yr2100"
EneTradeGr= EneTradeGr %>% mutate(GR_2020to2050=(((yr2050/yr2020)^(1/(2050-2020)))-1)*100) # Total Global Exports
EneTradeGr= EneTradeGr %>% mutate(GR_2020to2100=(((yr2100/yr2020)^(1/(2100-2020)))-1)*100) # Total Global Exports

# Carrier Traded
Carriers = subset(NewReg, variable=="TradePrimBiomassVol"|variable=="TradeSecLiquidsBiomassVol"|variable=="TradeSecSolidsBiomassVol")
Carriers = subset(Carriers, !(REGION=="LAM"|REGION=="OECD90"|REGION=="ASIA"))
Carriers = subset(Carriers, SCENARIO=="R3-B-hi-full")
Carriers$RegOrder = factor(Carriers$REGION, levels=c('Brazil','RLAM','USA','EU','ROECD90',"MAF","EAsia","RAsia","REF")) 
# IMAGE and POLES report secondary in primary equivalen
Carriers$value[Carriers$MODEL=="POLES EMF33" & Carriers$variable=="TradePrimBiomassVol"] <- 0
Carriers$value[Carriers$MODEL=="IMAGE" & Carriers$variable=="TradePrimBiomassVol"] <- 0

# Carriers1 <- ggplot(data=Carriers, mapping=aes(x=Year, y=value, fill=variable)) +
#   geom_bar(stat="identity") +
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("EJ/yr") +
#   xlab("") +
#   scale_color_manual(values=c("forestgreen","purple","red"),
#                      name="",
#                      breaks=c("TradePrimBiomassVol","TradeSecLiquidsBiomassVol","TradeSecSolidsBiomassVol"),
#                      labels=c("Primary","Liquids","Solids")
#   )+
#   facet_grid(RegOrder ~ MODEL , labeller=labeller(REGION = region_label, MODEL=model_labels))
# Carriers1
# List of relevant models
BioTradCheck = subset(EneTrade, variable=="TradePrimBiomassVol")
BioTradCheck = subset(BioTradCheck, !(BioTradCheck$Global==0))
BioTradCheck = unique(BioTradCheck$MODEL)
BioTradCheck<-as.data.frame(BioTradCheck)
colnames(BioTradCheck) <- c("MODEL")
Fossil<-data.frame("Fossil")
names(Fossil)<-c("MODEL")
BioTradCheck=rbind(BioTradCheck,Fossil)


# ---- DFs FOR FIGS ----
# Check which models actually trade biomass
BioProd = Trade2
BioProd = BioProd %>% mutate(BioProd=PrimBiomass+TradePrimBiomassVol)
BioProd$BioProd[BioProd$BioProd<0] <-0
BioProd = subset(BioProd, SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-vlo-full"|SCENARIO=="R3-BASE-0-full")
BioProd = subset(BioProd, !(REGION=="OECD90"|REGION=="LAM"|REGION=="ASIA"))
BioProd = melt(BioProd, id.vars=c("MODEL","SCENARIO","Year","REGION"))
BioProd$RegOrder = factor(BioProd$REGION, levels=c('Brazil','RLAM','USA','EU','ROECD90',"MAF","EAsia","RAsia","REF","Global")) 
BioProd$ScenOrder = factor(BioProd$SCENARIO, levels=c('R3-BASE-0-full','R3-B-hi-full','R3-B-lo-full','R3-B-vlo-full'))
BioProd=subset(BioProd, Year=="2020"|Year=="2040"|Year=="2060"|Year=="2080"|Year=="2100")
BioProd=subset(BioProd, MODEL %in% BioTradCheck$MODEL)

# Check proportion of models that agree on exporters/importers
BioProd.Map1 = subset(BioProd, variable=="BioExpFrac"|variable=="BioImpFrac"|variable=="TradePrimBiomassVol"|variable=="BioExporter")
ModelAgree=matrix(ncol=6, nrow=length(unique(BioProd.Map1$SCENARIO))*length(unique(BioProd.Map1$Year))*length(unique(BioProd.Map1$REGION)))
l=0
for(i in unique(BioProd.Map1$SCENARIO)){
  for(j in unique(BioProd.Map1$Year)){
    for(k in unique(BioProd.Map1$REGION)){
      l=l+1
      sample=subset(BioProd.Map1, SCENARIO==i&Year==j&REGION==k&variable=="BioExporter"&value==1)
      count=length(sample$MODEL)
      sample1=subset(BioProd.Map1, SCENARIO==i&Year==j&REGION==k&variable=="BioExporter")
      sample1=subset(sample1, !(REGION=="Brazil"&MODEL=="REMIND-MAGPIE"))
      count1=length(sample1$MODEL)
      ModelAgree[l,1:6]<-c(i,j,k,1,count,count1)
    }
  }
}
rm(l,i,j,k,sample,count,sample1,count1)
ModelAgree <- as.data.frame(ModelAgree)
colnames(ModelAgree) <- c("SCENARIO","Year","REGION","BioExporter","ModelAgree","ModelCount")
ModelAgree$ModelAgree=as.numeric(substr(ModelAgree$ModelAgree, start=1, stop=2))
ModelAgree$ModelCount=as.numeric(substr(ModelAgree$ModelCount, start=1,stop=2))
ModelAgree$BioExporter=as.numeric(substr(ModelAgree$BioExporter, start=1, stop=2))
ModelAgree$BioExporter[ModelAgree$ModelAgree < ModelAgree$ModelCount/2] <- -1
ModelAgree$ModelAgree1=ModelAgree$ModelAgree
ModelAgree$ModelAgree[ModelAgree$BioExporter==-1]<-(ModelAgree$ModelCount[ModelAgree$BioExporter==-1]-ModelAgree$ModelAgree1[ModelAgree$BioExporter==-1])
ModelAgree$ModelAgree1 <- NULL
ModelAgree = ModelAgree %>% mutate(ModelFraction=ModelAgree / ModelCount)
ModelAgree$BioExporter[ModelAgree$ModelFraction==0.5]<-0
ModelAgree = ModelAgree %>% mutate(ModelFraction2=BioExporter*ModelFraction*100)
ModelAgree$ModelFraction3=ModelAgree$ModelFraction2
ModelAgree$ModelFraction3[ModelAgree$ModelFraction2>60]<-">3/5 Export"
ModelAgree$ModelFraction3[ModelAgree$ModelFraction2 < -60]<-">3/5 Import"
ModelAgree$ModelFraction3[ModelAgree$ModelFraction2 > -60 & ModelAgree$ModelFraction2 < 66]<-"No Agreement"

# Among models that agree, how much do they trade? What fraction of global trade is it?
# need a list of models that agree# determine median trade across models
# determine median fraction of global trade agross models
ModelAgree.Model = subset(BioProd, variable=="TradePrimBiomassVol"|variable=="BioExporter")
ModelAgree.Model$RegOrder<-NULL
ModelAgree.Model1 = spread(ModelAgree.Model,variable,value)
ModelAgree.Model1=subset(ModelAgree.Model1, BioExporter==1)
ModelAgree.Model1$BioExporter<-NULL
ModelAgree.Model1 = spread(ModelAgree.Model1,REGION,TradePrimBiomassVol)
ModelAgree.Model1[is.na(ModelAgree.Model1)]<-0
ModelAgree.Model1= ModelAgree.Model1 %>% mutate(NetTrade=(Brazil+EAsia+EU+MAF+RAsia+REF+RLAM+ROECD90+USA)) # Total Global Exports
ModelAgree.Model1$ID=paste(ModelAgree.Model1$MODEL,ModelAgree.Model1$SCENARIO,ModelAgree.Model1$Year)

ModelAgree.Model = subset(ModelAgree.Model, variable=="TradePrimBiomassVol")
ModelAgree.Model=spread(ModelAgree.Model,REGION,value)
ModelAgree.Model$ID=paste(ModelAgree.Model$MODEL,ModelAgree.Model$SCENARIO,ModelAgree.Model$Year)
ModelAgree.Model$NetTrade <- ModelAgree.Model1[match(ModelAgree.Model$ID,ModelAgree.Model1$ID),14]

ModelAgree.Model = melt(ModelAgree.Model, id.vars=c("MODEL","SCENARIO","Year","variable","ScenOrder","NetTrade"))
colnames(ModelAgree.Model)[7] <- "REGION"
ModelAgree.Model$value=as.numeric(substr(ModelAgree.Model$value, start=1, stop=5))
ModelAgree.Model[is.na(ModelAgree.Model)]<-0
ModelAgree.Model = ModelAgree.Model %>% mutate(TradeFrac=(value/NetTrade)*100)
ModelAgree.Model$TradeFrac[ModelAgree.Model$TradeFrac=="NaN"] <- 0

ModelAgree.ModelExp = subset(ModelAgree.Model, TradeFrac > 0)
ModelAgree.ModelExpMed <- aggregate(ModelAgree.ModelExp$TradeFrac, by=list(SCENARIO=ModelAgree.ModelExp$SCENARIO,Year=ModelAgree.ModelExp$Year,ScenOrder=ModelAgree.ModelExp$ScenOrder, REGION=ModelAgree.ModelExp$REGION), FUN=median, na.rm=TRUE)
ModelAgree.ModelImp = subset(ModelAgree.Model, TradeFrac < 0)
ModelAgree.ModelImpMed <- aggregate(ModelAgree.ModelImp$TradeFrac, by=list(SCENARIO=ModelAgree.ModelImp$SCENARIO,Year=ModelAgree.ModelImp$Year,ScenOrder=ModelAgree.ModelImp$ScenOrder, REGION=ModelAgree.ModelImp$REGION), FUN=median, na.rm=TRUE)
ModelAgree.ModelMed = rbind(ModelAgree.ModelImpMed,ModelAgree.ModelExpMed)

#
Trade3=subset(Trade2, MODEL %in%BioTradCheck$MODEL)
Trade3$TradePrimCoalVol <-NULL
Trade3$TradePrimCoalVal <-NULL
Trade3$TradePrimOilVol <-NULL
Trade3$TradePrimOilVal <-NULL
Trade3$TradePrimGasVol <-NULL
Trade3$TradePrimGasVal <-NULL
Trade3$Prim <-NULL
Trade3$PrimBiomass <-NULL
Trade3$PrimFossil <-NULL
Trade3$TradeSecLiquidsBiomassVol <- NULL
Trade3$TradeSecSolidsBiomassVol <- NULL

BioTradVol = subset(Trade3, REGION=="Brazil"|REGION=="RLAM"|REGION=="USA"|REGION=="EU"|REGION=="ROECD90"|REGION=="MAF"|REGION=="EAsia"|REGION=="RAsia"|REGION=="REF")
BioTradVol = subset(BioTradVol, SCENARIO=="R3-BASE-0-full"|SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-vlo-full")
BioTradVol$ScenOrder = factor(BioTradVol$SCENARIO, levels=c('R3-BASE-0-full','R3-B-hi-full','R3-B-lo-full','R3-B-vlo-full'))
BioTradVol$RegOrder = factor(BioTradVol$REGION, levels=c('Brazil','RLAM','USA','EU','ROECD90',"MAF","EAsia","RAsia","REF")) 
BioTradVol=subset(BioTradVol,!is.na(BioTradVol$TradePrimBiomassVol))

BioTradVal = subset(Trade3, REGION=="Brazil"|REGION=="RLAM"|REGION=="USA"|REGION=="EU"|REGION=="ROECD90"|REGION=="MAF"|REGION=="EAsia"|REGION=="RAsia"|REGION=="REF")
BioTradVal = subset(BioTradVal, SCENARIO=="R3-BASE-0-full"|SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-vlo-full")
BioTradVal$ScenOrder = factor(BioTradVal$SCENARIO, levels=c('R3-BASE-0-full','R3-B-hi-full','R3-B-lo-full','R3-B-vlo-full'))
BioTradVal$RegOrder = factor(BioTradVal$REGION, levels=c('Brazil','RLAM','USA','EU','ROECD90',"MAF","EAsia","RAsia","REF")) 
BioTradVal = subset(BioTradVal, !(TradePrimBiomassVal==0))
BioTradVal=subset(BioTradVal,!is.na(BioTradVal$TradePrimBiomassVal))

BioPrice$ScenOrder = factor(BioPrice$SCENARIO, levels=c('R3-BASE-0-full','R3-B-hi-full','R3-B-lo-full','R3-B-vlo-full'))
BioPrice$RegOrder = factor(BioPrice$REGION, levels=c('Brazil','RLAM','USA','EU','ROECD90',"MAF","EAsia","RAsia","REF")) 


BioTradFrac = subset(Trade3, REGION=="Brazil"|REGION=="RLAM"|REGION=="USA"|REGION=="EU"|REGION=="ROECD90"|REGION=="MAF"|REGION=="EAsia"|REGION=="RAsia"|REGION=="REF")
BioTradFrac = subset(BioTradFrac, Year=="2050"|Year=="2100")
BioTradFrac=subset(BioTradFrac,!is.na(BioTradFrac$BioExpFrac)&!is.na(BioTradFrac$BioImpFrac))
BioTradFrac=subset(BioTradFrac,!is.na(BioTradFrac$BioExpFrac)&!is.na(BioTradFrac$BioImpFrac))
BioTradFrac$TradePrimBiomassVol <-NULL
BioTradFrac$TradePrimBiomassVal <-NULL
BioTradFrac$BioExporter <-NULL
BioTradFrac$FFExporter <-NULL
BioTradFrac$TradeFF <-NULL
BioTradFrac$BioImpDep <-NULL
BioTradFrac$BioDep <-NULL
BioTradFrac$FFImpDep <-NULL
BioTradFrac$FFDep <-NULL
BioTradFrac <- melt(BioTradFrac, measure.vars=c('BioExpFrac', 'BioImpFrac'))  
BioTradFrac = subset(BioTradFrac, !(value==0))
BioTradFrac$value[BioTradFrac$value > 1] <- 1 
BioTradFrac$value[BioTradFrac$value < -1] <- -1 
BioTradFrac1 = subset(BioTradFrac, SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-hi-nobeccs"|SCENARIO=="R3-B-hi-nofuel"|SCENARIO=="R3-B-hi-none")
BioTradFrac1$RegOrder = factor(BioTradFrac1$REGION, levels=c('Brazil','RLAM','USA','EU','ROECD90',"MAF","EAsia","RAsia","REF")) 

Security = subset(Trade3, REGION=="Brazil"|REGION=="RLAM"|REGION=="USA"|REGION=="EU"|REGION=="ROECD90"|REGION=="MAF"|REGION=="EAsia"|REGION=="RAsia"|REGION=="REF")
Security = subset(Security,Year=="2050"|Year=="2100")
Security = subset(Security, select=-c(TradePrimBiomassVol,TradePrimBiomassVal,BioExporter,FFExporter,TradeFF,FFImpDep,FFDep,BioExpFrac,BioImpFrac))
Security =subset(Security,!is.na(Security$BioImpDep))
names(Security)[names(Security)=="BioImpDep"] <- "value.x"
names(Security)[names(Security)=="BioDep"] <- "value.y"
names(FossilDepMedx)[names(FossilDepMedx)=="x"] <- "value.x"
names(FossilDepMedy)[names(FossilDepMedy)=="x"] <- "value.y"
FossilDepMed <- merge.data.frame(FossilDepMedx, FossilDepMedy, by = c("REGION","Year"))
FossilDepMed$MODEL <- "Fossil"
FossilDepMed$SCENARIO <- "R3-BASE-0-full"
FossilDepMed1=FossilDepMed
FossilDepMed2=FossilDepMed
FossilDepMed3=FossilDepMed
FossilDepMed1$SCENARIO <- "R3-B-hi-full"
FossilDepMed2$SCENARIO <- "R3-B-lo-full"
FossilDepMed3$SCENARIO <- "R3-B-vlo-full"
# FossilDepMed1=FossilDepMed
# FossilDepMed2=FossilDepMed
# FossilDepMed3=FossilDepMed
# FossilDepMed2$SCENARIO <- "R3-B-hi-full"
# FossilDepMed3$SCENARIO <- "R3-B-lo-full"
# FossilDepMed <- rbind(FossilDepMed1,FossilDepMed2,FossilDepMed3)
#FossilDepMed <-FossilDepMed[,c(5,6,1,2,3,4)]
Security <- rbind(Security,FossilDepMed,FossilDepMed1,FossilDepMed2,FossilDepMed3)

Security1 = subset(Security, SCENARIO=="R3-BASE-0-full"|SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-vlo-full")
Security1$ScenOrder = factor(Security1$SCENARIO, levels=c('R3-BASE-0-full','R3-B-hi-full','R3-B-lo-full','R3-B-vlo-full'))
Security1$ModelOrder = factor(Security1$MODEL, levels=c("Fossil","AIM/CGE","BET","COFFEE","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"))
Security1$RegOrder = factor(Security1$REGION, levels=c('Brazil','RLAM','USA','EU','ROECD90',"MAF","EAsia","RAsia","REF")) 
Security1$Year <- factor(Security1$Year)

# Shannon-Weiner Index for Diversity of Supply
SWDiversity = subset(Trade3, REGION=="Brazil"|REGION=="RLAM"|REGION=="USA"|REGION=="EU"|REGION=="ROECD90"|REGION=="MAF"|REGION=="EAsia"|REGION=="RAsia"|REGION=="REF")
SWDiversity$TradePrimBiomassVal <-NULL
SWDiversity$FFExporter <-NULL
SWDiversity$TradeFF <-NULL
SWDiversity$BioImpDep <-NULL
SWDiversity$BioDep <-NULL
SWDiversity$FFImpDep <-NULL
SWDiversity$FFDep <-NULL
SWDiversity$BioExpFrac <-NULL
SWDiversity$BioImpFrac <-NULL
SWDiversity =subset(SWDiversity,BioExporter==1)
SWDiversity$BioExporter <-NULL
SWDiversity <- melt(SWDiversity, measure.vars=c('TradePrimBiomassVol'))  
SWDiversity <- spread(SWDiversity,REGION,value)
SWDiversity[is.na(SWDiversity)]<-0
SWDiversity= SWDiversity %>% mutate(NetExport=(Brazil+EAsia+EU+MAF+RAsia+REF+RLAM+ROECD90+USA)) # Total Global Exports
SWDiversity$variable <-NULL
SWDiversity <- melt(SWDiversity, measure.vars=c(unique(BioTradVol$REGION)))  
names(SWDiversity)[names(SWDiversity)=="value"] <- "TradPrimBiomassVol"
names(SWDiversity)[names(SWDiversity)=="variable"] <- "REGION"
SWDiversity= SWDiversity %>% mutate(ShareExport=(TradPrimBiomassVol/NetExport)) # Share of exporters' global market
SWDiversity$TradPrimBiomassVol <-NULL
SWDiversity= SWDiversity %>% mutate(LNShareExport=(log(ShareExport)))
SWDiversity= SWDiversity %>% mutate(SWIndex1=(ShareExport*LNShareExport))
SWDiversity$SWIndex1[SWDiversity$SWIndex1 == "NaN"]  <- 0 
SWDiversity$ShareExport <-NULL
SWDiversity$LNShareExport <-NULL
SWDiversity <- spread(SWDiversity,REGION,SWIndex1)
SWDiversity= SWDiversity %>% mutate(SWIndex=-1*(Brazil+EAsia+EU+MAF+RAsia+REF+RLAM+ROECD90+USA))
SWDiversity[,5:13]<- list(NULL)
SWDiversity=subset(SWDiversity, Year=="2050"|Year=="2100")
SWDiversity1=subset(SWDiversity, SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-BASE-0-full"|SCENARIO=="R3-B-vlo-full")
SWDiversity1$ScenOrder = factor(SWDiversity1$SCENARIO, levels=c('R3-BASE-0-full','R3-B-hi-full','R3-B-lo-full','R3-B-vlo-full'))
SWDiversity1$Year <- factor(SWDiversity1$Year)

# Relative biomass prices
BioPrice.min <- aggregate(BioPrice$value, by=list(MODEL=BioPrice$MODEL, SCENARIO=BioPrice$SCENARIO, VARIABLE=BioPrice$VARIABLE, Year=BioPrice$Year), FUN=min, na.rm=TRUE)
BioPrice.min = subset(BioPrice.min, x>0)
BioPrice.min$ID = paste(BioPrice.min$MODEL, BioPrice.min$SCENARIO, BioPrice.min$VARIABLE, BioPrice.min$Year)

BioPrice.Norm=BioPrice
BioPrice.Norm$ID = paste(BioPrice.Norm$MODEL, BioPrice.Norm$SCENARIO, BioPrice.Norm$VARIABLE, BioPrice.Norm$Year)
BioPrice.Norm$MinPric = BioPrice.min[match(BioPrice.Norm$ID,BioPrice.min$ID),5] 
BioPrice.Norm$ID <- NULL
BioPrice.Norm = BioPrice.Norm %>% mutate(NormPric = value/MinPric)

# Net Trade vs. Market price
PricTrade = subset(Trade3, select=c(MODEL, SCENARIO, Year, REGION, TradePrimBiomassVol))
PricTrade$ID=paste(PricTrade$MODEL,PricTrade$SCENARIO,PricTrade$Year,PricTrade$REGION)
BioPrice2=BioPrice
BioPrice2$ID=paste(BioPrice2$MODEL,BioPrice2$SCENARIO,BioPrice2$Year,BioPrice2$REGION)
BioPrice2 = subset(BioPrice2, VARIABLE=="BiomassMarket")
PricTrade$BioPrice=BioPrice2[match(PricTrade$ID,BioPrice2$ID),6]
# ---- PREPARE STAT TESTS ----
TestScenLabels <- data.frame(c("BASE-B-hi",
                               "BASE-B-lo",
                               "B-lo-BASE",
                               "B-hi-BASE",
                               "B-lo-B-hi",
                               "B-vlo-BASE",
                               "B-vlo-B-hi",
                               "B-vlo-B-lo",
                               "R3-B-hi-nobeccs-R3-B-hi-full",
                               "R3-B-hi-nofuel-R3-B-hi-full",
                               "R3-B-hi-none-R3-B-hi-full",
                               "R3-B-hi-nofuel-R3-B-hi-nobeccs",
                               "R3-B-hi-none-R3-B-hi-nofuel",
                               "R3-B-hi-none-R3-B-hi-nobeccs"),
                             c("Baseline vs. hi",
                               "Baseline vs. lo",
                               "lo vs. Baseline",
                               "hi vs. Baseline",
                               "lo vs. hi",
                               "vlo vs. Baseline",
                               "vlo vs. hi",
                               "vlo vs. lo",
                               "NoBECCS vs. Full",
                               "NoFuel vs. Full",
                               "None vs. Full",
                               "NoFuel vs. NoBECCS",
                              "None vs. NoFuel",
                               "None vs. NoBECCS"))
colnames(TestScenLabels) <- c("NameIn","NameOut")

DiversityStat = subset(SWDiversity, Year=="2100")
DiversityStat$ScenID=substr(DiversityStat$SCENARIO,1,20)
DiversityStat$ScenID <-sub("R3-","",DiversityStat$ScenID,fixed=F)
DiversityStat$ScenID <-sub("-full","",DiversityStat$ScenID,fixed=F)
DiversityStat$ScenID <-sub("-none","",DiversityStat$ScenID,fixed=F)
DiversityStat$ScenID <-sub("-nobeccs","",DiversityStat$ScenID,fixed=F)
DiversityStat$ScenID <-sub("-nofuel","",DiversityStat$ScenID,fixed=F)
DiversityStat$ScenID <-sub("-0","",DiversityStat$ScenID,fixed=F)
DiversityStat$ScenOrder = factor(DiversityStat$ScenID, levels=c('BASE','B-hi','B-lo','B-vlo')) 

SecurityStat = Security
SecurityStat$ScenID=substr(SecurityStat$SCENARIO,1,20)
SecurityStat$ScenID <-sub("R3-","",SecurityStat$ScenID,fixed=F)
SecurityStat$ScenID <-sub("-full","",SecurityStat$ScenID,fixed=F)
SecurityStat$ScenID <-sub("-none","",SecurityStat$ScenID,fixed=F)
SecurityStat$ScenID <-sub("-nobeccs","",SecurityStat$ScenID,fixed=F)
SecurityStat$ScenID <-sub("-nofuel","",SecurityStat$ScenID,fixed=F)
SecurityStat$ScenID <-sub("-0","",SecurityStat$ScenID,fixed=F)
SecurityStat$ScenOrder = factor(SecurityStat$ScenID, levels=c('BASE','B-hi','B-lo','B-vlo')) 
SecurityStat = SecurityStat %>% mutate(Indicator=value.x+value.y)

SecurityCompare=SecurityStat
SecurityCompare$SCENARIO[SecurityCompare$MODEL=="Fossil"]<-"Fossil"
SecurityCompare$Year[SecurityCompare$MODEL=="Fossil"]<-"2050"
SecurityCompare1=subset(SecurityCompare,MODEL=="Fossil")
SecurityCompare1$Year <- "2100"
SecurityCompare=rbind(SecurityCompare,SecurityCompare1)
SecurityCompare$ScenID[SecurityCompare$MODEL=="Fossil"]<-"Fossil"
SecurityCompare$ScenID[SecurityCompare$SCENARIO=="R3-BASE-0-full"]<-"Baseline"
SecurityCompare$value.x <- NULL
SecurityCompare$value.y <- NULL
SecurityCompare$ScenOrder = factor(SecurityCompare$ScenID, levels=c('Fossil','Baseline','B-hi','B-lo','B-vlo'))
SecurityCompare=subset(SecurityCompare, Indicator<2.01)
SecurityCompare$RegOrder = factor(SecurityCompare$REGION, levels=c('Brazil','RLAM','USA','EU','ROECD90',"MAF","EAsia","RAsia","REF")) 
SecurityCompare$ModelOrder = factor(SecurityCompare$MODEL, levels=c("Fossil","AIM/CGE","BET","COFFEE","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"))

Drivers=subset(Trade2, REGION=="Brazil"|REGION=="RLAM"|REGION=="USA"|REGION=="EU"|REGION=="ROECD90"|REGION=="MAF"|REGION=="EAsia"|REGION=="RAsia"|REGION=="REF")
Drivers = subset(Drivers, SCENARIO=="R3-B-hi-full"|
                        SCENARIO=="R3-B-hi-nobeccs"|SCENARIO=="R3-B-hi-nofuel"|SCENARIO=="R3-B-hi-none"|
                        SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-vlo-full")
Drivers$ScenID=substr(Drivers$SCENARIO,1,20)
Drivers$ScenID <-sub("R3-B-","",Drivers$ScenID,fixed=F)
Drivers$ScenOrder = factor(Drivers$ScenID, levels=c('hi-full','hi-nobeccs','hi-nofuel','hi-none','lo-full',"vlo-full")) 

# ---- STAT TESTS ----
# Tukey HSD
boxplot(DiversityStat$SWIndex~DiversityStat$ScenOrder)
DiversityTest1.fit <- aov(SWIndex~ScenOrder, data=DiversityStat)
DiversityTest1.fit2 <- TukeyHSD(DiversityTest1.fit)
DiversityTest1.result= as.data.frame((DiversityTest1.fit2$ScenOrder))
names <- rownames(DiversityTest1.result)
DiversityTest <- cbind(names,DiversityTest1.result)
rownames(DiversityTest)<-NULL
colnames(DiversityTest)[5] <- 'p_adj'
DiversityTest$names <- TestScenLabels[match(DiversityTest$names,TestScenLabels$NameIn),2]
# T.test
DiversityTest.Ttest=pairwise.t.test(DiversityStat$SWIndex,DiversityStat$ScenOrder, p.adj="none")
DiversityTest.TtestDF=as.data.frame(DiversityTest.Ttest$p.value)
DiversityTest.Bonf=pairwise.t.test(DiversityStat$SWIndex,DiversityStat$ScenOrder, p.adj="bonferroni")
DiversityTest.BonfDF=as.data.frame(DiversityTest.Bonf$p.value)

# ---- DRIVERS OF TRADE ----
# DRIVERS OF TRADE: In order to evaluate drivers of trade get net global trade for different technology options...
Drivers = subset(Drivers, select=-c(TradePrimCoalVal,TradePrimCoalVol,TradePrimOilVal,TradePrimOilVol,TradePrimGasVal,TradePrimGasVol,
                                    TradeSecLiquidsBiomassVol,TradeSecSolidsBiomassVol,TradePrimBiomassVal,
                                    FFExporter,TradeFF,BioImpDep,BioDep,FFImpDep,FFDep,BioExpFrac,BioImpFrac,
                                    Prim,PrimBiomass,PrimFossil))
# First: Get annual global trade
Drivers1 =subset(Drivers,BioExporter==1)
Drivers1$BioExporter <-NULL
Drivers1 <- melt(Drivers1, measure.vars=c('TradePrimBiomassVol'))  
Drivers1 <- spread(Drivers1,REGION,value)
Drivers1[is.na(Drivers1)]<-0
Drivers1= Drivers1 %>% mutate(NetTrade=(Brazil+EAsia+EU+MAF+RAsia+REF+RLAM+ROECD90+USA)) # Total Global Exports
Drivers1$variable <-NULL
# also make sure importing regions are included in dataset
Drivers2 =subset(Drivers,BioExporter==0)
Drivers2$BioExporter <-NULL
Drivers2 <- melt(Drivers2, measure.vars=c('TradePrimBiomassVol'))  
Drivers2 <- spread(Drivers2,REGION,value)
Drivers2[is.na(Drivers2)]<-0
Drivers2$NetTrade<-0
#Drivers2= Drivers2 %>% mutate(NetTrade=(Brazil+EAsia+EU+MAF+RAsia+REF+RLAM+ROECD90+USA)) # Total Global Exports
Drivers2$variable <-NULL
Drivers =rbind(Drivers1,Drivers2)
# Second: Get cumulative trade 
Drivers <- melt(Drivers, measure.vars=c(unique(BioTradVol$REGION),'NetTrade'))  
Drivers<-Drivers[!(Drivers$value==0),]
Drivers <- spread(Drivers,Year,value)
Drivers[is.na(Drivers)]<-0
names(Drivers)[names(Drivers)=="2010"] <- "yr2010"
names(Drivers)[names(Drivers)=="2020"] <- "yr2020"
names(Drivers)[names(Drivers)=="2030"] <- "yr2030"
names(Drivers)[names(Drivers)=="2040"] <- "yr2040"
names(Drivers)[names(Drivers)=="2050"] <- "yr2050"
names(Drivers)[names(Drivers)=="2060"] <- "yr2060"
names(Drivers)[names(Drivers)=="2070"] <- "yr2070"
names(Drivers)[names(Drivers)=="2080"] <- "yr2080"
names(Drivers)[names(Drivers)=="2090"] <- "yr2090"
names(Drivers)[names(Drivers)=="2100"] <- "yr2100"
Drivers= Drivers %>% mutate(CumNetTrade=(yr2010+yr2020+yr2030+yr2040+yr2050+yr2060+yr2070+yr2080+yr2090+yr2100)) # Total Global Exports
Drivers = subset(Drivers, select=-c(yr2010,yr2020,yr2030,yr2040,yr2050,yr2060,yr2070,yr2080,yr2090,yr2100))
Drivers$RegOrder = factor(Drivers$variable, levels=c('Brazil','RLAM','USA','EU','ROECD90',"MAF","EAsia","RAsia","REF","NetTrade")) 
boxplot(Drivers$CumNetTrade~Drivers$ScenOrder)
# Third, start statistical tests across all regions and global (TECHNICAL SCENARIOS)
DriversTech=subset(Drivers, ScenID=="hi-full"|ScenID=="hi-nofuel"|ScenID=="hi-none"|ScenID=="hi-nobeccs")
l=0
DriversTech.Tuk = data.frame()
DriversTech.Ttest = data.frame()
for(i in unique(DriversTech$variable)){
  l=l+1
  # Tukey
  DriversTech1 = subset(DriversTech, variable==i)
  DriversTech1.fit <- aov(CumNetTrade~ScenOrder, data=DriversTech1)
  DriversTech1.Tuk <- TukeyHSD(DriversTech1.fit)
  DriversTech1.result= as.data.frame((DriversTech1.Tuk$ScenOrder))
    names <- rownames(DriversTech1.result)
    DriversTech1.result <- cbind(names,DriversTech1.result)
    rownames(DriversTech1.result)<-NULL
  DriversTech1.result$REGION <-i
  DriversTech1.result <- DriversTech1.result[,c(6,1,2,3,4,5)]
  DriversTech.Tuk= rbind(DriversTech.Tuk,DriversTech1.result)
  # T.Test
  DriversTech2 = subset(DriversTech, variable==i)
  DriversTech2.Ttest=pairwise.t.test(DriversTech2$CumNetTrade,DriversTech2$ScenOrder, p.adj="none")
  DriversTech2.TtestDF=as.data.frame(DriversTech2.Ttest$p.value)
    names2 <- rownames(DriversTech2.TtestDF)
    DriversTech2.TtestDF <- cbind(names2,DriversTech2.TtestDF)
    rownames(DriversTech2.TtestDF)<-NULL
  DriversTech2.TtestDF$REGION<-i
  DriversTech2.TtestDF <- DriversTech2.TtestDF[,c(5,1,2,3,4)]
  DriversTech.Ttest=rbind(DriversTech.Ttest,DriversTech2.TtestDF)
}
# Fourth, start statistical tests across all regions and global (BUDGET SCENARIOS)
DriversBudg=subset(Drivers, ScenID=="hi-full"|ScenID=="lo-full"|ScenID=="vlo-full")
l=0
DriversBudg.Tuk = data.frame()
DriversBudg.Ttest = data.frame()
for(i in unique(DriversTech$variable)){
  l=l+1
  # Tukey
  DriversBudg1 = subset(DriversBudg, variable==i)
  DriversBudg1.fit <- aov(CumNetTrade~ScenOrder, data=DriversBudg1)
  DriversBudg1.Tuk <- TukeyHSD(DriversBudg1.fit)
  DriversBudg1.result= as.data.frame((DriversBudg1.Tuk$ScenOrder))
  names <- rownames(DriversBudg1.result)
  DriversBudg1.result <- cbind(names,DriversBudg1.result)
  rownames(DriversBudg1.result)<-NULL
  DriversBudg1.result$REGION <-i
  DriversBudg1.result <- DriversBudg1.result[,c(6,1,2,3,4,5)]
  DriversBudg.Tuk= rbind(DriversBudg.Tuk,DriversBudg1.result)
  # T.Test
  DriversBudg2 = subset(DriversBudg, variable==i)
  DriversBudg2.Ttest=pairwise.t.test(DriversBudg2$CumNetTrade,DriversBudg2$ScenOrder, p.adj="none")
  DriversBudg2.TtestDF=as.data.frame(DriversBudg2.Ttest$p.value)
  names2 <- rownames(DriversBudg2.TtestDF)
  DriversBudg2.TtestDF <- cbind(names2,DriversBudg2.TtestDF)
  rownames(DriversBudg2.TtestDF)<-NULL
  DriversBudg2.TtestDF$REGION<-i
  DriversBudg2.TtestDF <- DriversBudg2.TtestDF[,c(4,1,2,3)]
  DriversBudg.Ttest=rbind(DriversBudg.Ttest,DriversBudg2.TtestDF)
}

# ---- SECURITY ----
# SECURITY: Check if Security indicator changes between scenarios (REGIONAL!!)
SecurityStat.1 = subset(SecurityStat, !(MODEL=="Fossil"))
SecurityStat.1 = subset(SecurityStat.1, Year=="2100")
SecurityStat.1[,5:6] <- NULL
SecurityStat.1 <- SecurityStat.1[!(SecurityStat.1$Indicator=="-Inf"),]
#SecurityStat$ScenOrder <- NULL
SecurityStat.1$Indicator[SecurityStat.1$Indicator>2] <-2
l=0
SecurityTest.Tuk = data.frame()
SecurityTest.Ttest = data.frame()
for(i in unique(SecurityStat.1$REGION)){
  l=l+1
  # Tukey
  SecurityTest1 = subset(SecurityStat.1, REGION==i)
  SecurityTest1.fit <- aov(Indicator~ScenOrder, data=SecurityTest1)
  SecurityTest1.fit2 <- TukeyHSD(SecurityTest1.fit)
  SecurityTest1.result= as.data.frame((SecurityTest1.fit2$ScenOrder))
    names <- rownames(SecurityTest1.result)
    SecurityTest1.result <- cbind(names,SecurityTest1.result)
    rownames(SecurityTest1.result)<-NULL
  SecurityTest1.result$REGION <-i
  SecurityTest1.result <- SecurityTest1.result[,c(6,1,2,3,4,5)]
  SecurityTest.Tuk= rbind(SecurityTest.Tuk,SecurityTest1.result)
  # T.Test
  SecurityTest2 = subset(SecurityStat.1, REGION==i)
  SecurityTest2.Ttest=pairwise.t.test(SecurityTest2$Indicator,SecurityTest2$ScenOrder, p.adj="none")
  SecurityTest2.TtestDF=as.data.frame(SecurityTest2.Ttest$p.value)
  names2 <- rownames(SecurityTest2.TtestDF)
  SecurityTest2.TtestDF <- cbind(names2,SecurityTest2.TtestDF)
  rownames(SecurityTest2.TtestDF)<-NULL
  SecurityTest2.TtestDF$REGION<-i
  SecurityTest2.TtestDF <- SecurityTest2.TtestDF[,c(5,1,2,3,4)]
  SecurityTest.Ttest=rbind(SecurityTest.Ttest,SecurityTest2.TtestDF)
}
colnames(SecurityTest.Tuk)[6] <- 'p_adj'

#SecurityTest$names <- TestScenLabels[match(SecurityTest$names,TestScenLabels$NameIn),2]
#SecurityTest = subset(SecurityTest, !(names=="Baseline vs. lo"))
boxplot(SecurityTest.Tuk$p_adj~SecurityTest.Tuk$names)

# DEMO TESTS
#Significance of Trade across scenarios
# boxplot(DiversityStat$NetExport~DiversityStat$ScenOrder)
# TradeSig <- aov(NetExport~ScenOrder, data=DiversityStat)
# summary(TradeSig)
# TukeyHSD(TradeSig)

# DRIVERS OF TRADE: Check if regional trade fraction changes across scenarios 
# RDriverSig = subset(DriverStat, REGION=="EU")
# RDriverSig$ScenOrder = factor(RDriverSig$SCENARIO, levels=c(unique(RDriverSig$SCENARIO)))
# boxplot(RDriverSig$value~RDriverSig$ScenOrder)
# fit <- aov(value~SCENARIO, data=RDriverSig)
# summary(fit)
# TukeyHSD(fit)

# ---- LABELS ----
scen_labels <- c("Fossil"="Fossil (2010)",
                 "R3-BASE-0-full"="Baseline",
                 "R3-B-hi-full"="Budget1600",
                 "R3-B-hi-limbio"="Budget1600-LimBio",
                 "R3-B-hi-nofuel"="Budget1600-Nofuel",
                 "R3-B-hi-nobeccs"="Budget1600-NoBECCS",
                 "R3-B-hi-none"="Budget1600-None",
                 "R3-B-lo-full"="Budget1000",
                 "R3-B-vlo-full"="Budget400",
                 "hi-full"="Budget1600",
                 "hi-nobeccs"="Budget1600-NoBECCS",
                 "hi-nofuel"="Budget1600-Nofuel",
                 "hi-none"="Budget1600-None",
                 "lo-full"="Budget1000",
                 "vlo-full"="Budget400",
                 "B-hi"="Budget1600",
                 "B-lo"="Budget1000",
                 "B-vlo"="Budget400")
var_labels <- c("Trade|Primary Energy|Biomass|Volume"="Biomass","Trade|Primary Energy|Coal|Volume"="Coal","Trade|Primary Energy|Gas|Volume"="Gas","Trade|Primary Energy|Oil|Volume"="Oil")
model_labels <- c("AIM/CGE"="AIM","BET"="BET","COFFEE"="COFFEE","DNE21+ V.14"="DNE21","MESSAGE-GLOBIOM"="MESSAGE","GCAM_EMF33"="GCAM","GRAPE-15"="GRAPE","IMACLIM-NLU"="IMACLIM","IMAGE"="IMAGE","POLES EMF33"="POLES","REMIND-MAGPIE"="REMIND-MAgPIE","FARM 3.1"="FARM")
region_label <- c("EU"="EU","USA"="USA","ROECD90"="Rest OECD","EAsia"="East Asia","RAsia"="Rest Asia","Brazil"="Brazil","RLAM"="Rest Lat.Am.","REF"="Former USSR","MAF"="M.East & Africa","NetTrade"="Global (gross)","Global"="Global (gross)")
#  
# ---- FIG: VOLUME PRODUCTION ----
FigProd <- ggplot(data=subset(BioProd, variable=="BioProd"), mapping=aes(x=Year, y=value, fill=RegOrder)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("A: Regional Primary Bioenergy Production") + theme(plot.title = element_text(lineheight=20, face="bold")) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, vjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("Regional Production,", EJ[Primary],"/yr",""))) +
  xlab("") +
  scale_x_continuous(breaks=seq(2020, 2100, 20)) +
  scale_fill_manual(values=c("forestgreen","greenyellow","navy","dodgerblue","cadetblue1","brown","blueviolet","pink","red"),
                      name ="Regions",
                      breaks=c("Brazil","RLAM","USA","EU","ROECD90","MAF","EAsia","RAsia","REF"),
                      labels=c("Brazil","Rest Lat.Am.","USA","EU","Rest OECD","M. East & Africa","East Asia","Rest Asia","Former USSR"),
                      guide=FALSE
  ) +
  facet_grid(ScenOrder ~ MODEL, labeller=labeller(MODEL=model_labels, ScenOrder=scen_labels))
FigProd

FigTrad <- ggplot(data=subset(BioProd,variable=="TradePrimBiomassVol"), mapping=aes(x=Year, y=value, fill=RegOrder)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("B: Regional Primary Bioenergy Net Trade") + theme(plot.title = element_text(lineheight=20, face="bold")) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, vjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  ylab(expression(paste("Regional Trade,", EJ[Primary],"/yr",""))) +
  xlab("") +
  scale_x_continuous(breaks=seq(2020, 2100, 20)) +
  scale_fill_manual(values=c("forestgreen","greenyellow","navy","dodgerblue","cadetblue1","brown","blueviolet","pink","red"),
                    name ="",
                    breaks=c("Brazil","RLAM","USA","EU","ROECD90","MAF","EAsia","RAsia","REF"),
                    labels=c("Brazil","Rest Lat.Am.","USA","EU","Rest OECD","M. East & Africa","East Asia","Rest Asia","Former USSR")
  ) +
  facet_grid(ScenOrder ~ MODEL, labeller=labeller(MODEL=model_labels, ScenOrder=scen_labels))
FigTrad

lay<-rbind(1,1,1,2,2,2,2) 
FigTradeFull <- grid.arrange(FigProd,FigTrad, layout_matrix=lay)
#
# ---- FIG: VOLUME TRADE ----
BioTradVol=subset(BioTradVol, MODEL %in% BioTradCheck$MODEL)
Trade_BioVol <-ggplot(data=BioTradVol, aes(x=Year, y=TradePrimBiomassVol, colour=REGION, fill=REGION)) + 
  #geom_bar(stat="identity", position="dodge")+
  geom_line(size=0.2)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2100) +
  # Text
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(EJ[Primary],"/yr",""))) +
  xlab("") +
  # Legend                      Brazil      EAsia      EU        MAF      RASIA     REF       RLAM      ROECD90   USA
  scale_colour_manual(values=c("#006400", "#FFB90F", "#0000FF","#8B4513","#838B8B","firebrick1","#00BFFF","#8A2BE2","#000000"), 
                      name ="Regions",
                      breaks=c("EU","USA","ROECD90","Brazil","RLAM","MAF","EAsia","RAsia","REF"),
                      labels=c("EU","USA","Rest OECD","Brazil","Rest Lat.Am.","M. East & Africa","East Asia","Rest Asia","Former USSR")
  ) +
  facet_grid(MODEL ~ ScenOrder, labeller=labeller(ScenOrder = scen_labels,MODEL = model_labels), scales="free_y")
Trade_BioVol
#
# ---- FIG: VALUE TRADE ----
BioPrice=subset(BioPrice, MODEL %in% BioTradCheck$MODEL)
BioPrice$ScenOrder = factor(BioPrice$SCENARIO, levels=c("R3-BASE-0-full","R3-B-hi-full","R3-B-lo-full","R3-B-vlo-full"))

BioPrice1 = subset(BioPrice, VARIABLE=="BiomassDelivered"&SCENARIO=="R3-B-hi-full")
Trade_BioVal <-ggplot(data=BioPrice1, aes(x=Year, y=value, colour=REGION, fill=REGION)) + 
  geom_line(size=0.2)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2100) +
  scale_y_continuous(breaks=seq(-600, 600, 200)) +
  # Text
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(text= element_text(size=7, face="plain")) +
  #ylab( expression(paste("Billion US$/yr",""))) +
  ylab( expression(paste("Billion US","$"[2005],"/yr",""))) +
  xlab("") +
  # Legend                      Brazil      EAsia      EU        MAF      RASIA     REF       RLAM      ROECD90   USA
  scale_colour_manual(values=c("#006400", "#FFB90F", "#0000FF","#8B4513","#838B8B","firebrick1","#00BFFF","#8A2BE2","#000000"), 
                      name ="Regions",
                      breaks=c("EU","USA","ROECD90","Brazil","RLAM","MAF","EAsia","RAsia","REF"),
                      labels=c("EU","USA","Rest OECD","Brazil","Rest Lat.Am.","M. East & Africa","East Asia","Rest Asia","Former USSR")
  ) +
  facet_grid(MODEL ~ ScenOrder, labeller=labeller(ScenOrder = scen_labels,MODEL = model_labels), scale="free_y")
Trade_BioVal


Trade_BioVal <-ggplot() +
  geom_point(data=subset(BioPrice.Norm, VARIABLE=="BiomassMarket"&(Year=="2050"|Year=="2100")&SCENARIO=="R3-B-hi-full")
                      , aes(x=RegOrder ,y=NormPric, colour=ScenOrder, shape=MODEL)) +#,position = position_jitter(w = 0.5, h = 0)) + 
  #geom_line(size=0.2)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), size=0.1, colour="gray") +
  #xlim(2010,2100) +
  #scale_y_continuous(breaks=seq(-600, 600, 200)) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=7, face="plain")) +
  ylab( expression(paste("Billion US","$"[2005],"/yr",""))) +
  xlab("") +
  # scale_colour_manual(values=c("#006400", "#FFB90F", "#0000FF","#8B4513","#838B8B","firebrick1","#00BFFF","#8A2BE2","#000000"), 
  #                     name ="Regions",
  #                     breaks=c("EU","USA","ROECD90","Brazil","RLAM","MAF","EAsia","RAsia","REF"),
  #                     labels=c("EU","USA","Rest OECD","Brazil","Rest Lat.Am.","M. East & Africa","East Asia","Rest Asia","Former USSR")
  # ) +
  scale_shape_manual(values=c(12,1,2,3,4,6,8,10),
                     name="",
                     breaks=c("AIM/CGE","COFFEE","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM","COFFEE","GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","REMIND-MAgPIE")
  ) +
  #facet_grid(Year ~ MODEL, labeller=labeller(RegOrder = region_label), scale="free_y")
  facet_wrap(Year ~ MODEL, nrow = 2, labeller=labeller(MODEL= model_labels,RegOrder = region_label), scale="free_y")
Trade_BioVal

# ---- FIG: PRICE VS. TRADE
PricTrade1=subset(PricTrade, SCENARIO=="R3-B-hi-full"&!(REGION=="ASIA"|REGION=="LAM"|REGION=="OECD90")&(Year=="2050"|Year=="2100"))
PricTrade1$ID <- NULL
PricTrade1=na.omit(PricTrade1)

FigPricTrad <-ggplot() + 
  geom_point(data=PricTrade1,aes(x=TradePrimBiomassVol, y=BioPrice, colour=REGION, shape=MODEL)) + 
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  #xlim(2010,2100) +
  #scale_y_continuous(breaks=seq(-600, 600, 200)) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(text= element_text(size=7, face="plain")) +
  #ylab( expression(paste("Billion US$/yr",""))) +
  # ylab( expression(paste("Billion US","$"[2005],"/yr",""))) +
  # xlab("") +
  # Legend                      Brazil      EAsia      EU        MAF      RASIA     REF       RLAM      ROECD90   USA
  scale_colour_manual(values=c("#006400", "#FFB90F", "#0000FF","#8B4513","#838B8B","firebrick1","#00BFFF","#8A2BE2","#000000"), 
                      name ="Regions",
                      breaks=c("EU","USA","ROECD90","Brazil","RLAM","MAF","EAsia","RAsia","REF"),
                      labels=c("EU","USA","Rest OECD","Brazil","Rest Lat.Am.","M. East & Africa","East Asia","Rest Asia","Former USSR")
  ) +
  scale_shape_manual(values=c(12,1,2,3,4,6,8,10),
                     name="",
                     breaks=c("AIM/CGE","COFFEE","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM","COFFEE","GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","REMIND-MAgPIE")
  ) + 
  facet_grid(MODEL~Year, scales="free_y")
FigPricTrad

#facet_grid(RegOrder ~SCENARIO , labeller=labeller(RegOrder = region_label, SCENARIO = scen_labels))

#
# ---- FIG: EXPORT FRAC ----
BioTradFrac1=subset(BioTradFrac1, MODEL %in% BioTradCheck$MODEL)
ExportFrac <- ggplot() +
  geom_jitter(data=BioTradFrac1, mapping=aes(x=Year, y=value, shape=MODEL, colour=variable), size = 1, width=9, alpha = 0.8) +
  #geom_jitter(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=2075, size=0.1, colour="gray2") +
  coord_cartesian(ylim=c(-1, 1)) +
  scale_y_continuous(breaks=seq(-1,1,0.5)) +
  coord_cartesian(xlim=c(2040,2110)) +
  scale_x_continuous(breaks = seq(2050,2100,50)) +
  # Text
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("Bioenergy Trade Fraction [-]") +
  xlab("") + 
  scale_shape_manual(values=c(12,1,2,3,4,6,8,10),
                     name="",
                     breaks=c("AIM/CGE","COFFEE","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM","COFFEE","GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","REMIND-MAgPIE")
  ) +
  scale_color_manual(values=c("forestgreen","firebrick"),
                      name="",
                      breaks=c("BioExpFrac","BioImpFrac"),
                      labels=c("Dependence on Exports","Dependence on Imports")
  )+
  facet_grid(RegOrder ~SCENARIO , labeller=labeller(RegOrder = region_label, SCENARIO = scen_labels))
ExportFrac
#
# ---- FIG: SECURITY ----
Security1=subset(Security1, MODEL %in% BioTradCheck$MODEL)
SecurityFig <- ggplot() +  
  geom_point(data=Security1, mapping=aes(x=value.x, y=value.y, color=Year, shape=ModelOrder), alpha=0.75) +
  # Limits
  coord_cartesian(ylim=c(0, 1), xlim=c(0, 1)) + 
  scale_y_continuous(breaks=seq(0,1,0.25)) +
  scale_x_continuous(breaks=seq(0,1,0.25))+
  # Text
  theme_bw() +
  theme(text= element_text(size=6, face="bold"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(text= element_text(size=7, face="plain")) +
  ylab("Bioenergy fraction of TPES [-]") +
  xlab("Bioenergy Import Fraction [-]") +
  scale_shape_manual(values=c(19,12,1,2,3,4,6,8,9),
                     name="",
                     breaks=c("Fossil","AIM/CGE","COFFEE","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("Fossil (median)","AIM","COFFEE","GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","REMIND-MAgPIE")
  ) +
  scale_color_manual(values=c("black","forestgreen","red"),
                     name="Year",
                     breaks=c("2010","2050","2100"),
                     labels=c("2010 (Fossil only)","2050","2100")
  ) +
  facet_grid(RegOrder ~ ScenOrder, labeller=labeller(ScenOrder = scen_labels,RegOrder = region_label))
SecurityFig
#
# ---- FIG: S-W DIVERSITY ----
SupplyDiversity <-ggplot() + 
  #geom_point(data=SWDiversity1, mapping=aes(x=SWIndex, y=NetExport, colour=Year, shape=MODEL ),position = position_jitter(w = 0.05, h = 0), alpha=0.75) +
  geom_point(data=SWDiversity1, mapping=aes(x=SWIndex, y=NetExport, colour=Year, shape=MODEL ), alpha=0.75) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(0,2) +
  # Text
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_blank(), legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(text= element_text(size=7, face="plain")) +
  ylab(expression(paste("Global Trade,", EJ[Primary],"/yr",""))) +
  xlab("Diversity of Bioenergy Supply Regions (Shannon-Weiner)") +
  scale_shape_manual(values=c(12,1,2,3,4,6,8,10),
                     name="",
                     breaks=c("AIM/CGE","COFFEE","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM","COFFEE","GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","REMIND-MAgPIE")
  ) +
  scale_color_manual(values=c("forestgreen","red"),
                     name="Year",
                     breaks=c("2050","2100"),
                     labels=c("2050","2100")
  ) +
  facet_grid( ~ ScenOrder, labeller=labeller(ScenOrder = scen_labels,MODEL = model_labels))
SupplyDiversity
#
# ---- FIG: SECURITY ACROSS BUDGETS ----
SecurityCompare=subset(SecurityCompare, MODEL %in% BioTradCheck$MODEL)
SecurityCompare=subset(SecurityCompare, SCENARIO=="Fossil"|SCENARIO=="R3-BASE-0-full"|SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-vlo-full")
SecurityCompare$ScenNames = scen_labels[SecurityCompare$SCENARIO]
SecurityCompare$ScenOrder2  = factor(SecurityCompare$ScenNames, levels=c("Baseline","Budget1600","Budget1000","Budget400","Fossil (2010)"))
#SecurityCompareBox <- ggplot(SecurityCompare, aes(x=ScenOrder, y=Indicator, fill=ScenOrder)) +
#  geom_boxplot(lwd=0.3, outlier.shape = 1) +
SecurityCompare.Corr =SecurityCompare[!duplicated(SecurityCompare),]
SecurityCompareFig <-ggplot() +
  geom_point(data=SecurityCompare.Corr, mapping=aes(x=ScenNames, y=Indicator, colour=ModelOrder, shape=ModelOrder),position = position_jitter(w = 0.25, h = 0), alpha = 0.75) +
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5), size=0.1, colour="gray") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("Compound Security Indicator") +
  xlab("") +
  scale_shape_manual(values=c(19,12,1,2,3,4,6,8,10),
                     name="",
                     breaks=c("Fossil","AIM/CGE","COFFEE","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("Fossil (2010)","AIM","COFFEE","GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","REMIND-MAgPIE")
  ) +
  scale_color_manual(values=c("black","burlywood4","red","blue","azure4","limegreen","coral2","darkolivegreen","darkorchid1"),
                     name="Year",
                     breaks=c("Fossil","AIM/CGE","COFFEE","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("Fossil (2010)","AIM","COFFEE","GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","REMIND-MAgPIE"),
                     guide=FALSE
  ) +
  facet_grid( RegOrder~Year, labeller=labeller(RegOrder = region_label, ScenOrder= scen_labels))
SecurityCompareFig
#
# ---- FIG: TRADE vs. BUDGET ----
Drivers.1=subset(Drivers, MODEL %in% BioTradCheck$MODEL)
Drivers.1$ModelOrder = factor(Drivers.1$MODEL, levels=c("Fossil","AIM/CGE","BET","COFFEE","DNE21+ V.14","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","MESSAGE-GLOBIOM","POLES EMF33","REMIND-MAGPIE"))
Drivers.1$ScenName <- scen_labels[Drivers.1$SCENARIO]
Drivers.1$ScenOrder2 <- factor(Drivers.1$ScenName, levels=c("Budget1600","Budget1600-NoBECCS","Budget1600-Nofuel","Budget1600-None","Budget1000","Budget400"))

axis <- data.frame(c(-100,300, #Brazil
                     -50,400, #RLAM
                     -200,150, #USA
                     -200,150, #EU
                     -200,150, #ROECD
                     -200,300, #MAF
                     -200,200, #EAsia
                     -200,200, #RAsia
                     -100,100, #REF
                     0,500),   #Global
                   c("Min","Max","Min","Max","Min","Max","Min","Max","Min","Max","Min","Max","Min","Max","Min","Max","Min","Max","Min","Max"))
colnames(axis)<-c("CumNetTrade","MODEL")
axis$ModelOrder=axis$MODEL
axis$SCENARIO<-"Budget1600"
axis$ScenID=axis$SCENARIO
axis$ScenOrder=axis$SCENARIO
axis$ScenOrder2=axis$SCENARIO
axis$variable<-c("Brazil","Brazil","RLAM","RLAM","USA","USA","EU","EU","ROECD90","ROECD90","MAF","MAF","EAsia","EAsia","RAsia","RAsia","REF","REF","NetTrade","NetTrade")
axis$RegOrder=factor(axis$variable, levels=c("Brazil","RLAM","USA","EU","ROECD90","MAF","EAsia","RAsia","REF","NetTrade"))

Glob1 <- unique(Drivers.1[,c("ScenOrder2","RegOrder")])
Glob1= subset(Glob1, RegOrder=="NetTrade")
#Glob1$CumNetTrade <-Glob1$ScenOrder <- 1
#Glob1$ModelOrder <- NULL

TradeComparePoint <- ggplot() +
  #geom_blank(data=axis, mapping=aes(x=ScenOrder2, y=CumNetTrade)) +
  geom_rect(data=Glob1, aes(fill = RegOrder),
            xmin = as.numeric(Glob1$ScenOrder2[[1]]) -1,
            xmax = as.numeric(Glob1$ScenOrder2[[6]]) +1,
            ymin = -Inf, ymax = Inf, alpha=0.04) +
  geom_point(data=Drivers.1, mapping=aes(x=ScenOrder2, y=CumNetTrade,colour=ModelOrder, shape=ModelOrder),position = position_jitter(w = 0.25, h = 0), alpha = 0.75) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5), size=0.1, colour="gray") +
  scale_y_continuous(breaks=seq(-300,500,100)) +
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("Cumulative BioenergyTrade", EJ[Primary],"/yr (2010-2100)",""))) +
  xlab("") +
  scale_shape_manual(values=c(12,1,2,3,4,6,8,10,0,0),
                     name="",
                     breaks=c("AIM/CGE","COFFEE","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE","Min","Max"),
                     labels=c("AIM","COFFEE","GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","REMIND-MAgPIE","","")
  ) +
  scale_color_manual(values=c("burlywood4","red","blue","azure4","limegreen","coral2","darkolivegreen","darkorchid1","white","white"),
                     name="Year",
                     breaks=c("AIM/CGE","COFFEE","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE","Min","Max"),
                     labels=c("AIM","COFFEE","GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","REMIND-MAgPIE","",""),
                     guide=FALSE
  ) +
  scale_fill_manual(values="red",name="",breaks="NetTrade",labels="Global",guide=FALSE) +
  theme(legend.position="bottom") +
  facet_wrap(~RegOrder, nrow=2, labeller=labeller(RegOrder = region_label), scale="free_y")
TradeComparePoint

TradeCompareBox <- ggplot(Drivers, aes(x=ScenOrder, y=CumNetTrade, fill=ScenOrder)) +
  geom_boxplot(lwd=0.3, outlier.shape = 1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("Cumulative BioenergyTrade", EJ[Primary],"/yr (2010-2100)",""))) +
  xlab("") +
  scale_fill_manual(values=c("green",
                             "blue",
                             "mediumorchid4", 
                             "black",
                             "forestgreen",
                             "mediumorchid1"),
                    name  ="",
                    breaks=c("hi-full",
                             "hi-nobeccs",
                             "hi-nofuel",
                             "hi-none",
                             "lo-full",
                             "vlo-full"),
                    labels=c("Hi Full",
                             "Hi NoBECCS",
                             "Hi NoFuel",
                             "Hi None",
                             "Lo",
                             "Vlo")
  ) +
  facet_wrap(~RegOrder,nrow=2, labeller=labeller(RegOrder = region_label))
TradeCompareBox

# ---- FIG: FOSSIL+BIO TRADE ----
# Bio and Fossil energy trade
EneTradeAll = subset(Trade2, select=c(MODEL,SCENARIO,Year,REGION,TradePrimBiomassVol,TradePrimCoalVol,TradePrimGasVol,TradePrimOilVol))
EneTradeAll = subset(EneTradeAll, SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-lo-full"|SCENARIO=="R3-B-vlo-full"|SCENARIO=="R3-BASE-0-full")
EneTradeAll = subset(EneTradeAll, !(REGION=="OECD90"|REGION=="LAM"|REGION=="ASIA"))
EneTradeAll = melt(EneTradeAll, measure.vars = c("TradePrimBiomassVol","TradePrimCoalVol","TradePrimGasVol","TradePrimOilVol"))
EneTradeAll = subset(EneTradeAll, MODEL=="AIM/CGE"|MODEL=="COFFEE"|MODEL=="GCAM_EMF33"|MODEL=="GRAPE-15"|MODEL=="IMACLIM-NLU"|MODEL=="IMAGE"|MODEL=="POLES EMF33"|MODEL=="REMIND-MAGPIE") 
# Correct IMACLIM mistake
EneTradeAll$value[EneTradeAll$MODEL=="IMACLIM-NLU"&EneTradeAll$variable=="TradePrimCoalVol"] <- EneTradeAll$value[EneTradeAll$MODEL=="IMACLIM-NLU"&EneTradeAll$variable=="TradePrimCoalVol"] *-1
EneTradeAll$value[EneTradeAll$MODEL=="IMACLIM-NLU"&EneTradeAll$variable=="TradePrimGasVol"] <- EneTradeAll$value[EneTradeAll$MODEL=="IMACLIM-NLU"&EneTradeAll$variable=="TradePrimGasVol"] *-1
EneTradeAll$value[EneTradeAll$MODEL=="IMACLIM-NLU"&EneTradeAll$variable=="TradePrimOilVol"] <- EneTradeAll$value[EneTradeAll$MODEL=="IMACLIM-NLU"&EneTradeAll$variable=="TradePrimOilVol"] *-1
#Get Gross Global Numbers
EneTradeAll1=EneTradeAll
EneTradeAll1$value[EneTradeAll1$value<0] <-0
EneTradeAll1 = spread(EneTradeAll1,REGION,value )
EneTradeAll1 = EneTradeAll1 %>% mutate(Global=Brazil+EAsia+EU+MAF+RAsia+REF+RLAM+ROECD90+USA)
EneTradeAll1[,5:13] <-NULL
EneTradeAll1 = melt(EneTradeAll1, id.vars = c("MODEL","SCENARIO","Year","variable"),  measure_vars="Global", variable_name="REGION")

EneTradeAll = rbind(EneTradeAll,EneTradeAll1)
EneTradeAll$RegOrder = factor(EneTradeAll$REGION, levels=c('Brazil','RLAM','USA','EU','ROECD90',"MAF","EAsia","RAsia","REF","Global")) 


EneTradeAll.Hi = subset(EneTradeAll, SCENARIO=="R3-B-hi-full")
EneTradeAll.Lo = subset(EneTradeAll, SCENARIO=="R3-B-lo-full")

Glob <- unique(EneTradeAll[,c("MODEL","RegOrder")])
Glob$Year <- Glob$value <-1

FFandBioHi <- ggplot(data=EneTradeAll.Hi, mapping=aes(x=Year, y=value, fill=variable)) +
  geom_rect(data=subset(Glob, RegOrder=="Global"),aes(fill = RegOrder), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("Regional Trade,", EJ[Primary],"/yr",""))) +
  xlab("") +
  xlim(2010,2100) +
  scale_fill_manual(values=c("red","forestgreen","Black","grey36","grey"),
                    name="",
                    breaks=c("TradePrimBiomassVol","TradePrimCoalVol","TradePrimGasVol","TradePrimOilVol"),
                    labels=c("Bioenergy","Coal","Naturel Gas","Oil")
  ) +
  facet_grid(MODEL ~ RegOrder, labeller=labeller(MODEL=model_labels, RegOrder=region_label), scales="free_y")
FFandBioHi


FFandBioLo <- ggplot(data=EneTradeAll.Lo, mapping=aes(x=Year, y=value, fill=variable)) +
  geom_rect(data=subset(Glob, RegOrder=="Global"),aes(fill = RegOrder), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("Regional Trade,", EJ[Primary],"/yr",""))) +
  xlab("") +
  xlim(2010,2100) +
  scale_fill_manual(values=c("red","forestgreen","Black","grey36","grey"),
                     name="",
                     breaks=c("TradePrimBiomassVol","TradePrimCoalVol","TradePrimGasVol","TradePrimOilVol"),
                     labels=c("Bioenergy","Coal","Naturel Gas","Oil")
  ) +
  facet_grid(MODEL ~ RegOrder, labeller=labeller(MODEL=model_labels, RegOrder=region_label), scales="free_y")
FFandBioLo

#
# ---- FIG: MAP ----
# 1. Map of exporters/ importers. Exporters=green, Importers=red, gradient = # of agreeing models
# 2. Mapg of trade volume importance. Median trade volume of agreeing models, as a % of total trade 
# Map of Importers / Exporters / No Agreement
l=0
for(i in unique(ModelAgree$SCENARIO))
  { for(j in c(2060,2100))
  {
l=l+1
ModelAgree.1=subset(ModelAgree, SCENARIO==i&Year==j)
map.EMF=read.csv("data/Trade/MapEMF.csv", sep=",", dec=".", stringsAsFactors = FALSE)
map.EMF1=map.EMF
map.EMF1$BioExporter <- ModelAgree.1[match(map.EMF1$EMFRegion,ModelAgree.1$REGION),4]
map.EMF1$ModelFraction <- ModelAgree.1[match(map.EMF1$EMFRegion,ModelAgree.1$REGION),9]
# Map of How important Imports/Exports are
ModelAgree.ModelExpMed1=subset(ModelAgree.ModelExpMed, SCENARIO==i&Year==j)
map.EMF.Exp=subset(map.EMF1, BioExporter==1|BioExporter==0)
map.EMF.Exp$MedTradFrac <- ModelAgree.ModelExpMed1[match(map.EMF.Exp$EMFRegion,ModelAgree.ModelExpMed1$REGION),5]
ModelAgree.ModelImpMed1=subset(ModelAgree.ModelImpMed, SCENARIO==i&Year==j)
map.EMF.Imp=subset(map.EMF1, BioExporter==-1)
map.EMF.Imp$MedTradFrac <- ModelAgree.ModelImpMed1[match(map.EMF.Imp$EMFRegion,ModelAgree.ModelImpMed1$REGION),5]

map.EMF2=rbind(map.EMF.Exp,map.EMF.Imp)
map.EMF2$MedTradFrac[map.EMF2$ModelFraction=="No Agreement"] <- 0
# Categorize Importance
map.EMF2$MedTradFracRound = round(map.EMF2$MedTradFrac, digits=-1)

MapTradFrac <- ggplot() +
  geom_polygon(data =map.EMF2, aes(x=long, y = lat, group=group,fill=MedTradFracRound), color="grey40", size = 0.1, alpha=0.9) + 
  #geom_map(data=map.EMF, map=map.EMF, aes(map_id=EMFRegion, x=long, y=lat, fill=ModelFraction))
  coord_fixed(1) +
  theme(title = element_text(size=7)) +
  ggtitle(paste(scen_labels[i],"in",j,sep=" ")) +
  theme_bw() +
  theme(text= element_text(size=7, face="plain")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border = element_blank()) +
  theme(axis.text = element_blank(), axis.ticks =element_blank(), axis.title = element_blank()) +
  theme(legend.position="bottom", legend.text = element_text(angle=0, size=7), legend.direction="horizontal") +
  scale_fill_gradient2(low="firebrick", high="forestgreen", mid="gainsboro", 
                       midpoint=0, space="Lab",
                       limits=c(-50,50),
                      guide_legend(title="Portion of Global Trade (%)"),
                      guide=FALSE)
MapTradFrac

assign(paste("MapTradFrac",i,j,sep="_"),MapTradFrac)
}
}

legend <- ggplot() +
  geom_polygon(data =map.EMF2, aes(x=long, y = lat, group=group,fill=MedTradFracRound), color="white", size = 0.1, alpha=0) + 
  coord_fixed(0) +
  theme(text= element_text(size=7, face="plain")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border = element_blank()) +
  theme(axis.text = element_blank(), axis.ticks =element_blank(), axis.title = element_blank()) +
  theme(legend.position="bottom", legend.text = element_text(angle=0, size=7), legend.direction="horizontal") +
  scale_fill_gradient2(low="firebrick", high="forestgreen", mid="gainsboro", 
                       midpoint=0, space="Lab",
                       limits=c(-50,50),
                       guide_legend(title="Portion of Global Trade (%)"))
legend

`MapTradFrac_R3-BASE-0-full_2060`
`MapTradFrac_R3-B-hi-full_2060`
`MapTradFrac_R3-B-lo-full_2060`
`MapTradFrac_R3-B-vlo-full_2060`

`MapTradFrac_R3-BASE-0-full_2100`
`MapTradFrac_R3-B-hi-full_2100`
`MapTradFrac_R3-B-lo-full_2100`
`MapTradFrac_R3-B-vlo-full_2100`

lay<-rbind(c(1,2),c(3,4),c(5,5)) 
MapTradFracAll <- grid.arrange(`MapTradFrac_R3-B-hi-full_2060`,`MapTradFrac_R3-B-hi-full_2100`,
                               `MapTradFrac_R3-B-lo-full_2060`,`MapTradFrac_R3-B-lo-full_2100`,
                               legend,
                               layout_matrix=lay)


MapImpExp <- ggplot() +
  geom_polygon(data =map.EMF2, aes(x=long, y = lat, group=group,fill=ModelFraction), color="grey40", size = 0.1, alpha=0.9) + 
  #geom_map(data=map.EMF, map=map.EMF, aes(map_id=EMFRegion, x=long, y=lat, fill=ModelFraction))
  coord_fixed(1) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border = element_blank()) +
  theme(axis.text = element_blank(), axis.ticks =element_blank(), axis.title = element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.text = element_text(angle=0, size=6), legend.direction="vertical") +
  # scale_fill_gradientn(colours = c("red","white","white","white","forestgreen"),
  #                     breaks = c(-100,-66, 0,66, 100),
  #                     labels = c("100% (Import)","","No agreement","","100% (Export)"),
  #                     guide_legend(title="Agreement Across Models")) +
  scale_fill_manual(values=c("forestgreen", "firebrick", "gainsboro"),
                    name ="Agreement Across Models",
                    breaks=c(">66% Export",">66% Import","No Agreement"),
                    labels=c("Agreement as Exporter","Agreement and Importer","No Agreement")
  )
MapImpExp

png("output/BioTrade/MapImpExp.png", width=8*ppi, height=6*ppi, res=ppi)
print(plot(MapImpExp))
dev.off()
 
#
# ---- EXPORT FRAC FOR DEMAND X-CUT ----
BioTradFracRCP = subset(Trade3, REGION=="OECD90"|REGION=="REF"|REGION=="ASIA"|REGION=="MAF"|REGION=="LAM")
BioTradFracRCP = subset(BioTradFracRCP, Year=="2050"|Year=="2100")
BioTradFracRCP=subset(BioTradFracRCP,!is.na(BioTradFracRCP$BioExpFrac)&!is.na(BioTradFracRCP$BioImpFrac))
BioTradFracRCP= subset(BioTradFracRCP,!is.na(BioTradFracRCP$BioExpFrac)&!is.na(BioTradFracRCP$BioImpFrac))
BioTradFracRCP = subset(BioTradFracRCP, select=-c(TradePrimBiomassVol,TradePrimBiomassVal,BioExporter,FFExporter,TradeFF,BioImpDep,BioDep,FFImpDep,FFDep))
BioTradFracRCP <- melt(BioTradFracRCP, measure.vars=c('BioExpFrac', 'BioImpFrac'))  
BioTradFracRCP = subset(BioTradFracRCP, !(value==0))
BioTradFracRCP$value[BioTradFracRCP$value > 1] <- 1 
BioTradFracRCP$value[BioTradFracRCP$value < -1] <- -1 
BioTradFracRCP1 = subset(BioTradFracRCP, SCENARIO=="R3-BASE-0-full"|SCENARIO=="R3-B-lo-full")
BioTradFracRCP1$ScenOrder = factor(BioTradFracRCP1$SCENARIO, levels=c('R3-BASE-0-full','R3-B-lo-full')) 

BioTradFracRCP1=subset(BioTradFracRCP1, MODEL %in% BioTradCheck$MODEL)
ExportFracRCP <- ggplot() +
  geom_jitter(data=BioTradFracRCP1, mapping=aes(x=Year, y=value, shape=MODEL, colour=variable), size = 1.5, width=7.5) +
  #geom_jitter(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  coord_cartesian(ylim=c(-1, 1)) +
  scale_y_continuous(breaks=seq(-1,1,0.5)) +
  coord_cartesian(xlim=c(2040,2110)) +
  scale_x_continuous(breaks = seq(2050,2100,50)) +
  # Text
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("Bioenergy Trade Fraction [-]") +
  xlab("") + 
  scale_shape_manual(values=c(12,1,2,3,4,6,8,10),
                     name="",
                     breaks=c("AIM/CGE","COFFEE","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE"),
                     labels=c("AIM","COFFEE","GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","REMIND-MAgPIE")
  ) +
  scale_color_manual(values=c("forestgreen","firebrick"),
                     name="",
                     breaks=c("BioExpFrac","BioImpFrac"),
                     labels=c("Dependence on Exports","Dependence on Imports")
  )+
  facet_grid(REGION ~ScenOrder , labeller=labeller(ScenOrder = scen_labels))
ExportFracRCP


# ---- OUTPUTS FOR PAPER----
# tiff(file = "output/BioTrade/Fig1.tiff", width = 6*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(FigTradeFull)
# dev.off()
# 
# tiff("output/BioTrade/Fig2.tiff", width=4.9*ppi, height=5*ppi, res=ppi)
# plot(MapTradFracAll)
# dev.off()
# 
# tiff("output/BioTrade/Fig3.tiff", width=6*ppi, height=8*ppi, res=ppi)
# plot(SecurityFig)
# dev.off()
# 
# tiff("output/BioTrade/Fig4.tiff", width=6.5*ppi, height=3*ppi, res=ppi)
# plot(SupplyDiversity)
# dev.off()

# ---- OTHER OUTPUTS FOR PAPER----
# png("output/BioTrade/BioProdTrade.png", width=6*ppi, height=8*ppi, res=ppi)
# print(plot(FigTradeFull))
# dev.off()
# 
# png("output/BioTrade/BioTradeVolume.png", width=6*ppi, height=8*ppi, res=ppi)
# print(plot(Trade_BioVol))
# dev.off()
# 
# png("output/BioTrade/BioTradeValue2.png", width=8*ppi, height=5*ppi, res=ppi)
# print(plot(Trade_BioVal))
# dev.off()
# 
# png("output/BioTrade/MapTradFrac.jpg", width=4.9*ppi, height=5*ppi, res=ppi)
# print(plot(MapTradFracAll))
# dev.off()
# 
# png("output/BioTrade/BioExportFrac.png", width=5.5*ppi, height=8*ppi, res=ppi)
# print(plot(ExportFrac))
# dev.off()
# 
# png("output/BioTrade/Security.png", width=6*ppi, height=8*ppi, res=ppi)
# print(plot(SecurityFig))
# dev.off()
# 
# png("output/BioTrade/SupplyDiversity.png", width=6.5*ppi, height=3*ppi, res=ppi)
# print(plot(SupplyDiversity))
# dev.off()
# 
# png("output/BioTrade/SecurityCompare.png", width=5*ppi, height=8*ppi, res=ppi)
# print(plot(SecurityCompareFig))
# dev.off()
# 
# png("output/BioTrade/TradeCompare.png", width=7*ppi, height=5*ppi, res=ppi)
# print(plot(TradeComparePoint))
# dev.off()
# 
# png("output/BioTrade/BioExportFracRCP.png", width=5*ppi, height=8*ppi, res=ppi)
# print(plot(ExportFracRCP))
# dev.off()
# 
# png("output/BioTrade/Carriers.png", width=8*ppi, height=8*ppi, res=ppi)
# print(plot(Carriers1))
# dev.off()
# 
# png("output/BioTrade/FFandBioHi.png", width=8*ppi, height=8*ppi, res=ppi)
# print(plot(FFandBioHi))
# dev.off()
# 
# png("output/BioTrade/FFandBioLo.png", width=8*ppi, height=8*ppi, res=ppi)
# print(plot(FFandBioLo))
# dev.off()
# 
# 
# write.xlsx(DiversityTest.TtestDF, file="output/BioTrade/Statistics.xlsx", sheetName="Diversity of Supply", row.names=TRUE, showNA = TRUE)
# write.xlsx(DriversTech.Ttest, file="output/BioTrade/Statistics.xlsx", sheetName="Drivers of Trade (Technology)", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(DriversBudg.Ttest, file="output/BioTrade/Statistics.xlsx", sheetName="Drivers of Trade (Budget)", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(SecurityTest.Ttest, file="output/BioTrade/Statistics.xlsx", sheetName="Security Indicator", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(EneTradeMed, file="output/BioTrade/Statistics.xlsx", sheetName="Medians", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(EneTradeGr, file="output/BioTrade/Statistics.xlsx", sheetName="Growth Rates", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(BiomassTrade, file="output/BioTrade/Statistics.xlsx", sheetName="Biomass Trade Summary", append=TRUE, row.names=FALSE, showNA = TRUE)

#
# # ---- EXTRA RESULT FOR BRAZIL PAPER ----
# BRTrade = subset(Trade2, REGION=="Brazil")
# BRTrade = subset(BRTrade, SCENARIO=="R3-B-hi-full"|
#                    SCENARIO=="R3-B-hi-nobeccs"|
#                    SCENARIO=="R3-B-hi-nofuel"|
#                    SCENARIO=="R3-B-lo-full"|
#                    SCENARIO=="R3-BASE-0-full")
# BRTrade=subset(BRTrade, select=-c(PrimFossil,TradePrimBiomassVal,TradePrimBiomassVol,TradePrimCoalVal,TradePrimCoalVol,TradePrimGasVal,TradePrimGasVol,TradePrimOilVal,TradePrimOilVol,
#                                   TradeSecLiquidsBiomassVol,TradeSecSolidsBiomassVol,FFExporter,TradeFF,FFImpDep,FFDep,
#                                   BioDep,BioImpDep))
# write.xlsx(BRTrade, file="output/Brazil/BRTrade.xlsx", sheetName="BrazilTrade", row.names=TRUE, showNA = TRUE)

# ---- END ----
# # Delete excess variables
# rm(FossilDepMed,FossilDepMedx,FossilDepMedy,DiversityStat,TestScenLabels,SecurityCompare1,Fossil)
# rm(Drivers1,Drivers2)
# rm(DriversTech1,DriversTech1.fit,DriversTech1.Tuk,DriversTech1.result)
# rm(DriversTech2,DriversTech2.Ttest,DriversTech2.TtestDF)
# rm(DriversBudg1,DriversBudg1.fit,DriversBudg1.Tuk,DriversBudg1.result)
# rm(DriversBudg2,DriversBudg2.Ttest,DriversBudg2.TtestDF)
# rm(SecurityTest1,SecurityTest1.fit,SecurityTest1.fit2,SecurityTest1.result)
# rm(SecurityTest2,SecurityTest2.Ttest,SecurityTest2.TtestDF)
# rm(EneTrade,EneTradeMed.Scen)
# rm(DiversityTest1.fit,DiversityTest1.fit2,DiversityTest1.result)
# rm(names,names2,i,l)

# ---- DISABLED ----
# TRADE FIGURE - EXPORT FRACTION (DISABLED)
# ExportFrac <- ggplot() +
#   geom_point(data=TradeDep, mapping=aes(x=SCENARIO, y=value, shape=MODEL, color=VARIABLE), size = 1) +
#   coord_cartesian(ylim=c(-1, 1)) + 
#   scale_y_continuous(breaks=seq(-1,1,0.5)) +
#   # Text
#   theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab("Biomass Trade Fraction [-]") +
#   xlab("") + 
#   scale_shape_manual(values=c(1,2,3,4,6,8),
#                      name="",
#                      breaks=c("GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33","REMIND-MAGPIE"),
#                      labels=c("GCAM","GRAPE-15","IMACLIM","IMAGE","POLES","ReMIND-MAgPIE")
#   ) + 
#  scale_color_manual(values=c("forestgreen","firebrick"),
#                     name="",
#                     breaks=c("Biomass Export Fraction","Biomass Import Fraction"),
#                     labels=c("Dependence on Exports","Dependence on Imports")
#                     )+
#   scale_x_discrete(labels=scen_labels) +
#   facet_grid(REGION ~Year , labeller=labeller(REGION = region_label))
# ExportFrac

# TRADE FIGURE - LOCAL CONSUMPTION AND TRADE (DISABLED)
# BioTradeDriver=BioImpExp
# BioTradeDriver$ModelID <- paste(BioTradeDriver$SCENARIO,BioTradeDriver$Year) 
# BioTradeDriver$ModelID=substr(BioTradeDriver$ModelID, start=6, stop=40)
# 
# TradeDriver <- ggplot(data=BioTradeDriver, mapping=aes(x=ModelID, y=value, fill=VARIABLE)) +
#   #geom_point(data=BioImpExp, mapping=aes(x=SCENARIO, y=value, shape=MODEL, color=VARIABLE), size = 1) +
#   geom_bar(stat="identity") +
#   geom_hline(yintercept=0,size = 0.1, colour='black') +
#   #coord_cartesian(ylim=c(-1, 1)) + 
#   #scale_y_continuous(breaks=seq(-1,1,0.5)) +
#   # Text
#   theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   ylab(expression(paste(EJ[Primary],"/yr",""))) +
#   xlab("") + 
#   scale_color_manual(values=c("firebrick","forestgreen"),
#                      name="",
#                      breaks=c("Primary Energy|Biomass|Modern","Trade|Primay Energy|Biomass|Volume"),
#                      labels=c("Bioenergy Production","BioEnergy Traded")
#   )+
#   #scale_x_discrete(labels=scen_labels)
#   facet_grid(REGION ~ MODEL , labeller=labeller(REGION = region_label, MODEL=model_labels))
# TradeDriver
