# R script to check regional variation for EMF33 Bio-Technologies Paper
# ----START----
# clear memory
rm(list=ls()) 

# Load Libraries
library(reshape2);
library(ggplot2);
#library(ggplot);
library(data.table);
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(xlsx)

# set directory path for csv file
setwd("~/disks/y/ontwapps/Timer/Users/Vassilis/Projects - Documents/EMF33/Scenario results/R-Scripts")
#setwd("C:/Users/vassi/Documents/Work/EMF33/R-Scripts")

# Read Data File
TechDataR=read.csv("data/TechDATA_Reg.csv", sep=",", dec=".", stringsAsFactors = FALSE)
TechDataR$X <- NULL

# ---- CORRECT VARS -----
#Correct Efficiency of renewables (=1)
TechDataR$Efficiency[TechDataR$Prim=="Geothermal"|TechDataR$Prim=="Hydro"|TechDataR$Prim=="Nuclear"|TechDataR$Prim=="Solar"|TechDataR$Prim=="Wind"|TechDataR$Prim=="Ocean"] <- 1

#Correct efficiency for oil based liquids which are NA (set to 1). This is important for IMAGE and IMACLIM which do not report efficiencies
TechDataR$Efficiency[TechDataR$Prim=="Oil" & TechDataR$CarrierID=="Liq" & is.na(TechDataR$Efficiency)] <- 1

# Remove Observations that either make no sense 
TechDataR1=TechDataR
TechDataR1=subset(TechDataR1, !(Efficiency=="NA"))
TechDataR1=subset(TechDataR1, !(CapitalCo=="NA"))
TechDataR1=subset(TechDataR1, Efficiency>0.03)

# Years
TechDataR1 = subset(TechDataR1, Year=="2100")

# Scenarios
TechDataR1 = subset(TechDataR1, SCENARIO=="R3-B-lo-full")

# Energy carriers
TechDataR1 = subset(TechDataR1, !(CarrierID=="Hea"))
TechDataR1 = subset(TechDataR1, Prim=="Biomass"|Prim=="BiomasswCCS")
# Replace NAs with 0
TechDataR1$OMCostFi[is.na(TechDataR1$OMCostFi)] <- 0
TechDataR1$OMCostVa[is.na(TechDataR1$OMCostVa)] <- 0
TechDataR1$SecEn[is.na(TechDataR1$SecEn)] <- 0
TechDataR1$Ctax[is.na(TechDataR1$Ctax)] <- 0

# Do not allow negative values of OMVar (relevant for IMAGE) and Capacity
TechDataR1$OMCostVa[TechDataR1$OMCostVa<0]<-0
TechDataR1$SecEn[TechDataR1$SecEn<0]<-0

# Make sure relevant columns are numeric
TechDataR1$CapitalCo = as.numeric(substr(TechDataR1$CapitalCo, start=1, stop=5))
TechDataR1$Efficiency = as.numeric(substr(TechDataR1$Efficiency, start=1, stop=5))
TechDataR1$OMCostFi = as.numeric(substr(TechDataR1$OMCostFi, start=1, stop=5))
TechDataR1$OMCostVa = as.numeric(substr(TechDataR1$OMCostVa, start=1, stop=5))
TechDataR1$SecEn = as.numeric(substr(TechDataR1$SecEn, start=1, stop=5))
TechDataR1$Ctax = as.numeric(substr(TechDataR1$Ctax, start=1, stop=5))

TechDataR1 = subset(TechDataR1, select=-c(Ctax,CarrierID2,Capt,SecEn,FeedCost))


TechDataR1$Tech = paste(TechDataR1$VARIABLE) 
TechDataR1$Tech <- gsub("Biomass","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("Coal","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("Gas","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("Oil","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("Geothermal","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("Wind","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("Hydro","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("Nuclear","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("SolarPV","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("SolarCSP","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("Onshore","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("Offshore","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("2","",TechDataR1$Tech,fixed=F)
TechDataR1$Tech <- gsub("3","",TechDataR1$Tech,fixed=F)
TechDataR1$Tech <- gsub("4","",TechDataR1$Tech,fixed=F)
TechDataR1$Tech <- gsub("woCCS","",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("gen","Hydrogen",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("HydrogenElectricity","Hydrogen",TechDataR1$Tech,fixed=F) 
TechDataR1$Tech <- gsub("es","Gas",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("LiquidsBiodiGasel","Biodeisel",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("LiquidsCellulosicNondiGasel","Lignocellulosic",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("LiquidsConventionalthanol","1st gen. ethanol",TechDataR1$Tech,fixed=F)  
TechDataR1$Tech <- gsub("LiquidsOther","Other biomass",TechDataR1$Tech,fixed=F)  

TechDataR1$TechOrder = factor(TechDataR1$Tech, levels=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Other biomass","Liquids","Electricity","Hydrogen","1st gen. ethanolwCCS","BiodeiselwCCS","LignocellulosicwCCS","Other biomasswCCS","LiquidswCCS","ElectricitywCCS","HydrogenwCCS","Gas"))
TechDataR1$Tech2 <- gsub("wCCS","",TechDataR1$Tech,fixed=F)  
TechDataR1$TechOrder2 = factor(TechDataR1$Tech2, levels=c("1st gen. ethanol","Biodeisel","Lignocellulosic","Other biomass","Liquids","Electricity","Hydrogen","Gas"))



# ---- COMPARE REGIONAL NUMBERS ----
TechData.LAM = subset(TechDataR1, REGION=="Brazil"|REGION=="LAM")
TechData.LAM$REGIONOrder = factor(TechData.LAM$REGION, levels=c("LAM","Brazil"))
TechData.ASIA = subset(TechDataR1, REGION=="China"|REGION=="India"|REGION=="ASIA")
TechData.ASIA$REGIONOrder = factor(TechData.ASIA$REGION, levels=c("ASIA","China","India"))
TechData.OECD = subset(TechDataR1, REGION=="USA"|REGION=="EU"|REGION=="Japan"|REGION=="OECD90")
TechData.OECD$REGIONOrder = factor(TechData.OECD$REGION, levels=c("OECD90","USA","EU","Japan"))

# ---- LABELS ----

Biotech_labeler <- c("1st gen. ethanol" = "1st Gen. Eth.",
                     "Biodeisel"        = "Biodiesel",
                     "Lignocellulosic"  = "Adv. Biofuel",
                     "Other biomass"    = "Other Biofuel",
                     "Electricity"      = "Bio-Elec.",
                     "Hydrogen"         = "Bio-H2",
                     "1st gen. ethanolwCCS" = "1st Gen. Eth w/CCS",
                     "BiodeiselwCCS"        = "Biodiesel w/CCS",
                     "LignocellulosicwCCS"  = "Adv. Biofuel w/CCS",
                     "Other biomasswCCS"    = "Other Biofuel w/CCS",
                     "ElectricitywCCS"      = "Bio-Elec. w/CCS",
                     "HydrogenwCCS"         = "Bio-H2 w/CCS",
                     "Gas"              = "Bio-Gas")


# ---- Figure LAM ----
LAMEffCost <- ggplot(TechData.LAM)+
  geom_point(aes(x=CapitalCo, y=Efficiency, shape=REGIONOrder, colour=REGIONOrder)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  #ggtitle("Bio-Technologies, Capital Costs vs. Efficiencies") +
  ylim(0,1) +
  xlab("Capital Costs") +
  ylab("Efficiency [-]") +
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  #ylab(expression("%")) + xlab("") +
  scale_colour_manual(values=c("black","red"),
                      name="",
                      breaks=c("LAM","Brazil"),
                      labels=c("LAM", "Brazil")
  ) +
  scale_shape_manual(values=c(2,3),
                     name="",
                     breaks=c("LAM","Brazil"),
                     labels=c("LAM", "Brazil")
  ) +
  facet_grid( MODEL~TechOrder)#, labeller=labeller(TechOrder2 = Biotech_labeler))
LAMEffCost

# ---- Figure ASIA ----
ASIAEffCost <- ggplot(TechData.ASIA)+
  geom_point(aes(x=CapitalCo, y=Efficiency, shape=REGIONOrder, colour=REGIONOrder)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  #ggtitle("Bio-Technologies, Capital Costs vs. Efficiencies") +
  ylim(0,1) +
  xlab("Capital Costs") +
  ylab("Efficiency [-]") +
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  #ylab(expression("%")) + xlab("") +
  scale_colour_manual(values=c("black","red","red2"),
                      name="",
                      breaks=c("ASIA","China","India"),
                      labels=c("ASIA","China","India")
  ) +
  scale_shape_manual(values=c(2,3,4),
                     name="",
                     breaks=c("ASIA","China","India"),
                     labels=c("ASIA","China","India")
  ) +
  facet_grid( MODEL~TechOrder)#, labeller=labeller(TechOrder2 = Biotech_labeler))
ASIAEffCost

# ---- Figure OECD90 ----
OECDEffCost <- ggplot(TechData.OECD)+
  geom_point(aes(x=CapitalCo, y=Efficiency, shape=REGIONOrder, colour=REGIONOrder), alpha=0.75) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  #ggtitle("Bio-Technologies, Capital Costs vs. Efficiencies") +
  ylim(0,1) +
  xlab("Capital Costs") +
  ylab("Efficiency [-]") +
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=90, size=7), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  #ylab(expression("%")) + xlab("") +
  scale_colour_manual(values=c("black","red","red2","red3"),
                      name="",
                      breaks=c("OECD90","USA","EU","Japan"),
                      labels=c("OECD90","USA","EU","Japan")
  ) +
  scale_shape_manual(values=c(2,3,4,1),
                     name="",
                     breaks=c("OECD90","USA","EU","Japan"),
                     labels=c("OECD90","USA","EU","Japan")
  ) +
  facet_grid( MODEL~TechOrder, 
              labeller=labeller(TechOrder=Biotech_labeler, label_wrap_gen(10)))
#labeller = labeller(conservation2 = label_wrap_gen(10))
OECDEffCost


# ---- OUTPUT ----
ppi <- 300
png("output/BioTech/Diagnostic/LAMEffCost.png", width=9*ppi, height=5*ppi, res=ppi)
print(plot(LAMEffCost))
dev.off()

png("output/BioTech/Diagnostic/ASIAEffCost.png", width=9*ppi, height=5*ppi, res=ppi)
print(plot(ASIAEffCost))
dev.off()

png("output/BioTech/Diagnostic/OECDEffCost.png", width=9*ppi, height=5*ppi, res=ppi)
print(plot(OECDEffCost))
dev.off()
