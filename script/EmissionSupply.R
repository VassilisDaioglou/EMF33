# AIM:
# R script that reads in EMF33 Supply Scenarios (R3) data to produce multi-model biomass emission-supply curves 
# 
# METHOD:
# Use EMF-33 senarios with linearly increased (modern) biomass demand from 0 EJ/yr to 100/200/300/400 EJ/yr by 2100
# For each of these scenarios we have the annual LUC emissions. By substracting the LUC emission of a 
# counterfactual scenario (R3B0), we can get the LUC emissions associated with increased biomass demand
#
# By determining the cumulative (2010-2100) biomass production and LUC emissions, we can get an aggregate emission 
# factor (kgCO2/GJ-prim) for biomass production for each scenario and model. If we determine these emission factors 
# for each scenario, we can then draw and emission-factor supply curve for each model
#
# We do this for the biomass supply projections till 2050. In the EMF-33 scenarios, this supply comes from both 
# residues and energy crops, however we are interested only in the latter. Resultsing energy crop supply in 2050
# is inconsistent across models/scenarios, so we round to the nearest 50EJ. This also allows us to draw boxplots
# of EFs across supply levels:
# x-axis: 0, 50, 100, 150, 200 EJ/yr 
# y-axis: Aggregate emission factor (based on cumulative LUC emissions divided by cumulative biomass production)
#
# We supplyment this with EF calculations from partial (land and crop growth) models 
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
  dataframe = dataframe %>% mutate(Cumulative = x2010 + x2020 + x2030 + x2040 + x2050)
  dataframe = subset(dataframe, select = -c(x2010,x2020,x2030,x2040,x2050))
  dataframe 
}

# --- MAKE RELEVANT DATASETS (EMF-33) ----
# ---- *** Land Use Emissions *** ----
Emis = subset(DATA, VARIABLE == "Emissions|CO2|Land Use")
Emis.temp = clean.data(Emis)
Emis.cum = get.cumulative(Emis.temp)
Emis.cum$ID = paste(Emis.cum$MODEL, Emis.cum$REGION, Emis.cum$SCENARIO)
rm(Emis.temp)
# 
# ---- *** Primary Biomass Production *** ----
# Get cumulative production for EF calculation
Prim = subset(DATA, VARIABLE == "Primary Energy|Biomass|Modern")
Prim.temp = clean.data(Prim)
Prim.cum = get.cumulative(Prim.temp)
Prim.cum$ID = paste(Prim.cum$MODEL, Prim.cum$REGION, Prim.cum$SCENARIO)
# Get production level in 2050 for figure drawing
Prim.2050 = subset(Prim.temp, Year == 2050)
Prim.2050$ID = paste(Prim.2050$MODEL, Prim.2050$REGION, Prim.2050$SCENARIO)
rm(Prim.temp)
# 
# ---- *** Emission Factor Dataframe *** ----
# Combine data
EmisFac = Emis.cum
EmisFac$CumulativeEC = Prim.cum[match(EmisFac$ID,Prim.cum$ID),"Cumulative"]
EmisFac$PrimEC_2050 = Prim.2050[match(EmisFac$ID,Prim.2050$ID),"value"]

rm(Prim.2050, Prim.cum, Emis.cum)
# EF calculation
EmisFac = EmisFac %>% mutate(EF_PrimEC = Cumulative / CumulativeEC)
EmisFac$UNIT <- "kgCO2/GJ"

EmisFac = subset(EmisFac, select=-c(VARIABLE, Cumulative, ID, PrimBio, PrimEC))

# Ascending order 
EmisFac$EFOrder = factor(EmisFac$SCENARIO, levels = c("Bio100","Bio200","Bio300","Bio300LP","Bio400"))
EmisFac$EFOrder = gsub( "Bio", "", EmisFac$EFOrder, fixed = F)

# Categorise 2050 production
bin_width = 50
EmisFac$PrimEC_2050_bin <- round(EmisFac$PrimEC_2050/(bin_width/10),-1)*(bin_width/10)
EmisFac$PrimEC_2050_bin = factor(EmisFac$PrimEC_2050_bin, levels = c("0", "50", "100", "150", "200"))

# clean
EmisFac <- EmisFac[!is.infinite(EmisFac$EF_PrimEC),]

# ---- *** Emission Factor min/max *** ----
EMF33_range <- aggregate(EmisFac$EF_PrimEC, by=list(PrimEC_2050_bin=EmisFac$PrimEC_2050_bin, REGION=EmisFac$REGION), FUN=min, na.rm=TRUE) 
colnames(EMF33_range)[1] <- "Pot_bin"
colnames(EMF33_range)[3]<- "Min"
EMF33_range$ID = paste(EMF33_range$Pot_bin, EMF33_range$REGION)

Max <- aggregate(EmisFac$EF_PrimEC, by=list(PrimEC_2050_bin=EmisFac$PrimEC_2050_bin, REGION=EmisFac$REGION), FUN=max, na.rm=TRUE) 
Max$ID = paste(Max$PrimEC_2050_bin, Max$REGION)

EMF33_range$Max = Max[match(EMF33_range$ID,Max$ID),"x"]
EMF33_range$label <- "EMF-33"
EMF33_range <- EMF33_range[!is.infinite(EMF33_range$Min),]
EMF33_range <- EMF33_range[!is.infinite(EMF33_range$Max),]
EMF33_range$ID <- NULL
rm(Max)
# 
# ---- LITERATURE DATA ----
daioglou=read.xlsx("GitHub/EMF33/data/EmissionSupply/Lit_EF.xlsx", sheetName="Daioglou_2017", startRow=1)
kalt = read.xlsx("GitHub/EMF33/data/EmissionSupply/Lit_EF.xlsx", sheetName="Kalt_2020", startRow=1)
bin_width2 = 50

daioglou <- na.omit(daioglou)
daioglou$Pot_ExclAg_30yr = round(daioglou$Pot_ExclAg_30yr,0)
daioglou$Pot_bin <- round(daioglou$Pot_ExclAg_30yr/(bin_width2/10),-1)*(bin_width2/10)
daioglou$Ref <- "Daiglou_2017"
daioglou_clean = subset(daioglou, select = c(EF_kgCO2pGJprim, Pot_bin, Ref))

kalt$Pot_bin <- round(kalt$Cum_Pot/(bin_width2/10),-1)*(bin_width2/10)
kalt$Ref <- "Kalt_2020"
kalt_clean = subset(kalt, select = c(EF_kgCO2pGJprim, Pot_bin, Ref))

  # Kalt Misses a value for 100 EJ bin
  missing <- c((max(kalt_clean$EF_kgCO2pGJprim[kalt_clean$Pot_bin==50]) + min(kalt_clean$EF_kgCO2pGJprim[kalt_clean$Pot_bin==150]))/2,
                            100,
                            "Kalt_2020")
  kalt_clean[nrow(kalt_clean)+1,] <- missing[]

# Combined dataset and determine ribbon min/max
Lit = rbind(daioglou_clean,kalt_clean)
Lit$EF_kgCO2pGJprim <- as.numeric(Lit$EF_kgCO2pGJprim)
Lit_range <- aggregate(Lit$EF_kgCO2pGJprim, by=list(Pot_bin=Lit$Pot_bin), FUN=min, na.rm=TRUE) 
colnames(Lit_range)[2]<- "Min"
Max <- aggregate(Lit$EF_kgCO2pGJprim, by=list(Pot_bin=Lit$Pot_bin), FUN=max, na.rm=TRUE) 
Lit_range$Max = Max[match(Lit_range$Pot_bin,Max$Pot_bin),"x"]
Lit_range$label <- "Literature"
Lit_range$REGION <- "WORLD"
rm(daioglou_clean, kalt_clean, Max)
#
# ---- MIN-MAX EFs (Lit + EMF-33) ----
EF_ranges = rbind(EMF33_range,Lit_range)
EF_ranges$Pot_Order = factor(EF_ranges$Pot_bin, levels =c("0","50","100","150","200"))
#
# ---- FIGURES ----
# ---- *** FIG: Literature data - lineplot  ----
Figlit<-ggplot() + 
  geom_line(data = daioglou, aes(x = Pot_ExclAg_30yr, y = EF_kgCO2pGJprim), colour = "red", size = 1) +
  geom_line(data = kalt, aes(x = Cum_Pot, y = EF_kgCO2pGJprim), colour = "blue", size = 1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("kgCO"[2],"/GJ-Prim"))) + 
  xlab("EJ Primary Biomass") 
Figlit
#
# ---- *** FIG: EMF-33 EF-Curve - boxplot  ----
boxplot<-ggplot(data = subset(EmisFac, REGION == "WORLD" & !SCENARIO=="Bio300LP"),
                 aes(x=PrimEC_2050_bin, y = EF_PrimEC)) + 
  geom_boxplot() +
  geom_jitter(aes(colour = MODEL, shape=SCENARIO),
              width=0.1, alpha = 0.9) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("kgCO"[2],"/GJ-Prim"))) + 
  xlab("EJ Primary Biomass") 
boxplot
#
# ---- *** FIG: EMF-33 EF-Curve - lineplot  ----
line<-ggplot(data = subset(EmisFac, REGION == "WORLD" & !SCENARIO=="Bio300LP"),
                aes(x=PrimEC_2050, y = EF_PrimEC,colour = MODEL)) + 
  geom_line() +
  geom_point(aes(shape=SCENARIO)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("kgCO"[2],"/GJ-Prim"))) + 
  xlab("EJ Primary Biomass") 
line
#
# ---- *** FIG: EMF-33 EF (box) + Literature (ribbon)  ----
combined<-ggplot(data = subset(EmisFac, REGION == "WORLD" & !SCENARIO=="Bio300LP"),
                aes(x=PrimEC_2050_bin, y = EFOrder)) + 
  geom_boxplot() +
  geom_jitter(aes(colour=MODEL),
              width=0.2, alpha = 0.9) +
  geom_ribbon(data = subset(EF_ranges, label == "Literature"),
              inherit.aes = FALSE,
              aes(x = factor(Pot_Order), ymin=Min, ymax=Max, group = 1, fill=label), alpha = "0.25") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab(expression(paste("kgCO"[2],"/GJ-Prim"))) + 
  xlab("EJ Primary Biomass")  +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.box = "vertical", legend.title= element_text(face="bold.italic")) +
  scale_color_discrete(name = "EMF-33 IAM:") +
  scale_fill_manual(values="dodgerblue",
                  name="Partial Models:",
                  breaks="Literature",
                  labels="Kalt et al. (2020) & Daioglou et al. (2017)")
combined
#
# ---- *** FIG: EMF-33 EF Literature (double ribbon)  ----
ribboned<-ggplot() + 
  geom_ribbon(data = subset(EF_ranges, REGION == "WORLD"),
              aes(x = Pot_bin, ymin=Min, ymax=Max, group = label, fill=label), alpha = "0.25") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab(expression(paste("kgCO"[2],"/GJ-Prim"))) + 
  xlab("EJ Primary Biomass")  +
  theme_bw() +
  theme(text= element_text(size=5, face="plain"), axis.text.x = element_text(angle=66, size=5, hjust=1), axis.text.y = element_text(size=5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.box = "vertical", legend.title= element_text(face="bold.italic")) +
  scale_fill_manual(values=c("dodgerblue","forestgreen"),
                    name="Model type",
                    breaks=c("Literature","EMF-33"),
  labels=c("Partial models","EMF-33 (IAM)"))
ribboned
#
# ---- OUTPUTS ----
# #
png(file = "GitHub/EMF33/output/EmissionSupply/Boxplot_EMF33.png", width = 4*ppi, height = 3*ppi, units = "px", res = ppi)
plot(boxplot)
dev.off()
# #
png(file = "GitHub/EMF33/output/EmissionSupply/Line_EMF33.png", width = 4*ppi, height = 3*ppi, units = "px", res = ppi)
plot(line)
dev.off()
# #
png(file = "GitHub/EMF33/output/EmissionSupply/Combined.png", width = 4*ppi, height = 4*ppi, units = "px", res = ppi)
plot(combined)
dev.off()
# #
png(file = "GitHub/EMF33/output/EmissionSupply/Ribboned.png", width = 4*ppi, height = 2*ppi, units = "px", res = ppi)
plot(ribboned)
dev.off()
# #
png(file = "GitHub/EMF33/output/EmissionSupply/LineLit.png", width = 4*ppi, height = 4*ppi, units = "px", res = ppi)
plot(Figlit)
dev.off()