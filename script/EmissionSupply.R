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
library(openxlsx)
library(forcats)

# ---- CONSTANTS ----
ppi <- 300
FSizeTitle = 9
FSizeStrip = 7
FSizeAxis = 7
FSizeLeg = 7

ActYears = c(2010,2020,2030,2040,2050)
# ActPotBins = factor(c(0,25,50,75,100,125,150,175,200))
# ActPotBins = factor(c(0,30,50,60,90,100,120,150,180,200))
ActPotBins = factor(c(0,50,100,150,200))
AllPotBins = c(0,25,30,50,60,75,90,100,120,125,150,175,180,200,210,225,240,250,270,275,300,325,330)
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

  data = subset(data, select = -c(R5B0, R5B100, R5B200, R5B300, R5B400))
  
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
EmisFac$CumPrimBio = Prim.cum[match(EmisFac$ID,Prim.cum$ID),"Cumulative"]
EmisFac$PrimBio_2050 = Prim.2050[match(EmisFac$ID,Prim.2050$ID),"value"]

rm(Prim.2050, Prim.cum, Emis.cum)
# EF calculation
EmisFac = EmisFac %>% mutate(EF_PrimBio = Cumulative / CumPrimBio)
EmisFac$UNIT <- "kgCO2/GJ"

EmisFac = subset(EmisFac, select=-c(VARIABLE, Cumulative, CumPrimBio, ID))

# Ascending order 
EmisFac$EFOrder = factor(EmisFac$SCENARIO, levels = c("Bio100","Bio200","Bio300","Bio300LP","Bio400"))
EmisFac$EFOrder = gsub( "Bio", "", EmisFac$EFOrder, fixed = F)

# Categorise 2050 production
bin_width = 50
EmisFac$Pot_bin <- round(EmisFac$PrimBio_2050/(bin_width/10),-1)*(bin_width/10)
EmisFac$Pot_bin = factor(EmisFac$Pot_bin, levels = c("0", "50", "100", "150", "200"))

# clean
EmisFac <- EmisFac[!is.infinite(EmisFac$EF_PrimBio),]

# ---- *** Emission Factor min/max *** ----
EMF33_range <- aggregate(EmisFac$EF_PrimBio, by=list(Pot_bin=EmisFac$Pot_bin, REGION=EmisFac$REGION), FUN=min, na.rm=TRUE) 
colnames(EMF33_range)[1] <- "Pot_bin"
colnames(EMF33_range)[3]<- "Min"
EMF33_range$ID = paste(EMF33_range$Pot_bin, EMF33_range$REGION)

Max <- aggregate(EmisFac$EF_PrimBio, by=list(Pot_bin=EmisFac$Pot_bin, REGION=EmisFac$REGION), FUN=max, na.rm=TRUE) 
Max$ID = paste(Max$Pot_bin, Max$REGION)

EMF33_range$Max = Max[match(EMF33_range$ID,Max$ID),"x"]
EMF33_range$label <- "EMF-33"
EMF33_range <- EMF33_range[!is.infinite(EMF33_range$Min),]
EMF33_range <- EMF33_range[!is.infinite(EMF33_range$Max),]
EMF33_range$ID <- NULL
EMF33_range$LandCounterfactual <- "IAM Scenario"
rm(Max)
# 
# ---- LITERATURE DATA ----
clean.lit <- function(dataframe,bin,reference){
  dataframe$CumPrimBio = as.numeric(dataframe$CumPrimBio)
  dataframe$CumPrimBio = round(dataframe$CumPrimBio,1)
  dataframe$Pot_bin <- round(dataframe$CumPrimBio/(bin/10),-1)*(bin/10)
  dataframe$Ref <- reference
  dataframe
}

daioglou=read.xlsx("GitHub/EMF33/data/EmissionSupply/Lit_EF.xlsx", sheet="Daioglou_2017", startRow=1)
kalt = read.xlsx("GitHub/EMF33/data/EmissionSupply/Lit_EF.xlsx", sheet="Kalt_2020", startRow=1)
bin_width2 = 50

daioglou = clean.lit(daioglou,bin_width2,"Daioglou_2017")
kalt = clean.lit(kalt,bin_width2,"Kalt_2020")

# Combined dataset and determine ribbon min/max
Lit = rbind(daioglou,kalt)
Lit$EF_kgCO2pGJprim <- as.numeric(Lit$EF_kgCO2pGJprim)
Lit$Pot_bin = factor(Lit$Pot_bin, levels = AllPotBins)

Lit_range <- aggregate(Lit$EF_kgCO2pGJprim, by=list(Pot_bin=Lit$Pot_bin, LandCounterfactual=Lit$LandCounterfactual), FUN=min, na.rm=TRUE) 
colnames(Lit_range)[3]<- "Min"
Lit_range$ID = paste(Lit_range$Pot_bin, Lit_range$LandCounterfactual)
Max <- aggregate(Lit$EF_kgCO2pGJprim, by=list(Pot_bin=Lit$Pot_bin, LandCounterfactual=Lit$LandCounterfactual), FUN=max, na.rm=TRUE) 
Max$ID = paste(Max$Pot_bin, Max$LandCounterfactual)
Lit_range$Max = Max[match(Lit_range$ID,Max$ID),"x"]
Lit_range$label <- "Literature"
Lit_range$REGION <- "WORLD"
Lit_range$Pot_bin = factor(Lit_range$Pot_bin, levels = AllPotBins)
Lit_range$ID <- NULL
rm(daioglou,kalt, Max)
#
# ---- COMBINED EMF-33 & LITERATURE ----
EMF33.temp = subset(EmisFac, REGION == "WORLD")
EMF33.temp = subset(EMF33.temp, select = c(SCENARIO, Pot_bin, EF_PrimBio))
EMF33.temp$label <- "EMF-33"
EMF33.temp$Ref <- "Rose_2021"
colnames(EMF33.temp)[] <- c("Scenario","Pot_bin","EF_kgCO2pGJprim","label","Ref")

Lit.temp = Lit
Lit.temp$label <- paste("Partial Models", Lit$LandCounterfactual)
Lit.temp = subset(Lit.temp, select = c(EF_kgCO2pGJprim, Scenario, Pot_bin, Ref, label))

EmisFac.all = rbind(EMF33.temp,Lit.temp)
EmisFac.all$Pot_bin = factor(EmisFac.all$Pot_bin, levels = AllPotBins)
EmisFac.all = subset(EmisFac.all, Pot_bin %in% ActPotBins)

rm(EMF33.temp, Lit.temp)
#
# ---- SUMMARY STATISTICS ----
# Determine median, Q1 and Q3 of emission factor data
meds <- aggregate(EmisFac.all$EF_kgCO2pGJprim, by=list(Pot_bin=EmisFac.all$Pot_bin, label=EmisFac.all$label), FUN=median, na.rm=TRUE) 
q1 <- aggregate(EmisFac.all$EF_kgCO2pGJprim, by=list(Pot_bin=EmisFac.all$Pot_bin, label=EmisFac.all$label), FUN=quantile, probs=0.25, na.rm=TRUE) 
q3 <- aggregate(EmisFac.all$EF_kgCO2pGJprim, by=list(Pot_bin=EmisFac.all$Pot_bin, label=EmisFac.all$label), FUN=quantile, probs=0.75, na.rm=TRUE) 

colnames(meds)[3] <- "Median"
colnames(q1)[3] <- "1st Quartile"
colnames(q3)[3] <- "3rd Quartile"

summary_stats = merge(meds,q1,by=c("Pot_bin","label"))
summary_stats = merge(summary_stats,q3,by=c("Pot_bin","label"))
summary_stats <- summary_stats %>%
  mutate(label = gsub("Partial Models", "Sectoral Models", label))
  
# ---- MIN-MAX EFs (Lit + EMF-33) ----
EF_ranges = rbind(EMF33_range,Lit_range)
EF_ranges$Pot_bin = factor(EF_ranges$Pot_bin, levels = AllPotBins) #c("0","25","50","75","100","125","150","175","200","225","250","275","300","325"))
EF_ranges = subset(EF_ranges, Pot_bin %in% ActPotBins)
#
# ---- CURVES PER MODEL ----
model_curves <- EmisFac %>%
  group_by(MODEL) %>%
  subset(REGION=="WORLD") %>%
  arrange(MODEL, EF_PrimBio) %>%
  mutate(csum = cumsum(PrimBio_2050))
  
#
# ---- FIGURES ----
# ---- *** FIG: Literature data - lineplot  ----
Figlit<-ggplot() + 
  geom_point(data = Lit, aes(x = CumPrimBio, y = EF_kgCO2pGJprim, colour = Scenario), size = 1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylim(0,100) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("kgCO"[2],"/GJ-Prim"))) + 
  xlab("EJ Primary Biomass") 
Figlit
#
# ---- *** FIG: EMF-33 EF-Curve - boxplot  ----
boxplot<-ggplot(data = subset(EmisFac, REGION == "WORLD"),
                 aes(x=Pot_bin, y = EF_PrimBio)) + 
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
line<-ggplot(data = subset(EmisFac, REGION == "WORLD"),
                aes(x=PrimBio_2050, y = EF_PrimBio,colour = MODEL)) + 
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
# ---- *** FIG: EMF-33 EF + Literature (boxplot + points)  ----
boxed<-ggplot(data = EmisFac.all) + 
  geom_boxplot(aes(x=Pot_bin, y = EF_kgCO2pGJprim, fill = label),
               size = 0.5, outlier.shape = NA) +
  geom_jitter(aes(x=Pot_bin, y = EF_kgCO2pGJprim, colour = label), alpha = 0.25, width=0.1, size = 1) +
  ylim(-20,100) +
  scale_x_discrete(limits=ActPotBins) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab(expression(paste("kgCO"[2]," / GJ"[Prim]))) + 
  xlab(expression(paste("EJ"[Prim]," Biomass")))  +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.box = "vertical", legend.title= element_text(face="bold.italic"))  +
  scale_fill_manual(values=c("dodgerblue", "darksalmon", "firebrick"),
                  name="Model type",
                  breaks = unique(EmisFac.all$label),
                  labels = c("IAM \n(EMF-33)", "Sectoral Models \n(Natural Regrowth)", "Sectoral Models \n(Constant Land Cover)")) +
  scale_colour_manual(values=c("dodgerblue", "darksalmon", "firebrick"),
                  name="Model type",
                  breaks = unique(EmisFac.all$label),
                  labels = c("IAM \n(EMF-33)", "Sectoral Models \n(Natural Regrowth)", "Sectoral Models \n(Constant Land Cover)"))
boxed
# 
# ---- *** FIG: EMF-33 EF + Literature (boxplot)  ----
boxed2<-ggplot(data = EmisFac.all) + 
  geom_boxplot(aes(x=Pot_bin, y = EF_kgCO2pGJprim, fill = label),
               size = 0.5, outlier.shape = NA) +
  ylim(-20,100) +
  scale_x_discrete(limits=ActPotBins) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab(expression(paste("kgCO"[2]," / GJ"[Prim]))) + 
  xlab(expression(paste("EJ"[Prim]," Biomass")))  +
  theme(plot.title = element_text(size = FSizeTitle, face = "bold"),
        plot.margin = margin(t = 0.15, r = 0.15, b = 0.15, l = 0.15, "cm"),
        text = element_text(size=FSizeStrip, face="plain"), 
        axis.title.x = element_text(size = FSizeAxis, face="bold"),
        axis.title.y = element_text(size = FSizeAxis, face="bold"),
        axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), 
        axis.text.y = element_text(size=FSizeAxis),
        axis.line = element_line(colour = "black", size = 0.5),
        strip.background = element_rect(colour = "black", fill = "white", size = 0.5),
        strip.text.x = element_text(size = FSizeStrip, face="bold"), 
        strip.text.y = element_text(size = FSizeStrip, face="bold"), 
        legend.position = "right",
        legend.box = "vertical", 
        legend.direction = "vertical", 
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.01,"cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour="gray80", size = 0.3),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank()) +
  
  scale_fill_manual(values=c("dodgerblue", "honeydew2", "darkgreen"),
                    name="Model type",
                    breaks = unique(EmisFac.all$label),
                    labels = c("IAM \n(EMF-33)", "Sectoral Models \n(Natural Regrowth)", "Sectoral Models \n(Constant Land Cover)")) +
  scale_colour_manual(values=c("dodgerblue", "honeydew2", "darkgreen"),
                      name="Model type",
                      breaks = unique(EmisFac.all$label),
                      labels = c("IAM \n(EMF-33)", "Sectoral Models \n(Natural Regrowth)", "Sectoral Models \n(Constant Land Cover)"))
boxed2
# 
# ---- *** FIG: EMF-33 EF + Literature (points)  ----
points<-ggplot(data = EmisFac.all) + 
  geom_jitter(aes(x=Pot_bin, y = EF_kgCO2pGJprim, colour = label), alpha = 1, width=0.2, size = 1) +
  ylim(-20,100) +
  scale_x_discrete(limits=ActPotBins) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab(expression(paste("kgCO"[2],"/GJ-Prim"))) + 
  xlab("EJ Primary Biomass")  +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.box = "vertical", legend.title= element_text(face="bold.italic"))  +
  scale_colour_manual(values=c("dodgerblue", "darksalmon", "firebrick"),
                      name="Model type",
                      breaks = unique(EmisFac.all$label),
                      labels = c("IAM \n(EMF-33)", "Partial Models \n(Natural Regrowth)", "Partial Models \n(Constant Land Cover)"))
points

#
# ---- *** FIG: EMF-33 EF Literature (double ribbon)  ----
ribboned<-ggplot() + 
  geom_ribbon(data = subset(EF_ranges, REGION == "WORLD"),
              aes(x = Pot_bin, ymin=Min, ymax=Max, group = LandCounterfactual, fill=LandCounterfactual), alpha = "0.25") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab(expression(paste("kgCO"[2],"/GJ-Prim"))) + 
  xlab("EJ Primary Biomass")  +
  theme_bw() +
  theme(text= element_text(size=5, face="plain"), axis.text.x = element_text(angle=66, size=5, hjust=1), axis.text.y = element_text(size=5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.box = "vertical", legend.title= element_text(face="bold.italic")) +
  scale_fill_manual(values=c("dodgerblue","darksalmon","firebrick"),
                    name="Model type",
                    breaks = unique(EF_ranges$LandCounterfactual),
                    labels = c("IAM \n(EMF-33)", "Partial Models \n(Natural Regrowth)", "Partial Models \n(Constant Land Cover)"))
ribboned
#
# ---- *** FIG: EMF-33 EF (points)  ----
points1 <-ggplot(data = subset(model_curves, MODEL == "IMAGE")) + 
  geom_point(aes(x=PrimBio_2050, y = EF_PrimBio), alpha = 1, size = 1) +
  geom_line(aes(x=PrimBio_2050, y = EF_PrimBio)) +
  ylim(0,100) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=0,size = 0.1, colour='black') +
  ylab(expression(paste("kgCO"[2],"/GJ-Prim"))) + 
  xlab("EJ Primary Biomass")  +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right", legend.box = "vertical", legend.title= element_text(face="bold.italic"))
points1

#
# ---- OUTPUTS ----
# write.xlsx(summary_stats, file="GitHub/EMF33/output/EmissionSupply/EF_statistics.xlsx", sheetName="Emission Factor Statistics", append=FALSE, row.names=FALSE, showNA = TRUE)
#
# For TSU
# write.xlsx(subset(EmisFac.all, select = -c(Scenario, Ref)), file="GitHub/EMF33/output/EmissionSupply/Box_7_7_Figure_1_Data.xlsx", sheetName="Data (Points)", append=FALSE, row.names=FALSE, showNA = TRUE)
# write.xlsx(summary_stats, file="GitHub/EMF33/output/EmissionSupply/Box_7_7_Figure_1_Data.xlsx", sheetName="Data (Boxplot)", append=TRUE, row.names=FALSE, showNA = TRUE)
#
# pdf(file = "GitHub/EMF33/output/EmissionSupply/Box_7_7_Figure_1.pdf", width = 6, height = 3)
# plot(boxed)
# dev.off()
# #
# Other
# png(file = "GitHub/EMF33/output/EmissionSupply/Boxplot_EMF33_2.png", width = 6*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(boxed2)
# dev.off()
# # #
# png(file = "GitHub/EMF33/output/EmissionSupply/Boxplot_EMF33.png", width = 4*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(boxplot)
# dev.off()
# # #
# png(file = "GitHub/EMF33/output/EmissionSupply/Line_EMF33.png", width = 4*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(line)
# dev.off()
# # #
# png(file = "GitHub/EMF33/output/EmissionSupply/Boxed.png", width = 5*ppi, height = 4*ppi, units = "px", res = ppi)
# plot(boxed)
# dev.off()
# #
# png(file = "GitHub/EMF33/output/EmissionSupply/Points.png", width = 4*ppi, height = 2*ppi, units = "px", res = ppi)
# plot(points)
# dev.off()
# # #
# png(file = "GitHub/EMF33/output/EmissionSupply/Ribboned.png", width = 4*ppi, height = 2*ppi, units = "px", res = ppi)
# plot(ribboned)
# dev.off()
# # #
# png(file = "GitHub/EMF33/output/EmissionSupply/Line_Lit.png", width = 4*ppi, height = 4*ppi, units = "px", res = ppi)
# plot(Figlit)
# dev.off()