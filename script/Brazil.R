# R script to make figures for EMF33 Bioenergy Brazil crosscut

# ---- START ----
# clear memory
rm(list=ls()) 

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
library(ggpubr)
library(grid)


# ---- CONSTANTS ----
ppi <- 600

FSizeTitle = 10
FSizeStrip = 9
FSizeAxis = 9
FSizeLeg = 9

ActiveModel = c("AIM/CGE","BET","COFFEE","DNE21+ V.14","FARM 3.1","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33")
ActiveYear = c(2010,2030,2050,2070)
# ActiveYear = c(2020,2030,2040,2050,2060,2070,2080,2090,2100)
ActiveYear2 = c(2050,2100)

# ---- READ DATA FILE ----
BraDATA = read.csv(paste0(getwd(),"/GitHub/EMF33/data/Brazil/BraDATA.csv"), sep=",", dec=".", stringsAsFactors = FALSE)
BraDATA$X <- NULL
# ---- PROCESS DATA FILE ----
BraDATA = subset(BraDATA, (MODEL %in% ActiveModel) & (Year %in% ActiveYear))

# GCAM data lacks values for "Emissions|CO2|Energy"
# Calculate thisas the difference between total and AFOLU
BraDATA.GCAMCor <- BraDATA %>%
  subset(MODEL == "GCAM_EMF33" & !(VARIABLE == "Emissions|CO2|Energy")) %>%
  spread(key = "VARIABLE", value = "value") %>%
  set_colnames(c("MODEL","SCENARIO","REGION","UNIT","Year","TotalEmis","AFOLU")) %>%
  mutate(Energy = TotalEmis - AFOLU) %>%
  set_colnames(c("MODEL","SCENARIO","REGION","UNIT","Year","Emissions|CO2","Emissions|CO2|Land Use","Emissions|CO2|Energy")) %>%
  melt(id.vars=c("MODEL","SCENARIO","REGION","UNIT","Year")) %>%
  set_colnames(c("MODEL","SCENARIO","REGION","UNIT","Year","VARIABLE","value"))

BraDATA = BraDATA %>%
  subset(!MODEL == "GCAM_EMF33") %>%
  rbind(BraDATA.GCAMCor)

rm(BraDATA.GCAMCor)
# ---- LABELS ----
#Model labels with text wraps                
model_labels <- c("AIM/CGE"="AIM/CGE","BET"="BET","COFFEE"="COFFEE","DNE21+ V.14"="DNE21+","FARM 3.1"="FARM","MESSAGE-GLOBIOM"="MESSAGEix-\nGLOBIOM","GCAM_EMF33"="GCAM","GRAPE-15"="GRAPE","IMACLIM-NLU"="IMACLIM-\nNLU","IMAGE"="IMAGE","POLES EMF33"="POLES","REMIND-MAGPIE"="REMIND-\nMAgPIE")
#Model labels without text wraps                
model_labels2 <- c("AIM/CGE"="AIM/CGE","BET"="BET","COFFEE"="COFFEE","DNE21+ V.14"="DNE21+","FARM 3.1"="FARM","MESSAGE-GLOBIOM"="MESSAGEix-GLOBIOM","GCAM_EMF33"="GCAM","GRAPE-15"="GRAPE","IMACLIM-NLU"="IMACLIM-NLU","IMAGE"="IMAGE","POLES EMF33"="POLES","REMIND-MAGPIE"="REMIND-MAgPIE")

# ---- FIGURES ----
# ---- FIG: Total Emissions  ----
TotEmis <- ggplot() + 
  geom_line(data=subset(BraDATA, REGION == "Brazil" & SCENARIO == "R3-B-lo-full" & VARIABLE == "Emissions|CO2"),
            aes(x=Year,y = value, color=VARIABLE), size=1, alpha=1) +
  geom_line(data=subset(BraDATA, REGION == "Brazil" & SCENARIO == "R3-B-lo-full"  & !VARIABLE == "Emissions|CO2"),
            aes(x=Year,y = value, color=VARIABLE), size=1, alpha=0.75) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ylab("Emissions MtCO2/yr") + xlab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) +
  theme(plot.title = element_text(size = FSizeTitle, face = "bold")) +
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.box="vertical", legend.direction = "horizontal", legend.spacing.y=unit(0.01,"cm")) +
  scale_colour_manual(values=c("black","red","forestgreen"),
                      name="Emission Source:",
                      breaks=c("Emissions|CO2","Emissions|CO2|Energy","Emissions|CO2|Land Use"),
                      labels=c("Total","Energy","AFOLU"),
                      guide="legend") +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold")) +
  facet_wrap(~MODEL, scales="free_y", labeller=labeller(MODEL = model_labels))
TotEmis

GlobEmis <- ggplot() + 
  geom_line(data=subset(BraDATA,  SCENARIO == "R3-B-lo-full" & VARIABLE == "Emissions|CO2"),
            aes(x=Year,y = value, color=VARIABLE), size=1, alpha=1) +
  geom_line(data=subset(BraDATA, SCENARIO == "R3-B-lo-full"  & !VARIABLE == "Emissions|CO2"),
            aes(x=Year,y = value, color=VARIABLE), size=1, alpha=0.75) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ylab(expression(paste("Emissions MtCO"[2],"/yr"))) + xlab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) +
  theme(plot.title = element_text(size = FSizeTitle, face = "bold")) +
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.box="vertical", legend.direction = "horizontal", legend.spacing.y=unit(0.01,"cm")) +
  scale_colour_manual(values=c("black","red","forestgreen"),
                      name="Emission Source:",
                      breaks=c("Emissions|CO2","Emissions|CO2|Energy","Emissions|CO2|Land Use"),
                      labels=c("Total","Energy","AFOLU"),
                      guide="legend") +
  scale_x_continuous(breaks=ActiveYear)   +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold")) +
  facet_grid(REGION~MODEL, scales="free_y", labeller=labeller(MODEL = model_labels))
GlobEmis


# ---- OUTPUT ----
# png(paste0(getwd(),"/GitHub/EMF33/output/Brazil/Emissions.png"), width=6*ppi, height=5*ppi, res=ppi)
# print(plot(TotEmis))
# dev.off()
# 
# png(paste0(getwd(),"/GitHub/EMF33/output/Brazil/GlobalEmissions.png"), width=10*ppi, height=4*ppi, res=ppi)
# print(plot(GlobEmis))
# dev.off()
# 
