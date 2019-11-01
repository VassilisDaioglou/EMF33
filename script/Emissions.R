# R script to make figures for EMF33 Emission variables
# clear memory

# ---- START ----
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

# set directory path for csv file
# setwd("~/disks/y/ontwapps/Timer/Users/Vassilis/Projects - Documents/EMF33/Scenario results/R-Scripts")
setwd("C:/Users/Asus/Documents/GitHub/EMF33")

ppi <- 600
FSizeStrip = 6.5
FSizeLeg = 6.5

# ---- READ DATA FILE ----
Emis=read.csv("data/Emissions/EmisCO2.csv", sep=",", dec=".", stringsAsFactors = FALSE)
Emis$X <- NULL
Emis$Year = as.numeric(substr(Emis$Year, start=1, stop=4))
Emis$value = as.numeric(substr(Emis$value, start=1, stop=10))

# ---- PROCESS ----
Emis$ScenOrder = factor(Emis$SCENARIO, levels=c('R3-BASE-0-full','R3-B-hi-full','R3-B-lo-full','R3-B-vlo-full'))

# ---- LABELS ----
scen_labels <- c("R3-BASE-0-full"="Baseline",
                  "R3-B-hi-full"=">2°C",
                  "R3-B-lo-full"="2°C", 
                  "R3-B-vlo-full"="1.5°C")


# ---- FIG: EU Total Emissions ----
EUEmis <- ggplot(data=subset(Emis, REGION=="EU"&MODEL=="IMAGE"&(SCENARIO=="R3-BASE-0-full"|SCENARIO=="R3-B-hi-full"|SCENARIO=="R3-B-lo-full")&VARIABLE=="Emis|CO2"),
                 aes(x=Year, y=value, colour=ScenOrder, fill=REGION)) + 
  #geom_bar(stat="identity", position="dodge")+
  geom_line(size=1)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  xlim(2010,2050) +
  ggtitle("Total EU Emissions") +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_blank(), legend.position="right") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("Mt ",CO[2],"/yr",""))) +
  # ylab(expression(paste(EJ[Primary],"/yr",""))) +
  xlab("") +
  scale_colour_manual(values=c("black", "red","forestgreen"), 
                      name ="Scenarios",
                      breaks=c("R3-BASE-0-full","R3-B-hi-full","R3-B-lo-full"),
                      labels=c("Baseline",">2°C","2°C")
  ) 
EUEmis


# png(file = "output/Emissions//EUEmis.png", width = 4*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(EUEmis)
# dev.off()
