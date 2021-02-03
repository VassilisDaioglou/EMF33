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

# ---- CONSTANTS ----
ppi <- 600
FSizeStrip = 6.5
FSizeLeg = 6.5
ActiveModel = c("AIM/CGE","BET","COFFEE","DNE21+ V.14","FARM 3.1","GCAM_EMF33","GRAPE-15","IMACLIM-NLU","IMAGE","POLES EMF33")

# ---- READ DATA FILE ----
BraDATA = read.csv(paste0(getwd(),"/GitHub/EMF33/data/Brazil/BraDATA.csv"), sep=",", dec=".", stringsAsFactors = FALSE)
BraDATA$X <- NULL
# ---- PROCESS DATA FILE ----
BraDATA = subset(BraDATA, MODEL %in% ActiveModel)



# ---- CALCULATIONS ----
# HAve to:
# Get cumulative emissions and cumulative CDR
# Determine fraction of Brazil to global emissions
# Plot Brazil emissions
#   Also dissagregated across energy and AFOLU