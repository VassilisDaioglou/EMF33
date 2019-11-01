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

# ---- READ DATA FILE ----
ppi <- 600
FSizeStrip = 6.5
FSizeLeg = 6.5
Emis=read.csv("data/Emissions/EmisCO2.csv", sep=",", dec=".", stringsAsFactors = FALSE)
