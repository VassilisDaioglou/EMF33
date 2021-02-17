# ---- INFORMATION ----
# AIM
# R script to determine the displaced fossil fuel emissions due to the use of bioenergy
# To be combined with CDR from BECCS in order to understand the full mitigation potential of bioenergy with CCS. 
#
# METHOD:
# Use EMF-33 scenarios to get range of results across models and scenarios.
# 1. Determine carbon content of electrifcity and liquids use based on baseline projections
# 2. Determine extent of bioenergy use in these energy carriers across a mitigation scenario
# 3. Determine displaced CO2
#
# Calculations are done for a carbon price of approximately 100 $/tCO2, in order to be consistent with BECCS
# potential calculations. Sinnce the EMF-33 projections do not include results for a carbon price of exactly 100$/tCO2,
# we instead identify the timestep that car bon prices are projected to surpass 100$/tCO2, and use the data of that timestep.
#
# AUTHORSHIP:
# Author: Vassilis Daioglou
# Date: February 2020
# Reference: Roe et al. 2021, Global Environmental Change
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
library(stringr)
library(xlsx)
library(openxlsx)
library(ggpubr)

setwd("C:/Users/Asus/Documents/Github/EMF33/")

# ---- INPUTS: Constants ----
ppi <- 300
FSizeTitle = 10
FSizeStrip = 9
FSizeAxis = 9
FSizeLeg = 9

# ---- READ DATA ----
DATA=read.csv("data/FossilDisplacement/DisplacementData.csv", sep=",", dec=".", stringsAsFactors = FALSE)

DATA$X <- NULL
DATA = subset(DATA, !(Year=="1990"|Year=="1995"|Year=="2000"|Year=="2005"|Year=="2010"|Year=="2015"|Year=="2110"|Year=="2130"|Year=="2150"))
DATA$ID1 = paste(DATA$MODEL, DATA$SCENARIO, DATA$REGION)
DATA$ID2 = paste(DATA$MODEL, DATA$SCENARIO, DATA$REGION,DATA$Year)

cprice = subset(DATA, VARIABLE == "Price|Carbon")
cprice$Tech <- NULL
# ---- YEAR OF INTEREST ----
# Identify first year when carbon prices are above 100$/tCO2
Above100 = subset(cprice, value > 100)
Cross100 = aggregate(Above100$Year, by=list(ID1=Above100$ID1), FUN=min, na.rm=TRUE)
Cross100$ID2 = paste(Cross100$ID1, Cross100$x)

DATA.cor = subset(DATA, ID2 %in% Cross100$ID2 & !(VARIABLE=="Price|Carbon"))

# ----  CARBON CONTENTS OF FINAL ENERGY CARRIERS ----