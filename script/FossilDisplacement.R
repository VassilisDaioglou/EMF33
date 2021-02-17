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

setwd("C:/Users/Asus/Documents/Github/SHAPE/")

# ---- INPUTS: Constants ----
ppi <- 300
FSizeTitle = 10
FSizeStrip = 9
FSizeAxis = 9
FSizeLeg = 9
