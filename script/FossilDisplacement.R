# ---- INFORMATION ----
# AIM
# R script to process IAM results for CLEW nexus interactions 
# Author: Vassilis Daioglou
# 
# This script reads in IAMC template scenario projections and plots nexus interactions
# Compares the behaviour for selected indicators across the SSP-RCP matrix


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
