  # THIS MACHINE CONSUMES DATA
  # R script to make figures for EMF33 Bioenergy Trade crosscut
  # clear memory
  rm(list=ls()) 
  
  # Load Libraries
  library(reshape);
  library(ggplot2);
  library(tidyr)
  library(data.table);
  library(reshape2);
  library(plyr);
  library(dplyr)
  
  # set directory path for csv file
  # setwd("~/disks/y/ontwapps/Timer/Users/Vassilis/Projects - Documents/EMF33/Scenario results/R-Scripts")
  setwd("C:/Users/Asus/Documents/GitHub/EMF33")
  
  # Read Data File
  TradDATA=read.csv("data/Trade/TradDATA.csv", sep=",", dec=".", stringsAsFactors = FALSE)
  TradPrice=read.csv("data/Trade/TradPrice.csv", sep=",", dec=".", stringsAsFactors = FALSE)
  # Make dataframes of relevant varibales
  # Delete extra column created when data in imported
  TradDATA$X <-NULL
  TradPrice$X <-NULL
  
  TradDATA$Year = as.numeric(substr(TradDATA$Year, start=1, stop=4))
  TradPrice$Year = as.numeric(substr(TradPrice$Year, start=1, stop=4))
  
  TradDATA = subset(TradDATA, SCENARIO=="R3-BASE-0-full"|
                      SCENARIO=="R3-B-hi-full"|
                      SCENARIO=="R3-B-lo-full"|
                      SCENARIO=="R3-B-vlo-full"|
                      SCENARIO=="R3-B-hi-none"|
                      SCENARIO=="R3-B-hi-nofuel"|
                      SCENARIO=="R3-B-hi-nobeccs"|
                      SCENARIO=="R3-B-lo-none"|
                      SCENARIO=="R3-B-lo-nofuel"|
                      SCENARIO=="R3-B-lo-nobeccs")
  
  mod.vals <- unique(TradDATA$MODEL)
  scen.vals <- unique(TradDATA$SCENARIO)
  var.vals <- unique(TradDATA$VARIABLE)
  year.vals <- unique(TradDATA$Year)
  
  TradDATA$VARID <- substr(TradDATA$VARIABLE, start=1, stop=50)
  TradDATA=data.table(TradDATA)
  TradDATA$VARID <-gsub( "[[:punct:]]","",TradDATA$VARID,fixed=F)
  TradDATA$VARID <-gsub( "[[:space:]]","",TradDATA$VARID,fixed=F)
  TradDATA$VARID <-gsub( "PrimaryEnergy","Prim",TradDATA$VARID,fixed=F)
  TradDATA$VARID <-gsub( "Volume","Vol",TradDATA$VARID,fixed=F)
  TradDATA$VARID <-gsub( "Value","Val",TradDATA$VARID,fixed=F)
  TradDATA$VARID <-gsub( "Modern","",TradDATA$VARID,fixed=F)
  TradDATA$VARID <-gsub( "SecondaryEnergy","Sec",TradDATA$VARID,fixed=F)
  TradDATA$VARID <-gsub( "PrimaryEqu","PrimEqu",TradDATA$VARID,fixed=F)
  TradDATA$VARID <-gsub( "AgriculturalProduction","AgriProd",TradDATA$VARID,fixed=F)
  TradDATA$VARIABLE <-NULL
  TradDATA$UNIT <-NULL
  TradDATA=spread(TradDATA, VARID, value, drop=TRUE)
  TradDATA[is.na(TradDATA)]<-0
  TradDATA=TradDATA %>% mutate(TradePrimBiomassVol2=TradePrimBiomassVol+TradeSecLiquidsBiomassPrimEqu)
  TradDATA=TradDATA %>% mutate(AgriProdFood=AgriProd-AgriProdEnergyCrops)
  TradDATA$AgriProdFood[TradDATA$AgriProdFood<0] <-0
  TradDATA$TradePrimBiomassVol <-NULL
  TradDATA$TradeSecLiquidsBiomassPrimEqu <-NULL
  TradDATA$AgriProd <- NULL
  TradDATA<-melt(TradDATA, measure.vars=c("Prim","PrimBiomass","PrimFossil",
                                    "TradePrimBiomassVal","TradePrimCoalVal","TradePrimGasVal","TradePrimOilVal",
                                    "TradePrimBiomassVol2","TradePrimCoalVol","TradePrimGasVol","TradePrimOilVol",
                                    "TradeSecLiquidsBiomassVol","TradeSecSolidsBiomassVol","AgriProdFood","AgriProdEnergyCrops"))
  TradDATA$variable <-gsub( "TradePrimBiomassVol2","TradePrimBiomassVol",TradDATA$variable,fixed=F)
  TradDATA=spread(TradDATA,REGION,value,drop=TRUE)
  TradDATA[is.na(TradDATA)]<-0
  
  # Determine values for new regions
  # Correct Brazil (BRA+Brazil)
  TradDATA=TradDATA %>% mutate(Brazil2=BRA+Brazil)
  # Correct China (CHN+China)
  TradDATA=TradDATA %>% mutate(China2=CHN+China)
  # Correct Japan (JPN+Japan)
  TradDATA=TradDATA %>% mutate(Japan2=JPN+Japan)
  # Correct EU (EU+WEU+CEU+EEU)
  TradDATA=TradDATA %>% mutate(EU2=EU+WEU+CEU+EEU)
  # REST ASIA (Asia-China)
  TradDATA=TradDATA %>% mutate(RAsia=ASIA-China2)
  # EAST ASIA (China+Japan)
  TradDATA=TradDATA %>% mutate(EAsia=China2+Japan2)
  # REST OECD (OECD90-EU-USA-Japan)
  TradDATA=TradDATA %>% mutate(ROECD90=OECD90-EU2-USA-Japan2)
  # REST LAM (LAM-Brazil)
  TradDATA=TradDATA %>% mutate(RLAM=LAM-Brazil2)
  
  
  AllR <- melt(TradDATA, measure.vars=c('ASIA','BRA','Brazil','CEU','China','CHN','EEU','EU','India','Japan','JPN','LAM','MAF','OECD90','REF','USA','WEU','World','Brazil2','China2','Japan2','EU2','RAsia','EAsia','ROECD90','RLAM'))
  colnames(AllR)[5] <- 'REGION'
  NewReg1 = subset(AllR, REGION=="EU2"|REGION=="USA"|REGION=="ROECD90"|REGION=="EAsia"|REGION=="RAsia"|REGION=="Brazil2"|REGION=="RLAM"|REGION=="REF"|REGION=="MAF"|
                         REGION=="OECD90"|REGION=="ASIA"|REGION=="LAM") # Also RCP regions for demand x-cut paper
  
  NewReg1 = na.omit(NewReg1)
  NewReg1<-NewReg1
  NewReg1$REGION <-gsub( "EU2","EU",NewReg1$REGION,fixed=F)
  NewReg1$REGION <-gsub( "Brazil2","Brazil",NewReg1$REGION,fixed=F)
  NewReg1$value[NewReg1$variable=="PrimBiomass" & NewReg1$value<0] = 0
  NewReg <-NewReg1
  
  write.csv(NewReg, file = "data/Trade/TradeRegData.csv")
  
  # SAME BUT FOR PRICES
  # Have to weigh prices of different regions based on biomass importance of different regions
  # Biomass importance = Production + Imports
  #BioSize = subset(AllR, variable=="TradePrimBiomassVol"|variable=="PrimBiomass")
  BioSize = subset(AllR, variable=="PrimBiomass")
  BioSize = subset(BioSize, !(REGION=="BRA"|REGION=="CHN"|REGION=="JPN"|REGION=="Brazil"|REGION=="EU"|REGION=="China"|REGION=="Japan"))
  BioSize$REGION <-gsub( "EU2","EU",BioSize$REGION,fixed=F)
  BioSize$REGION <-gsub( "Brazil2","Brazil",BioSize$REGION,fixed=F)
  BioSize$REGION <-gsub( "Japan2","Japan",BioSize$REGION,fixed=F)
  BioSize$REGION <-gsub( "China2","China",BioSize$REGION,fixed=F)
  
  BioSize$value[BioSize$variable=="PrimBiomass" & BioSize$value<0.01] = 0.01
  BioSize$value[BioSize$variable=="PrimBiomass" & BioSize$value==0] = 0.01
  # BioSize$value[BioSize$variable=="TradePrimBiomassVol" & BioSize$value>0] = 0
  # BioSize$value[BioSize$variable=="TradePrimBiomassVol"] = BioSize$value[BioSize$variable=="TradePrimBiomassVol"]*-1
  BioSize = spread(BioSize, REGION,value)
  #is.na(BioSize[5:23]) <- 0.01
  
  
  BioSize = BioSize %>% mutate(EAsia_CHNW = China/(China+Japan))
  BioSize = BioSize %>% mutate(EAsia_JAPW = Japan/(China+Japan))
  BioSize = BioSize %>% mutate(RAsia_CHNW = -China/max(ASIA-China,0.01))
  BioSize = BioSize %>% mutate(RAsia_ASIAW = ASIA/max(ASIA-China,0.01))
  BioSize = BioSize %>% mutate(ROECD90_OECD90W = OECD90/max(OECD90-EU-USA-Japan,0.01))
  BioSize = BioSize %>% mutate(ROECD90_EUW = -EU/max(OECD90-EU-USA-Japan,0.01))
  BioSize = BioSize %>% mutate(ROECD90_USAW = -USA/max(OECD90-EU-USA-Japan,0.01))
  BioSize = BioSize %>% mutate(ROECD90_JAPW = -Japan/max(OECD90-EU-USA-Japan,0.01))
  BioSize = BioSize %>% mutate(RLAM_LAMW = LAM/max(LAM-Brazil,0.01))
  BioSize = BioSize %>% mutate(RLAM_BRAW = -Brazil/max(LAM-Brazil,0.01))
  
  BioSize=subset(BioSize, select=c(MODEL,SCENARIO,Year,variable,EAsia_CHNW,EAsia_JAPW,RAsia_CHNW,RAsia_ASIAW,ROECD90_OECD90W,ROECD90_EUW,ROECD90_USAW,ROECD90_JAPW,RLAM_LAMW,RLAM_BRAW))
  
  BioSize = melt(BioSize, id.vars=c("MODEL","SCENARIO","Year","variable"))
  colnames(BioSize)[5] <- "weight"
  BioSize$Region_out[BioSize$weight=="EAsia_CHNW"]<-"EAsia"
  BioSize$Region_out[BioSize$weight=="EAsia_JAPW"]<-"EAsia"
  BioSize$Region_out[BioSize$weight=="RAsia_CHNW"]<-"RAsia"
  BioSize$Region_out[BioSize$weight=="RAsia_ASIAW"]<-"ASIA"
  BioSize$Region_out[BioSize$weight=="ROECD90_OECD90W"]<-"ROECD90"
  BioSize$Region_out[BioSize$weight=="ROECD90_EUW"]<-"ROECD90"
  BioSize$Region_out[BioSize$weight=="ROECD90_USAW"]<-"ROECD90"
  BioSize$Region_out[BioSize$weight=="ROECD90_JAPW"]<-"ROECD90"
  BioSize$Region_out[BioSize$weight=="RLAM_LAMW"]<-"RLAM"
  BioSize$Region_out[BioSize$weight=="RLAM_BRAW"]<-"RLAM"
  
  BioSize$Region_in[BioSize$weight=="EAsia_CHNW"]<-"China"
  BioSize$Region_in[BioSize$weight=="EAsia_JAPW"]<-"Japan"
  BioSize$Region_in[BioSize$weight=="RAsia_CHNW"]<-"China"
  BioSize$Region_in[BioSize$weight=="RAsia_ASIAW"]<-"ASIA"
  BioSize$Region_in[BioSize$weight=="ROECD90_OECD90W"]<-"OECD90"
  BioSize$Region_in[BioSize$weight=="ROECD90_EUW"]<-"EU"
  BioSize$Region_in[BioSize$weight=="ROECD90_USAW"]<-"USA"
  BioSize$Region_in[BioSize$weight=="ROECD90_JAPW"]<-"Japan"
  BioSize$Region_in[BioSize$weight=="RLAM_LAMW"]<-"LAM"
  BioSize$Region_in[BioSize$weight=="RLAM_BRAW"]<-"Brazil"
  
  BioSize$ID = paste(BioSize$MODEL,BioSize$SCENARIO,BioSize$Region_in,BioSize$Region_out)
  
  # Biomass prices
  # TradPrice=spread(TradPrice,REGION,value,drop=TRUE)
  # TradPrice[is.na(TradPrice)]<-0
  
  Price=TradPrice
  Price$Region_out[Price$REGION=="ASIA"] <- "RAsia"
  Price$Region_out[Price$REGION=="Brazil"] <- "Brazil"
  Price$Region_out[Price$REGION=="China"] <- "EAsia"
  Price$Region_out[Price$REGION=="EU"] <- "EU"
  Price$Region_out[Price$REGION=="Japan"] <- "EAsia"
  Price$Region_out[Price$REGION=="LAM"] <- "RLAM"
  Price$Region_out[Price$REGION=="MAF"] <- "MAF"
  Price$Region_out[Price$REGION=="OECD90"] <- "ROECD90"
  Price$Region_out[Price$REGION=="REF"] <- "REF"
  Price$Region_out[Price$REGION=="USA"] <- "USA"
  Price$ID = paste(Price$MODEL,Price$SCENARIO,Price$REGION, Price$Region_out)
  Price$Region_in_W <- BioSize[match(Price$ID,BioSize$ID),6]
  Price$Region_in_W[is.na(Price$Region_in_W)] <- 1
  Price$Region_in_W[Price$REGION==Price$Region_out] = 1
  Price$ID <- NULL
  Price$UNIT <- NULL
  Price = Price %>% mutate(PriceW=value*Region_in_W)
  Price=subset(Price, select=c(MODEL,SCENARIO,REGION,VARIABLE,Year,PriceW))
  Price=spread(Price,REGION,PriceW)
  # Determine values for new regions
  # REST ASIA (Asia-China)
  Price=Price %>% mutate(RAsia=ASIA+China)
  # EAST ASIA (China+Japan)
  Price=Price %>% mutate(EAsia=China+Japan)
  # REST OECD (OECD90-EU-USA-Japan)
  Price=Price %>% mutate(ROECD90=OECD90+EU+USA+Japan)
  # REST LAM (LAM-Brazil)
  Price=Price %>% mutate(RLAM=LAM+Brazil)
  
  test1=subset(Price, MODEL=="IMAGE"&SCENARIO=="R3-B-hi-full")
  #test1=subset(test1, select=c(MODEL,SCENARIO,VARIABLE,Year,RAsia,EAsia,ROECD90,RLAM))
  test2=spread(TradPrice,REGION,value)
  test2=subset(test2, MODEL=="IMAGE"&SCENARIO=="R3-B-hi-full")
  
  PriceAllR <- melt(Price, measure.vars=c('ASIA','Brazil','China','EU','Japan','LAM','MAF','OECD90','REF','USA','RAsia','EAsia','ROECD90','RLAM'))
  colnames(PriceAllR)[5] <- 'REGION'
  PriceAllR1 = subset(PriceAllR, REGION=="EU"|REGION=="USA"|REGION=="ROECD90"|REGION=="EAsia"|REGION=="RAsia"|REGION=="Brazil"|REGION=="RLAM"|REGION=="REF"|REGION=="MAF")
  PriceAllR1$VARIABLE <-gsub( "[[:punct:]]","",PriceAllR1$VARIABLE,fixed=F)
  PriceAllR1$VARIABLE <-gsub( "[[:space:]]","",PriceAllR1$VARIABLE,fixed=F)
  PriceAllR1$VARIABLE <-gsub( "PricePrimaryEnergy","",PriceAllR1$VARIABLE,fixed=F)
  
  write.csv(PriceAllR1, file = "data/Trade/TradeRegPrice.csv")
  
  
  rm(TradDATA,mod.vals,scen.vals,var.vals,year.vals,AllR,NewReg,NewReg1)
  
  #source(file="script/Trade_Figures2.R")
  
