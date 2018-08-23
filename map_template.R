# R script to make map template for EMF33 Bioenergy Trade crosscut
# clear memory
# ---- START ----

rm(list=ls())

# # Load Libraries
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
# # set directory path for csv file
setwd("~/disks/y/ontwapps/Timer/Users/Vassilis/Projects - Documents/EMF33/Scenario results/R-Scripts")

# ---- AGGREGATE REGIONS ----

map.world <- map_data("world")
map.EMF = map.world
map.EMF$EMFRegion=NA
map.EMF$EMFRegion[map.EMF$region=="Brazil"]<-"Brazil"
map.EMF$EMFRegion[map.EMF$region=="Argentina"|
                    map.EMF$region=="Antigua"|
                    map.EMF$region=="Brbuda"|
                    map.EMF$region=="Aruba"|
                    map.EMF$region=="Anguilla"|
                    map.EMF$region=="Bahamas"|
                    map.EMF$region=="Barbados"|
                    map.EMF$region=="Belize"|
                    map.EMF$region=="Bermuda"|
                    map.EMF$region=="Bolivia"|
                    map.EMF$region=="Bonaire"|
                    map.EMF$region=="Cayman Islands"|
                    map.EMF$region=="Chile"|
                    map.EMF$region=="Cocos Islands"|
                    map.EMF$region=="Colombia"|
                    map.EMF$region=="Costa Rica"|
                    map.EMF$region=="Cuba"|
                    map.EMF$region=="Curacao"|
                    map.EMF$region=="Dominica"|
                    map.EMF$region=="Ecuador"|
                    map.EMF$region=="El Salvador"|
                    map.EMF$region=="Falkland Islands"|
                    map.EMF$region=="French Guiana"|
                    map.EMF$region=="Grenada"|
                    map.EMF$region=="Guadeloupe"|
                    map.EMF$region=="Guatemala"|
                    map.EMF$region=="Guyana"|
                    map.EMF$region=="Haiti"|
                    map.EMF$region=="Honduras"|
                    map.EMF$region=="Jamaica"|
                    map.EMF$region=="Martinique"|
                    map.EMF$region=="Montserrat"|
                    map.EMF$region=="Mexico"|
                    map.EMF$region=="Netherlands Antilles"|
                    map.EMF$region=="Nevis"|
                    map.EMF$region=="Nicaragua"|
                    map.EMF$region=="Panama"|
                    map.EMF$region=="Paraguay"|
                    map.EMF$region=="Peru"|
                    map.EMF$region=="Puerto Rico"|
                    map.EMF$region=="Saba"|
                    map.EMF$region=="Saint Barthelemy"|
                    map.EMF$region=="Saint Kitts"|
                    map.EMF$region=="Saint Lucia"|
                    map.EMF$region=="Saint Martin"|
                    map.EMF$region=="Saint Vincent"|
                    map.EMF$region=="Sint Eustatium"|
                    map.EMF$region=="Sint Maarten"|
                    map.EMF$region=="Suriname"|
                    map.EMF$region=="Trinidad"|
                    map.EMF$region=="Tobago"|
                    map.EMF$region=="Turks and Caicos Islands"|
                    map.EMF$region=="Uruguay"|
                    map.EMF$region=="Virgin Islands"|
                    map.EMF$region=="Venezuela"]<-"RLAM"
map.EMF$EMFRegion[map.EMF$region=="USA"|
                  map.EMF$region=="American Samoa"]<-"USA"
map.EMF$EMFRegion[map.EMF$region=="Austria"|
                    map.EMF$region=="Andorra"|
                    map.EMF$region=="Azores"|
                    map.EMF$region=="Belgium"|
                    map.EMF$region=="Bulgaria"|
                    map.EMF$region=="Canary Islands"|
                    map.EMF$region=="Croatia"|
                    map.EMF$region=="Cyprus"|
                    map.EMF$region=="Czech Republic"|
                    map.EMF$region=="Denmark"|
                    map.EMF$region=="Estonia"|
                    map.EMF$region=="Finland"|
                    map.EMF$region=="France"|
                    map.EMF$region=="Germany"|
                    map.EMF$region=="Greece"|
                    map.EMF$region=="Guernsey"|
                    map.EMF$region=="Hungary"|
                    map.EMF$region=="Ireland"|
                    map.EMF$region=="Italy"|
                    map.EMF$region=="Isle of Man"|
                    map.EMF$region=="Jersey"|
                    map.EMF$region=="Latvia"|
                    map.EMF$region=="Liechtenstein"|
                    map.EMF$region=="Lithuania"|
                    map.EMF$region=="Luxembourg"|
                    map.EMF$region=="Malta"|
                    map.EMF$region=="Monaco"|
                    map.EMF$region=="Netherlands"|
                    map.EMF$region=="Poland"|
                    map.EMF$region=="Portugal"|
                    map.EMF$region=="Romania"|
                    map.EMF$region=="San Marino"|
                    map.EMF$region=="Slovakia"|
                    map.EMF$region=="Slovenia"|
                    map.EMF$region=="Spain"|
                    map.EMF$region=="Sweden"|
                    map.EMF$region=="UK"] <-"EU"
map.EMF$EMFRegion[map.EMF$region=="Australia"|
                    map.EMF$region=="Canada"|
                    map.EMF$region=="Cook Islands"|
                    map.EMF$region=="Fiji"|
                    map.EMF$region=="Faroe Islands"|
                    map.EMF$region=="French Polynesia"|
                    map.EMF$region=="Guam"|
                    map.EMF$region=="Iceland"|
                    map.EMF$region=="Norway"|
                    map.EMF$region=="New Caledonia"|
                    map.EMF$region=="New Zealand"|
                    map.EMF$region=="Samoa"|
                    map.EMF$region=="Solomon Islands"|
                    map.EMF$region=="Switzerland"|
                    map.EMF$region=="Turkey"|
                    map.EMF$region=="Vanuatu"]<-"ROECD90"
map.EMF$EMFRegion[map.EMF$region=="China"|map.EMF$region=="Japan"]<-"EAsia"
map.EMF$EMFRegion[map.EMF$region=="Afghanistan"|
                    map.EMF$region=="Bangladesh"|
                    map.EMF$region=="Bhutan"|
                    map.EMF$region=="Brunei"|
                    map.EMF$region=="Cambodia"|
                    map.EMF$region=="Christmas Island"|
                    map.EMF$region=="North Korea"|
                    map.EMF$region=="East Timor"|
                    map.EMF$region=="India"|
                    map.EMF$region=="Indonesia"|
                    map.EMF$region=="Laos"|
                    map.EMF$region=="Malaysia"|
                    map.EMF$region=="Maldives"|
                    map.EMF$region=="Micronesia"|
                    map.EMF$region=="Mongolia"|
                    map.EMF$region=="Myanmar"|
                    map.EMF$region=="Nepal"|
                    map.EMF$region=="Pakistan"|
                    map.EMF$region=="Papua New Guinea"|
                    map.EMF$region=="Philippines"|
                    map.EMF$region=="South Korea"|
                    map.EMF$region=="Singapore"|
                    map.EMF$region=="Sri Lanka"|
                    map.EMF$region=="Taiwan"|
                    map.EMF$region=="Timor-Leste"|
                    map.EMF$region=="Thailand"|
                    map.EMF$region=="Vietnam"]<-"RAsia"
map.EMF$EMFRegion[map.EMF$region=="Albania"|
                    map.EMF$region=="Armenia"|
                    map.EMF$region=="Azerbaijan"|
                    map.EMF$region=="Belarus"|
                    map.EMF$region=="Bosnia and Herzegovina"|
                    map.EMF$region=="Croatia"|
                    map.EMF$region=="Georgia"|
                    map.EMF$region=="Kazakhstan"|
                    map.EMF$region=="Kosovo"|
                    map.EMF$region=="Kyrgyzstan"|
                    map.EMF$region=="Moldova"|
                    map.EMF$region=="Montenegro"|
                    map.EMF$region=="Russia"|
                    map.EMF$region=="Serbia"|
                    map.EMF$region=="Macedonia"|
                    map.EMF$region=="Tajikistan"|
                    map.EMF$region=="Turkmenistan"|
                    map.EMF$region=="Ukraine"|
                    map.EMF$region=="Uzbekistan"|
                    map.EMF$region=="Yugoslavia"]<-"REF"
map.EMF$EMFRegion[map.EMF$region=="Greenland"|
                    map.EMF$region=="Antarctica"|
                    map.EMF$region=="French Southern and Antarctic Lands"|
                    map.EMF$region=="Heard Island"|
                    map.EMF$region=="Chagos Archipelago"|
                    map.EMF$region=="Siachen Glacier"|
                    map.EMF$region=="Kiribati"|
                    map.EMF$region=="Marshall Islands"|
                    map.EMF$region=="Northern Mariana Islands"|
                    map.EMF$region=="Norfolk Island"|
                    map.EMF$region=="Niue"|
                    map.EMF$region=="Nauru"|
                    map.EMF$region=="Pitcairn Islands"|
                    map.EMF$region=="Palau"|
                    map.EMF$region=="Madeira Islands"|
                    map.EMF$region=="South Sandwich Islands"|
                    map.EMF$region=="South Georgia"|
                    map.EMF$region=="Saint Helena"|
                    map.EMF$region=="Ascension Island"|
                    map.EMF$region=="Saint Pierre and Miquelon"|
                    map.EMF$region=="Seychelles"|
                    map.EMF$region=="Tonga"|
                    map.EMF$region=="Vatican"|
                    map.EMF$region=="Grenadines"|
                    map.EMF$region=="Wallis and Futuna"]<-"OTHER"
map.EMF$EMFRegion[is.na(map.EMF$EMFRegion)]<-"MAF"                    

map.EMF=subset(map.EMF, !(EMFRegion=="OTHER"))
map.EMF=subset(map.EMF, select=-c(order,subregion))



MapFig <- ggplot() + geom_polygon(data =map.EMF, aes(x=long, y = lat, group=group,fill=EMFRegion), color="black", size = 0.1) + 
  coord_fixed(1) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border = element_blank()) +
  theme(text= element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
MapFig

write.csv(map.EMF, file = "data/Trade/MapEMF.csv")


# MapFig <- ggplot() + geom_polygon(data =map.EMF, region=="Afghanistan"), aes(x=long, y = lat, group = group),fill=NA, color="red", size = 0.1) + 
#   coord_fixed(1) +
#   theme_bw() +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border = element_blank()) +
#   theme(text= element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
# 
# MapFig
