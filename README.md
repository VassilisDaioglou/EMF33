# Energy Modelling Forum 33  
![](EMF-Logo_v2.1.png) \
Repository of R-scripts for processing EMF-33 results 

*Author: Vassilis Daioglou*\
*Contact: v.daioglou@uu.nl | @vassican*

## Process
These scripts aggregate the model projections on the EMF-33 database: \
https://tntcat.iiasa.ac.at/EMF30BIODB/dsd?Action=htmlpage&page=welcome

A snapshot of this database is processed in the ***ReadData.R*** script to produce scaled down datasets to be further processed for specific areas of interest.

Additional data (model results not reported on EMF-33 database & external literature) are provided in this repository in the *data* folder.

## Scripts
### Reading and Preparing Datasets ###
- ***ReadData.R:*** Reads in raw data from EMF-33 database for bioenergy ***demand*** scenarios and subsets into relevant datasets 
  - *Input:* Snapshot of demand scenarios from EMF-33 database and some extra model corrections
  - *Output:* 
    - *For study on bioenergy trade (Trade.R)*: TradDATA.csv; TradPrice.csv
    - *For study on bioenergy technologies (Technologies.R)*: TechDATA.csv; TechDATA_RCP.csv; PriceDATA.csv; SecEnTot.csv, TechDATA_Reg.csv
    - *For visualization of Brazilian GHG mitigation strategies (Brazil.R)*: BraDATA.csv
    - *For determination of GHG mitigation due to bioenergy (FossilDisplacement.R)*: DisplacementData.csv

- ***ReadSupData.R:*** Reads in raw data from EMF-33 database for bioenerg ***supply*** scenarios and subsets into relevant datasets 
  - *Input:* Snapshot of supply scenarios from EMF-33 database 
  - *Output:* 
    - *For determination of biomass Emission-Supply curves (EmissionSupply.R)*: EmissSupplyDat.csv

### Processing of Specific Resluts ###
- ***Trade.R:*** Processes trade data to provide relevant results, figures, and databses for Daioglou et al. (2020b)
  - *map_template.R:* Make regional template for mapped figures (MapEMF.csv)
  - *TradeData2.R:* Additional data for trade paper concerning regional biomass prices (TradeRegData.csv; TradeRegPrice.csv)

- ***Technologies.R:*** Processes technologies data to provide relevant results, figures, and databases for Daioglou et al. (2020a)
  - *Technologies_Region_Diag.R:* Diagnostics for scpecific regions. Not used elsewhere.

- ***Brazil.R:*** Produces visualization of CO2 emission trajectories of the Energy and AFOLU sectors for Brazil and the world for Koberle et al. (2021). 
	Used to	better understand the role of Brazil in Global mitigation strategies. 

- ***FossilDisplacement.R:*** Investigates the CO2 emissions avoided by using bioenergy to  displace fossil fuels, for Roe et al. (2021). 

- ***EmissionSupply.R:*** Determines Emission-Supply curves for the AR6 AFOLU chapter, specifically *Box7.10 Figure 1* (Nabuurs et al. 2021). Also uses data from partial models. 

## References
Daioglou, V., Rose, S., Bauer, N. et al. (2020a). Bioenergy technologies in long-run climate mitigation: Results from the EMF-33 study. *Climatic Change*.\
 https://doi.org/10.1007/s10584-020-02799-y

Daioglou V., Muratori, M., Lamers, P. et al. (2020b). Implications of climate change mitigation strategies on international bioenergy trade. *Climatic Change*. \
https://doi.org/10.1007/s10584-020-02877-1

Koberle A., Daioglou, V., Rochedo, P. et al. (2021). Can global models provide insights into regional mitigation strategies? A diagnostic model comparison study of bioenergy in Brazil. *Climatic Change*. 

Roe S., Streck, C., Beach, R. et al. (2021). Land-based measures to mitigate climate change: Potential and feasibility by country. *Global Change Biology*. 

Nabuurs, G. J., Mrabet, R., Abu Hatab, A. et al. (2021). Agriculture, Forestry, and Other Land Uses (AFOLU). *Contribution of Working Group III to the 6th Assessment Report of the Intergovernmental Panel on Climate Change.*
