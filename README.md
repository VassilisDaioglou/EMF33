# Energy Modelling Forum 33 
Repository of R-scripts for processing EMF-33 results for the "Technologies" and "Trade" papers

*Author: Vassilis Daioglou*\
*Contact: v.daioglou@uu.nl | @vassican*

## Process
This script process a snapshot of the entire EMF-33 database (available here: https://tntcat.iiasa.ac.at/EMF30BIODB/dsd?Action=htmlpage&page=welcome)\

This is read in from the ***ReadData.R*** script to produce scaled down datasets to be further processed for each specific cross-cut.\

Additional data (model results not reported on EMF-33 database or external literature) are provided in this repository in the *data* folder.\

## Scripts
- ***ReadData.R:*** Reads in raw data from EMF-33 database and subsets into relevant datasets for *Technologies.R* and *Trade.R*
  - *Input:* Snapshot of EMF-33 database. Some extra model correctoions
  - *Output:* 
    - *For Trade.R*: TradDATA.csv; TradPrice.csv
    - *For Technologies.R*: TechDATA.csv; TechDATA_RCP.csv; PriceDATA.csv; SecEnTot.csv, TechDATA_Reg.csv
 
- ***Technologies.R:*** Processes technologies data to provide relevant results, figures, and databases for Daioglou et al. (2020)
  - *Technologies_Region_Diag.R:* Diagnostics for scpecific regions. Not used elsewhere.
 
- ***Trade.R:*** Processes trade data to provide relevant results, figures, and databses for Daioglou et al. (under review)
  - *map_template.R:* Make regional template for mapped figures (MapEMF.csv)
  - *TradeData2.R:* Additional data for trade paper concerning regional biomass prices (TradeRegData.csv; TradeRegPrice.csv)

## References
Daioglou, V., Rose, S., Bauer, N., et al. (2020). Bioenergy technologies in long-run climate mitigation: Results from the EMF-33 study. Climatic Change (accepted)

Daioglou V., Muratori M., et al. , Implications of climate change mitigation strategies on international bioenergy trade, (under review)
