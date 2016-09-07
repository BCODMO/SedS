#run all the scripts associated with SedS.

##Download raw GPC data:
	
##Slant correct data from raw GPC output for rates and standards; On the command line run:
	
RScript RawGPCProcess_SedS.R "CoreComparison-raw-data-asc" "CoreComparison-csvs-for-rates" "CoreComparison-chroms-raw-data-png"
RScript RawGPCProcess_SedS.R "MarmaraMethodsDevel-raw-data-asc" "MarmaraMethodsDevel-csvs-for-rates" "MarmaraMethodsDevel-chroms-raw-data-png"
RScript RawGPCProcess_SedS.R "stds-gpc1-110515/raw-asc" "stds-gpc1-110515/slant-corrected-csv" "stds-gpc1-110515/chroms-png"
RScript RawGPCProcess_SedS.R "stds-gpc2-110315/raw-asc" "stds-gpc2-110315/slant-corrected-csv" "stds-gpc2-110315/chroms-png"
RScript RawGPCProcess_SedS.R "stds-gpc3-052715/raw-asc" "stds-gpc3-052715/slant-corrected-csv" "stds-gpc3-052715/chroms-png"
RScript RawGPCProcess_SedS.R "stds-gpc1-012413/raw-asc" "stds-gpc1-012413/slant-corrected-csv" "stds-gpc1-012413/chroms-png"
RScript RawGPCProcess_SedS.R "stds-gpc2-020113/raw-asc" "stds-gpc2-020113/slant-corrected-csv" "stds-gpc2-020113/chroms-png"
RScript RawGPCProcess_SedS.R "stds-gpc2-071913/raw-asc" "stds-gpc2-071913/slant-corrected-csv" "stds-gpc2-071913/chroms-png"

##Find max fluorescence for overall, HMW, and LMW portion of all chromatograms:
#for CoreComparison Guaymas and Med data
RScript GetMaxFluorescence_SedS.R "CoreComparison-csvs-for-rates" "IntegratedFluorescence_overall_SedS.csv" "IntegratedFluorescence_HMW_SedS.csv" "IntegratedFluorescence_LMW_SedS.csv"
#for PreX Marmara data
RScript GetMaxFluorescence_SedS.R "MarmaraMethodsDevel-csvs-for-rates" "PreX_IntegratedFluorescence_overall_SedS.csv" "PreX_IntegratedFluorescence_HMW_SedS.csv" "PreX_IntegratedFluorescence_LMW_SedS.csv"

##Calculate standard bins for all six sets of standards:
RScript CalculateStdBins_SedS.R "stds-gpc1-110515/slant-corrected-csv" "stdbins-gpc1-110515.csv" "stdbins-gpc1-110515.png"
RScript CalculateStdBins_SedS.R "stds-gpc2-110315/slant-corrected-csv" "stdbins-gpc2-110315.csv" "stdbins-gpc2-110315.png"
RScript CalculateStdBins_SedS.R "stds-gpc3-052715/slant-corrected-csv" "stdbins-gpc3-052715.csv" "stdbins-gpc3-052715.png"
RScript CalculateStdBins_SedS.R "stds-gpc1-012413/slant-corrected-csv" "stdbins-gpc1-012413.csv" "stdbins-gpc1-012413.png"
RScript CalculateStdBins_SedS.R "stds-gpc2-020113/slant-corrected-csv" "stdbins-gpc2-020113.csv" "stdbins-gpc2-020113.png"
RScript CalculateStdBins_SedS.R "stds-gpc2-071913/slant-corrected-csv" "stdbins-gpc2-071913.csv" "stdbins-gpc2-071913.png"

##Calculate hydrolysis rates:
#For CoreComparison Guaymas and Med data
RScript FlaRates_SedS.R "CoreComparison-csvs-for-rates" "FlaRates_SedS.csv"
#For PreX Marmara data
RScript PreXFlaRates_SedS.R "MarmaraMethodsDevel-csvs-for-rates" "PreXFlaRates_SedS.csv"

##Process rates: 
#for CoreComparison Guaymas and Med data
RScript FlaProcess_SedS.R "FlaRates_SedS.csv" "FlaRatesFinal_SedS.csv" "FlaMaxRatesFinal_SedS.csv"
#for PreX Marmara data
RScript PreXFlaProcess_SedS.R "PreXFlaRates_SedS.csv" "PreXFlaRatesFinal_SedS.csv" "PreXFlaMaxRatesFinal_SedS.csv"

##Complete statistical analyses and generate figures in publication:
RScript FiguresAndAnalysis_SedS.R
	

##Process treatment development preliminary experiments
