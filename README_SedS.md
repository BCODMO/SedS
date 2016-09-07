This is the ReadMe file corresponding to scripts used in the publication Hoarfrost & Arnosti 2016 in Frontiers Earth Science.

## This repository contains six scripts:

* **RawGPCProcess_SedS.R** - will take a folder containing raw asc file output from a Shimadzu fluorescence detector, slant correct it, output the slant corrected data as a csv file, and visualize the chromatograms as png files. Accepts an external argument of the path to the directory to be processed.
* **GetMaxFluorescence_SedS.R** - takes folder path containing slant corrected csvs for each chromatogram as external argument, and output file names for overall, HMW, and LMW max fluorescences (in that order). Outputs three csv files with: overall maximum fluorescence, max fluorescence for HMW portion, and max fluorescence for LMW portion (along with factor info about chrom) 
* **CalculateStdBins_SedS.R** - from slant corrected csvs of a set of standards of known molecular weight, calculates bins of time within a GPC run corresponding to particular molecular weight to use in rate calculations, and outputs as csv file, as well as a png file visualizing bin cutoffs.
* **FlaRates_SedS.R** - Accepts a directory from which to calculate hydrolysis rates and output file name as external arguments (also requires HydrolysisCutsInfo.csv and FlaTimepointsStdRefs_SedS.csv), and calculates hydrolysis rates for each timepoint within an incubation. 
* **FlaProcess_SedS.R** - Accepts the name of the file input containing rates (output from FlaRates_SedS.R) and output file name as external arguments, and adds factor columns, changes particular rate values to zero (using ChangeToZeroKeys.txt, after manual inspection of chromatogram quality), and extracts only the timepoint of maximum activity. Outputs max rates with factors as csv file. 
	
	
## To reproduce rates and figures from the publication:

1. Download raw GPC data:
	
2. Slant correct data from raw GPC output for rates and standards; On the command line run:
	
```
RScript RawGPCProcess_SedS.R "CoreComparison-raw-data-asc" "CoreComparison-csvs-for-rates" "CoreComparison-chroms-raw-data-png"
RScript RawGPCProcess_SedS.R "MarmaraMethodsDevel-raw-data-asc" "MarmaraMethodsDevel-csvs-for-rates" "MarmaraMethodsDevel-chroms-raw-data-png"
RScript RawGPCProcess_SedS.R "stds-gpc1-110515/raw-asc" "stds-gpc1-110515/slant-corrected-csv" "stds-gpc1-110515/chroms-png"
RScript RawGPCProcess_SedS.R "stds-gpc2-110315/raw-asc" "stds-gpc2-110315/slant-corrected-csv" "stds-gpc2-110315/chroms-png"
RScript RawGPCProcess_SedS.R "stds-gpc3-052715/raw-asc" "stds-gpc3-052715/slant-corrected-csv" "stds-gpc3-052715/chroms-png"
RScript RawGPCProcess_SedS.R "stds-gpc1-012413/raw-asc" "stds-gpc1-012413/slant-corrected-csv" "stds-gpc1-012413/chroms-png"
RScript RawGPCProcess_SedS.R "stds-gpc2-020113/raw-asc" "stds-gpc2-020113/slant-corrected-csv" "stds-gpc2-020113/chroms-png"

```

2.5. Find max fluorescence for overall, HMW, and LMW portion of all chromatograms:

```
#for CoreComparison Guaymas and Med data
RScript GetMaxFluorescence_SedS.R "CoreComparison-csvs-for-rates" "IntegratedFluorescence_overall_SedS.csv" "IntegratedFluorescence_HMW_SedS.csv" "IntegratedFluorescence_LMW_SedS.csv"#for PreX Marmara data

RScript GetMaxFluorescence_SedS.R "MarmaraMethodsDevel-csvs-for-rates" "PreX_IntegratedFluorescence_overall_SedS.csv" "PreX_IntegratedFluorescence_HMW_SedS.csv" "PreX_IntegratedFluorescence_LMW_SedS.csv"

```

3. Calculate standard bins for all three sets of standards:

```
RScript CalculateStdBins_SedS.R "stds-gpc1-110515/slant-corrected-csv" "stdbins-gpc1-110515.csv" "stdbins-gpc1-110515.png"
RScript CalculateStdBins_SedS.R "stds-gpc2-110315/slant-corrected-csv" "stdbins-gpc2-110315.csv" "stdbins-gpc2-110315.png"
RScript CalculateStdBins_SedS.R "stds-gpc3-052715/slant-corrected-csv" "stdbins-gpc3-052715.csv" "stdbins-gpc3-052715.png"
RScript CalculateStdBins_SedS.R "stds-gpc1-012413/slant-corrected-csv" "stdbins-gpc1-012413.csv" "stdbins-gpc1-012413.png"
RScript CalculateStdBins_SedS.R "stds-gpc2-020113/slant-corrected-csv" "stdbins-gpc2-020113.csv" "stdbins-gpc2-020113.png"
RScript CalculateStdBins_SedS.R "stds-gpc2-071913/slant-corrected-csv" "stdbins-gpc2-071913.csv" "stdbins-gpc2-071913.png"
```

4. Calculate hydrolysis rates:

```
RScript FlaRates_SedS.R "SedComparison-csvs-for-rates" "FlaRates_SedS.csv"
```


5. Process rates: 

```
RScript FlaProcess_SedS.R "FlaRates_SedS.csv" "FlaMaxRatesFinal_SedS.csv"
```

6. Complete statistical analyses and generate figures in publication:


```
RScript FiguresAndAnalysis_SedS.R
```	

*Med-N-440cm-lam
Guaymas-P1-5/55cm-lam
*Guaymas-P3-5cm-chon
Med-S4-chon okay
Med-N-575cm-chon okay

prelim labels:
Marmara-treatment (notrt,pH10,pH11,CD,pH10SDS,pH11SDS,CDSDS)-low/high/x/bl-t0/t1

7. Process treatment development preliminary experiments

```
RScript RawGPCProcess.R