# SedS
## Improved measurement of extracellular enzymatic activities in subsurface sediments using competitive desorption treatment
### Hoarfrost, A., Snider, R., and Arnosti, C.

This is the ReadMe file corresponding to scripts used in the publication Hoarfrost et al. 2016.

## This repository contains eight scripts:

* **RawGPCProcess_SedS.R** - will take a folder containing raw asc file output from a Shimadzu fluorescence detector, slant correct it, output the slant corrected data as a csv file, and visualize the chromatograms as png files. Accepts an external argument of the path to the directory to be processed.
* **GetMaxFluorescence_SedS.R** - takes folder path containing slant corrected csvs for each chromatogram as external argument, and output file names for overall, HMW, and LMW max fluorescences (in that order). Outputs three csv files with: overall maximum fluorescence, max fluorescence for HMW portion, and max fluorescence for LMW portion (along with factor info about chrom)
* **CalculateStdBins_SedS.R** - from slant corrected csvs of a set of standards of known molecular weight, calculates bins of time within a GPC run corresponding to particular molecular weight to use in rate calculations, and outputs as csv file, as well as a png file visualizing bin cutoffs.
* **FlaRates_SedS.R** - Accepts a directory from which to calculate hydrolysis rates and output file name as external arguments (also requires HydrolysisCutsInfo.csv and FlaTimepointsStdRefs_SedS.csv), and calculates hydrolysis rates for each timepoint within an incubation.
* **FlaProcess_SedS.R** - Accepts the name of the file input containing rates (output from FlaRates_SedS.R) and output file name as external arguments, and adds factor columns, changes particular rate values to zero (using ChangeToZeroKeys.txt, after manual inspection of chromatogram quality), and extracts only the timepoint of maximum activity. Outputs max rates with factors as csv file.
* **PreXFlaRates_SedS.R** - Calculates hydrolysis rates for PreX methods developement experiments. Accepts a directory from which to calculate hydrolysis rates and output file name as external arguments (also requires HydrolysisCutsInfo.csv and FlaTimepointsStdRefs_SedS.csv), and calculates hydrolysis rates for each timepoint within an incubation.
* **PreXFlaProcess_SedS.R** - Accepts the name of the file input containing rates (output from PreXFlaRates_SedS.R) and output file name as external arguments, and adds factor columns, changes particular rate values to zero (using ChangeToZeroKeys.txt, after manual inspection of chromatogram quality), and extracts only the timepoint of maximum activity. Outputs max rates with factors as csv file.
* **FiguresAndAnalysis_SedS.R** - Computes statistics and plots figures presented in the associated publication.
* **SupplementalInfo_SedS.R** - creates SI Fig 1 from publication.

## This repository contains three reference files necessary for running scripts:

* **ChangeToZeroKeys.txt** - Chromatography output is manually inspected for presence or absence of hydrolysis, since low intensities or non-hydrolytic autohydrolysis of fluorophore can occur. This text file contains the samples for which no hydrolysis is observed - hydrolysis rates are set to zero for these samples.
* **FlaTimepointsStdRefs_SedS.csv** - This file contains the sampling times, elapsed time since t0, and which set of GPC standards to use for hydrolysis rate calculations.
* **HydrolysisCutsInfo.csv** - This file contains information about the substrates used in these incubations (its molecular weight, the number of hydrolysis cuts necessary to produce a molecule of the MW of the standards used, etc.), and is used to calculate hydrolysis rates.


## To reproduce rates and figures from the publication:

Clone this repository, and run the shell script do_SedS.sh on the command line:

```
git clone https://github.com/ahoarfrost/SedS.git
cd SedS
sh do_SedS.sh
```
