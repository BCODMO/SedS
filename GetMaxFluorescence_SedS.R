#take csv inputs of each slant-corrected chromatogram, 
#output maximum fluorescence intensity for overall chromatogram, for HMW portion (first 1/3 of chrom), and for LMW portion (last 1/3)
#save those three outputs, along with factor info about file came from, as .csv files 

args = commandArgs(trailingOnly=TRUE)

overall_output <- args[2]
HMW_output <- args[3]
LMW_output <- args[4]

#make max intensity df from all the csvs in csvs-for-rates
intensities <- data.frame()
lmw_intensities <- data.frame()
hmw_intensities <- data.frame()
CsvNameList <- list.files(path=args[1],pattern="*.csv")
for (csv in CsvNameList) {
    IncName <- sub("([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9]).csv","\\1-\\2-\\3cm-\\5-\\6-t\\7",csv)
    TrtName <- sub("([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9]).csv","\\4",csv)
    Loc <- sub("([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9]).csv","\\1",csv)
    Site <- sub("([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9]).csv","\\2.\\3cm",csv)
    Substrate <- sub("([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9]).csv","\\5",csv)
    Timepoint <- sub("([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9]).csv","t\\7",csv)
    
    table <- read.csv(paste(args[1],csv,sep="/"),header=TRUE)
    #split table into hmw (first half chromatogram) and lmw (last half)
    hmw <- table[1:(round(nrow(table)/3,0)),]
    lmw <- table[(nrow(table)-round(nrow(table)/3,0)):nrow(table),]
    IntegratedIntensity <- sum(table$SlantCorrectedReads)
    HMWIntensity <- sum(hmw$SlantCorrectedReads)
    LMWIntensity <- sum(lmw$SlantCorrectedReads)
    
    intensities[IncName,"location"] <- Loc
    intensities[IncName,"core.depth"] <- Site
    intensities[IncName,"substrate"] <- Substrate
    intensities[IncName,"timepoint"] <- Timepoint
    intensities[IncName,TrtName] <- IntegratedIntensity
    
    lmw_intensities[IncName,"location"] <- Loc
    lmw_intensities[IncName,"core.depth"] <- Site
    lmw_intensities[IncName,"substrate"] <- Substrate
    lmw_intensities[IncName,"timepoint"] <- Timepoint
    lmw_intensities[IncName,TrtName] <- LMWIntensity
    
    hmw_intensities[IncName,"location"] <- Loc
    hmw_intensities[IncName,"core.depth"] <- Site
    hmw_intensities[IncName,"substrate"] <- Substrate
    hmw_intensities[IncName,"timepoint"] <- Timepoint
    hmw_intensities[IncName,TrtName] <- HMWIntensity
    
}

#save outputs
write.csv(intensities,file=overall_output)
write.csv(hmw_intensities,file=HMW_output)
write.csv(lmw_intensities,file=LMW_output)
