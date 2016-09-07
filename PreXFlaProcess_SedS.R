#Extract factors and add as columns, change key incubation rates to zero, and find max rates
#Save new data frame as FlaMaxRatesFinal_SedS.csv

args = commandArgs(trailingOnly=TRUE)

master <- read.csv(args[1], row.names=1)
zeros <- read.table("ChangeToZeroKeys.txt",sep=",")
output_factors <- args[2]
output_max <- args[3]
time <- read.csv("FlaTimepointsStdRefs_SedS.csv",row.names=1)

##Extract factors as columns
#add location, core number/type, sediment depth, treatment, substrate, timepoint, and elapsed time factor columns
factors <- data.frame(kcrate.live.nM.hr=master$kcrate.live.nM.hr, mean.kcrate.nM.hr=master$mean.kcrate.nM.hr, sd.kcrate.nM.hr=master$sd.kcrate.nM.hr,row.names=row.names(master))
#location
factors$location <- factor(sub(pattern="([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-t([0-9])",replacement="\\1",row.names(factors)))
#core number/type
factors$core <- factor(sub(pattern="([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-t([0-9])",replacement="\\2",row.names(factors)))
#sediment depth
factors$seddepth.cm <- factor(sub(pattern="([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-t([0-9])",replacement="\\3cm",row.names(factors)))
#core.depth
factors$core.depth <- factor(sub(pattern="([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-t([0-9])",replacement="\\2.\\3cm",row.names(factors)))
#treatment
factors$treatment <- factor(sub(pattern="([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-t([0-9])",replacement="\\4",row.names(factors)))
#substrate
factors$substrate <- factor(sub(pattern="([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-t([0-9])",replacement="\\5",row.names(factors)))
#timepoint
factors$timepoint <- factor(sub(pattern="([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-t([0-9])",replacement="t\\6",row.names(factors)))
#elapsed time
row <- gsub(pattern="([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-\\4-\\5-live-t\\6",x=row.names(master))
factors$elapsedtime <- time[row,"elapsed.time"]


##Change key rates to zero (based on looking at chromatograms, some wobbliness in baseline due to 
#low chromatogram quality may translate into a positive rate even though there is no true activity)
for (key in zeros) {
    key <- as.character(key)
    factors[grep(pattern=key,row.names(master)),"mean.kcrate.nM.hr"] <- 0
}

#save zero-corrected factors
write.csv(factors,output_factors,row.names=TRUE)

##Using trt incubations only, find timepoint with maximum rate for each incubation, save that timepoint only in new dataframe maxes
#save as FlaMaxRatesFinal_SedS.csv
maxes <- data.frame()

#loop through each core.depth, then substrate, find max activity timepoint for that substrate, 
#and rbind to maxes matrix
#for one core.depth...
for (slice in levels(factors$core.depth)) {
    #split into separate df with just that incubation
    preX <- factors[factors$core.depth==slice,]
    #and one substrate...
    for (sub in levels(preX$substrate)) {
        #and one treatment...
        preX$treatment <- factor(preX$treatment,levels=as.character(unique(preX$treatment)))
        for (treat in levels(preX$treatment)) {
            subset <- factors[factors$core.depth==slice&factors$substrate==sub&factors$treatment==treat,]
            #...find row with max activity
            m <- max(subset$mean.kcrate.nM.hr)
            maxsub <- subset[subset$mean.kcrate.nM.hr==m,]
            #if >1 row is max (e.g. rates are 0 at every timepoint), use first row
            if(nrow(maxsub)>1) {
                maxsub <- maxsub[1,]
            }
            
            #Add max to FLAmax
            maxes <- rbind(maxes,maxsub)
        }
    }
}

###save as FlaMaxes_DeepDOM.csv
write.csv(maxes,output_max,row.names=TRUE)

