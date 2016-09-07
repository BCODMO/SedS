#SedS labeling scheme: source-core-#cm-trt/notrt-substrate-rep-t#
#e.g. Guaymas-P13-5cm-trt-chon-x-t0 or Med-S5-540cm-notrt-lam-1-t3 or Med-N-365cm-trt-chon-3-t2

args = commandArgs(trailingOnly=TRUE)

path <- getwd()
#name folder in your wd in which have slant corrected data want to calc rates with
CsvDir <- args[1] 
#define substrates used in this experiment (using abbreviations used in file names)
substrates <- c("chon")
#incubation volume for bulk incs in CDEBI is 21mL=0.021L
IncVolume <- 0.021
#a data frame of the monomer-equivalent substrate concentration in incubations for each substrate, in uM
MonEquivSubConc <- data.frame(chon=175,lam=175)

#Read in info about mw of substrates and the # of cuts needed to get to a certain std size bin
cuts <- read.csv("HydrolysisCutsInfo.csv", header=TRUE, row.names=1)
#Read in table of sampling times and the Elapsed Time since t0
FLAElapsed <- read.csv("FlaTimepointsStdRefs_SedS.csv", header=TRUE, row.names=1)
#FLAMaster is the main sheet where you will record all your rates info
outputMaster <- args[2] 
#Read in info about which std bins to use for which samples
StdsForRates <- FLAElapsed[,"std.ref",drop=FALSE]

#check if output file already exists; if does, reads it in; if doesn't, create blank dataframe to store calculated rates in
if(file.exists(outputMaster)==TRUE) {
    FLAMaster <- read.csv(outputMaster,header=TRUE,row.names=1)  
} else {
    FLAMaster <- list()
}

##Make parent lists that divide csvs into useful groups for later 
#make a list of all the csv file names in the CsvDir folder 
CsvNameList <- list.files(path=CsvDir,pattern="*.csv")
RatesMasterList <- list()
IncList <- list()
TimeList <- list()

setwd(paste(path,CsvDir,sep="/"))

for (i in 1:length(CsvNameList)) {
    CsvName <- CsvNameList[i]
    
    PartialName <- sub("([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9]).csv","\\1-\\2-\\3cm-\\4-\\5",CsvName)
    IncName <- sub("([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9]).csv","\\1-\\2-\\3cm-\\4-\\5-\\6",CsvName)
    FullName <- sub("([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9]).csv","\\1-\\2-\\3cm-\\4-\\5-\\6-t\\7",CsvName)
    TimeName <- sub("([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9]).csv","\\1-\\2-\\3cm-\\4-\\5-t\\7",CsvName)
    RatesMasterList[[PartialName]][[FullName]] <- read.csv(CsvName, header=TRUE)
    IncList[[PartialName]][[IncName]][[FullName]] <- read.csv(CsvName,header=TRUE)
    TimeList[[PartialName]][[TimeName]][[FullName]] <- read.csv(CsvName,header=TRUE)
}

setwd(path)
#print out a summary of your lists 
print("RatesMasterList:")
print(summary(RatesMasterList))
print("IncList:")
print(summary(IncList))
print("TimeList:")
print(summary(TimeList))

for (j in 1:length(RatesMasterList)) {
    #Detect which std bins to use for this incubation set
    stdsID <- as.character(StdsForRates[names(RatesMasterList[[j]][1]),])
    #Read in info about cutoffs for std bins from csv with correct stds
    StdBins <- read.csv(paste(stdsID,".csv",sep=""),header=TRUE)
    #Define std bins from StdBins.csv
    kD150 <- StdBins$read.number[1] #Is the read number (e.g. row 145) that bin starts; to the right of this read everything in the '150kD' bin
    kD10 <- StdBins$read.number[2] #to the right of this in '10kD' bin
    kD4 <- StdBins$read.number[3] #to the right of this in '4kD' bin
    kDmon <- StdBins$read.number[4] #to the right of this in 'monomer' bin
    kDfla <- StdBins$read.number[5] #to the right of this in 'FLA' bin                                                2
    
    #Make matrix with 6 rows and correct number columns (depending on number timepoints) named FullName
    BinSums <- matrix(nrow=6, ncol=(length(names(RatesMasterList[[j]]))+1), dimnames=list(c("150kD", "10kD", "4kD", "monomer", "FLA", "total"), c("BinReadStart", names(RatesMasterList[[j]]))))
    #Add bin starts from standards to "BinReadStart" column in matrix
    BinSums[,"BinReadStart"] <- c(kD150,kD10,kD4,kDmon,kDfla, nrow(RatesMasterList[[j]][[1]]))
    
    for (k in 1:length(RatesMasterList[[j]])) {
        reads <- RatesMasterList[[j]][[k]]$SlantCorrectedReads
        #For each FullName dataframe in this list, sum up fluorescence reads within the bins and insert into BinSums matrix in corresponding row
        BinSums[,k+1] <- c(sum(reads[kD150:(kD10-1)]),sum(reads[kD10:(kD4-1)]),sum(reads[kD4:(kDmon-1)]),sum(reads[kDmon:(kDfla-1)]),sum(reads[kDfla:nrow(RatesMasterList[[j]][[k]])]), sum(reads[kD150:nrow(RatesMasterList[[j]][[k]])]))
    }
    
    #Make new matrix, in which will calculate percent of total fluorescence is in each bin
    PercentTotal <- matrix(nrow=5,ncol=ncol(BinSums)-1, dimnames=list(c("%150kD", "%10kD", "%4kD", "%monomer", "%FLA"), c(colnames(BinSums[,2:ncol(BinSums)]))))
    
    #Calculate percent of total fluorescence from BinSums and insert into PercentTotal matrix in appropriate cell
    for (n in 1:ncol(PercentTotal)) {
        PercentTotal[,n] <- c(BinSums[1,n+1]/BinSums[6,n+1], BinSums[2,n+1]/BinSums[6,n+1], BinSums[3,n+1]/BinSums[6,n+1], BinSums[4,n+1]/BinSums[6,n+1], BinSums[5,n+1]/BinSums[6,n+1])
    }
    
    #Make new matrix in which will calculate change from time zero for each sample
    ChangeZero <- matrix(nrow=5,ncol=ncol(PercentTotal), dimnames=list(c("150kD-0","10kD-0","4kD-0","monomer-0","FLA-0"), c(colnames(PercentTotal))))
    
    #remove columns from PercentTotal by IncName (e.g. all columns that start with stn10-d2-chon-1), calculate ChangeZero, insert into correct column in ChangeZero
    for (p in 1:length(IncList[[j]])) {
        #makes new matrix with one incubation set, each column is T0 T1, T2...Tn
        PT <- PercentTotal[,grep(paste(names(IncList[[j]][[p]]),collapse="|"),x=colnames(PercentTotal))]
        #calculate change zero for that incubation, subtract column 1 from all other columns
        CZ <- PT-PT[,1]
        #insert CZ into correct columns in ChangeZero matrix
        ChangeZero[,colnames(CZ)] <- CZ
    }
    
    #Correct any negative values to be zero
    for (t in 1:length(ChangeZero)) { 
        if (ChangeZero[t]<0) {
            ChangeZero[t]=0
        } 
    }
    
    #Calculate number of hydrolysis events in 10kD-FLA bins (excluding 150kD) for each sample (column), using correct hydrolysis cuts and nmol substrate info from cuts.csv read in above; record in new matrix hydrolEventPerBin
    for (aa in substrates) {
        if (grepl(paste("*",aa,sep=""),names(RatesMasterList[j]))==TRUE) {
            u <- cuts[3:6,aa]
            umat <- matrix(rep(u), ncol=ncol(ChangeZero), nrow=4, dimnames=list(c("# cuts 10kD","# cuts 4kD","# cuts monomer","# cuts fla"), c(rep(aa, ncol(ChangeZero)))))
            nmolMonInVial <- MonEquivSubConc[,aa]*1000*IncVolume #nmol of monomer in the incubation
            subMW <- cuts["substrate MW",aa] #molecular weight of the substrate
            monMW <- cuts["dehydrated monomer MW",aa] #molecular weight of substrate's monomeric component
            nmolSub <- nmolMonInVial/(subMW/monMW) #nmol of substrate in the incubation
            hydrolEventPerBin <- matrix(ChangeZero[2:5,]*umat*nmolSub, nrow=4, ncol=ncol(ChangeZero), dimnames=list(c("10kD events","4kD events","monomer events","FLA events"), c(colnames(ChangeZero))))
        }
    }
    
    #Sum hydrolysis events from each bin (sum all rows in each column), new matrix with each column FullName (name of incubation and timepoint) and 1 row of total hydrolysis events 
    hydrolysisEvents <- matrix(data=colSums(hydrolEventPerBin), nrow=1, ncol=ncol(hydrolEventPerBin), dimnames=list(c("#cuts"),c(colnames(hydrolEventPerBin))))
    
    #make matrix for calculating rates
    Rates <- matrix(nrow=1,ncol=ncol(hydrolysisEvents),dimnames=list(c("rate.nM.hr"),c(colnames(hydrolysisEvents))))
    
    #calculate rates from hydrolysis events - retrieve elapsed time from spreadsheet, divide hydrolysisEvents/(timeElapsed*volume incubation)
    for (v in 1:length(colnames(hydrolysisEvents))) {
        timeElapsed <- FLAElapsed[colnames(hydrolysisEvents)[v],"elapsed.time"]
        volumeL <- IncVolume
        Rates[,v] <- hydrolysisEvents[,v]/timeElapsed/volumeL
    }
    
    #if there are any NaN    values in Rates matrix (e.g. at t0 because dividing by 0), change value to 0
    for (w in 1:length(Rates)) { 
        if (is.nan(Rates[w])==TRUE) {
            Rates[w]=0
        } 
    }
    
    #group Rates by timepoint using TimeList, then kill correct and record pre-kc and kc rates in FLAMaster
    for (x in 1:length(TimeList[[j]])) {
        #makes new matrix with one timepoint set, each column is rep1, rep2,...repn, x
        R2 <- Rates[,grep(paste(names(TimeList[[j]][[x]]),collapse="|"),x=colnames(Rates))]
        #if there are multiple x incubations, average rate, make one column for x. If there's only one x incubation, move that value to the first value to make subtracting kill control easier
        y <- R2[grep("-[x]-",x=names(R2),value=TRUE)]
        if (length(y)>1) {
            #mean of all the kills
            ymean <- mean(y)
            #identify position in R2 where the kills are
            yn <- which(names(R2) %in% names(y))
            #remove all the kill columns
            R2 <- R2[-yn]
            #Add the new kill column with the avg ymean in the first column
            R2 <- append(R2,values=c(ymean),after=0)
            #Name the appended column
            names(R2)[1] <- names(y)[1]
        } else {
            #Remove the one kill column, append it to front of vector. 
            yn <- which(names(R2) %in% names(y))
            R2 <- R2[-yn]
            R2 <- append(R2,values=c(y),after=0)
        }
        
        #calculate kill corrected rates by subtracting x rate from all other rates
        kcR2 <- R2 - R2[1]
        
        #If any kill corrected rates <0, turn to zero
        for (z in 1:length(kcR2)) { 
            if (kcR2[z]<0) {
                kcR2[z]=0
            } 
        }
        
        #Add mean and sd values to rates (R2) and kill corrected rates (kcR2)
        R2 <- append(R2,values=c("mean.rate.nM.hr"=mean(R2[2:length(R2)]),"sd.rate.nM.hr"=sd(R2[2:length(R2)])),after=length(R2))
        kcR2 <- append(kcR2,values=c("mean.kcrate.nM.hr"=mean(kcR2[2:length(kcR2)]),"sd.kcrate.nM.hr"=sd(kcR2[2:length(kcR2)])),after=length(kcR2))
        
        #rename columns in R2 and kcR2 to match what want in FLAMaster. takes just the replicate info, e.g. "stn4-d0-lam-2-t2" -> "rate.2.nM.hr"
        names(R2) <- gsub(pattern="([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9])","rate.\\6.nM.hr",names(R2))
        names(kcR2) <- gsub(pattern="([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-zA-Z0-9]+)-([a-z]+)-([0-9a-z]+)-t([0-9])","kcrate.\\6.nM.hr",names(kcR2))
        #append kcR2 to R2, so can insert into FLAMaster 
        finalR <- append(R2,kcR2,after=length(R2))
        
        #Write rates before kill corrected (R2) and kill corrected rates (kcR2 to correct row and columns in FLA Master Sheet)
        if (class(FLAMaster)=="data.frame") {
            FLAMaster[names(TimeList[[j]][x]),names(finalR)] <- finalR
            #make sure colnames of FLAMaster match colnames finalR. If don't, throw a warning
            if (all(names(finalR) %in% colnames(FLAMaster)==TRUE)==FALSE) {
                print("WARNING: final rates trying to insert do not match destination column names. Check your column names match script formatting.")
            }
        } else {
            FLAMaster[[names(TimeList[[j]][x])]] <- finalR
            FLAMaster <- do.call("rbind",FLAMaster)
            FLAMaster <- as.data.frame(FLAMaster)
        }
        
        print(paste("...Finished Processing",names(TimeList[[j]][x])))
        }
    
    #save the new FLAMaster with your calculated rates to a csv file in your wd named as defined by outputMaster
    write.csv(FLAMaster,outputMaster,quote=FALSE)
    
}


