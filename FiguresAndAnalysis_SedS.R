#figures and analysis for SedS

packages <- c("ggplot2","RColorBrewer","grDevices","reshape2","grid","gridExtra")
to_install <- packages[!packages %in% installed.packages()]
if (length(to_install)>0) {
    lapply(to_install,install.packages, repos="http://cran.rstudio.com/") 
}
lapply(packages,library,character.only=TRUE)

#create figures folder if it doesn't already exist
if (file.exists("figures")) {
} else {
    dir.create("figures")
}

#read in files needed to generate figures
maxes <- read.csv("FlaMaxRatesFinal_SedS.csv",header=TRUE,row.names=1)
intensities <- read.csv("IntegratedFluorescence_overall_SedS.csv",header=TRUE)
lmw_intensities <- read.csv("IntegratedFluorescence_LMW_SedS.csv",header=TRUE)
hmw_intensities <- read.csv("IntegratedFluorescence_HMW_SedS.csv",header=TRUE)
prex_intensities <- read.csv("PreX_IntegratedFluorescence_overall_SedS.csv",header=TRUE)
prex_lmw_intensities <- read.csv("PreX_IntegratedFluorescence_LMW_SedS.csv",header=TRUE)
prex_hmw_intensities <- read.csv("PreX_IntegratedFluorescence_HMW_SedS.csv",header=TRUE)
time <- read.csv("FlaTimepointsStdRefs_SedS.csv",header=TRUE,row.names=1)

#adjust column names, factor level orders
colnames(intensities)[1] <- "Inc"
colnames(lmw_intensities)[1] <- "Inc"
colnames(hmw_intensities)[1] <- "Inc"
colnames(prex_intensities)[1] <- "Inc"
colnames(prex_lmw_intensities)[1] <- "Inc"
colnames(prex_hmw_intensities)[1] <- "Inc"
intensities$core.depth <- factor(intensities$core.depth,levels=c("P1.5cm","P1.55cm","P3.5cm","P3.55cm","P5.5cm","P5.55cm","P8.5cm","P8.55cm","P10.5cm","P10.55cm","P13.5cm","P13.55cm","N.365cm","S4.385cm","N.440cm","S5.470cm","N.575cm","S7.590cm"))
lmw_intensities$core.depth <- factor(lmw_intensities$core.depth,levels=c("P1.5cm","P1.55cm","P3.5cm","P3.55cm","P5.5cm","P5.55cm","P8.5cm","P8.55cm","P10.5cm","P10.55cm","P13.5cm","P13.55cm","N.365cm","S4.385cm","N.440cm","S5.470cm","N.575cm","S7.590cm"))
hmw_intensities$core.depth <- factor(hmw_intensities$core.depth,levels=c("P1.5cm","P1.55cm","P3.5cm","P3.55cm","P5.5cm","P5.55cm","P8.5cm","P8.55cm","P10.5cm","P10.55cm","P13.5cm","P13.55cm","N.365cm","S4.385cm","N.440cm","S5.470cm","N.575cm","S7.590cm"))
maxes$core.depth <- factor(maxes$core.depth,levels=c("P1.5cm","P1.55cm","P3.5cm","P3.55cm","P5.5cm","P5.55cm","P8.5cm","P8.55cm","P10.5cm","P10.55cm","P13.5cm","P13.55cm","N.365cm","S4.385cm","N.440cm","S5.470cm","N.575cm","S7.590cm"))
maxes$seddepth.cm <- factor(maxes$seddepth.cm,levels=c("5cm","55cm","365cm","385cm","440cm","470cm","575cm","590cm"))

#define color palette
colors <- brewer.pal("Set2",n=8)
SubstrateColors <- colors[1:2] 
TreatColors <- colors[3:4]
LocationColors <- colors[5:6]
MWColors <- colors[7:8]
#define PreX color palette
#prex1TrtColors are based off of brewer.pal("RdYlGn",n=3), but first color is replaced to be consisted with prex2TrtColors
prex1TrtColors <- c(colors[3],"#FEE8C8","#FFFFBF","#91CF60")
prex2TrtColors <- c(colors[3],brewer.pal("OrRd",n=3))
prex3TrtColors <- c(colors[3],"#E34A33",colors[4])

############# Figure 2  ###############
#example of treatment improvement on chromatogram quality; S4-chon, P13-55cm-lam, P1-55cm-lam
#chon_example <- row.names(maxes)[grep("S4-385cm-trt-chon",row.names(maxes))]
#lam_example <- row.names(maxes)[grep("P13-55cm-trt-lam",row.names(maxes))]
#define chon chromatograms to compile
#cnotrt_x <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-notrt-\\5-x-t\\6.png",chon_example),sep="")
#cnotrt_1 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-notrt-\\5-1-t\\6.png",chon_example),sep="")
#cnotrt_2 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-notrt-\\5-2-t\\6.png",chon_example),sep="")
#cnotrt_3 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-notrt-\\5-3-t\\6.png",chon_example),sep="")
#ctrt_x <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-trt-\\5-x-t\\6.png",chon_example),sep="")
#ctrt_1 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-trt-\\5-1-t\\6.png",chon_example),sep="")
#ctrt_2 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-trt-\\5-2-t\\6.png",chon_example),sep="")
#ctrt_3 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-trt-\\5-3-t\\6.png",chon_example),sep="")
#chon_paths <- c(cnotrt_x,ctrt_x,cnotrt_1,ctrt_1,cnotrt_2,ctrt_2,cnotrt_3,ctrt_3)
##read in all the chon pngs
#crl = lapply(chon_paths, readPNG)
#cgl = lapply(crl, rasterGrob)
#cgl2 <- append(cgl,list(ncol=2),0)
#define lam chromatograms to compile
#lnotrt_x <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-notrt-\\5-x-t\\6.png",lam_example),sep="")
#lnotrt_1 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-notrt-\\5-1-t\\6.png",lam_example),sep="")
#lnotrt_2 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-notrt-\\5-2-t\\6.png",lam_example),sep="")
#lnotrt_3 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-notrt-\\5-3-t\\6.png",lam_example),sep="")
#ltrt_x <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-trt-\\5-x-t\\6.png",lam_example),sep="")
#ltrt_1 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-trt-\\5-1-t\\6.png",lam_example),sep="")
#ltrt_2 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-trt-\\5-2-t\\6.png",lam_example),sep="")
#ltrt_3 <- paste("CoreComparison-chroms-raw-data-png/",gsub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])",replacement="\\1-\\2-\\3cm-trt-\\5-3-t\\6.png",lam_example),sep="")
#lam_paths <- c(lnotrt_x,ltrt_x,lnotrt_1,ltrt_1,lnotrt_2,ltrt_2,lnotrt_3,ltrt_3)
##read in all the lam pngs
#lrl = lapply(lam_paths, readPNG)
#lgl = lapply(lrl, rasterGrob)
#lgl2 <- append(lgl,list(ncol=2),0)

#fig 9a, chon example
#png("figures/Fig9a_ChonChromsTreatmentComparison.png",width=85,height=140,res=900,units="mm")
#do.call(grid.arrange,cgl2)
#dev.off()
#fig 9b, lam example
#png("figures/Fig9b_LamChromsTreatmentComparison.png",width=85,height=140,res=900,units="mm")
#do.call(grid.arrange,lgl2)
#dev.off()

###################### Figure 3 ########################
##difference in maximum fluorescence intensity notrt/trt, and percent improvement 
intensities$PercentImprovement <- ((intensities$trt - intensities$notrt)/intensities$notrt)*100
intensities$Diff <- intensities$trt-intensities$notrt

lmw_intensities$PercentImprovement <- ((lmw_intensities$trt - lmw_intensities$notrt)/lmw_intensities$notrt)*100
lmw_intensities$Diff <- lmw_intensities$trt-lmw_intensities$notrt

hmw_intensities$PercentImprovement <- ((hmw_intensities$trt - hmw_intensities$notrt)/hmw_intensities$notrt)*100
hmw_intensities$Diff <- hmw_intensities$trt-hmw_intensities$notrt

intensities_complete <- intensities[complete.cases(intensities)==TRUE,] 
intensities_test <- t.test(intensities_complete$trt,intensities_complete$notrt,paired=TRUE,alt="greater")
improvement_test <- t.test(intensities_complete$PercentImprovement,alternative="greater")

print("Result of t-test, percent improvement in fluorescence intensity from notrt to trt, Fig 2a:")
print(improvement_test)
print(paste("median percent improvement in fluorescence intensity:",median(intensities_complete$PercentImprovement)))
print("Result of paired t-test, change in integrated fluorescence intensity from notrt to trt (in raw FUs), Fig 2b:")
print(intensities_test)
print(paste("median change in fluorescence intensity (FUs):",median(intensities_complete$Diff)))
print("--------------------------")

intensities_melt <- melt(intensities_complete,id.vars=c("Inc","location","core.depth","substrate","timepoint"))

##Fig 3a
percentFl <- intensities_melt[intensities_melt$variable=="PercentImprovement",]
png("figures/Fig3a_PercentImprovementFluorescence.png",width=85,height=90,units="mm",res=900)
ggplot(percentFl,aes(x=variable,y=value)) + geom_boxplot(width=0.5) + theme_bw() + theme(axis.text.x=element_blank(),axis.title.y=element_text(vjust=1)) + labs(x="",y="Improvement in Fluorescence Intensity (%)") + coord_cartesian(ylim=c(0,650)) + scale_y_continuous(breaks=c(0,50,100,200,400,600))
dev.off()

#max fluorescence intensity between notrt/trt for hmw and lmw portions (first third and last third of chromatogram)
lmw_intensities_complete <- lmw_intensities[complete.cases(lmw_intensities),]
lmw_intensities_melt <- melt(lmw_intensities_complete,id.vars=c("Inc","location","core.depth","substrate","timepoint"))
percentLmw <- lmw_intensities_melt[lmw_intensities_melt$variable=="PercentImprovement",]

hmw_intensities_complete <- hmw_intensities[complete.cases(hmw_intensities),]
hmw_intensities_melt <- melt(hmw_intensities_complete,id.vars=c("Inc","location","core.depth","substrate","timepoint"))
percentHmw <- hmw_intensities_melt[hmw_intensities_melt$variable=="PercentImprovement",]
percentHmw$MW <- "high"
percentLmw$MW <- "low"
percentMW <- rbind(percentLmw,percentHmw)
Hmw_test <- t.test(percentHmw$value,alternative="greater")
Lmw_test <- t.test(percentLmw$value,alternative="greater")
print("Result of t-test, percent improvement in integrated fluorescence intensity for high molecular weight products (Fig3):")
print(Hmw_test)
print(paste("median % improvement in fluorescence intensity for HMW:",median(percentHmw$value)))
print("Result of t-test, percent improvement in peak fluorescence intensity for lmw molecular weight products (Fig3):")
print(Lmw_test)
print(paste("median % improvement in fluorescence intensity for LMW:",median(percentLmw$value)))
print("-----------------------------")

### Fig 3b
png("figures/Fig3b_PercentImprovementHMWvsLMW.png",width=85,height=75,units="mm",res=900)
ggplot(percentMW,aes(x=MW,y=value)) + geom_boxplot(width=0.5,aes(fill=MW)) + scale_fill_manual(values=MWColors) + theme_bw() + theme(axis.title.y=element_text(vjust=1,size=10),axis.title.x=element_text(vjust=-0.4,size=10),axis.text=element_text(size=7),legend.position="none") + labs(x="Molecular Weight",y="Improvement in Fluorescence Intensity (%)") + coord_cartesian(ylim=c(0,1000)) 
dev.off()

#difference in maximum hydrolysis rate between notrt/trt
#uses maxes 
maxes_boxplot <- data.frame()
for (row in row.names(maxes)) {
    IncName <- sub("([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])","\\1-\\2-\\3cm-\\5",row)
    TrtName <- sub("([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])","\\4",row)
    Loc <- sub("([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-t([0-9])","\\1",row)
    MaxRate <- maxes[row,"mean.kcrate.nM.hr"]
    maxes_boxplot[IncName,TrtName] <- MaxRate
    maxes_boxplot[IncName,"location"] <- Loc
}
maxes_boxplot$Inc <- row.names(maxes_boxplot)
maxes_boxplot_melt <- melt(maxes_boxplot,id.vars=c("location","Inc"))
maxes_boxplot$Diff <- maxes_boxplot$trt-maxes_boxplot$notrt
max_test <- t.test(maxes_boxplot$trt,maxes_boxplot$notrt,paired=TRUE,alternative="less")
print("Result of paired t-test, difference in calculated maximum hydrolysis rate notrt vs. trt, Fig 4:")
print(max_test)
print(paste("median decrease in maximum hydrolysis rate with treatment:",median(maxes_boxplot$Diff)))
print("----------------------------")

##Fig 3c
png("figures/Fig3c_DecreaseMaxHydrolRateNotrtTrt.png",width=85,height=80,units="mm",res=900)
ggplot(maxes_boxplot_melt,aes(x=variable,y=value)) + geom_boxplot(fill=TreatColors) + geom_line(color="grey25",alpha=0.2,size=0.25,aes(group=Inc)) + theme_bw() + labs(x="",y="Max Hydrolysis Rate (nM/hr)") + scale_x_discrete(labels=c("no treatment","competitive desorption + SDS")) +  theme(axis.title.y=element_text(vjust=1,size=10),axis.title.x=element_text(vjust=-0.4,size=10),axis.text=element_text(size=7)) + coord_cartesian(ylim=c(0,250))
dev.off()


###################### Figure 4 ########################
#uses maxes
#errorbar
errorbars <- geom_errorbar(aes(ymin=mean.kcrate.nM.hr-sd.kcrate.nM.hr,ymax=mean.kcrate.nM.hr+sd.kcrate.nM.hr),width=0.4,color="grey25",alpha=0.7,size=0.3)
theme_barplot <- theme_bw() + theme(axis.text.y=element_blank(),strip.background=element_rect(fill="grey93"))
guaymas.trt <- maxes[maxes$location=="Guaymas" & maxes$treatment=="trt",]
med.trt <- maxes[maxes$location=="Med" & maxes$treatment=="trt",]

## Fig 4a - horizontal barplot Guaymas 
png("figures/Fig4a_GuaymasMaxRatesBarplot.png",width=180,height=50,units="mm",res=900)
ggplot(guaymas.trt,aes(x=substrate,y=mean.kcrate.nM.hr)) + geom_bar(stat="identity",aes(fill=substrate)) + facet_grid(seddepth.cm~core) + errorbars + theme_barplot + scale_fill_manual(values=SubstrateColors) + coord_flip() + scale_y_continuous(breaks=c(0,100,200)) + labs(y="hydrolysis rate nM/hr",x="")
dev.off()

## Fig 4b - barplot Med
png("figures/Fig4b_MedMaxRatesBarplot.png",width=85,height=150,units="mm",res=900)
ggplot(med.trt,aes(x=substrate,y=mean.kcrate.nM.hr)) + geom_bar(stat="identity",aes(fill=substrate)) + facet_grid(core.depth~.) + errorbars + theme_barplot + scale_fill_manual(values=SubstrateColors) + coord_flip() + scale_y_continuous(breaks=c(0,10,20,30)) + coord_flip(ylim=c(0,40)) + labs(y="hydrolysis rate nM/hr",x="")
dev.off()


###################### Figure 5 ########################
#uses percentFl defined in fig 3a
theme_fig5a <- theme_bw() + theme(strip.background=element_rect(fill="grey93"),axis.text.y=element_text(size=6),axis.text.x=element_blank(),axis.title=element_text(size=7),strip.text=element_text(size=5,face="bold"),legend.position="none")
png("figures/Fig5a_PercentImprovementByLocation.png",width=180,height=48,units="mm",res=900)
ggplot(percentFl,aes(x=variable,y=value)) + geom_boxplot(width=0.5,size=0.3,outlier.size=1,aes(fill=location)) + theme_fig5a + labs(x="Site (Core.Depth)",y="Improvement in Fluorescence Intensity (%)") + coord_cartesian(ylim=c(0,750)) + scale_y_continuous(breaks=c(0,100,300,500,700)) + facet_grid(.~core.depth) + scale_fill_manual(values=LocationColors)
dev.off()

site_tests <- data.frame()
for (level in levels(percentFl$core.depth)) {
    site <- percentFl[percentFl$core.depth==level,]
    test <- t.test(site$value)
    pvalue <- round(test$p.value,3)
    estimate <- round(mean(site$value),1)
    median <- round(median(site$value),1)
    site_tests[level,"MedianImprovement"] <- median
    site_tests[level,"MeanImprovement"] <- estimate
    site_tests[level,"P"] <- pvalue
}
print("mean and median % improvement in integrated fluorescence at each site:")
print("(P-values not corrected for multiple testing, outliers not removed)")
print(site_tests)
print("----------------------------")

theme_fig5b <- theme_bw() + theme(axis.title=element_text(size=9,vjust=1),axis.text=element_text(size=7),legend.position="none")
png("figures/Fig5b_PercentImprovementBySubstrate.png",width=85,height=75,units="mm",res=900)
ggplot(percentFl,aes(x=substrate,y=value)) + geom_boxplot(aes(fill=substrate)) + theme_fig5b + labs(x="",y="Improvement in Fluorescence Intensity (%)") + coord_cartesian(ylim=c(-10,500)) + scale_fill_manual(values=SubstrateColors)
dev.off()

lam <- percentFl[percentFl$substrate=="lam",]
lam_test <- t.test(lam$value)
chon <- percentFl[percentFl$substrate=="chon",]
chon_test <- t.test(chon$value)
print("median % improvement in fluorescence for laminarin:")
print(paste(round(median(lam$value),2),"; (P=",round(lam_test$p.value,3),")"))
print("median % improvement in fluorescence for chondroitin:")
print(paste(round(median(chon$value),2),"; (P=",round(chon_test$p.value,3),")"))
print("-------------------------------")



###################### Figure 6 ########################
#define prex1, prex2, prex3
prex1 <- prex_intensities[grep("PreX1",prex_intensities$Inc),]
prex1 <- prex1[,colSums(is.na(prex1))==0]
prex1_melt <- melt(prex1,id.vars=c("Inc","location","core.depth","substrate","timepoint"))
#reorder factor levels
prex1_melt$variable <- factor(prex1_melt$variable,levels=c("notrt","CD","pH10","pH11"))

prex2 <- prex_intensities[grep("PreX2",prex_intensities$Inc),]
prex2 <- prex2[,colSums(is.na(prex2))==0]
prex2_melt <- melt(prex2,id.vars=c("Inc","location","core.depth","substrate","timepoint"))
#reorder treatment factor order
prex2_melt$variable <- factor(prex2_melt$variable,levels=c("notrt","X700","X1400","X2800"))

prex3 <- prex_intensities[grep("PreX3",prex_intensities$Inc),]
prex3 <- prex3[,colSums(is.na(prex3))==0]
prex3_melt <- melt(prex3,id.vars=c("Inc","location","core.depth","substrate","timepoint"))

theme_fig6 <- theme_bw() + theme(legend.position="none",axis.title=element_text(size=8,vjust=1),axis.text=element_text(size=5))
#Fig 6a, prex1
png("figures/Fig6a_PreX1Fluor.png",width=85,height=75,units="mm",res=900)
ggplot(prex1_melt,aes(x=variable,y=value)) + geom_boxplot(width=0.8,size=0.3,outlier.size=1,aes(fill=variable)) + scale_fill_manual(values=prex1TrtColors) + scale_x_discrete(labels=c("no treatment","competitive desorption","pH10","pH11")) + theme_fig6 + labs(x="",y="Integrated Fluorescence (FUs)")
dev.off()

#Fig 6b, prex2
png("figures/Fig6b_PreX2Fluor.png",width=85,height=75,units="mm",res=900)
ggplot(prex2_melt,aes(x=variable,y=value)) + geom_boxplot(width=0.8,size=0.3,outlier.size=1,aes(fill=variable)) + scale_fill_manual(values=prex2TrtColors) + scale_x_discrete(labels=c("no treatment","700uM","1400uM","2800uM")) + theme_fig6 + labs(x="",y="Integrated Fluorescence (FUs)") + coord_cartesian(ylim=c(400000,1100000))
dev.off()

#Fig 6c, prex3
png("figures/Fig6c_PreX3Fluor.png",width=85,height=75,units="mm",res=900)
ggplot(prex3_melt,aes(x=variable,y=value)) + geom_boxplot(width=0.8,size=0.3,outlier.size=1,aes(fill=variable)) + scale_fill_manual(values=prex3TrtColors) + scale_x_discrete(labels=c("no treatment","2800 -SDS","2800 +SDS")) + theme_fig6 + labs(x="",y="Integrated Fluorescence (FUs)")
dev.off()

prex1_test <- t.test((((prex1$CD-prex1$notrt)/prex1$notrt)*100),alternative="greater")
print("median % increase in peak fluorescence using competitive desorption in PreX1:")
print(paste(round(median(((prex1$CD-prex1$notrt)/prex1$notrt)*100),1),"; P=",round(prex1_test$p.value,3)))

prex2_test <- t.test((((prex2$X2800-prex2$notrt)/prex2$notrt)*100),alternative="greater")
test1400 <- t.test((((prex2$X1400-prex2$notrt)/prex2$notrt)*100),alternative="greater")
test700 <- t.test((((prex2$X700-prex2$notrt)/prex2$notrt)*100),alternative="greater")
print("average % increase in integrated fluorescence using 2800uM in PreX2:")
print(paste("mean:",round(as.numeric(prex2_test$estimate),1),"; median:",round(median((((prex2$X2800-prex2$notrt)/prex2$notrt)*100)),1)," ; P=",round(prex2_test$p.value,3)))
print("average % increase in integrated fluorescence using 1400uM in PreX2:")
print(paste("mean:",round(as.numeric(test1400$estimate),1),"; median:",round(median((((prex2$X1400-prex2$notrt)/prex2$notrt)*100)),1)," ; P=",round(test1400$p.value,3)))
print("average % increase in integrated fluorescence using 700uM in PreX2:")
print(paste("mean:",round(as.numeric(test700$estimate),1),"; median:",round(median((((prex2$X700-prex2$notrt)/prex2$notrt)*100)),1)," ; P=",round(test700$p.value,3)))

prex3_test <- t.test((((prex3$sds-prex3$notrt)/prex3$notrt)*100),alternative="greater")
print("median % increase in peak fluorescence units using SDS in PreX3:")
print(paste(round(median(((prex3$sds-prex3$notrt)/prex3$notrt)*100),1),"; P=",round(prex3_test$p.value,3)))

 
############## Discussion of any effect of the waterbath step on treatment rates ###############
#rates of increase in LMW portions of chromatograms
IncName <- sub("([Guaymas|Med|Marmara]+)-([A-Za-z0-9]+)-([0-9]+)cm-([a-z]+)-([0-9a-z]+)-t([0-9])","\\1-\\2-\\3cm-\\4-\\5",lmw_intensities_complete$Inc)
slopes <- data.frame()
for (inc in 1:length(levels(as.factor(IncName)))) {
    name <- levels(as.factor(IncName))[inc]
    one_inc <- lmw_intensities_complete[grep(levels(as.factor(IncName))[inc],lmw_intensities_complete$Inc),]
    if(nrow(one_inc)==3) {
        one_inc$time <- c(0,504,1512)
    } else {
        one_inc$time <- c(0,504,1008,1512)        
    }
    one_inc$slope_trt <- (one_inc$trt-one_inc$trt[1])/one_inc$time
    one_inc$slope_notrt <- (one_inc$notrt-one_inc$notrt[1])/one_inc$time
    notrt <- mean(one_inc[2:nrow(one_inc),"slope_notrt"])
    trt <- mean(one_inc[2:nrow(one_inc),"slope_trt"])
    slopes[name,"slope_trt"] <- trt
    slopes[name,"slope_notrt"] <- notrt
}
slopes$Inc <- row.names(slopes)
slopes_melt <- melt(slopes,id.vars=c("Inc"))
lower_bound_trt <- quantile(slopes$slope_trt)[2] - 1.5*(quantile(slopes$slope_trt)[4] - quantile(slopes$slope_trt)[2])
upper_bound_trt <- quantile(slopes$slope_trt)[4] + 1.5*(quantile(slopes$slope_trt)[4] - quantile(slopes$slope_trt)[2])
slopes_outliers <- slopes[slopes$slope_trt>lower_bound_trt&slopes$slope_trt<upper_bound_trt,]
lower_bound_notrt <- quantile(slopes$slope_notrt)[2] - 1.5*(quantile(slopes$slope_notrt)[4] - quantile(slopes$slope_notrt)[2])
upper_bound_notrt <- quantile(slopes$slope_notrt)[4] + 1.5*(quantile(slopes$slope_notrt)[4] - quantile(slopes$slope_notrt)[2])
slopes_outliers$slope_notrt[slopes_outliers$slope_notrt<lower_bound_notrt|slopes_outliers$slope_notrt>upper_bound_notrt] <- NA
slopes_outliers <- slopes_outliers[complete.cases(slopes_outliers),]
slopes_outliers_melt <- melt(slopes_outliers,id.vars=c("Inc"))
#ggplot(slopes_outliers_melt,aes(x=variable,y=value)) + geom_boxplot() + geom_line(aes(group=Inc))
rate_test <- t.test(slopes_outliers$slope_trt,slopes_outliers$slope_notrt,paired=TRUE)
print("Rate of increase in LMW portion of chromatogram is the same between trt and no-trt")
print(rate_test)
print("-----------------------------")
