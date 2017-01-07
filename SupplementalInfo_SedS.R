
############# SI Fig 1  ###############
#example of treatment improvement on chromatogram quality; S4-chon, P13-55cm-lam, P1-55cm-lam
S4_example <- intersect(list.files(path=csvDir,pattern=("S4-385cm")),list.files(path=csvDir,pattern=("chon")))
P13_example <- intersect(list.files(path=csvDir,pattern=("P13-55cm")),list.files(path=csvDir,pattern=("lam")))
P1_example <- intersect(list.files(path=csvDir,pattern=("P1-55cm")),list.files(path=csvDir,pattern=("lam")))
examples <- list("a-Med-S4-385cm-chon"=S4_example,"b-Guaymas-P13-55cm-lam"=P13_example,"c-Guaymas-P1-55cm-lam"=P1_example)
#load in the slant corrected csvs for that example, compile into 32 dataframes with time, fluorescence, sample, and trt columns
#rbind all 32 dataframes into 1
theme_chroms <- theme(panel.grid.major=element_line(color="gray85"),strip.text=element_text(size=6),strip.background=element_rect(fill="grey93"),axis.text=element_text(size=4),axis.title=element_text(size=6),plot.title=element_text(face="bold",size=7))
fig2 <- list()
blankPlot <- ggplot()+geom_blank(aes(1,1))+
    theme(
        plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
    )
for (example in 1:length(examples)) {
    subfig <- c("a","b","c")
    part <- paste("(",subfig[example],")",sep="")
    label <- ggplot() + annotate(geom="text",x=1,y=1,label=part,size=10) + theme(plot.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.border = element_blank(),panel.background = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),axis.line = element_blank())
    df <- data.frame()
    for (file in examples[[example]]) {
        one_sample <- read.csv(paste(csvDir,file,sep=""),header=TRUE)
        one_sample$sample <- rep(sub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-([a-z0-9])-t([0-9]).csv",replacement="\\6-t\\7",file),nrow(one_sample))
        one_sample$treatment <- rep(sub(pattern="([Guaymas|Med]+)-([A-Z0-9]+)-([0-9]+)cm-(trt|notrt)-([a-z]+)-([a-z0-9])-t([0-9]).csv",replacement="\\4",file),nrow(one_sample))
        df <- rbind(df,one_sample)
    }
    plot_title <- substr(names(examples[example]),3,nchar(names(examples[example])))
    plot_title <- gsub("-","_",plot_title)
    figno <- substr(names(examples[example]),1,1)
    figs <- list()
    header <- list(label,blankPlot,blankPlot,blankPlot,blankPlot,blankPlot,blankPlot,blankPlot,blankPlot,blankPlot,blankPlot,blankPlot,blankPlot,blankPlot,blankPlot,blankPlot)
    for (treatment in levels(factor(df$treatment))) {
        treat <- df[df$treatment==treatment,]
        for (sample in levels(factor(treat$sample))) {
            fig_data <- treat[treat$sample==sample,]
            plot_name <- paste(treatment,sample,sep="-")
            entry_name <- paste(plot_title,plot_name,sep="-")
            #make plot
            plot <- ggplot(fig_data,aes(x=time,y=SlantCorrectedReads)) + geom_point(color="blue4",pch=20,size=0.05) + theme_bw() + labs(x="",y="",title=sample) + theme(plot.margin=unit(c(0,0,1,0),"mm"),panel.grid=element_blank(),axis.text=element_text(size=7),axis.title=element_blank(),plot.title=element_text(face="bold",size=7))
            #store plot in list
            figs[[plot_name]] <- plot
            fig2[[entry_name]] <- plot
            header[[plot_name]] <- plot
        }
    }
    png(paste("figures/SIFig1",figno,"_",plot_title,"ChromsComparison.png",sep=""),width=540,height=120,res=900,units="mm")
    #do.call(grid.arrange,c(header,ncol=16,nrow=3, left="mV",sub="time (min)"))
    do.call(grid.arrange,c(header,ncol=16,nrow=3, left="mV",sub="time (min)"))
    dev.off()
}
rl = lapply(list("figures/SIFig1a_Med_S4_385cm_chonChromsComparison.png","figures/SIFig1b_Guaymas_P13_55cm_lamChromsComparison.png","figures/SIFig1c_Guaymas_P1_55cm_lamChromsComparison.png"), readPNG)
gl = lapply(rl, rasterGrob)
png("figures/Fig2_chromcomparison.png",res=900,units="mm") #width=540,height=360,
grid.arrange(gl) #grobs=
dev.off()
