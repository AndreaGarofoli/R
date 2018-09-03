
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)


gctf = read.table("/Users/andrea/Documents/BI/BI/B/panel/all.interval_amplicon_metrics_FROZEN_norma.txt", header = T, sep="\t")
gctp = read.table("/Users/andrea/Documents/BI/BI/B/panel/all.interval_amplicon_metrics_FFPE_norma.txt", header = T, sep="\t")
gctmf = read.table("/Users/andrea/Documents/BI/BI/B/panel/all.interval_amplicon_metrics_FROZEN_mean.txt", header = T, sep="\t")
gctmp = read.table("/Users/andrea/Documents/BI/BI/B/panel/all.interval_amplicon_metrics_FFPE_mean.txt", header = T, sep="\t")
amp = read.table("/Users/andrea/Documents/BI/BI/B/panel/all.amplicon_metrics_ppl_nlab.txt", header = T, sep="\t")
ampo = read.table("/Users/andrea/Documents/BI/BI/B/panel/all.amplicon_metrics_ppl_pool.txt", header = T, sep="\t")
bed = read.table("/Users/andrea/Documents/BI/BI/B/panel/file/AmpliSeqHCC_WG_IAD97706_v2.20160511.designed.bed", header = F, sep="\t")
lin = read.table("/Users/andrea/Documents/BI/BI/B/panel/line.txt", header = T, sep="\t")

write.table(dat_m2, file = "/Users/andrea/Documents/BI/BI/B/panel/templine.txt", quote = FALSE, sep = "\t", row.names = FALSE)


# histplot distribuzione lunghezze ampliconi
###################                  --------  2A  ---------

# senza outliner

# par(mar=c(4.3, 4, 1.1, 1.1), xpd=FALSE, xaxs = "i",yaxs = "i",mfrow=c(1,1))
# hist(as.numeric(bed$V8), xlim=c(60,180), ylim=c(0,100), col="grey 70", 
#      #main="Distribution of lengths", 
#      xlab= "Length", family = "Arial", font.lab=2 , breaks=seq(60,260,by=1), main="")
# grid(lwd=1.4)
# hist(as.numeric(bed$V8), xlim=c(60,180), ylim=c(0,100), col="grey 70", 
#      #main="Distribution of lengths", 
#      xlab= "Length", family = "Arial", font.lab=2 , breaks=seq(60,260,by=1), add = TRUE, main="")


# con outliner, può spaziar male

split.screen(matrix(c(0,   .95,  .7, 1,  0,  0,  1,  1), ncol=4))
screen(1)
par(mar=c(4.3, 4, 1.1, 4.1), xpd=FALSE, xaxs = "i",yaxs = "i",mfrow=c(1,1))
hist(as.numeric(bed$V8), xlim=c(60,180), ylim=c(0,100), col="grey 70", 
     #main="Distribution of lengths", 
     xlab= "Amplicon Length", family = "Arial", font.lab=2 , breaks=seq(60,260,by=1), main="")
grid(lwd=1.4)
hist(as.numeric(bed$V8), xlim=c(60,180), ylim=c(0,100), col="grey 70", 
     #main="Distribution of lengths", 
     xlab= "Amplicon Length", family = "Arial", font.lab=2 , breaks=seq(60,260,by=1), add = TRUE, main="")
screen(2)
par(mar=c(4.3, 23.8, 1.1, 1.1), xpd=FALSE, xaxs = "i",yaxs = "i",mfrow=c(1,1))
hist(as.numeric(bed$V8), xlim=c(240,260), ylim=c(0,100), col="grey 70", 
     #main="Distribution of lengths", 
     xlab= "", family = "Arial", font.lab=2 , breaks=seq(60,260,by=1), main="", yaxt='n', ann=FALSE)
grid(lwd=1.4, ny = 5, nx = 2)
hist(as.numeric(bed$V8), xlim=c(240,260), ylim=c(0,100), col="grey 70", 
     #main="Distribution of lengths", 
     xlab= "", family = "Arial", font.lab=2 , breaks=seq(60,260,by=1), add = TRUE, main="", yaxt='n', ann=FALSE)
#axis(side=1, labels=seq(250))
close.screen(all = TRUE)


# violin boxplot MEAN_AMPLICON_COVERAGE senza pool  

###################                  --------  2B  ---------

amp = read.table("/Users/andrea/Documents/BI/BI/B/panel/all.amplicon_metrics_ppl_nlab.txt", header = T, sep="\t")
amp$TYPE<- factor(amp$TYPE, levels = c("Frozen_Non-Tumor","Frozen_Tumor","FFPE_Non-Tumor","FFPE_Tumor"))


ggplot(amp, aes( y=MEAN_AMPLICON_COVERAGE , x=TYPE, fill=TYPE
)) + geom_violin() + #geom_boxplot(notch = TRUE)+  #+ geom_smooth(method=lm, se=FALSE)
  geom_point(aes(y=MEAN_AMPLICON_COVERAGE , x=TYPE), color = "black", size = 1, data = amp) + 
  labs(fill = "Type",x = "", y = "Mean Amplicon Coverage") + scale_shape(solid = FALSE) + #+guides(shape=FALSE)+
  scale_fill_manual(values=c("grey70", "grey90","grey45", "grey55") ,
                    labels=c("Frozen\nNon-Tumor","Frozen\nTumor", "FFPE\nNon-Tumor","FFPE\nTumor")) +
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "#d3d3d3", linetype = 1), panel.grid.minor = element_line(colour="#d3d3d3", size=0.1, linetype = 1),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Arial", face = "bold"),
        text=element_text(family="Arial"),
        axis.title = element_text(face="bold"),
        axis.text.y=element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black"),
        # axis.text.x=element_blank(),
        legend.title=element_blank()) +
  scale_colour_manual(values=c("grey50", "grey30")) +
  scale_y_continuous(limits = c(0, 2515), expand = c(0, 0),breaks=c(0,500,1000,1500,2000,2500,3000,3500))+ 
  guides(fill=guide_legend( keyheight=1, default.unit="cm"))+ 
  scale_x_discrete(labels=c("Frozen\nNon-Tumor","Frozen\nTumor", "FFPE\nNon-Tumor","FFPE\nTumor"))




### boxplot Coverage Depth no pools 2C
###################                  --------  2C  ---------


lin = read.table("/Users/andrea/Documents/BI/BI/B/panel/line.txt", header = T, sep="\t")
dat_m2 <- melt(amp, id.vars = "TYPE",
               measure.vars = grep("^PCT_TARGET_BASES_", names(lin), value = TRUE))

ggplot(dat_m2, aes(fill=TYPE,x=variable, y=value)) +
  geom_boxplot()+
  scale_x_discrete(labels=c("1x","2x","10x","20x","30x")) +
  labs(colour = "Type",x = "Coverage Depth", y = "Percentages")+
  # scale_color_manual(values=c("#4169E1", "#B22222")) +
  scale_y_continuous(labels=percent, expand = c(0, 0),limits = c(0.9, 1)) + theme_light()+
  scale_fill_manual(values=c("grey70", "grey90","grey45", "grey55"),
                    labels=c("Frozen\nNon-Tumor","Frozen\nTumor", "FFPE\nNon-Tumor","FFPE\nTumor"))+   
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_line(colour="#d3d3d3", size=0.09),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Arial", face = "bold"),
        text=element_text(family="Arial"),
        axis.title = element_text(face="bold"),
        axis.text.y=element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black"),
        #axis.text.x=element_blank(),
        legend.title=element_blank())+
  guides(fill=guide_legend( keyheight=1.5, default.unit="cm"))
#box(col = 'black')


# violin brutto

# dat_m2 <- melt(amp, id.vars = "TYPE",
#                measure.vars = grep("^PCT_TARGET_BASES_", names(lin), value = TRUE))
# ggplot(dat_m2, aes(fill=TYPE,x=variable, y=value)) +
#   geom_violin(scale="width")+
#   scale_x_discrete(labels=c("1x","2x","10x","20x","30x")) +
#   labs(colour = "Type",x = "", y = "Percentages")+
#   # scale_color_manual(values=c("#4169E1", "#B22222")) +
#   scale_y_continuous(labels=percent, expand = c(0, 0),limits = c(0.9, 1)) + theme_light()+
#   scale_fill_manual(values=c("grey70", "grey90","grey45", "grey55"),
#                     labels=c( "Frozen\nNon-Tumor","Frozen\nTumor","FFPE\nNon-Tumor","FFPE\nTumor"))+   
#   theme(panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_line(colour="#d3d3d3", size=0.09),
#         panel.border = element_blank(), panel.background = element_blank(),
#         plot.title = element_text(size = 14, family = "Arial", face = "bold"),
#         text=element_text(family="Arial"),
#         axis.title = element_text(face="bold"),
#         axis.text.y=element_text(colour="black", size = 9),
#         axis.line = element_line(size=0.5, colour = "black"),
#         legend.title=element_blank())+
#   guides(fill=guide_legend( keyheight=1.5, default.unit="cm"))


# boxplot jitter

###################                  --------  2D  ---------


amp = read.table("/Users/andrea/Documents/BI/BI/B/panel/tot_deep.txt", header = T, sep="\t")
amp$TYPE<- factor(amp$TYPE, levels = c("Frozen","FFPE"))
ggplot(amp, aes( y=perc/100 , x=TYPE    
)) + geom_boxplot(aes(fill=TYPE),size = 0.8, notch = FALSE, alpha=1, outlier.colour=c("grey70", "grey50"), outlier.alpha=0, outlier.size=1) +  #+ geom_smooth(method=lm, se=FALSE)
  labs(fill = "Type",x = "Samples", y = "Percentages") + scale_shape(solid = FALSE) + #+guides(shape=FALSE)+
  scale_fill_manual(values=c("grey70", "grey50")) +
  guides(color=FALSE)+
  theme_bw() + theme(panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_line(colour="#d3d3d3", size=0.15),
                     panel.border = element_blank(), panel.background = element_blank(),
                     plot.title = element_text(size = 14, family = "Arial", face = "bold"),
                     text=element_text(family="Arial"),
                     axis.title = element_text(face="bold"),
                     #axis.text.x=element_text(colour="black", size = 11),
                     #axis.text.x=element_blank(),
                     axis.text.y=element_text(colour="black", size = 9),
                     axis.line = element_line(size=0.5, colour = "black")) +
  scale_colour_manual(values=c("grey50", "grey30")) +
  geom_jitter(aes(color=TYPE),size=2,alpha=.5) +
  scale_y_continuous(labels=percent, limits = c(0.7, 1.004), expand = c(0, 0),breaks=c(0.7,0.8,0.9,1), minor_breaks = c(0.75,0.85,0.95))+ 
  scale_x_discrete(labels=c("Frozen", "FFPE"))



#### mean normalized coverage  2E
# due scatterplot in colonna

###################                  --------  2E  ---------


sc1 = ggplot(gctf, aes(x=X.gc, y=MNC, fill=STDEV)) + geom_point(shape=21, size=1) + #+ geom_smooth(method=lm, se=FALSE)+
  labs(color = "Standard Deviation",title = "GC_Frozen", y = "Mean Normalized Coverage", x="") + theme_light() +
  scale_y_sqrt(limits = c(0, 3.6), expand = c(0, 0), breaks=c(0,0.5,0.1,0.5,1,1.5,2,2.5,3,3.5))+
  scale_x_continuous(limits = c(0, 1.01), expand = c(0, 0))+
  #  scale_size(range=c(1,8))+
  scale_fill_continuous(low = "white", high = "grey30") + geom_hline(aes(yintercept=0.1), colour="#990000", linetype="dashed") +
  geom_hline(aes(yintercept=0.05), colour="#990000", linetype="dotted")+   theme(panel.grid.major = element_line(colour = "#d3d3d3"), 
                                                                                 panel.grid.minor = element_line(colour="#d3d3d3", size=0.09),
                                                                                 panel.border = element_blank(), panel.background = element_blank(),
                                                                                 plot.title = element_text(size = 14, family = "Arial", face = "bold"),
                                                                                 text=element_text(family="Arial"),
                                                                                 axis.title = element_text(face="bold"),
                                                                                 axis.text.y=element_text(colour="black", size = 9),
                                                                                 axis.line = element_line(size=0.5, colour = "black"),
                                                                                 legend.title=element_blank())
sc2 = ggplot(gctp, aes(x=X.gc, y=MNC, fill=STDEV)) + geom_point(shape=21, size=1) + #+ geom_smooth(method=lm, se=FALSE)+
  labs(color = "Standard Deviation",title = "GC_FFPE", y = "Mean Normalized Coverage", x="") + theme_light()  +
  scale_y_sqrt(limits = c(0, 3.6), expand = c(0, 0), breaks=c(0,0.5,0.1,0.5,1,1.5,2,2.5,3,3.5))+
  scale_x_continuous(limits = c(0, 1.01), expand = c(0, 0))+
  scale_fill_continuous(low = "grey 70", high = "black") + geom_hline(aes(yintercept=0.1), colour="#990000", linetype="dashed") +
  geom_hline(aes(yintercept=0.05), colour="#990000", linetype="dotted")+   theme(panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_line(colour="#d3d3d3", size=0.09),
                                                                                 panel.border = element_blank(), panel.background = element_blank(),
                                                                                 plot.title = element_text(size = 14, family = "Arial", face = "bold"),
                                                                                 text=element_text(family="Arial"),
                                                                                 axis.title = element_text(face="bold"),
                                                                                 axis.text.y=element_text(colour="black", size = 9),
                                                                                 axis.line = element_line(size=0.5, colour = "black"),
                                                                                 legend.title=element_blank())
grid.arrange(sc1, sc2, nrow = 2)









##### supplementary
# 8 boxplot MEAN_AMPLICON_COVERAGE con pool gruppo

ampo$TYPE<- factor(ampo$TYPE, levels = c("Frozen_Non-Tumor_Pool1","Frozen_Non-Tumor_Pool2","Frozen_Tumor_Pool1","Frozen_Tumor_Pool2",
                                         "FFPE_Non-Tumor_Pool1","FFPE_Non-Tumor_Pool2","FFPE_Tumor_Pool1","FFPE_Tumor_Pool2"))



ggplot(ampo, aes( y=MEAN_AMPLICON_COVERAGE , x=TYPE, fill=TYPE
)) + geom_boxplot() +  #+ geom_smooth(method=lm, se=FALSE)
  labs(fill = "Type",x = "", y = "") + scale_shape(solid = FALSE) + #+guides(shape=FALSE)+
  scale_fill_manual(values=c("grey60","grey70", "grey80", "grey90","grey30","grey40", "grey50", "grey60"),
                    labels=c("Frozen\nNon-Tumor\nPool1","Frozen\nNon-Tumor\nPool2","Frozen\nTumor\nPool1","Frozen\nTumor\nPool2",
                             "FFPE\nNon-Tumor\nPool1","FFPE\nNon-Tumor\nPool2","FFPE\nTumor\nPool1","FFPE\nTumor\nPool2")) + theme_light() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_line(colour="#d3d3d3", size=0.09),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Arial", face = "bold"),
        text=element_text(family="Arial"),
        axis.title = element_text(face="bold"),
        axis.text.y=element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x=element_blank(),
        legend.title=element_blank()) +
  scale_colour_manual(values=c("grey50", "grey30")) +
  scale_y_continuous(limits = c(0, 3515), expand = c(0, 0),breaks=c(0,500,1000,1500,2000,2500,3000,3500))+ 
  guides(fill=guide_legend( keyheight=1.5, default.unit="cm"))
wilcox.test(Pool ~ MEAN_AMPLICON_COVERAGE, data=ampo)
wilcox.test(MEAN_AMPLICON_COVERAGE ~ Pool, data=ampo)[3]




# Set color by cond

  sc1 = ggplot(gctf, aes(x=(end-start), y=MNC , fill=STDEV
                  #                  ,color=cond
  )) + scale_x_log10(breaks=c(1,100,300,700,15000)) + geom_point(shape=21, size=2)+ scale_y_sqrt(breaks=c(0.25,0.5,1,2,3.5)) +
   labs(fill = "Standard Deviation",x = "Amplicon Size_Frozen", y = "Mean Normalized Coverage") + theme_light()  + scale_y_sqrt(limits = c(0, 3.5))+
    scale_size(range=c(1,8))
  scale_fill_continuous(low = "white", high = "grey40")
  sc2 = ggplot(gctp, aes(x=(end-start), y=MNC , fill=STDEV
                  #                  ,color=cond
  )) + scale_x_log10(breaks=c(1,100,300,700,15000)) + geom_point(shape=21, size=2)+ scale_y_sqrt(breaks=c(0.25,0.5,1,2,3.5)) +
    labs(fill = "Standard Deviation",x = "Amplicon Size_FFPE", y = "Mean Normalized Coverage") + theme_light()+ scale_y_sqrt(limits = c(0, 3.5))+
scale_size(range=c(1,5.5))
  scale_fill_continuous(low = "grey 80", high = "black")
  grid.arrange(sc1, sc2, nrow = 2)


#### mean normalized coverage  (non serve piu)
# sccatterplot

sc1 = ggplot(gctf, aes(x=(end-start), y=MNC , fill=STDEV)) + scale_x_log10(limits = c(50, 15000),breaks=c(50,100,300,700,15000), expand = c(0, 0)) +
  geom_point(shape=21, size=1)+
  labs(color = "Standard Deviation",x = "Amplicon Size_Frozen", y = "Mean Normalized Coverage") + theme_light()  +
  scale_y_sqrt(limits = c(0, 3.5), expand = c(0, 0),breaks=c(0,0.25,1,2,3.5))+
  #    scale_size(range=c(1,8))
  scale_fill_continuous(low = "white", high = "grey30")
sc2 = ggplot(gctp, aes(x=(end-start), y=MNC , fill=STDEV)) + scale_x_log10(limits = c(50, 15000),breaks=c(50,100,300,700,15000), expand = c(0, 0)) +
  geom_point(shape=21, size=1) +
  labs(color = "Standard Deviation",x = "Amplicon Size_FFPE", y = "Mean Normalized Coverage") + theme_light()+
  scale_y_sqrt(limits = c(0, 3.5), expand = c(0, 0),breaks=c(0,0.25,1,2,3.5))+
  #    scale_size(range=c(1,5.5))
  scale_fill_continuous(low = "grey 70", high = "black")

grid.arrange(sc1, sc2, nrow = 2)


# barplot colonna bruttino

ggplot(colMeans(gctmf[,7:18]), aes(name, MNC ))  + geom_bar(stat = "identity")+ scale_y_continuous(limits = c(0, 5000))+
  labs(x = "Samples_Frozen", y = "Mean Coverage") + theme_light()  + theme(axis.text.x=element_blank())
sc1 = ggplot(gctmf, aes(name, MNC ))  + geom_bar(stat = "identity")+ scale_y_continuous(limits = c(0, 5000))+
  labs(x = "Samples_Frozen", y = "Mean Coverage") + theme_light()  + theme(axis.text.x=element_blank())
sc2 = ggplot(gctmp, aes(x=name, y=MNC  ))  + geom_bar(stat = "identity")+ scale_y_continuous(limits = c(0, 5000))+
  labs(x = "Samples_FFPE", y = "Mean Coverage") + theme_light()  + theme(axis.text.x=element_blank())
grid.arrange(sc1, sc2, nrow = 2)


# boxplot doppi gruppi (2x4)

dat_m2 <- melt(ampo, id.vars = "TYPE",
               measure.vars = grep("^PCT_TARGET_BASES_", names(lin), value = TRUE))

ggplot(dat_m2, aes(fill=TYPE,x=variable, y=value)) +
  geom_boxplot()+
  scale_x_discrete(labels=c("1x","2x","10x","20x","30x")) +
  labs(colour = "Type",x = "", y = "Percentages")+
  # scale_color_manual(values=c("#4169E1", "#B22222")) +
  scale_y_continuous(labels=percent, expand = c(0, 0),limits = c(0.875, 1)) + theme_light()+
  scale_fill_manual(values=c("grey60","grey70", "grey80", "grey90","grey30","grey40", "grey50", "grey60"),
                    labels=c( "Frozen\nNon-Tumor\nPool1","Frozen\nNon-Tumor\nPool2","Frozen\nTumor\nPool1","Frozen\nTumor\nPool2",
                              "FFPE\nNon-Tumor\nPool1","FFPE\nNon-Tumor\nPool2","FFPE\nTumor\nPool1","FFPE\nTumor\nPool2"))+   
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_line(colour="#d3d3d3", size=0.09),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Arial", face = "bold"),
        text=element_text(family="Arial"),
        axis.title = element_text(face="bold"),
        axis.text.y=element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title=element_blank())+
  guides(fill=guide_legend( keyheight=1.5, default.unit="cm"))


# line plot

library(reshape2)
dat_m <- melt(lin, id.vars = "TYPE",
              measure.vars = grep("^PCT_TARGET_BASES_", names(lin), value = TRUE))

  library(ggplot2)
ggplot(dat_m, aes(y = value, x = variable, colour = as.factor(TYPE))) +
  geom_point() +
  geom_line(aes(group = TYPE)) + labs(colour = "Type",x = "Coverage Depth", y = "Coverage") +
  scale_x_discrete(labels=c("1x","2x","10x","20x","30x"))



######  HEATMAP  sgpath
# Labels of rows and columns 

name_genes <- c("Activated.CD8.T.cell", "Central.memory.CD8.T.cell", "Effector.memory.CD8.T.cell", "Activated.CD4.T.cell", "Central.memory.CD4.T.cell", "Effector.memory.CD4.T.cell", "T.follicular.helper.cell", "Gamma.delta.T.cell", "Type.1.T.helper.cell", "Type.17.T.helper.cell", "Type.2.T.helper.cell", "Regulatory.T.cell", "Activated.B.cell", "Immature.B.cell", "Memory.B.cell", "Natural.killer.cell", "CD56bright.natural.killer.cell", "CD56dim.natural.killer.cell", "Myeloid.derived.suppressor.cell", "Natural.killer.T.cell", "Activated.dendritic.cell", "Plasmacytoid.dendritic.cell", "Immature.dendritic.cell", "Macrophage", "Eosinophil", "Mast.Cell", "Monocyte", "Neutrophil") # rows
name_patients <- c("Histological.growth.\npattern.trabecular", "Histological.growth.\npattern.pseudoglandular", "Histological.growth.\npattern.compact", "Edmondson.Grade", "Cholestasis", "Cytological.variants.\nPleomorphic.cells", "Cytological.variants.\nClear.cells", "Cytological.variants.\nMultinucleated.cells", "Cytological.variants.\nFatty.change", "Cytological.variants.\nHyaline.bodies", "Cytological.variants.\nPale.bodies", "Cytological.variants.\nGround.glass", "Mallory.Bodies", "Vessel.infiltration", "Necrosis", "Lymphosc", "Chronic.liver.disease.\nNon.present", "Chronic.liver.disease.\nAlcohol", "Chronic.liver.\ndisease.HBV", "Chronic.liver.\ndisease.HCV", "Chronic.liver.disease.\nHemochromatosis", "Chronic.liver.disease.\nNASH.NAFLD", "Chronic.liver.disease.\nAutoimmune.liver.disease.PBC", "Chronic.liver.\ndisease.Undetermined", "Chronic.liver.disease.Alpha.1.\nantitrypsin.deficiency", "Chronic.liver.\ndisease.Any.Virus", "Vascular.Invasion", "Neo") # columns

name_genes <- c("PD1", "PD-L1", "PD-L2", "AFP")
name_patients <- c("Histological.growth.\npattern.trabecular", "Histological.growth.\npattern.pseudoglandular", "Histological.growth.\npattern.compact", "Edmondson.Grade", "Cholestasis", "Cytological.variants.\nPleomorphic.cells", "Cytological.variants.\nClear.cells", "Cytological.variants.\nMultinucleated.cells", "Cytological.variants.\nFatty.change", "Cytological.variants.\nHyaline.bodies", "Cytological.variants.\nPale.bodies", "Cytological.variants.\nGround.glass", "Mallory.Bodies", "Vessel.infiltration", "Necrosis", "Lymphosc", "Chronic.liver.disease.\nNon.present", "Chronic.liver.disease.\nAlcohol", "Chronic.liver.\ndisease.HBV", "Chronic.liver.\ndisease.HCV", "Chronic.liver.disease.\nHemochromatosis", "Chronic.liver.disease.\nNASH.NAFLD", "Chronic.liver.disease.\nAutoimmune.liver.disease.PBC", "Chronic.liver.\ndisease.Undetermined", "Chronic.liver.disease.Alpha.1.\nantitrypsin.deficiency", "Chronic.liver.\ndisease.Any.Virus", "Vascular.Invasion", "bho") # columns

name_genes <- c("mutations",	"neoantigens",	"percentage_clonal_neo",	"neo_mut_ratio")


# Generation of dataframe

# value_expression <- data.frame(name_patients, 
#                                matrix(c(4.6, -2.92, -7.15, -4.03, -6.41, 0.02,
#                                         -0.02, 0.0, -6.48, -0.02 , -0.02, 0.0,
#                                         0.0, 0.0, 8.39, 0.0, 0.02, -0.02,#
#                                         -0.02, 0.02, 0.02, 10.45, 14.72, 0.02,
#                                         -9.44, -0.0, 0.02, 0.02, 6.46, 0.0,
#                                         0.02, -0.0, -0.02, -0.0, -0.02, -23.3), nrow=6, ncol=6) )
value_expression <- data.frame(name_patients, 
                               matrix(c(0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.384717195, -0.246194999, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.212417719, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.296458933, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.789140062, 0.05, -0.05, -0.05, 0.328470715, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.197366551, 0.05, -0.05, 0.205060092, 0.225101872, -0.58089788, 0.187334905, 0.05, 0.228188926, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, 0.227869664, -0.334598776, 0.05, 0.289830433, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.296847065, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.375262935, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.337768564, -0.309058318, 0.05, -0.05, 0.314093542, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.31920125, -0.373403857, 0.05, 0.05, 0.465339766, 0.05, 0.05, 0.05, -0.05, -0.05, 0.315883968, -0.05, -0.05, 0.05, 0.05, -0.219236644, -0.287965846, 0.26441607, -0.05, -0.05, 0.05, 
                                        0.201585616, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.285890255, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, -0.246689153, 0.328926992, -0.05, 0.05, 0.05, 0.05, 0.05, 0.250099146, -0.249116566, -0.291508151, 0.422887757, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.271980886, -0.259109691, 0.05, -0.05, -0.05, 0.05, -0.350380336, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.333916521, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.305090256, 0.255033336, -0.05, 0.05, -0.05, 0.295900788, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.803676863, -0.05, 0.05, 0.05, 0.05, 0.384717195, -0.233770287, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 
                                        0.05, 0.05, 0.05, 0.471478734, -0.271184561, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.193885807, -0.05, -0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.29155393, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.198441138, -0.05, -0.05, 0.05, -0.05, -0.05, -0.271203374, -0.05, -0.283105693, 0.05, -0.350365495, 0.05, 0.05, 0.05, -0.05, 0.05, -0.219197653, -0.27909926, 0.29155393, -0.23569789, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.2800466, -0.05, 0.05, 0.211454338, -0.19234305, -0.05, 0.05, 0.05, -0.247784916, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.191911552, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.228188926, 0.05, -0.05, 0.700231254, 0.05, -0.05, -0.05, 0.275694868, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.365841944, -0.271184561, 0.240923853, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.365841944, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
                                        0.05, -0.05, 0.247780628, 0.29155393, -0.283754568, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.605942747, -0.05, -0.05, -0.05, -0.05, 0.365841944, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.605942747, 0.05, 0.05, -0.05, 0.05, 0.375262935, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.19234305, -0.404815452, 0.25113981, -0.366532708, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.384279856, 0.05, -0.05, 0.05, -0.05, 0.328206327, 0.05, 0.746877974, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.605942747, 0.05, 0.05, -0.05, 0.05, 0.365841944, -0.221388933, 0.230444969, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.201585616, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.229517842, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, -0.210601783, 0.05, -0.266238741, 0.05, 0.05, 0.05, 0.05, 
                                        0.276750391, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05), ncol=28, nrow=28) )

max(0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.384717195, -0.246194999, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.212417719, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.296458933, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.789140062, 0.05, -0.05, -0.05, 0.328470715, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.197366551, 0.05, -0.05, 0.205060092, 0.225101872, -0.58089788, 0.187334905, 0.05, 0.228188926, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, 0.227869664, -0.334598776, 0.05, 0.289830433, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.296847065, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.375262935, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.337768564, -0.309058318, 0.05, -0.05, 0.314093542, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.31920125, -0.373403857, 0.05, 0.05, 0.465339766, 0.05, 0.05, 0.05, -0.05, -0.05, 0.315883968, -0.05, -0.05, 0.05, 0.05, -0.219236644, -0.287965846, 0.26441607, -0.05, -0.05, 0.05, 
    0.201585616, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.285890255, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, -0.246689153, 0.328926992, -0.05, 0.05, 0.05, 0.05, 0.05, 0.250099146, -0.249116566, -0.291508151, 0.422887757, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.271980886, -0.259109691, 0.05, -0.05, -0.05, 0.05, -0.350380336, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.333916521, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.305090256, 0.255033336, -0.05, 0.05, -0.05, 0.295900788, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.803676863, -0.05, 0.05, 0.05, 0.05, 0.384717195, -0.233770287, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 
    0.05, 0.05, 0.05, 0.471478734, -0.271184561, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.193885807, -0.05, -0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.29155393, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.198441138, -0.05, -0.05, 0.05, -0.05, -0.05, -0.271203374, -0.05, -0.283105693, 0.05, -0.350365495, 0.05, 0.05, 0.05, -0.05, 0.05, -0.219197653, -0.27909926, 0.29155393, -0.23569789, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.2800466, -0.05, 0.05, 0.211454338, -0.19234305, -0.05, 0.05, 0.05, -0.247784916, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.191911552, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.228188926, 0.05, -0.05, 0.700231254, 0.05, -0.05, -0.05, 0.275694868, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.365841944, -0.271184561, 0.240923853, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.365841944, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
    0.05, -0.05, 0.247780628, 0.29155393, -0.283754568, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.605942747, -0.05, -0.05, -0.05, -0.05, 0.365841944, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.605942747, 0.05, 0.05, -0.05, 0.05, 0.375262935, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.19234305, -0.404815452, 0.25113981, -0.366532708, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.384279856, 0.05, -0.05, 0.05, -0.05, 0.328206327, 0.05, 0.746877974, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.605942747, 0.05, 0.05, -0.05, 0.05, 0.365841944, -0.221388933, 0.230444969, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.201585616, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.229517842, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, -0.210601783, 0.05, -0.266238741, 0.05, 0.05, 0.05, 0.05, 
    0.276750391, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05)
min(0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.384717195, -0.246194999, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.212417719, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.296458933, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.789140062, 0.05, -0.05, -0.05, 0.328470715, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.197366551, 0.05, -0.05, 0.205060092, 0.225101872, -0.58089788, 0.187334905, 0.05, 0.228188926, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, 0.227869664, -0.334598776, 0.05, 0.289830433, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.296847065, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.375262935, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.337768564, -0.309058318, 0.05, -0.05, 0.314093542, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.31920125, -0.373403857, 0.05, 0.05, 0.465339766, 0.05, 0.05, 0.05, -0.05, -0.05, 0.315883968, -0.05, -0.05, 0.05, 0.05, -0.219236644, -0.287965846, 0.26441607, -0.05, -0.05, 0.05, 
    0.201585616, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.285890255, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, -0.246689153, 0.328926992, -0.05, 0.05, 0.05, 0.05, 0.05, 0.250099146, -0.249116566, -0.291508151, 0.422887757, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.271980886, -0.259109691, 0.05, -0.05, -0.05, 0.05, -0.350380336, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.333916521, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.305090256, 0.255033336, -0.05, 0.05, -0.05, 0.295900788, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.803676863, -0.05, 0.05, 0.05, 0.05, 0.384717195, -0.233770287, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 
    0.05, 0.05, 0.05, 0.471478734, -0.271184561, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.193885807, -0.05, -0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.29155393, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.198441138, -0.05, -0.05, 0.05, -0.05, -0.05, -0.271203374, -0.05, -0.283105693, 0.05, -0.350365495, 0.05, 0.05, 0.05, -0.05, 0.05, -0.219197653, -0.27909926, 0.29155393, -0.23569789, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.2800466, -0.05, 0.05, 0.211454338, -0.19234305, -0.05, 0.05, 0.05, -0.247784916, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.191911552, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, 0.228188926, 0.05, -0.05, 0.700231254, 0.05, -0.05, -0.05, 0.275694868, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.365841944, -0.271184561, 0.240923853, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.365841944, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
    0.05, -0.05, 0.247780628, 0.29155393, -0.283754568, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, 0.605942747, -0.05, -0.05, -0.05, -0.05, 0.365841944, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.605942747, 0.05, 0.05, -0.05, 0.05, 0.375262935, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.19234305, -0.404815452, 0.25113981, -0.366532708, -0.05, -0.05, 0.05, -0.05, -0.05, -0.05, 0.05, -0.05, -0.384279856, 0.05, -0.05, 0.05, -0.05, 0.328206327, 0.05, 0.746877974, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, 0.605942747, 0.05, 0.05, -0.05, 0.05, 0.365841944, -0.221388933, 0.230444969, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.05, -0.05, 0.201585616, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.229517842, -0.05, 0.05, 0.05, -0.05, 0.05, -0.05, -0.05, -0.210601783, 0.05, -0.266238741, 0.05, 0.05, 0.05, 0.05, 
    0.276750391, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05)

names(value_expression)[2:29] <- name_genes

pval=matrix(c(0.503890115, 0.694374962, 0.722708681, 0.139118419, 0.870821952, 0.166125031, 0.580545704, 0.515143026, 0.962773882, 0.461554207, 0.074158756, 0.460759385, 0.084363762, 0.403408376, 0.183746722, 7.71E-05, 0.027335073, 0.145928285, 0.136235309, 0.119412551, 0.853501288, 0.829316991, 0.260137823, 0.211455945, 0.454960249, 0.027485904, 0.134580839, 0.205076971, 0.088172898, 0.253116609, 0.465869256, 0.532770922, 0.497228963, 0.311961054, 0.073616479, 0.740358098, 0.173058087, 0.024441579, 0.783312061, 0.871398404, 0.474638891, 0.52572787, 0.765360991, 0.977459709, 0.164331593, 0.165244203, 0.899425881, 0.385699509, 0.546387915, 0.800674829, 0.994936918, 0.536902683, 0.799628184, 0.818691896, 0.228506743, 0.539481098, 0.503890115, 0.294087899, 0.941090112, 0.322964742, 0.753502615, 0.196556664, 0.459718925, 0.905863138, 0.337839063, 0.630529106, 0.121765034, 0.037292366, 0.133044521, 0.233620628, 0.430593423, 0.000651756, 0.075866153, 0.079369744, 0.577844388, 0.194078325, 0.559942538, 0.977559245, 0.260137823, 0.461600217, 0.454960249, 0.147395097, 0.078548136, 0.038454402, 0.901010272, 0.072403216, 0.030303156, 0.024296391, 3.32E-06, 0.047578576, 0.698568093, 0.034932767, 0.444797612, 0.08007367, 0.609808684, 0.659849779, 0.850158821, 0.588616023, 0.110239449, 0.01645707, 0.003140533, 0.515708189, 0.006516838, 0.194078325, 0.827617254, 0.646470106, 0.994925307, 0.363874718, 0.454960249, 
              0.002310589, 0.26303584, 0.205076971, 0.881473008, 0.107378364, 0.984236416, 0.428529664, 0.258394645, 0.793214613, 0.27547079, 0.990680924, 0.719674866, 0.678769464, 0.426660389, 0.112562533, 0.347001637, 0.569152002, 0.849881523, 0.00011207, 0.075866153, 0.452845036, 0.488997983, 0.091777452, 0.175876474, 0.60682487, 0.529549227, 0.572811448, 0.786118522, 0.757912004, 0.326122524, 0.850248659, 0.673092222, 0.500087584, 0.499590391, 0.996975877, 0.697665995, 0.154853841, 0.787322843, 0.170392366, 0.077543261, 0.678769464, 0.989680749, 0.460759385, 0.624706458, 0.832800204, 0.827956772, 0.000464957, 0.006116704, 0.050696647, 0.084060908, 0.020097361, 0.175876474, 0.60682487, 0.991659009, 0.635628531, 0.454960249, 0.982046246, 0.420847546, 0.36551976, 0.221284592, 0.388774217, 0.499590391, 0.488115266, 0.334127905, 0.58700289, 0.185482325, 0.990680924, 0.449604922, 0.387722312, 0.783312061, 0.065071089, 0.547106259, 0.405981206, 0.723322294, 0.000906985, 0.001076384, 0.248907784, 0.165576658, 0.000907107, 0.827617254, 0.977559245, 0.260137823, 0.827208652, 0.805756706, 0.001225361, 0.281275036, 0.372688059, 0.901010272, 0.720373575, 0.020767091, 0.004183457, 0.021879212, 0.196556664, 0.841278434, 0.113531269, 0.049317223, 0.975675887, 0.305105636, 0.659849779, 0.521680909, 0.504549887, 0.447163314, 0.080302038, 0.199805206, 0.005472452, 0.093124476, 0.057452968, 0.179891711, 0.800674829, 0.997225767, 0.075473481, 0.994812077, 0.880048709, 0.260581487, 0.479260763, 0.088172898, 0.911332697, 0.693326633, 0.225368101, 0.514020993, 0.009553209, 0.006790976, 0.087035192, 0.095992932, 0.678769464, 0.074158756, 
              0.108117026, 0.029927374, 0.012784681, 0.008945701, 1.62E-05, 0.287014947, 0.469972822, 
              0.221802443, 0.543813639, 0.836580841, 0.149131493, 0.534701535, 0.555129639, 0.793095456, 0.17142288, 0.013731648, 0.006727322, 0.901010272, 0.837398058, 0.269969964, 0.557002318, 0.002811678, 0.984840913, 0.330262623, 0.113531269, 0.386861233, 0.263485609, 0.44159615, 0.460759385, 0.098446712, 0.183382047, 0.935754851, 0.071069407, 0.095536051, 0.646248731, 0.199470247, 0.298844192, 0.318219871, 0.785809509, 0.529549227, 0.963958951, 0.136022774, 0.122702148, 0.805769126, 0.4252143, 0.69114348, 0.694374962, 0.560863444, 0.432956718, 0.004306975, 0.940439983, 0.185482325, 0.515143026, 0.449604922, 0.678769464, 0.803215532, 0.871398404, 0.939515353, 0.480109183, 0.400573605, 0.199604857, 0.217837281, 0.206261278, 0.165576658, 0.436343153, 0.538357006, 0.785809509, 0.529549227, 0.695625636, 0.440693759, 0.054636487, 0.947346926, 0.602488308, 0.673092222, 0.107378364, 0.909701514, 0.492877907, 0.224220336, 0.216492841, 0.391721306, 0.758061632, 0.962773882, 0.425408633, 0.609808684, 0.888980797, 0.347001637, 0.753654406, 0.006162146, 0.007458632, 0.05965511, 0.09805565, 0.268229701, 0.02796939, 0.09665369, 0.239182508, 0.260137823, 0.363874718, 0.440693759, 0.61802986, 0.45155, 0.850248659, 0.349129141, 0.561533868, 0.253231477, 0.846729444,
              0.772421804, 0.693710799, 0.502911913, 0.84033046, 0.204238095, 0.170124061, 0.014898343, 0.460759385, 0.706745611, 0.753654406, 0.464067435, 7.71E-05, 0.035808244, 0.120123564, 0.508935744, 0.069585811, 0.827617254, 0.347134881, 0.260137823, 0.963958951, 0.234393067, 0.175738145, 0.843810099, 0.514404623, 0.69114348, 0.12933076, 0.560863444, 0.169587045, 0.157041605, 0.216492841, 0.25501262, 0.34384996, 0.962773882, 0.425408633, 0.426660389, 0.659849779, 0.084363762, 0.585445651, 0.290402469, 1.97E-06, 0.015465722, 0.145928285, 0.165576658, 0.119412551, 0.827617254, 0.483649239, 0.994925307, 0.280512709, 0.786118522, 0.04378842, 0.73645473, 1, 0.881473008, 0.986047143, 0.388577116, 0.975864022, 0.840627976, 0.358831161, 0.75067831, 0.448898853, 0.062106068, 0.678769464, 0.803215532, 0.643896373, 0.106496947, 0.730676947, 0.514609088, 0.002339102, 0.091146294, 0.413505529, 0.096190975, 0.375026081, 0.178349719, 0.822409399, 0.532572753, 0.35776054, 0.444051638, 0.03873164, 0.604015798, 0.397873476, 0.503890115, 0.072403216, 0.653817576, 0.006832649, 0.44407671, 0.003079634, 0.698568093, 0.001518878, 0.28935147, 0.503154516, 0.121765034, 0.888980797, 0.051308362, 0.027876021, 0.012138355, 0.002339102, 0.033964073, 0.308439034, 0.830127, 0.073248233, 0.178349719, 0.325204418, 0.262259638, 0.454297141, 0.444051638, 0.189511831, 0.01162563, 0.538587059, 0.69114348, 0.025956654, 0.041923045, 0.716595063, 0.467264565, 0.072619426, 0.038715318, 0.34384996, 0.204238095, 0.72393641, 0.609808684, 0.871398404, 0.386308369, 0.480109183, 0.807500356, 0.042572672, 0.546870825, 0.605674786, 0.098938266, 0.251492505, 0.832282559, 0.456215699, 0.541104834, 0.138041603, 0.228783343, 0.24770677, 0.573128023, 0.130984771, 0.69114348, 
              0.864814192, 0.41065149, 0.764634117, 0.458958865, 0.561134084, 0.209468193, 0.034932767, 0.512584975, 0.461554207, 0.025631336, 0.293191758, 0.451099984, 0.504549887, 0.013114759, 0.686793531, 0.803312381, 0.063703149, 0.128214554, 0.436343153, 0.559942538, 0.347134881, 0.991659009, 0.898195497, 0.226444488, 0.459810638, 0.179441348, 0.073921274, 0.503890115, 0.561533868, 0.693326633, 0.770422453, 0.120745561, 0.625903575, 0.431187963, 0.905863138, 0.245811388, 0.821720876, 0.121765034, 0.303117705, 0.201293829, 0.832800204, 0.074794613, 0.000161666, 0.015465722, 0.0186749, 0.782620611, 0.069585811, 0.827617254, 0.785809509, 0.260137823, 0.695625636, 0.440693759, 0.43297917, 0.449222543, 0.47829236, 0.881473008, 0.216323874, 0.763634093, 0.062495318, 0.36561876, 0.107610811, 0.934052318, 0.462987732, 0.242561847, 0.975675887, 0.591948125, 0.888980797, 0.706745611, 0.80221041, 0.357497798, 0.000161666, 0.179876566, 0.292565231, 0.282010105, 0.436343153, 0.318219871, 0.158479404, 0.260137823, 0.461600217, 0.440693759, 0.329933665, 0.887331507, 0.816060663, 0.349129141, 0.339305577, 0.183780085, 0.37750373, 0.589345089, 0.388700239, 0.934052318, 0.292430194, 0.725708649, 0.425408633, 0.989680749, 0.182040235, 0.243814396, 0.613248353, 0.025068348, 0.002339102, 0.011463312, 0.292565231, 0.945971668, 0.153271026, 0.175876474, 0.319669137, 0.260137823, 0.76342807, 0.805756706, 0.61802986, 0.362388801, 0.924789433, 0.230316499, 0.791695474, 0.41065149, 0.149303494, 0.650747132, 0.58700289, 0.75067831, 0.515143026, 0.574407497, 0.630529106, 0.043973645, 0.888980797, 0.939515353, 0.46369111, 0.80615707, 0.000161666, 0.172432719, 0.068954177, 0.911856849, 0.449049002, 0.832282559, 0.640522634, 0.532572753, 0.35776054, 0.453555718, 0.580001438, 0.525848365, 0.91604663, 0.69114348, 0.442420383, 0.625577002, 0.358466184, 0.633689197, 0.625903575, 0.391721306, 0.53031667, 0.645339267, 0.678769464, 0.305105636, 0.888980797, 0.066136554, 0.661411165, 0.496685312, 0.199604857, 0.287014947, 0.323743593, 0.059464571, 0.860158453, 0.324297751, 0.620688284, 0.534701535, 0.429950783, 0.446419704, 0.11563565, 0.807780465, 0.203045474, 0.230316499, 0.253116609, 0.835957055, 0.94572786, 0.428463367, 0.910255236, 0.972260308, 0.332109938, 0.143398322, 0.821720876, 0.043973645, 0.871398404, 0.051308362, 0.070320499, 0.110239449, 0.00011207, 0.251309054, 0.677632649, 0.760613117, 0.619625586, 0.832282559, 0.456215699, 0.532572753, 0.646201333, 0.789833882, 0.911024374, 0.219115719, 0.811618605, 0.145241507, 0.442420383, 0.041923045, 8.05E-05, 0.029122881, 0.000156789, 0.934052318, 0.069264441, 0.077543261, 0.542729139, 
              0.803215532, 0.888980797, 0.201293829, 0.637480183, 0.000690546, 0.141730283, 0.113832112, 0.308439034, 0.180023426, 0.015244861, 0.321779459, 0.011913305, 0.266394855, 0.152885884, 0.444051638, 0.457319814, 0.307669786, 0.349474441, 0.69114348, 0.339305577, 0.294117923, 0.37750373, 0.802019173, 0.253250723, 0.663422359, 0.84033046, 0.79677833, 0.874138075, 0.043973645, 0.871398404, 0.347001637, 0.637480183, 0.094040634, 0.000161666, 0.04644723, 0.024279646, 0.628407088, 0.119412551, 0.175876474, 0.483649239, 0.529549227, 0.898195497, 0.226444488, 0.757912004, 0.765574738, 0.880597291, 0.349129141, 0.500087584, 0.154891654, 0.716595063, 1, 0.721963568, 0.119490373, 0.448898853, 0.049317223, 0.678769464, 0.121765034, 0.643896373, 0.051308362, 0.121708431, 0.328617751, 0.31622015, 0.252792218, 0.233026031, 0.050190528, 0.285360651, 0.542810946, 0.614135262, 0.262259638, 0.422004813, 0.228783343, 0.017231993, 0.124649832, 0.073291772, 0.503890115, 0.339305577, 0.693326633, 0.250829999, 0.909864144, 0.026297962, 0.073616479, 0.014444307, 0.173058087, 0.191708017, 0.426660389, 0.643896373, 0.016744007, 0.260452024, 0.849276338, 0.728801324, 0.41189278, 0.93039016, 0.911856849, 0.813924636, 0.178349719, 0.614135262, 0.532572753, 0.252357271, 0.228783343, 0.98666803, 0.2628249, 0.542371098))

# Melt dataframe

df_heatmap <- melt(value_expression, id.vars = "name_patients")
names(df_heatmap)[2:3] <- c("patient", "correlation")

ggplot(df_heatmap, aes(  name_patients , patient)) +
  geom_tile(aes(fill = correlation),  color = "black") +
  scale_fill_gradient(low = "yellow2", high = "dodgerblue1", breaks=c(-1, 1), labels =c("Inverse\nCorrelation","Direct\nCorrelation")) +
  ylab("") +
  xlab("Histopatological Features") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(hjust = 1, face = "bold"),
        axis.text.x = element_text(angle = 50, hjust = 1, face = "bold")) +
  labs(fill = "Correlation")+ 
  geom_text(aes(label = round(pval, 5)), fontface = "bold", size=2.5) + labs(fill="")



##########  barplot gruppati

barpl = read.table("/Users/andrea/Documents/BI/BI/B/panel/barplot.txt", header = T, sep="\t")


# barpl <- within(barpl, 
#                    perc <- factor(perc, 
#                                       levels=names(sort(table(perc), 
#                                                         decreasing=TRUE))))

# ggplot(barpl, aes(class, as.double(perc))) +   
#   geom_bar(aes(fill = gene), position = "dodge", stat="identity") + guides(fill=FALSE)
# 
# 
# ggplot(data, aes(y=as.double(perc), x=gene)) + 
#   geom_bar( stat="identity") +    
#   facet_wrap(~group)

barpl$class_f = factor(barpl$class, levels=c("Complete coding regions","Coding hotspots","Regulatory regions","Complete non-coding genes", "CNA genes"))
cbbPalette <- c("grey60","grey70", "grey90", "grey40","grey20")



ggplot(data=barpl, aes(x=reorder(gene, -as.double(perc)), fill = art, y=as.double(perc/100))) + 
  geom_bar(stat='identity') +    scale_fill_manual(values=cbbPalette) +
  facet_wrap( ~ class_f, scales = "free_x", nrow = 1)+   
  theme(
    axis.text.x=element_text(colour="black", size = 6, angle = 90)
  )+ labs(fill = "Source",x = "Mutated genes", y = "Mutations Percentage") +
  scale_y_continuous(labels=percent, limits = c(0, 0.5), expand = c(0, 0),breaks=c(0.1,0.2,0.3,0.4,0.5), minor_breaks = c(0.5,0.15,0.25,0.35,0.45))



levels(barpl$class) <- c("CCR", "CH", "RR", "CNCG", "CNA")
levels(barpl$class) <- c("CH", "CCR", "CNCG", "RR", "CNA")


barpl$class_f = factor(barpl$class, levels=c("CCR", "CH", "RR", "CNCG", "CNA"))
cbbPalette <- c("grey60","grey80", "grey90", "grey40","grey20")


ggplot(data=barpl, aes(x=reorder(gene, -as.double(perc)), fill = art, y=as.double(perc/100))) + 
  geom_bar(stat='identity') +    scale_fill_manual(values=cbbPalette) +
  facet_grid( ~ class_f, scales = "free_x", space = "free_x")+   
  theme(axis.text.x = element_text(angle = 50, hjust = 1.5), 
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ 
  labs(fill = "Source",x = "", y = "% HCC mutated") +
  scale_y_continuous(labels=percent, limits = c(0, 0.5), expand = c(0, 0),
                     breaks=c(0.1,0.2,0.3,0.4,0.5), minor_breaks = c(0.5,0.15,0.25,0.35,0.45))



#histbackback(split())

#########  barplot laterali

DATA <- data.frame(
  state = c("AK", "TX", "CA", "MT", "NM", "AZ", "NV", "CO", "OR", "WY", "MI", "MN", "UT", "ID", "KS", "NE", "SD", "WA", "ND", "OK"),
  sales_staff = c(20,30,40,10,15,35,18,25,22,7,12,22,3,4,5,8,14,28,24,32)
)

set.seed(1)
DATA$sales <- DATA$sales_staff * 50 + (runif(nrow(DATA)) * 1000)

# Order the state factor by number of sales staff so that it is plotted in that order
DATA$state <- factor(DATA$state, levels = DATA[order(DATA$sales_staff),"state"])

library(grid)
g.mid<-ggplot(barpl,aes(x=1,y= reorder(gene, -as.double(order))  ))+geom_text(aes(label=gene))+
  # geom_segment(aes(x=0.94,xend=0.96,yend=gene))+
  # geom_segment(aes(x=1.04,xend=1.065,yend=gene))+
  ggtitle("")+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-1,1,-1), "mm"))

g1 <- ggplot(data = barpl, aes(x = reorder(gene, -as.double(order)), y = perc, fill = art)) + 
  scale_fill_manual(values=cbbPalette) +
  geom_bar(stat = "identity") + ggtitle("Mutations") +
  #  facet_grid( ~ class_f, scales = "free_x", space = "free_x")+  
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,-1,1,0), "mm"), 
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_reverse() + coord_flip()+ guides(fill=FALSE)

g2 <- ggplot(data = barpl, aes(x = reorder(gene, -as.double(order)), y = CAN, fill = art)) +xlab(NULL)+ 
  scale_fill_manual(values=cbbPalette) +
  geom_bar(stat = "identity") + ggtitle("CNA") +
  #  facet_grid( ~ class_f, scales = "free_x", space = "free_x")+  
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm"), 
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip()+
  scale_y_continuous(limits=c(0,43.2))+ guides(fill=FALSE)

library(gridExtra)
gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))





#### scatter, violin brutti

TFL = read.table("/Users/andrea/Documents/BI/BI/B/fujimoto/V/LIRItable.txt", header = T, sep="\t")
TFL2 = read.table("/Users/andrea/Documents/BI/BI/B/fujimoto/V/BRCAtable.txt", header = T, sep="\t")


ggplot(TFL, aes(x= as.numeric(sub("%","",TFL$perc)), y=TOTmut)) + geom_point(shape=21, size=1) + #+ geom_smooth(method=lm, se=FALSE)+
  labs( y = "Total mutations", x="% TFBS Mutations") + theme_light() +
  scale_y_continuous(limits = c(0, 100000))+
  scale_x_continuous(limits = c(0, 0.8))+
  #  scale_size(range=c(1,8))+
  scale_fill_continuous(low = "white", high = "grey30") + #geom_hline(aes(yintercept=0.1), colour="#990000", linetype="dashed") +#geom_hline(aes(yintercept=0.05), colour="#990000", linetype="dotted")+   
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_line(colour="#d3d3d3", size=0.09),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Arial", face = "bold"),
        text=element_text(family="Arial"),
        axis.title = element_text(face="bold"),
        axis.text.y=element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title=element_blank())



ggplot(TFL[,1:4], aes(1, y=TFBSmut )) + geom_violin(fill="gray") + scale_y_sqrt(limits = c(0, 200), breaks=c(0, 5, 10, 20, 50, 200), expand = c(0, 0))+ #geom_boxplot(notch = TRUE)+  #+ geom_smooth(method=lm, se=FALSE)
  geom_jitter(aes(1, y=TFBSmut ), color = "black", size = 1, data = TFL[,1:4]) + 
  labs( y = "TFBS mutations") + scale_shape(solid = FALSE) + #+guides(shape=FALSE)+
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "#d3d3d3", linetype = 1), panel.grid.minor = element_line(colour="#d3d3d3", size=0.1, linetype = 1),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Arial", face = "bold"),
        text=element_text(family="Arial"),
        axis.title = element_text(face="bold"),
        axis.text.y=element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black"),
        # axis.text.x=element_blank(),
        legend.title=element_blank())+ stat_summary(fun.y=median, geom="point", size=4, color="red")


#### heatmap

HM = read.table("/Users/andrea/Documents/BI/BI/B/fujimoto/V/motifsB.txt", header = T, sep="\t", row.names=1)

mee <- melt(HM)

# ggplot(data = mee, aes(x=Donor, y=variable, fill=value)) + 
#   geom_tile() +
#   scale_fill_gradientn(colours=c("whitesmoke", "gray75", "gray25", "gray0"),
#                        values=rescale(c(0, 1, 15, 160))) + theme(axis.text.x=element_blank())



row.order <- hclust(dist(HM))$order # clustering
col.order <- hclust(dist(t(HM)))$order
dat_new <- HM[row.order, col.order] # re-order matrix accoring to clustering

df_molten_dat <- melt(as.matrix(dat_new))

ggplot(data = df_molten_dat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradientn(colours=c("whitesmoke", "gray75", "gray25", "gray0"),
                       values=rescale(c(0, 1, 15, 160))) + theme(axis.text.x=element_blank())+ theme(legend.position="none")


HM2 = read.table("/Users/andrea/Documents/BI/BI/B/fujimoto/V/motifsC.txt", header = T, sep="\t", row.names=1)

mee2 <- melt(HM2)

# ggplot(data = mee2, aes(x=Donor, y=variable, fill=value)) + 
#   geom_tile() +
#   scale_fill_gradientn(colours=c("whitesmoke", "gray75", "gray25", "gray0"),
#                        values=rescale(c(0, 1, 15, 160))) + theme(axis.text.x=element_blank())


row.order <- hclust(dist(HM2))$order # clustering
col.order <- hclust(dist(t(HM2)))$order
dat_new <- HM2[row.order, col.order] # re-order matrix accoring to clustering

df_molten_dat <- melt(as.matrix(dat_new))

ggplot(data = df_molten_dat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradientn(colours=c("whitesmoke", "gray75", "gray25", "gray0"),
                       values=rescale(c(0, 1, 15, 160))) + theme(axis.text.x=element_blank())+ theme(legend.position="none")


### barplot gruppati

HMh = read.table("/Users/andrea/Documents/BI/BI/B/fujimoto/V/tabhist.txt", header = T, sep="\t")

mee3 <- melt(HMh)


ggplot(data=mee3, aes(x=variable, y=value, fill=Donor))+  
  geom_bar(stat="identity", position="dodge")+ scale_y_sqrt(breaks = c(1,50,100,150))+ 
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(hjust = 1, face = "bold"),
        axis.text.x = element_text(angle = 50, hjust = 1, face = "bold"))
  


HMh = read.table("/Users/andrea/Downloads/mutf.txt", header = T, sep="\t", row.names = 1)


dat_m2 <- melt(HMh, id.vars = "codone")

ggplot(data=dat_m2, aes(x=codone, y=value, fill=variable))+  
  geom_bar(stat="identity", position="dodge")+ 
  labs(fill = "Source",x = "Codon", y = "Mutations found") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(hjust = 1, face = "bold"),
        axis.text.x = element_text(angle = 50, hjust = 1, face = "bold"))


# HMh = read.table("/Users/andrea/Downloads/mutft.txt", header = T, sep="\t", row.names = 1)
# 
# 
# dat_m2 <- melt(HMh, id.vars = "codone")
# 
# ggplot(data=dat_m2, aes(x=codone, y=value, fill=variable))+  
#   geom_bar(stat="identity", position="dodge")+ 
#   labs(fill = "Source",x = "Codon", y = "Mutations found") +
#   theme(legend.title = element_text(size = 10),
#         legend.text = element_text(size = 12),
#         plot.title = element_text(size=16),
#         axis.title=element_text(size=14,face="bold"),
#         axis.text.y = element_text(hjust = 1, face = "bold"),
#         axis.text.x = element_text(angle = 50, hjust = 1, face = "bold"))
# 
# 
# 
# 
# hm = read.table("/Users/andrea/Downloads/HEATab.txt", header = T, sep="\t")
# 
# sa=c("HPU201T", "HPU202T", "HPU203T", "HPU205T", "HPU206T", "HPU207T", "HPU209T", "HPU211T", "HPU212T", "HPU213T")
# 
# 
# for (i in sa) {
# ggplot(subset(hm,sample=="HPU207T"), aes(  sample , change)) +
#   geom_tile(aes(fill = wes),  color = "black") +
#     scale_colour_gradient2(low = "yellow2", high = "dodgerblue1", midpoint= 0, breaks=c(-1, 1)) + ylab("") +
#     xlab("")+ coord_fixed(ratio=1, expand = TRUE) + coord_polar() +
#     theme(legend.title = element_text(size = 10), legend.position="none",
#           legend.text = element_text(size = 12),
#           plot.title = element_text(size=16),
#           axis.title=element_text(size=14,face="bold"),
#           axis.text.y = element_text(hjust = 1, face = "bold"),
#           axis.text.x = element_text(angle = 50, hjust = 1, face = "bold")) 
# }
# 
# hm = read.table("/Users/andrea/Downloads/HEATab2.txt", header = T, sep="\t")
# 
# hm$change <- with(hm, reorder(change, sample))
# 
# 
# 
# sequence_length = length(unique(hm$change))
# first_sequence = c(1:(sequence_length%/%2)) 
# second_sequence = c((sequence_length%/%2+1):sequence_length) 
# first_angles =c(90 - 180/length(first_sequence) * first_sequence)
# second_angles = c(-90 - 180/length(second_sequence) * second_sequence)
# 
# 
# hm = read.table("/Users/andrea/Downloads/HEATab2.txt", header = T, sep="\t")
# 
# hm2 = hm
# hm2$change = factor(hm2$change, levels = rev(unique(hm$change)))
# hm2$class = factor(mpg2$class, levels = unique(mpg2$class))
# 
# sequence_length = length(unique(hm2$change))
# first_sequence = c(1:(sequence_length%/%2)) 
# second_sequence = c((sequence_length%/%2+1):sequence_length) 
# first_angles =c(90 - 180/length(first_sequence) * first_sequence)
# second_angles = c(-90 - 180/length(second_sequence) * second_sequence)
# 
# ggplot(hm2, aes( change  , caller, fill=factor(pres), group = sample)) +
#   geom_tile(  color = "white", stat='identity') +
#   scale_colour_gradient2(low = "yellow2", high = "dodgerblue1",na.value="white", midpoint= 0, breaks=c(-1, 1)) + ylab("") +
#   xlab("")  + coord_polar() +
#   theme(panel.background=element_blank(),
#         axis.title=element_blank(),
#         panel.grid=element_blank(),
#         axis.text.x=element_text(angle= c(first_angles,second_angles),size=8),
#         axis.ticks=element_blank(),
#         axis.text.y=element_blank(),
#         legend.position="none",
#         plot.margin=unit(c(18,0,8,0), "cm"))


#####  circle barplot
##########  phenotypicForest
  
  library(phenotypicForest)
  
  hm = read.table("/Users/andrea/Downloads/HEATab2.txt", header = T, sep="\t")
  
  print(head(as.data.frame.matrix(hm)))  
  
  polarHistogram(as.data.frame.matrix(hm) , familyLabel = TRUE)

  
  set.seed(42)
  nFamily <- 20
  nItemPerFamily <- sample(1:6, nFamily,replace = TRUE)
  nValues <- 3
  
  df <- data.frame(
    family = rep( randomWord(nFamily), times = nValues * nItemPerFamily),
    item   = rep( randomWord(sum(nItemPerFamily), 3), each = nValues ),
    score  = rep( paste0("V",1:nValues), times = sum(nItemPerFamily)),
    value  = round(500 * runif( sum(nItemPerFamily * nValues)),2))
  
  
  randomWord<-function(n, nLetters = 5)
    replicate(n,paste(sample(letters, nLetters, replace = TRUE),sep = '', collapse=''))
  
  toyData <- function(nPhenotype = 17, nSNP = 7, nPhenotypeGroups = 3 ){
    df <- data.frame(
      phenotype      = rep(randomWord(nPhenotype), 1, each = nSNP),
      value          = rep(1:nSNP, nPhenotype) + rnorm(nSNP * nPhenotype, mean = 0, sd = 0.1),
      lowerBound     = runif(nPhenotype * nSNP, min = 0.0, max = 0.1),
      upperBound     = runif(nPhenotype * nSNP, min = 0.0, max = 0.1),
      phenotypeGroup = rep(sample(toupper(randomWord(nPhenotypeGroups)), nPhenotype, replace = TRUE), 1, each = nSNP),
      SNP            = paste('rs',rep(sample(100000, nSNP), nPhenotype), sep = '')
    )
    df <- within(df, {
      lowerBound <- value - lowerBound
      upperBound <- value + upperBound}
    )
    df
  }    
  
  polarHistogram(df , familyLabel = TRUE)
  
  
#######  circ le barplot
  ######## mio
  
  
  # library
  library(tidyverse)
  
  # Create dataset
  data=data.frame(
    individual=paste( "Mister ", seq(1,60), sep=""),
    group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
    value=sample( seq(10,100), 60, replace=T)
  )
  
  data = read.table("/Users/andrea/Downloads/HEATab3.txt", header = T, sep="\t")
  
  data = read.table("/Users/andrea/Downloads/HEATab4.txt", header = T, sep="\t")
  
  data = read.table("/Users/andrea/Downloads/HEATab31.txt", header = T, sep="\t")
  
  data = read.table("/Users/andrea/Downloads/HEATab41.txt", header = T, sep="\t")
  
   
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar=2
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$group=rep(levels(data$group), each=empty_bar)
  data=rbind(data, to_add)
  data=data %>% arrange(group)
  data$id=seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     
  # I substract 0.5 because the letter must have the angle of the center of the bars. 
  # Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data=data %>% 
    group_by(group) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data = base_data
  grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  grid_data=grid_data[-1,]
  

  # Make the plot
  #p = 
    ggplot(data, aes(x=as.factor(id), y=value, fill=as.factor(tipo))) +      
    # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x=as.factor(id), y=value, fill=as.factor(tipo)), stat="identity", alpha=1)+
    geom_bar(aes(x=as.factor(id), y=-30, fill=as.factor(wes)), stat="identity", alpha=1)  +
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    #geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), 
    #             colour = "grey", alpha=1, size=0.15 , inherit.aes = FALSE ) +
    #geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), 
    #             colour = "grey", alpha=1, size=0.15 , inherit.aes = FALSE ) +
    #geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), 
    #             colour = "grey", alpha=1, size=0.15 , inherit.aes = FALSE ) +
    #geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), 
    #             colour = "grey", alpha=1, size=0.15 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    #annotate("text", x = rep(max(data$id+2),4), y = c(20, 40, 60, 80), 
    #         label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    
    geom_bar(aes(x=as.factor(id), y=value, fill=as.factor(tipo)), stat="identity", alpha=0.5) +
    ylim(-80,80) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-3,4), "cm") 
    ) +
    coord_polar() + 
   # geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust),
    #          color="black", fontface="bold",alpha=0.7, size=2.1, angle= label_data$angle-1, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start-2, y = -1, xend = end+3, yend = -1), 
                 colour = "white", alpha=1, size=2 , inherit.aes = FALSE )  +
    geom_segment(data=base_data, aes(x = start-0.5, y = 0, xend = end+0.5, yend = 0), 
                 colour = "black", alpha=0.8, size=0.4 , inherit.aes = FALSE ) 
  
  #+
  #  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), 
  #            colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
  
  #p
  
    ggplot(data, aes(x=as.factor(id), y=value, fill=as.factor(tipo)))+
      geom_bar(aes(x=as.factor(id), y=value, fill=as.factor(tipo)), stat="identity", alpha=1) +
    geom_bar(aes(x=as.factor(id), y=-30, fill=as.factor(wes)), stat="identity", alpha=1)  +
      theme(legend.position="top")      
      
  