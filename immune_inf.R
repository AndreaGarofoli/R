library("gplots")
library("RColorBrewer")
library("pvclust")

data = read.table("/Users/andrea/Documents/BI/BI/B/immune/GSEA112.txt", header = T, sep="\t", row.names = 1)

my_palette <- colorRampPalette(c("green", "black", "red")) #(n = 299)

col_breaks = c(seq(-3,-0.51,length=100),  
               seq(-0.5,0.5,length=100),          
               seq(0.51,3,length=100)) 

#data2= as.matrix(data[1:28,])

data2=t(as.matrix(data[,1:28]))

#data2=as.matrix(data[,1:28])

col2 <- brewer.pal(5, "Set1")

MYC=col2[data[,35]]
TERT=col2[data[,29]]
CTNNB1=col2[data[,30]]
TP53=col2[data[,31]]
TIL=col2[data[,32]]
NEO=col2[data[,34]]
clab=t(cbind(TERT,CTNNB1,TP53,TIL))
colnames(clab)=c("TERT","CTNNB1","TP53","TIL")


plot(pvclust(data2, nboot=100, method.hclust="ward.D2"))


# map2color<-function(x,pal,limits=NULL){
#   if(is.null(limits)) limits=range(x)
#   pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
# }
# 
# mypal <- colorRampPalette( c( "blue", "red" ) )( 5 )
# 
# TORT<- map2color(TERT, mypal)

library("ConsensusClusterPlus")

heatmap.2(data2,
  #        cellnote = data,  # same data set for cell labels
   #       main = "Correlation", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(3,13),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
  #dendrogram = "both", cluster_columns = data.pv$hclust, 
  ColSideColors=MYC, labCol=FALSE, hclustfun = function(x) hclust(x,method = 'ward.D2')
  #ColSideColorsSize=3
  #Rowv = "NA",
  #Colv = "NA"
  ) 


results = ConsensusClusterPlus(data2, reps=100)




cib = read.table("/Users/andrea/Documents/BI/BI/B/immune/GSEAlogreg.txt", header = T, sep="\t")



varlist <- colnames(data)[1:28]

file.remove("/Users/andrea/Documents/BI/BI/B/immune/naroba.txt")

models <- lapply(varlist, function(x) {
  tem<- glm(substitute(as.factor(MYC) ~ i, list(i = as.name(x))), data = data, family = "binomial")
  results_df <-summary.glm(tem)$coefficients
  
  write.table(results_df, file = "/Users/andrea/Documents/BI/BI/B/immune/naroba.txt", quote = FALSE, sep = "\t", append=TRUE)
})






mut = read.table("/Users/andrea/Documents/BI/BI/B/immune/CTNNB1_M.txt", header = T, sep="\t", row.names = 1)

wt = read.table("/Users/andrea/Documents/BI/BI/B/immune/CTNNB1_WT.txt", header = T, sep="\t", row.names = 1)



z.test2sam = function(a, b, var.a, var.b){
  n.a = length(a)
  n.b = length(b)
  zeta = (mean(a) - mean(b)) / (sqrt(var.a/n.a + var.b/n.b))
  return(zeta)
}

for (h in 2:90){
  
  print(z.test2sam(mut[,h], wt[,h], var(mut[,h]), var(wt[,h])))
}



qk = read.table("/Users/andrea/Documents/BI/BI/B/immune/4k.txt", header = T, sep="\t")


df_heatmap <- melt(qk, id.vars = "Sampl")
names(df_heatmap)[2:3] <- c("patient", "correlation")

max(df_heatmap[,3])
min(df_heatmap[,3])

ggplot(df_heatmap, aes(  Sampl , patient, fill=correlation)) +
  geom_tile(aes(fill = correlation),  color = "black") +
  scale_fill_gradient(low = "skyblue2", high = "tomato1", breaks=c(-7, 7), labels =c("Low","High")) +
  ylab("Mutations") +
  xlab("Genes") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(hjust = 1, face = "bold"),
        axis.text.x = element_text(angle = 50, hjust = 1, face = "bold")) +
  labs(fill = "Correlation")








data = read.table("/Users/andrea/Documents/BI/BI/B/immune/fig4_f.txt", header = T, sep="\t", row.names = 1)

max(data)
min(data)


col_breaks = c(seq(-13,-2,length=100),  
               seq(-1.99,1.99,length=100),          
               seq(2,13,length=100)) 

#data2= as.matrix(data[1:28,])

data2=as.matrix(data)

#data2=as.matrix(data[,1:28])


plot(pvclust(data2, nboot=100, method.hclust="ward.D2"))


# map2color<-function(x,pal,limits=NULL){
#   if(is.null(limits)) limits=range(x)
#   pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
# }
# 
# mypal <- colorRampPalette( c( "blue", "red" ) )( 5 )
# 
# TORT<- map2color(TERT, mypal)

my_palette <- colorRampPalette(c("green", "black", "red")) #(n = 299)

heatmap.2(data2,
          #        cellnote = data,  # same data set for cell labels
          #       main = "Correlation", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(5,3),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
          #dendrogram = "both", cluster_columns = data.pv$hclust, 
          #ColSideColors=TIL, 
          labRow = FALSE , hclustfun = function(x) hclust(x,method = 'ward.D2')
          #ColSideColorsSize=3
          #Rowv = "NA",
          #Colv = "NA"
)



data = read.table("/Users/andrea/Documents/BI/BI/B/immune/GSEA112_2.txt", header = T, sep="\t", row.names = 1)

my_palette <- colorRampPalette(c("green", "black", "red")) #(n = 299)

col_breaks = c(seq(-3,-0.51,length=100),  
               seq(-0.5,0.5,length=100),          
               seq(0.51,3,length=100)) 

#data2= as.matrix(data[1:28,])

data2=t(as.matrix(data[,1:28]))

#data2=as.matrix(data[,1:28])

col2 <- brewer.pal(4, "Set1")


CT=c("#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#103a27", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#3f8be2", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444", "#e24444")


plot(pvclust(data2, nboot=100, method.hclust="ward.D2"))


heatmap.2(data2,
          #        cellnote = data,  # same data set for cell labels
          #       main = "Correlation", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(3,13),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
          #dendrogram = "both", cluster_columns = data.pv$hclust, 
          ColSideColors=CT, labCol=FALSE, hclustfun = function(x) hclust(x,method = 'ward.D2')
          #ColSideColorsSize=3
          #Rowv = "NA",
          #Colv = "NA"
) 


a=c(3123, 5186, 1037, 681, 99, 1958, 598, 4540, 1074, 1887, 2724, 1614, 1128, 865, 905, 126, 6504, 4031, 3573, 1299, 60, 2411, 9950, 3033, 907, 19572, 5024, 9986, 3238, 4548, 6887, 631, 2075, 11608, 5455, 17415, 3714, 136, 1146, 74, 1046, 3293, 706, 3022, 4099, 2902, 690, 10336, 5235, 3741, 10080, 4578, 2120, 3668, 162, 2795, 956, 1606, 1750, 1953, 5440, 3109, 1573, 5669, 4187, 1884, 1589, 5163, 913, 4362, 9364, 6679, 433, 4964, 1001, 129, 1706, 132, 1603, 5096, 7099, 3719, 1002, 4427, 365, 6903, 2036, 3334, 713, 434, 1553, 8833, 4970, 2121, 1434, 5843, 413, 1311, 373, 5067, 124, 297, 122, 2402, 1362, 2389, 1508, 129, 5350, 5130, 1996, 11899, 1921, 2904, 3999, 1500, 1811, 1082, 1859, 3035, 2480, 1647, 2038, 5246, 2633, 3250, 5547, 2845, 3476, 786, 2572, 3643, 3283, 6161, 4636, 433, 6311, 1470, 2201, 3298, 399, 3834, 826, 2155, 1298, 14216, 2777, 1427, 3865, 6661, 1490, 3659, 1632, 3211, 5671, 1111, 2453, 2382, 1722, 3455, 723, 5141, 3193, 2790, 1263, 5403, 1050, 5386, 3735, 266, 142, 219, 1084, 402, 510, 11603, 9536, 3757, 1415, 1265, 1256, 2897)

quantile(a)
