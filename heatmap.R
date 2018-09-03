
#########################################################
### A) Installing and loading required packages
#########################################################

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}


#########################################################
### B) Reading in data and transform it into matrix format
#########################################################

data = read.table("/Users/andrea/Documents/BI/BI/B/age/bye.csv")
#data <- read.csv("../datasets/heatmaps_in_r.csv", comment.char="#")

rnames <- data[,1]  # assign labels in column 1 to "rnames"
cnames <- t(data)[,1]
mat_data <- t(data.matrix(data[,2:ncol(data)]))  # transform column 2-5 into a matrix
colnames(mat_data) <- rnames                  # assign row names
rownames(mat_data) <- c("TP53","PIK3CA","GATA3","PTEN","RB1","MLL3","SF3B1","PTPRD","COL1A1","MAP3K1","HIST1H3B","BRCA1","NPAS4","MET","KCNB2","PREX2","ZMYM2","ARID1B","EWSR1","CREBBP","MAP2K4","AKT1","NCOR1","PIK3R1","ARID1A","CTCF","NF1","SPEN","ZFP36L1","MLLT4","BRCA2","XBP1","HLA-DRB1","ERBB2","FBXW7","MEN1","ATM","ERBB3","ESR1","DLG1","ZFHX3","NLRP2","FIP1L1","FOXO3","C11orf30","NF2","IL7R","LZTR1","IL6ST","SET","SOX10","CACNA1D","GRB7","BAG4","NIN")


#rnames <- data[,1]  # assign labels in column 1 to "rnames"
#cnames <- t(data)[,1]
#mat_data <- data.matrix(data[,2:ncol(data)])  # transform column 2-5 into a matrix
#rownames(mat_data) <- rnames                  # assign row names
#colnames(mat_data) <- c("TP53","MAP3K1","CDH1","PTEN","MAP2K4","PIK3CA","CBFB","AKT1","RB1","MLL3","NCOR1","TBX3","GATA3","RUNX1","PIK3R1","ARID1A","CTCF","CDKN1B","FOXA1","MED23","NF1","SPEN","ZFP36L1","HIST1H3B","MLLT4","TBL1XR1","BRCA2","BRCA1","XBP1","HLA-DRB1","SF3B1","FOXP1","CASP8","MYB","ERBB2","FBXW7","SETD2","CRIPAK","MEN1","SMAD4","ATM","ERBB3","ESR1","HSP90AB1","NCOA3","STK11","DLG1","ZFHX3","NPAS4","NLRP2","HSP90AA1","FIP1L1","PSIP1","FOXO3","EIF4A2","C11orf30","MLL","MET","PDGFB","KCNB2","KRAS","PRKAR1A","AFF3","PRRX1","UNC13C","HSPA8","AXL","NF2","PREX2","MYH9","ATIC","PTPRD","KEAP1","ZMYM2","NOTCH4","IL7R","CASC5","GRIN2A","LIFR","NTRK1","MAP3K13","LZTR1","IL6ST","EXT1","ARID1B","SET","JAK1","RPL5","EWSR1","SOX10","TRA2B","CACNA1D","COL1A1","GRB7","NOTCH2","BAG4","AURKA","CREBBP","ARNT","NIN")

#########################################################
### C) Customizing and plotting the heat map
#########################################################

# creates a own color palette from red to green
my_palette <- colorRampPalette(c("white", "white", "red"))(n = 299)

# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(0,1.5,length=100),  # for red
               seq(1.51,1.8,length=100),           # for yellow
               seq(1.81,2,length=100))             # for green

# creates a 5 x 5 inch image
png("/Users/andrea/Documents/BI/BI/B/age/YoungHeatmap.png",    # create PNG for the heat map
    width = 4*450,        # 5 x 300 pixels
    height = 4*650,
    res = 300,            # 300 pixels per inch
    pointsize = 8)        # smaller font size

heatmap.2(mat_data,
         # cellnote = mat_data,  # same data set for cell labels
          main = "Young Simple Somatic Mutations", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(5,5),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          sepcolor="black",
          colsep=1:ncol(mat_data),
          key=FALSE,
          rowsep=1:nrow(mat_data),
          Rowv=FALSE,
          Colv="NA")#, ColSideColors=cc)            # turn off column clustering

dev.off()               # close the PNG device



#########################################################
### A) Installing and loading required packages
#########################################################

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}


#########################################################
### B) Reading in data and transform it into matrix format
#########################################################

data = read.table("/home/andrea/BI/B/age/bo.txt")
#data <- read.csv("../datasets/heatmaps_in_r.csv", comment.char="#")

rnames <- data[,1]  # assign labels in column 1 to "rnames"
cnames <- t(data)[,1]
mat_data <- t(data.matrix(data[,2:ncol(data)]))  # transform column 2-5 into a matrix
colnames(mat_data) <- rnames                  # assign row names
rownames(mat_data) <- c("TP53","PIK3CA","GATA3","MAP3K1","MLL3","CDH1","PTEN","RB1","TBX3","ARID1A","SPEN","AKT1","CBFB","NCOR1","MAP2K4","FOXA1","NF1","SF3B1","PREX2","NOTCH2","MED23","ERBB2","MLL","CACNA1D","C11orf30","PTPRD","PIK3R1","MLLT4","BRCA2","UNC13C","NOTCH4","CTCF","SETD2","ERBB3","KCNB2","AFF3","CASC5","BRCA1","ATM","ZFHX3","AXL","MYH9","NTRK1","FBXW7","ESR1","NPAS4","HSP90AA1","FIP1L1","GRIN2A","AURKA","CREBBP","NIN")


#rnames <- data[,1]  # assign labels in column 1 to "rnames"
#cnames <- t(data)[,1]
#mat_data <- data.matrix(data[,2:ncol(data)])  # transform column 2-5 into a matrix
#rownames(mat_data) <- rnames                  # assign row names
#colnames(mat_data) <- c("TP53","MAP3K1","CDH1","PTEN","MAP2K4","PIK3CA","CBFB","AKT1","RB1","MLL3","NCOR1","TBX3","GATA3","RUNX1","PIK3R1","ARID1A","CTCF","CDKN1B","FOXA1","MED23","NF1","SPEN","ZFP36L1","HIST1H3B","MLLT4","TBL1XR1","BRCA2","BRCA1","XBP1","HLA-DRB1","SF3B1","FOXP1","CASP8","MYB","ERBB2","FBXW7","SETD2","CRIPAK","MEN1","SMAD4","ATM","ERBB3","ESR1","HSP90AB1","NCOA3","STK11","DLG1","ZFHX3","NPAS4","NLRP2","HSP90AA1","FIP1L1","PSIP1","FOXO3","EIF4A2","C11orf30","MLL","MET","PDGFB","KCNB2","KRAS","PRKAR1A","AFF3","PRRX1","UNC13C","HSPA8","AXL","NF2","PREX2","MYH9","ATIC","PTPRD","KEAP1","ZMYM2","NOTCH4","IL7R","CASC5","GRIN2A","LIFR","NTRK1","MAP3K13","LZTR1","IL6ST","EXT1","ARID1B","SET","JAK1","RPL5","EWSR1","SOX10","TRA2B","CACNA1D","COL1A1","GRB7","NOTCH2","BAG4","AURKA","CREBBP","ARNT","NIN")

#########################################################
### C) Customizing and plotting the heat map
#########################################################

# creates a own color palette from red to green
my_palette <- colorRampPalette(c("white", "white", "red"))(n = 299)

# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(0,1.5,length=100),  # for red
               seq(1.51,1.8,length=100),           # for yellow
               seq(1.81,2,length=100))             # for green

# creates a 5 x 5 inch image
png("/home/andrea/BI/B/age/OldHeatmap.png",    # create PNG for the heat map
    width = 4*1650,        # 5 x 300 pixels
    height = 4*650,
    res = 300,            # 300 pixels per inch
    pointsize = 8)        # smaller font size

heatmap.2(mat_data,
          # cellnote = mat_data,  # same data set for cell labels
          main = "Old Simple Somatic Mutations", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(5,5),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          sepcolor="black",
          colsep=1:ncol(mat_data),
          key=FALSE,
          rowsep=1:nrow(mat_data),
          Rowv=TRUE,
          Colv="none")            # turn off column clustering

dev.off()               # close the PNG device
