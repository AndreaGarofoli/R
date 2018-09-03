library(edgeR)

DE = read.table("/Users/andrea/Downloads/ionre2.txt", header = T, sep="\t")

rownames(DE) = DE[, 1]
DE = DE[, -1]
group <- c('A', 'B', 'A', 'B')



y <- DGEList( DE , group = group , gene=rownames(DE))

y <- calcNormFactors(y)


keep <- rowSums(cpm(DE) >= 1) >= 37
table(keep)


# dis = read.table("/Users/andrea/Documents/BI/BI/B/TEAD4/TabDist2.txt", header = T, sep="\t")
# dist= dis$ENSG
# keep<- rownames(y$counts) %in% dist
y <- y[keep, , keep.lib.sizes=FALSE]

design <- model.matrix(~ 0 + group)
colnames(design) <- c("A", "B")
design

y <- estimateDisp(y, design, robust=TRUE)
#plotBCV(y)

# dis = read.table("/Users/andrea/Documents/BI/BI/B/ALB/TabDist2.txt", header = T, sep="\t")
# dist= dis$ENSG
# keep<- rownames(y$counts) %in% dist
# y <- y[keep, , keep.lib.sizes=FALSE]

fit <- glmQLFit(y, design, robust=TRUE)
#plotQLDisp(fit)

con <- makeContrasts(B - A, levels=design)
qlf <- glmQLFTest(fit, contrast=con)

x<-topTags(qlf, n=999999)
View(x)
write.table(x, file = "/Users/andrea/Downloads/ionre2R.txt", sep="\t", quote = FALSE)
