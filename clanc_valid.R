## Make sure your R working directory is set to the folder containing the 
## file clanc.R.
source("/Users/andrea/Documents/BI/BI/B/PhD/clanc_share/clanc.R")
load("/Users/andrea/Documents/BI/BI/B/PhD/20161103.RData")
load("/Users/andrea/Downloads/metabric.geneCN.RData")
write.csv(pheno, file = "/Users/andrea/Documents/BI/BI/B/PhD/pheno_r_file.csv")




## Load the example data sets in the "data" directory.  These are simulated 
## microarray data sets, consisting of 10 arrays each from 4 classes; the 
## first 10 arrays are from class 1, the second 10 from class 2, etc.  And 
## each array has 1500 genes on it.  The "train" data set is intended for 
## training purposes; these are the data you would use in training and 
## building your classifier.  The "test" data are intended for testing a 
## classifier, using data that was not used in the classifier building 
## process.  You may or may not have test data.  If you intend to use all of 
## your samples to build the classifier and rely on cross-validation to 
## estimate the resulting error rates, you need not have *any* test data.  

# Y_train = read.delim("/Users/andrea/Documents/BI/BI/B/PhD/clanc_share/data/trainExample.txt", header = T)
# Y_test = read.delim("/Users/andrea/Documents/BI/BI/B/PhD/clanc_share/data/testExample.txt", header = T)




#Y_train = read.delim("/Users/andrea/Documents/BI/BI/B/PhD/prova.csv", header = T, sep=",")
Y_train = read.delim("/Users/andrea/Documents/BI/BI/B/PhD/Vdatasetdiscov.csv", header = T, sep=",")


rownames(Y_train) = Y_train[, 1]
Y_train = as.matrix(Y_train[, -1])




#Y_test = read.delim("/Users/andrea/Documents/BI/BI/B/PhD/provaV.csv", header = T, sep=",")
Y_test = read.delim("/Users/andrea/Documents/BI/BI/B/PhD/Vdatasetval.csv", header = T, sep=",")


rownames(Y_test) = Y_test[, 1]
Y_test = as.matrix(Y_test[, -1])

## Create vector for class membership.  For these example data, we just 
## need 10 ones followed by 10 twos, then 10 threes, then 10 fours.
#id = rep(1:4, each = 10)

#id=c( 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 2, 1, 2, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1)
#id=c( 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, NA, 0, 0, 0, NA, NA, 0, 0, 0, NA, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, NA, 0, 0, 1, 0, 0, 0, 0, NA, NA, 0, 0, 0, NA, 0, 0, 0, 0, 0, NA, NA, 0, 1, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, NA, 0, NA, 0, 0, 1, NA, 0, NA, 1, 0, 0, 0, 1, 0, 1, 0, NA, 0, 0, NA, 0, NA, 0, 0, NA, NA, NA, 0, 1, 0, NA, 0, 1, NA, NA, NA, NA, NA, NA, 0, NA, NA, 1, NA, NA, 1, 0, NA, NA, 1, 1, NA, 1, NA, NA, NA, 0, 0, 0, 1, 1, NA, NA, 1, 1, 1, 1, NA, 0, 1, NA, 1, 1, NA, 0, 0, NA, 0, NA, 1, 0, 0, 0, NA, NA, NA, NA, NA, 1, 0, NA, NA, 0, NA, 0, 0, 0, 1, 1, 1, 1, 0, NA, 0, NA, 0, 1, 1, NA, 1, NA, NA, 0, 0, NA, NA, NA, NA, 0, NA, 0, NA, NA, NA, 0, NA, 0, NA, NA, NA, 1, 1, 1, 0, 0, 1, NA, NA, 0, NA, 1, NA, 0, 1, 1, NA, 0, 1, NA, NA, 0, 1, NA, 1, 0, 0, 0, 1, 1, NA, 1, NA, NA, NA, 1, 1, NA, 1, NA, NA, NA, NA, NA, 0, NA, 0, 0, 0, 0, 0, NA, NA, NA, 0, 0, 0, NA, 1, 0, 0, 0, 1, 1, 0, 0, NA, NA, 1, 0, 1, NA, 0, 0, 0, 0, 0, 1, 1, 0, NA, 1, 0, NA, 1, 0, NA, 0, 0, 0, 0, NA, NA, NA, NA, 0, NA, NA, 0, 1, 0, 0, 0, 0, 1, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, NA, 0, 1, 0, 0, 0, 0, 0, NA, 0, 0, 0, 1, NA, 0, 1, 0, 0, NA, 0, NA, 0, NA, 1, NA, 1, 0, 0, 0, 0, 1, NA, NA, 0, 1, NA, 0, NA, 0, 1, 0, NA, NA, NA, 1, 0, 0, 0, 0, NA, 0, 0, NA, 1, NA, 0, 1, 1, 0, NA, 1, 0, 0, 0, NA, NA, NA, 0, 1, 0, 1, 1, 0, 1, 1, NA, 0, NA, NA, 0, NA, 0, 0, NA, NA, 0, 1, 0, 0, 0, 0, 0, 0, NA, 0, 0, 1, NA, NA, 0, 0, 0, 0, 0, 0, 0, NA, 0, NA, NA, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, NA, NA, 0, NA, 0, 0, NA, 0, 0, NA, 0, NA, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, NA, 0, 0, 0, NA, 1, 0, 0, 1, 0, 0, 0, 0, NA, 0, 0, 0, NA, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, NA, 1, 0, 0, NA, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, NA, 0, NA, 0, 1, NA, 0, 1, 1, 0, 1, NA, 0, 0, 0, 1, 0, NA, NA, 0, 0, 0, 1, NA, 1, 0, 0, NA, 0, NA, 0, NA, NA, NA, NA, NA, NA, 0, 0, 1, 0, 0, 0, NA, 1, NA, 0, 0, 1, NA, NA, 1, 0, 0, 0, 0, NA, 0, 0, 0, 0, NA, 0, 0, NA, NA, 0, 0, 0, 0, 0, NA, NA, 0, 0, 0, 0, NA, NA, 0, 0, 0, 0, 1, NA, 1, 0, NA, 0, NA, NA, 0, 0, 0, NA, 0, 0, NA, 0, 0, 0, 0, 1, 1, NA, 1, 0, 1, 1, NA, 1, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA, NA, NA, 0, NA, NA, 1, 1, 0, NA, 0, 0, 0, 0, 1, NA, 0, 1, NA, 0, 0, 1, 0, NA, NA, 0, 0, 0, NA, NA, NA, NA, 0, 0, 0, 1, NA, 0, 0, NA, NA, NA, NA, 1, NA, 0, 0, NA, 0, NA, 0, 1, 1, NA, 0, 1, NA, NA, 0, 0, 0, 1, 0, NA, 1, 0, 1, 0, 1, 0, 0, 0, NA, NA, 0, 1, NA, 1, 0, NA, 0, NA, 0, 0, 0, 0, NA, 0, NA, 0, NA, 1, NA, 1, NA, 0, 0, 0, NA, 1, NA, 0, 0, 1, 0, 0, NA, NA, NA, 1, NA, NA, 0, NA, 0, 1, 1, 0, 0, 0, 0, NA, 0, 0, 0, 0, NA, 0, 0, 1, NA, 0, 0, 0, 0, 0, 0, 0, 1, NA, 1, 0, NA, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, NA, 0, 0, 0, 0, 1, NA, NA, 0, 0, NA, 0, NA, NA, 0, NA, 0, 0, 0, 1, 0, NA, NA, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, NA, NA, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, NA, 0, 0, 0, NA, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, NA, NA, 0, 1, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 1, 0, 0, 0, NA, 0, 0, NA, 0, 0, 0, 1, 0 )
#id=c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
#id=c(  0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1)      

id=c( 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 2, 1, 1, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 1, 2, 1, 1, 1, 2, 2, 1, 2, 2, 1, 2, 1, 2, 2, 2, 2, 1, 1, 1, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 2, 2, 2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 1, 2, 1, 2, 2, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 1, 2)      


## The gene names are just the row names from the data matrices, the same 
## for both the example train and test data sets.
gene_names = rownames(Y_train)

## We'll just set the class names to be the numbers 1, 2, 3, 4.
#class_names = 1:4

class_names = c("grey","red")


## Now ready to use cross-validation to estimate the error rates for 
## classifiers of different sizes (different numbers of genes used in 
## building the classifier).

source("/Users/andrea/Documents/BI/BI/B/PhD/clanc_share/clanc.R")

cv_out = cvClanc(Y_train, id, c(1:50))

## View the estimated error rates associated with different feature-set 
## sizes.  By default, cvClanc() will assess the classifiers built using 1, 
## 2, 3, ..., 10 features.  In this example, an estimated 100% accuracy is 
## attained with as few as 5 features per class.
plot(c(1:50), cv_out$overallErrors, type = "l", lwd = 2, col = "blue", xlab = 
  "Number of features", ylab = "CV error")
  
## Assuming we're happy with using 5 features per class as the basis for 
## our model, we can now train and build our final classifier.
train_out = trainClanc(Y_train, id, gene_names)
build_out = buildClanc(Y_train, id, class_names, train_out, active = 50)

## And if we have test data, we can test the classifier on them.  In our 
## example, we make one misclassification (for one of the samples in class 
## 4.

# id=c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
# 
 #test_out = testClanc(Y_test, id, gene_names, build_out)
# 
# 
# 
# predictClanc(Y_test, gene_names, build_out)

pre <- predictClanc(Y_test, gene_names, build_out)
vero <- c( 1, 2, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 2, 1, 2, 2, 2, 2, 1, 1, 2, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 2, 2, 1, 2, 1, 1, 2, 1, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 1, 2, 1, 2, 2, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 1, 2, 2, 1, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 1, 2, 2)


#vero2<- c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
source("/Users/andrea/Documents/BI/BI/B/PhD/clanc_share/clanc.R")


test_out = testClanc(Y_test, vero, gene_names, build_out)

valg=c()
vals=c()

vv=0
ff=0
vs=0
fs=0

for (i in 1:276){
  if (vero[i]==1 && pre[i]==1){ff=ff+1}
  if (vero[i]==1 && pre[i]==2){vs=vs+1}
  if (vero[i]==2 && pre[i]==2){
    valg <- c(valg, colnames(Y_test)[i])
    vv=vv+1}
  if (vero[i]==2 && pre[i]==1){
    vals <- c(vals, colnames(Y_test)[i])
    fs=fs+1}
}



sen=(vv/(vv + fs))*100

spe=(ff/(ff + vs))*100




disg=c()
diss=c()


pre <- predictClanc(Y_train, gene_names, build_out)
test_out = testClanc(Y_train, id, gene_names, build_out)


tvv=0
tff=0
tvs=0
tfs=0

for (i in 1:306){
  if (id[i]==1 && pre[i]==1){tff=tff+1}
  if (id[i]==1 && pre[i]==2){tvs=tvs+1}
  if (id[i]==2 && pre[i]==2){
    tvv=tvv+1
    disg <- c(disg, colnames(Y_train)[i])}
  if (id[i]==2 && pre[i]==1){
    tfs=tfs+1
    diss <- c(diss, colnames(Y_train)[i])}
}

tsen=(tvv/(tvv + tfs))*100

tspe=(tff/(tff + tvs))*100







library(Rcpp)
sourceCpp('/Users/andrea/Documents/BI/BI/B/PhD/TSS/TSS.cpp')
source('/Users/andrea/Documents/BI/BI/B/PhD/TSS/predict_TSS.r')
source('/Users/andrea/Documents/BI/BI/B/PhD/TSS/MannTest.R') ## Mann-Whitney test
source('/Users/andrea/Documents/BI/BI/B/PhD/TSS/KWTest.r') ## Kruskal-Wallis test

source('/Users/andrea/Documents/BI/BI/B/PhD/TSS/GreedySearch_init.R') ## requires by the greedy search algorithm
source('/Users/andrea/Documents/BI/BI/B/PhD/TSS/GreedySearch_augt.R') ## requires by the greedy search algorithm


## load the training and test set of leukemia cancer data
#trainData = read.csv('./data/Leukemia1_train.csv',header=F , row.names=1)
trainData = read.delim("/Users/andrea/Documents/BI/BI/B/PhD/prova.csv", header = T, sep=",")
#testData =  read.csv('./data/Leukemia1_test.csv',header=F  , row.names=1)
testData = read.delim("/Users/andrea/Documents/BI/BI/B/PhD/provaV.csv", header = T, sep=",")

## load the class labels of patients
#trclass = c(as.numeric(trainData[1,]),as.numeric(testData[1,]))
trclass = c(id,vero)
#trM = cbind(as.matrix(trainData[-1,]),as.matrix(testData[-1,]))
trM = cbind(as.matrix(trainData[,-1]),as.matrix(testData[,-1]))




library(Rcpp)
sourceCpp('/Users/andrea/Documents/BI/BI/B/PhD/TSS/TSS.cpp')
source('/Users/andrea/Documents/BI/BI/B/PhD/TSS/predict_TSS.r')

## load bioconductor libraries to extract pathway information
library(Biobase)
library(annotate)
library(GSEABase)

## load the training and test set of leukemia cancer data
#trainData = read.csv('./data/Leukemia1_train.csv',header=F , row.names=1)
trainData = read.delim("/Users/andrea/Documents/BI/BI/B/PhD/prova.csv", header = T, sep=",")
#testData =  read.csv('./data/Leukemia1_test.csv',header=F  , row.names=1)
testData = read.delim("/Users/andrea/Documents/BI/BI/B/PhD/provaV.csv", header = T, sep=",")

## load class labels
trainclass = id
testclass = vero

trainM = as.matrix(trainData[, -1])
testM = as.matrix(testData[, -1])

## extract KEGG pathway information from the training set
trainEset = new("ExpressionSet", exprs=trainM, annotation = "hu6800")
Kcol = GeneSetCollection(trainEset, setType=KEGGCollection())
allProbes = featureNames(trainEset)

probes = geneIds(Kcol[['05340']]) ## select genes from pathway 05340
genes = integer(0)
for(j in 1:length(probes))
  genes = c(genes,which(allProbes == probes[j]))

## form gene sets and obtain top scoring sets
geneSets = t(combn(genes,3))
tss = TopScoringSet(trainM,trainclass,3,geneSets)

## predict on the test set and get the confusion matrix
pred = predict_TSS(tss, testM)
table(pred,testclass)



