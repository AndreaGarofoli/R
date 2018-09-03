# Final_Garofoli.Andrea.R
# Garofoli Andrea
# 2 January, 2018
# Introduction to Statistics with R, Final Paper


# [T4]

library(yarrr)
library(dplyr)
library(ggplot2)

# [T5]


Mdata <- read.table("/Users/andrea/A/rcourse/metabric.txt", #of course this is just for me
                   sep = "\t", header = TRUE, row.names = 1)

# [T6]


summary(Mdata)
str(Mdata)
nrow(Mdata)
ncol(Mdata)

# [T7]

names(Mdata)[12] <- "Nottingham.Prognostic.Index"

# [T8]

sd(Mdata$size)
mean(Mdata$age_at_diagnosis)
median(Mdata$Nottingham.Prognostic.Index)

# [T9]

table(Mdata$histology)

# [T10]

Mdata$erStatus<-  case_when(Mdata$erStatus == "POS" ~ "+",
          Mdata$erStatus == "NEG" ~ "-")
Mdata$her2Status<- case_when(Mdata$her2Status == "POS" ~ "+",
          Mdata$her2Status == "NEG" ~ "-")

# [T11]

is.outlier <- Mdata$size > (mean(Mdata$size) + 3 * sd(Mdata$size)) | 
  Mdata$size < (mean(Mdata$size) - 3 * sd(Mdata$size))
length(is.outlier[is.outlier==TRUE])


# [T12]

ggplot(Mdata, aes(x=age_at_diagnosis, y=size, fill=grade)) + geom_point(shape=21, size=1) + 
  labs(color = "Grade",title = "Patient age", y = "Tumor size", x="") + theme_light() +
  #scale_y_sqrt(limits = c(0, 3.6), expand = c(0, 0), breaks=c(0,0.5,0.1,0.5,1,1.5,2,2.5,3,3.5))+
  #scale_x_continuous(limits = c(0, 1.01), expand = c(0, 0))+
  #  scale_size(range=c(1,8))+
  scale_fill_continuous(low = "white", high = "grey30") +  theme( panel.border = element_blank(), panel.background = element_blank(),
                                                                  plot.title = element_text(size = 14, family = "Arial", face = "bold"),
                                                                  text=element_text(family="Arial"),
                                                                  axis.title = element_text(face="bold"),
                                                                  axis.text.y=element_text(colour="black", size = 9),
                                                                  axis.line = element_line(size=0.5, colour = "black"),
                                                                  legend.title=element_blank())  + 
  geom_smooth(method='lm')


# [T13]

hist(as.numeric(Mdata$size), col="grey 70", 
     xlab= "Tumor Size", family = "Arial", font.lab=1 , main="", breaks=seq(0,200,by=2))
     

# [T14]

ggplot(Mdata, aes( y=size , x=sizeGreater50
)) + geom_violin() + #geom_boxplot(notch = TRUE)+  #+ geom_smooth(method=lm, se=FALSE)
  geom_point(aes(y=size , x=sizeGreater50), color = "black", size = 1, data = Mdata) + 
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "#d3d3d3", linetype = 1), 
        panel.grid.minor = element_line(colour="#d3d3d3", size=0.1, linetype = 1),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Arial", face = "bold"),
        text=element_text(family="Arial"),
        axis.title = element_text(face="bold"),
        axis.text.y=element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black"),
        # axis.text.x=element_blank(),
        legend.title=element_blank()) +
  scale_colour_manual(values=c("grey50", "grey30"))


# [T15]

aggregate(formula = size ~ sizeGreater50,
          data = Mdata,
          FUN = mean)


# [T16]

x<- t.test(formula = ageGreater55 ~ sizeGreater50, data = Mdata)


# Welch Two Sample t-test
# 
# data:  ageGreater55 by sizeGreater50
# t = -0.021938, df = 24.085, p-value = 0.9827
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2166168  0.2120593
# sample estimates:
#   mean in group FALSE  mean in group TRUE 
# 0.6498952           0.6521739 


# [T17]

t.test(formula = size ~ lnPositive,  data = Mdata, subset= size %in% c(20, 50))


# Welch Two Sample t-test
# 
# data:  size by lnPositive
# t = -2.1017, df = 63.765, p-value = 0.03954
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -8.9568743 -0.2267992
# sample estimates:
#   mean in group FALSE  mean in group TRUE 
# 21.83673            26.42857 


# [T18]

cor.test(formula = ~ size + grade,
         data = Mdata)

# Pearson's product-moment correlation
# 
# data:  size and grade
# t = 3.6947, df = 498, p-value = 0.0002445
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.07674924 0.24748625
# sample estimates:
# cor 
# 0.1633405 


# [T19]

cor.test(formula = ~ stage + age_at_diagnosis,
         data = Mdata, subset= size %in% c(50, 80))

# Pearson's product-moment correlation
# 
# data:  stage and age_at_diagnosis
# t = 0.30128, df = 12, p-value = 0.7684
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.4653269  0.5900962
# sample estimates:
# cor 
# 0.08664452 

# [T20]

chisq.test(x = table(Mdata$histology))

# Chi-squared test for given probabilities
# 
# data:  table(Mdata$histology)
# X-squared = 1635.4, df = 7, p-value < 2.2e-16

# [T21]

xx<-aov(formula = size ~ age_at_diagnosis,
        data = Mdata)
summary(xx)


# [T22]

xx<-aov(formula = stage ~ size + histology,
        data = Mdata)
summary(xx)


# [T23]

aov(formula = size ~ her2Status * erStatus,
    data = Mdata)

# [T24]

glm(formula = ageGreater55 ~ sizeGreater50,
    data = Mdata,
    family = binomial)

# [T25]

summary(xx)
