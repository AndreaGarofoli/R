# Assignment: WPA 6
# Name: GAROFOLI ANDREA
# Date: 1 11 2017

#Open your class R project. Open a new script and enter your name, date, and the wpa number at the top. 
#Save the script in the R folder in your project working directory as wpa_6_LastFirst.R, 
#where Last and First are your last and first names.

#The data are stored in a tab–delimited text file located at 
# https://raw.githubusercontent.com/ndphillips/IntroductionR_Course/master/assignments/wpa/data/studentsurvey.txt. 
# Using  read.table() load this data into R as a new object called survey.
# 


survey <- read.table(file = "https://raw.githubusercontent.com/ndphillips/IntroductionR_Course/master/assignments/wpa/data/studentsurvey.txt", 
                     sep = "\t",                             
                     header = TRUE,                         
                     stringsAsFactors = FALSE)



# Using head(), str(), and View() look at the dataset and make sure that it was loaded correctly. 
# If the data don’t look correct (i.e; if there isn’t a header row and 100 rows and 12 columns), 
# then something must have gone wrong when you loaded the data. Try and fix it!
#   
head(survey)
str(survey)
View(survey)

# Save a local copy of the data as a tab-separated text file called studentsurvey.txt to the 
# data folder of your project using  write.table().

write.table(x = survey,                    # Save the object matthews
            file = "/Users/andrea/A/rcourse/data/studentsurvey.txt",      # Write the object to matthews.txt in the data folder
            sep = "\t")


# The average global IQ is known to be 100. Do the participants have an IQ different from the general population? 
# Answer this with a one-sample t-test.
# 
t.test(x = survey$iq, 
       mu = 100)

# A friend of yours claims that students have 2.5 siblings on average. Test this claim with a one-sample t-test.
# 
t.test(x = survey$siblings, 
       mu = 2.5)

# Do students that have smoked marijuana have different IQ levels than those who have never smoked marijuana? 
# Test this claim with a two-sample t-test (you can either use the vector or the formula notation for a t-test)
# 
t.test(formula = iq ~ marijuana,
       data = survey, 
       subset = marijuana %in% c("1", "0"))


# Do students with higher multitasking skills tend to have more romantic partners than those with lower multitasking skills? 
# Test this with a correlation test:
#   
cor.test(formula = ~ multitasking + partners,
         data = survey)

# Do people with higher IQs perform faster on the logic test? Answer this question with a correlation test.
# 
cor.test(formula = ~ iq + logic,
         data = survey)

# Are some majors more popular than others? Answer this question with a one-sample chi-square test.
# 
chisq.test(table(survey$major))

# In general, were students more likely to take a risk than not? 
#Answer this question with a one-sample chi-square test
# 
chisq.test(table(survey$risk))

# Is there a relationship between hair color and students’ academic major? 
#Answer this with a two-sample chi-square test
# 
chisq.test(table(survey$haircolor,
                 survey$major))

# Is there a relationship between whether a student has ever smoked marijuana and his/her 
#decision to accept or reject the risky gamble?
# 
chisq.test(table(survey$haircolor,
                 survey$major))

# Do males and females have different numbers of sexual partners on average?
# 
t.test(formula = partners ~ sex,
       data = survey, 
       subset = sex %in% c("m", "f"))

# Do males and females differ in how likely they are to have smoked marijuana?
# 
chisq.test(table(survey$sex,
                 survey$marijuana))

# Do people who have smoked marijuana have different logic scores on average than those who 
#never have smoked marijuana?
#
t.test(formula = logic ~ marijuana,
       data = survey, 
       subset = marijuana %in% c("1", "0"))



#Run the following code to create the anscombe dataframe. 
#This dataframe contains 11 pairs of data for four different datasets: A, B, C and D
# JUST COPY, PASTE, AND RUN!

anscombe <- data.frame(x = c(c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5),
                             c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5),
                             c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5),
                             c(8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8)),
                       y = c(c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 4.68),
                             c(9.14, 8.14, 8.74, 8.77, 9.26, 8.1, 6.13, 3.1, 9.13, 7.26, 4.74),
                             c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73),
                             c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.5, 5.56, 7.91, 6.89)),
                       set = rep(c("A", "B", "C", "D"), each = 11))

#Look at the anscombe dataframe to see how it is structured. 
#Then, calculate the correlation between x and y separately for each dataset. 
#That is, what is the correlation between x and y for dataset A? 
#What about for datasets B, C and then D? You don’t need to report full APA style 
#for these tests, just make a note of the correlation coefficients. 
#What do you notice about the correlation coefficients for each dataset? 
#Would you conclude that these datasets are very similar or very different?
#
for (i in c("A", "B", "C", "D")){
print(cor.test(formula = ~ x + y,
         data = anscombe, subset=set==i))
  }


# Now it’s time to actually look at the data and see if your prediction holds up! 
#   Run the following code to generate a scatterplot of each data pair, what do you find?
# # JUST COPY, PASTE, AND RUN!

library(ggplot2)    # Load ggplot2

ggplot(data = anscombe,
       aes(x = x, y = y, col = set)) + 
  geom_point(size = 2) + 
  facet_wrap(~set) + 
  labs(title = "Anscombe's Quartet", 
       subtitle = "Always look at your data before conducting hypothesis tests!",
       caption = "Anscombe Wikipedia page: https://en.wikipedia.org/wiki/Anscombe%27s_quartet") +
  theme_bw() +
  guides(colour = "none")


# Do people with higher iq scores tend to perform better on the 
# logic test that those with lower iq scores?
# 
chisq.test(table(survey$iq, survey$logic))

# Are Germans more likely than not to have tried marijuana? 
# (Hint: this is a one-sample chi-square test on a subset of the original data).
# 
chisq.test(table(survey$marijuana[survey$country=="germany"]))

# Does the IQ of people with brown hair differ from blondes? 
# (Hint: This is a two-sample t-test that requires you to use the 
#subset() argument to tell R which two groups you want to compare)
# 
t.test(formula = iq ~ haircolor,
       data = survey, 
       subset = haircolor %in% c("blonde", "brown"))

# Only for men from Switzerland, is there a relationship between age and IQ?
# 
chisq.test(table(survey$marijuana[survey$country=="switzerland" & survey$sex=="m"]))

# Only for people who chose the risky gamble and have never tried 
# marijuana, is there a relationship between iq and performance on the logic test?
#
chisq.test(table(survey$iq[survey$risk=="1" & survey$marijuana=="0"], 
                 survey$logic[survey$risk=="1" & survey$marijuana=="0"]))


#What do you predict the correlation will be between the following two vectors x and y? 
#Test your prediction by conducting the appropriate test 
#(hint: you may want to put the vectors into a dataframe by running  
#df <-data.frame(x = x, y = y) before running the correlation test),

x <- rnorm(n = 100, mean = 10, sd = 10)
y <- rnorm(n = 100, mean = 50, sd = 10)
df <-data.frame(x = x, y = y)
cor.test(formula = ~ x + y,
         data = df)

#What about the correlation between the following two vectors? 
#Test your prediction by conducting the appropriate test.
x <- rnorm(n = 100, mean = 10, sd = 10)
y <- -5 * x
df <-data.frame(x = x, y = y)
cor.test(formula = ~ x + y,
         data = df)

#What about these? Test your prediction by conducting the appropriate test.
x <- rnorm(n = 100, mean = 10, sd = 10)
y <- x + rnorm(n = 100, mean = 50, sd = 10)
df <-data.frame(x = x, y = y)
cor.test(formula = ~ x + y,
         data = df)


# First, let’s draw a random sample from the world when the null hypothesis H0 is True.
# We’ll save the results as sample.H0. Then, we’ll conduct a one-sample t-test to test 
# if the true population mean is really 0:
  # Generate a vector of 20 samples from a normal distribution with mu = 0
  #  Here, the null hypothesis H0: Mu = 0 IS true
  
  sample.H0 <- rnorm(n = 20, mean = 0, sd = 1)


# Now look at the p-values from one-sample t-tests of each object
t.test(x = sample.H0, mu = 0, alternative = "two.sided")$p.value



# Copy and paste your code above several times and see what happens! 
#   As you’ll see, the value keeps changing, this is because you’re 
# getting new samples each time. How often do you get a ‘significant’ p-value?
#
## never

# Now, let’s repeat the process in a world where the null hypothesis is False
# (that is, the true mean is 0.5). Run the following code several times to see what the p-values look like
#
# Generate a vector of 20 samples from a normal distribution with mu = 0.5
#  Here, the null hypothesis H0: Mu = 0 is FALSE

sample.H1 <- rnorm(n = 20, mean = 0.5, sd = 1)

# Now look at the p-values from one-sample t-tests of each object
t.test(x = sample.H1, mu = 0, alternative = "two.sided")$p.value


# It’s time to put it all together. In the code below, you’ll run a simulation where you
# repeat the process above 100 times for the H0 world, and 100 times for the H1 world
H0<-c()
H1<-c()
H<-matrix(, nrow = 100, ncol = 2)

for (i in 1:100){
  sample.H0 <- rnorm(n = 20, mean = 0, sd = 1)
  H[i,1]<-t.test(x = sample.H0, mu = 0, alternative = "two.sided")$p.value
  
  sample.H1 <- rnorm(n = 20, mean = 0.5, sd = 1)
  H[i,2]<-t.test(x = sample.H1, mu = 0, alternative = "two.sided")$p.value
}


# Visualize the distribution of p values from the simulation separately for 
# each world (for example, you could create a boxplot)
#
dat <- stack(as.data.frame(H))
ggplot(data = dat) + 
  geom_boxplot(aes(x = ind, y = values))



# Is the mean p-value from the H0 world significantly different 
# from the mean p-value from the H1 world? Conduct the appropriate test!
#   
t.test(formula = values ~ ind, data=dat)

#   Use your simulation to answer the following question: “Given that the null 
# hypothesis is true, what is the probability of obtaining a p-value less than 0.05?” 
# In other words, for all of your simulations in the H1 world, what percent 
# resulted in a p-value less than 0.05? (Hint: use  aggregate() or dplyr).
# 
mean(H1 < 0.05)

# Now use your simulation to answer a slightly different question: “Given that 
# a p-value is less than .05, what is the probability that the null hypothesis 
# is really False?” In other words, for all the simulations that resulted in a 
# p-value less than 0.05, what proportion were in the H1 world (and not in the H0 world)?
# 
HH=as.data.frame(H)
mean(HH$V2 < 0.05 && HH$V1 > 0.05)


# Now, run the simulation again, but this time instead of performing 100 
# simulations from both worlds, do 100 simulations from the H0 world, and 500 
# from the H1 world. Based on this simulation, now make the same calculations as 
# the previous two questions. That is, “Given that the null hypothesis is true, 
# what is the probability of obtaining a p-value less than 0.05?” and “Given that 
# a p-value is less than 0.05, what is the probability that the null hypothesis 
# is False?” Are you answers the same or different from before?
# 
HHH<-matrix(, nrow = 500, ncol = 2)

for (i in 1:100){
  sample.H0 <- rnorm(n = 20, mean = 0, sd = 1)
  HHH[i,1]<-t.test(x = sample.H0, mu = 0, alternative = "two.sided")$p.value
}  
for (i in 1:500){
  sample.H1 <- rnorm(n = 20, mean = 0.5, sd = 1)
  HHH[i,2]<-t.test(x = sample.H1, mu = 0, alternative = "two.sided")$p.value
}
HHH=as.data.frame(HHH)

mean(HHH$V2 < 0.05)
mean(HH$V2 < 0.05 && HH$V1 > 0.05)


# Based on what you’ve learned, what is the correct answer to the general question: 
#   “Given that the p-value from a hypothesis test is less than 0.05, what is the 
# probability that the null hypothesis is True?”

## 100%, I guess
