# Assignment: WPA 4
# Name: GAROFOLI ANDREA
# Date: 18 10 2017




#At the top of your script load the dplyr package using library()
library(yarrr) # Load yarrr for the pirates dataframe
library(dplyr) # Load dplyr for aggregation



#Using getwd() print the current working directory of your project. 
#This is the directory on your computer where your project is located.

getwd()


#Now it’s time to load the data. The data for this WPA are stored at
#http://journal.sjdm.org/15/15909/data1.csv. Load the data into R by using 
#read.table() into a new object called matthews by running the following code.
#Once you have done this, kook at the first few rows of matthews using head(),
#and str() to make sure the data were loaded correctly into R.

matthews <- read.table(file = "http://journal.sjdm.org/15/15909/data1.csv", # Link to the file
                       sep = ",",                             # File is comma-separated
                       header = TRUE,                         # There IS a header column
                       stringsAsFactors = FALSE)              # Do NOT convert strings to factors

head(matthews)
str(matthews)

#Now that you’ve loaded the data into R, let’s save a local copy of the data as a text 
#file called matthews.txt into your data folder. Using  write.table(), save the data as a 
#tab–delimited text file called matthews.txt in the data folder as follows:
  
write.table(x = matthews,                    # Save the object matthews
            file = "data/matthews.txt",      # Write the object to matthews.txt in the data folder
            sep = "\t")                      # Separate columns by tabs

  
#What are the names of the data columns?

colnames(matthews)

#What was the mean age?

mean(matthews$age)

#Currently the column gender is coded as 1 (male) and 2 (female). 
#Let’s create a new character column called gender_a that codes the data as 
#"male" and "female" (instead of 1 and 2). Do this by using the following template:
  
matthews$gender_a <- NA   # Start with a column of NA values.
matthews$gender_a[matthews$gender == 1] <- "male" # Change 1 to "male"
matthews$gender_a[matthews$gender == 2] <- "female" # Change 2 to "female"

#What percent of participants were male? (Hint: use the template: mean(__$__ == "__"))

mean(matthews$gender_a == "male")

#Using either basic indexing, calculate the mean age for males only using the following templates

mean(matthews$age[matthews$gender_a == "male"])

#Using either basic indexing, calculate the mean age for females only using the following template

mean(matthews$age[matthews$gender_a == "female"])  
  


#Using aggregate() calculate the mean age of male and female participants separately 
#using the following template. Do you get the same answers as before?

aggregate(formula = age ~ gender_a,
          FUN = mean,
          data = matthews)
# yes, I did

#Now use dplyr to do the same calculations using the following template. Do you get the same answers as before?

matthews %>%
  group_by(gender_a) %>%
  summarise(
    N = n(),
    age_mean = mean(age)
  )
# ditto


#The variable pcmore reflects the question: “What percent of people taking part 
#in this survey do you think earn more than you do?”. Using  aggregate(), calculate 
#the median value of this variable separately for each level of income. What does the result tell you?

aggregate(formula = pcmore ~ income,
          FUN = median,
          data = matthews)



#I created a new table containing fictional demographic information about each participant.
#The data are stored in a tab–delimited text file (with a header row) at
#https://raw.githubusercontent.com/ndphillips/IntroductionR_Course/master/data/matthews_demographics.txt.
#Using  read.table(), load the data into an object called matthews_demo into R using the following template:

matthews_demo <- read.table(file = "https://raw.githubusercontent.com/ndphillips/IntroductionR_Course/master/data/matthews_demographics.txt",          # File location
                              sep = "\t",            # How are columns separted?
                              header = TRUE,           # Is there a header row?
                              stringsAsFactors = TRUE) # Should strings be converted to factors?
  
  
#Using merge() add the demographic data to the matthews data using the following template:

matthews <- merge(x = matthews,       # First dataframe
                    y = matthews_demo,       # Second dataframe
                    by = "id")    # Column to match rows

  
#Using aggregate(), calculate the mean value of havemore for each combination of 
#gender and race using the following template. Is there a difference between men 
#and women, or people of different races, in how often they think other people earn more money than them?

aggregate(formula = havemore ~ gender_a + race, 
          FUN = mean, 
          data = matthews)

# everyone thinks to be poorer than everyone else, except for female hispanics.


#Now do the same calculations using dplyr using the following template. Do you get the same answer?

matthews %>% 
  filter(is.na(havemore) == FALSE) %>% 
  group_by(race, gender_a) %>%
  summarise(
    N = n(),
    havemore_mean = mean(havemore)
  )




#Create a new dataframe called product that only contain columns p1, p2, … p10 from 
#matthews by running the following code. After you run the code, look at it with head() to see what it looks like.

product <- matthews[,paste0("p", 1:10)]


#The colMeans() function takes a dataframe as an argument, and returns a vector 
#showing means across rows for each column of data. Using colMeans(), calculate 
#the percentage of participants who indicated that the ‘typical’ participant would 
#be willing to pay more than them for each item. Do your values match what the authors reported in Table 1?

colMeans(matthews[,paste0("p", 1:10)])

#The rowMeans() function is like colMeans(), but for calculating means across 
#columns for every row of data. Using rowMeans() calculate for each participant, 
#the percentage of the 10 items that the participant believed other people would 
#spend more on. Save this data as a vector called pall.

pall <- rowMeans(matthews[,paste0("p", 1:10)])

#Add the pall vector as a new column called pall to the matthews dataframe using basic assignment (__$__ <- __)

matthews$pall <- pall

#What was the mean value of pall across participants? This value is the answer 
#to the question: “How often does the average participant think that someone else 
#  would pay more for an item than themselves?”

mean(matthews$pall)


#Calculate the mean pall value for male and female participants separately. Which 
#gender tends to think that others would pay more for products than them?

aggregate(formula = pall ~ gender_a, 
          FUN = mean, 
          data = matthews)


#Calculate the mean pall value of participants for each level of income. 
#Do you find a consistent relationship between pall and income?

aggregate(formula = pall ~ income, 
          FUN = mean, 
          data = matthews)

#everyone think to be poorer than anyone else. 1 person out of 8 is right


#For each level of gender, calculate the summary statistics in the following 
#table using the following template. Save the summary statistics to an object called gender_agg

gender_agg <- matthews %>%
  group_by(gender_a) %>%
  summarise(
    N = n(),
    age.mean = mean(age),
    age.sd = sd(age),
    income.mean = mean(income),
    pcmore.mean = mean(pcmore),
    pall.mean = mean(pall)
  )


#For each level of income, calculate the summary statistics in the following 
#table – only for participants older than 21 – and save them to a new object called income_df.

income_df <- matthews %>%
  filter(age < 21) %>% 
  group_by(income) %>%
  summarise(
    N = n(),
    age.mean = mean(age),
    age.sd = sd(age),
    income.mean = mean(income),
    pcmore.mean = mean(pcmore),
    pall.mean = mean(pall)
  )


#Calculate the maximum and minimum age, and mean income aggregated at each 
#level of race and gender. Save the results to an object called racegender_agg

racegender_agg <- matthews %>%
  group_by(race, gender_a) %>%
  summarise(
    N = n(),
    age.min = min(age),
    age.max = max(age),
    income.mean = mean(income)
  )


#Calculate the mean value of pcmore, the percent of participants that were black, 
#and the mean age) aggregated at each level of task and gender. But do this only 
#for people with an income greater than 5. Save the results to an object called taskgender_agg.

taskgender_agg <- matthews %>%
  filter(income < 5) %>% 
  group_by(task, gender_a) %>%
  summarise(
    N = n(),
    pcmore.mean = mean(pcmore),
    mean.black = mean(race == "black"),
    mean.age = mean(age)
  )    


#Using save(), save matthews, gender_agg, income_df, racegender_agg, and taskgender_agg 
#objects to a file called  matthews.RData in the data folder in your working directory.

save( matthews, gender_agg, income_df, racegender_agg, taskgender_agg,
     file = "data/matthews.RData")
