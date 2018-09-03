# Assignment: WPA 10
# Name: GAROFOLI ANDREA
# Date: 29 11 2017


# 3

student.math <- read.table("https://raw.githubusercontent.com/ndphillips/IntroductionR_Course/master/assignments/wpa/data/studentmath.txt",
                           sep = "\t",
                           header = TRUE)

student.por <- read.table("https://raw.githubusercontent.com/ndphillips/IntroductionR_Course/master/assignments/wpa/data/studentpor.txt",
                          sep = "\t",
                          header = TRUE)

write.table(student.math, 
            file = "/Users/andrea/A/rcourse/data/studentmath.txt",
            sep = "\t")

write.table(student.por, 
            file = "/Users/andrea/A/rcourse/data/studentpor.txt",
            sep = "\t")

