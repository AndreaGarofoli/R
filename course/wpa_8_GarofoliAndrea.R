# Assignment: WPA 8
# Name: GAROFOLI ANDREA
# Date: 15 11 2017

# 2

student_m <- read.table(file = "https://raw.githubusercontent.com/ndphillips/IntroductionR_Course/master/assignments/wpa/data/studentmath.txt", 
                     sep = "\t",                             
                     header = TRUE,                         
                     stringsAsFactors = FALSE)

student_p <- read.table(file = "https://raw.githubusercontent.com/ndphillips/IntroductionR_Course/master/assignments/wpa/data/studentpor.txt", 
                     sep = "\t",                             
                     header = TRUE,                         
                     stringsAsFactors = FALSE)


# 3

head(student_m)
head(student_p)
View(student_m)
View(student_p)

# 4

names(student_m)
names(student_p)
str(student_m)
str(student_p)

# 5

write.table(x = student_m, file = "/Users/andrea/A/rcourse/data/student_m.txt",  sep = "\t")
write.table(x = student_m, file = "/Users/andrea/A/rcourse/data/student_p.txt",  sep = "\t")

# 6

lm_6<- lm(formula = G1 ~ age, data = student_m)

# 7

names(lm_6)
summary(lm_6)
lm_6$coefficients

# 8 

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.6915 -2.7749 -0.1916  2.3085  8.3085 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  13.6919     2.1926   6.245  1.1e-09 ***
#   age          -0.1667     0.1309  -1.273    0.204    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.317 on 393 degrees of freedom
# Multiple R-squared:  0.004106,	Adjusted R-squared:  0.001572 
# F-statistic:  1.62 on 1 and 393 DF,  p-value: 0.2038

# Pvalue > 0.05 so age is not significantly correlated to G1


# 9

predict(lm_6, newdata = student_m, subset=(age=18))


# 10

lm_10 <- lm(formula = G3 ~ G1, data = student_p)
summary(lm_10)

# 11

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11.5179  -0.5454   0.3996   0.6196  10.1796 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.82040    0.30545   2.686  0.00742 ** 
#   G1           0.97250    0.02605  37.329  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.821 on 647 degrees of freedom
# Multiple R-squared:  0.6829,	Adjusted R-squared:  0.6824 
# F-statistic:  1393 on 1 and 647 DF,  p-value: < 2.2e-16

# Pvalue is < 0.05 so G1 and G3 are correlated, the estimate is positive so they are directly correlated

# 12

predict(lm_10, newdata = student_p, subset=(G1=10))


# 13

t.test(formula = G1 ~ G3, data = student_p)

# 14

cor.test(formula = ~ age + G1, data = student_p)

##

# 15

lm_15<-  lm(formula = G3 ~ sex + age + internet + failures, data = student_m)
summary(lm_15)

# 16

# failure is highly and reversly correlated (p 3.86e-12, negative estimate), males are 
# also correlated (p = 0.0151, positive estimate)

# 17

lm_17<-  lm(formula = G3 ~ sex + age + internet + failures, data = student_p)
summary(lm_17)

# same as above for the failures, but males have now an inverse but significant correlation (p = 0.002568,
# negative estiamte) and also now internet has a positive and significant corerlation (p = 0.000803)  

# 18

# guys are better at math, girls at portugese. also interet is good for the portugese but not significant for math

# 19

summary(lm(formula = G1 ~ ., data = student_m))

# sex is no longer significant 

# 20

summary(glm(formula = goout ~ G1 ,  data = student_m, subset=(school=='GP')))

# 21

summary(glm(formula = goout ~ G1 ,  data = student_p, subset=(school=='GP')))

# nope, it is significant only for the math

# 22

student_m$grade_improve <- student_m$G3 > student_m$G1 
student_p$grade_improve <- student_p$G3 > student_p$G1 


# 23

summary(glm(formula = grade_improve ~ . ,  data = student_m, family = "binomial"))


# 24

summary(glm(formula = grade_improve ~ . ,  data = student_p, family = "binomial"))

# I conclude I've probably messed up somewhere

# 25

lm_25 <- lm(formula = G1 ~. , data = student_m)

# 26

names(lm_25)

student_m$G1_predicted <- lm_25$fitted.values

# 27

mean(abs(student_m$G1 - student_m$G1_predicted))

# 28

library(ggplot2)
ggplot(data = student_m,
       aes(x = G1, y = G1_predicted)) + 
  geom_point()

# sorta of ok I guess

# 29

library(tidyverse) # For dplyr

set.seed(100)  # Fix the randomisation

# Create a dataframe with 4 predictors (A, B, C and D) and noise

df <- data.frame(A = rnorm(n = 100, mean = 0, sd = 1),
                 B = rnorm(n = 100, mean = 0, sd = 1),
                 C = rnorm(n = 100, mean = 0, sd = 1),
                 D = rnorm(n = 100, mean = 0, sd = 1),
                 noise = rnorm(n = 100, mean = 0, sd = 10))

# Calculate y, a linear combination of A, B, C plus noise

df <- df %>%
  mutate(
    dv = 20 + A + 5 * B - 4 * C + 0 * C + noise
  )

# 30

# not very correlated I guess

# 31

summary(lm(formula = dv ~ A + B + C + D , data = df))

# 32

set.seed(100)  # Fix the randomisation

# Create a dataframe with 4 predictors (A, B, C and D) and noise

df <- data.frame(A = rnorm(n = 100, mean = 0, sd = 1),
                 B = rnorm(n = 100, mean = 0, sd = 1),
                 C = rnorm(n = 100, mean = 0, sd = 1),
                 D = rnorm(n = 100, mean = 0, sd = 1),
                 noise = rnorm(n = 100, mean = 0, sd = 0.001))

# Calculate y, a linear combination of A, B, C plus noise

df <- df %>%
  mutate(
    dv = 20 + A + 5 * B - 4 * C + 0 * C + noise
  )

summary(lm(formula = dv ~ A + B + C + D , data = df))

# there is hardly any noise, they are correlated (except for D)