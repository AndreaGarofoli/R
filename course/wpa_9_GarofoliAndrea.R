# Assignment: WPA 9
# Name: GAROFOLI ANDREA
# Date: 22 11 2017


# 2

for (i in 1:50) {
  
  message(i)
  
}

# 3

for (i in 1:50) {
  
  message(i^2)
  
}


# 3

# Vector of original scores on the 10 questions
original_scores <- c(0, 1, 0, 1, 1, 2, 0, 1, 1, 2)

# Create a vector of NAs where the cumulative scores will go
cumulative_scores <- rep(NA, 10)

# Loop over each of the 10 questions
for(i in 1:10) {
  
  # Calculate the cumulative sum for the current question
  cumsum_i <- sum(original_scores[1:i])
  
  # Add the value to the ith element in cumulative_scores
  cumulative_scores[i] <- cumsum_i
  
}

# print the result!
cumulative_scores


# 4

cumsum(original_scores)


# 5


feed.me <- function(x) {
  
  output <- paste0("Yum! I love to eat ", x)
  
  print(output)
}

feed.me("food")


# 6

feed.me("apples")


# 7


feed.me <- function(x) {
  
  if(x != "avacados") {
    
    output <- paste0("Yum! I love to eat ", x)
    
  }
  
  if( x == "avacados") {
    
    output <- "NOOOOO, I HATE AVACADOS!"
    
  }
  
  print(output)
}

feed.me("avacados")


# 8


my.mean <- function(x) {
  
  result <- sum(x) / length(x)
  
  return(result)
  
}

# 9

my.mean(ChickWeight$weight)
mean(ChickWeight$weight)
# it's the same, yay

# 10


how.many.na <- function(x) {
  
  output <- sum(is.na(x))
  
  return(output)
}

# 11
x = c(4, 7, 3, NA, NA, 1)
how.many.na(x)

# 12

ttest.apa <- function(x,       # A vector of data
                      mu) {    # The mean under the null hypothesis
  
  # Store the one-sample ttest in object a
  
  a <- t.test(x = x,    # The vector of data
              mu = mu)   # The mean under the null hypothesis
  
  df <- a$parameter     # Get the degrees of freedom
  test.stat <- a$statistics      # Get the test statistic
  p.value <- a$p.value        # Get the p-value
  
  # If the test is significant
  if(p.value <= 0.05) {
    
    # Sentence to print for significant result
    
    print(paste0("The test is significant! t(",
                 df, ") = ", test.stat, 
                 ", p = ", p.value, 
                 " (H0 = ", mu, ")"))
  }
  
  # If the test is NOT significant...
  if(p.value > 0.05) {
    
    # Sentence to print for significant result
    
    print(paste0("The test is not significant, p.value is above the treshold"))
    
  }
}


# 12

x <- c(6, 8, 12, 6, 8, 3, 5, 3, 7, 0)

ttest.apa(x, 10)
ttest.apa(x, 5)


# 13

ttest.apa <- function(x,       # A vector of data
                      mu, p.sig) {    # The mean under the null hypothesis
  
  # Store the one-sample ttest in object a
  
  a <- t.test(x = x,    # The vector of data
              mu = mu)   # The mean under the null hypothesis
  
  df <- a$parameter     # Get the degrees of freedom
  test.stat <- a$statistics      # Get the test statistic
  p.value <- a$p.value        # Get the p-value
  
  # If the test is significant
  if(p.value <= p.sig) {
    
    # Sentence to print for significant result
    
    print(paste0("The test is significant! t(",
                 df, ") = ", test.stat, 
                 ", p = ", p.value, 
                 " (H0 = ", mu, ")"))
  }
  
  # If the test is NOT significant...
  if(p.value > p.sig) {
    
    # Sentence to print for significant result
    
    print(paste0("The test is not significant, p.value is above the treshold"))
    
  }
}

ttest.apa(x, 10, 0.001)


# 15

confidence_data <- read.table(file = "https://raw.githubusercontent.com/ndphillips/IntroductionR_Course/master/assignments/wpa/data/outlier_data.txt", 
                        sep = "\t",                             
                        header = TRUE,                         
                        stringsAsFactors = FALSE)

# 16


# Loop over columns
for(i in 1:ncol(confidence_data)) {
  
  # Get data from column i
  data.i <- confidence_data[, i]
  
  # Count number of responses below 0 or above 100
  count_invalid <- sum(data.i < 0 | data.i > 100)
  
  # Print message
  message(paste0("In column ", i, "I found ", count_invalid, "values that were either below 0 or over 100"))
  
}


# 17

# Loop over columns
for(i in 1:ncol(confidence_data)) {
  
  # Get data from column i
  data.i <- confidence_data[,i]
  
  # Count number of responses below 0 or above 100
  count_invalid <- sum(data.i < 0 | data.i > 100)
  
  # Print message
  message(paste0("In column ", i, "I found ", count_invalid, "values that were either below 0 or over 100"))
  
  # Replace values below 0 or above 100 with NA
  data.i[data.i < 0 | data.i > 100] <- NA
  
  # Assign data.i back to the ith column of the data
  confidence_data[, i] <- data.i
  
}


# 18

# NAvalues that were either below 0 or over 100, yay

# 19

for (i in 1:100) {
  
  # Create a random dataset from a hypothetical subject
  data_temp <- data.frame(subject = rep(i, 10),
                          trial = 1:10,
                          response = sample(1:7, size = 10, replace = TRUE))
  
  # Write the data to a text file
  write.table(data_temp, 
              file = paste0("subject_", i, ".txt"), 
              sep = "\t")
  
}


# 20

for (i in 1:100) {
  
  # Load the data from participant i to a temporary object x
  data_temp <- read.table(file = paste0("subject_", i, ".txt"), 
                          header = TRUE, 
                          sep = "\t")
  
  # Create a new object subject_i containing the data!
  assign(x = paste0("subject_", i), 
         value = data_temp)
  
}


# 21

# Set up all_data object
all_data <- NULL

for (i in 1:100) {
  
  # Load the data from participant i to a temporary object x
  data_temp <- read.table(file = paste0("subject_", i, ".txt"), 
                          header = TRUE, 
                          sep = "\t")
  
  # Add temprorary data to to all_data!
  all_data <- rbind(data_temp, all_data)
  
}

# 22
all_data <- NULL

for (i in 1:100) {
  message(paste0("Reading subject ", i))
  # Load the data from participant i to a temporary object x
  data_temp <- read.table(file = paste0("subject_", i, ".txt"), 
                          header = TRUE, 
                          sep = "\t")
  
  # Add temprorary data to to all_data!
  all_data <- rbind(data_temp, all_data)
  
}

# 23

# Create a vector of 1000 NA values
p.values <- rep(NA, 1000)

# Loop over all 1000 values
for(i in 1:1000) {
  
  # Generate a random vector of data
  x <- rnorm(n = 10, mean = 0, sd = 1)
  
  # Calculate the te.test
  result <- t.test(x, mu=0)$p.value
  
  # Store p-value from test in p.values
  p.values[i] <- result
  
}

# 24

cat(sum(p.values < 0.05)/1000 * 100, "%")


# 25

survey <- data.frame("p" = c(1, 2, 3, 4, 5),
                     "q1" = c(5, 3, 6, 3, 5),
                     "q2" = c(-1, 4, 3, 6, 11),
                     "q3" = c(6, 22, 4, 6, -5),
                     "q4" = c(6, 3, 4, -2, 4),
                     "q5" = c(1, 1, 900, 1, 2))

survey_corrected <- survey   # Copy original survey

for(column.i in 1:6 ) {  # Loop over columns
  
  x <- survey[,column.i]   # Copy original column vector
  x[(x %in% 1:10) == FALSE] <- NA  # Replace any bad values
  
  survey_corrected[,column.i] <- x # Assign x back to survey.correced
  
}

# 26

# it worked. trust me, I'm a doctorand
