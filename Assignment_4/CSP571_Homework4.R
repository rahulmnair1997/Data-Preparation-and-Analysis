# CSP571
# Homework 4


#Load in the auto mpg data set: https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data
# Note with this data set, you are trying to predict "mpg"
# Additional information is found at the link below
#, however additional interpretation may be needed from you
# https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.names

# Load Data
library(tidyr)
data <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data',
                       header=F, sep = '\t')
data <- separate(data = data, col = V1, into = c("mpg", "cylinders", "displacement", "horsepower", "weight", "cceleration", "model year", 'origin'), sep = "\\s+")
colnames(data)[which(names(data) == "V2")] <- "car name"
summary(data)
num_col <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "cceleration", "model year", 'origin')
data[num_col] <- sapply(data[num_col],as.numeric)
summary(data)

# 1. Identify the columns (if any) with ANY data that is missing our
# could be reasonably construed as missing. Replace with median or mode, where
# appropriate
# 4 points

# checking the null or missing values
# Null_Counter <- apply(data, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "999" | x == "0"))/length(x))
Null_Counter <- apply(data, 2, function(x) length(which(is.na(x)))/length(x))
Null_Counter
summary(data)
data$horsepower <- apply(data$horsepower, function(x) which(is.na(x)) = 93.5)

# 2. Identify all of the categorical variables, 
# all of the numeric variables
# Store it in the variables below.
# 2 points

numVars <- names(which(sapply(data, is.numeric)))
catVars <- names(which(sapply(data, is.factor)))

# 3. Identify the appropriate descriptive statistics and graph for this data set.
# Execute on those and use the comments to discuss relevant relationships or insights discovered.
# 2 points



# 4. Create a correlation matrix for all of the numeric variables.
# 2 points
indices <- which(sapply(data, is.numeric))
correlation <- cor(data[indices])


# 5. Create a box plot of mpg versus origin
# 2 points
boxplot(data$mpg~data$origin)


# 6. Divide the data into a train/test set (80% and 20% respectively) using stratified sampling
# 2 points



# 7. Fit a linear model to the data using the numeric variables only. Calculate the R**2 on the test set.
# 3 points




# 8. Programmatically identify and remove the non-significant variables (alpha = .05). Fit a new model with those variables removed.
# Calculate the R**2 on the test set with the new model. Did this improve performance?
# 4 points


# 9. Attempt to fit a model on all of the relevant independent variables (including carName).
# Then calculate the R**2 on a test set. You will likely encounter an error.
# Explain why this error occurs. Fix this error.
# 4 points



# 10. Determine the relationship between model year and mpg.
# Interpret this relationship.
# Theorize why this relationship might occur.
# 4 points




# 11. Using only the variables provided, build the best linear model 
# you can (as measured by R**2 on the test data)
# Record the value obtained in the comments below. Make sure to show all your code.
# Record the best R**2 value on the test set in the comments below.
# My Best R**2 value: 
# 4 points



# 12. Your boss wants to know if the 
# brand of the car will add predictive power to 
# your model. Create new variables called "brand" and "model" from the carName
# column. Do some research to figure out how to do this.
# Clean up the brand variable. Add the cleaned up "brand" variable to the
# best model you built from the previous question.
# Compare the adjusted R**2 on the test data set.
# Best Adjusted R**2 without brand variable:
# Best Adjusted R**2 with brand variable: 
# 4 points
