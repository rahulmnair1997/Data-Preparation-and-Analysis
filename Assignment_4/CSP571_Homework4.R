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
data <- separate(data = data, col = V1, into = c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model_year", 'origin'), sep = "\\s+")
colnames(data)[which(names(data) == "V2")] <- "car name"
summary(data)

# As we can see the numeric columns are loaded as characters which needs to be converted to numeric
num_col <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model_year", 'origin')
data[num_col] <- sapply(data[num_col],as.numeric)
summary(data)
# Now all the columns are in their required data type

# 1. Identify the columns (if any) with ANY data that is missing our
# could be reasonably construed as missing. Replace with median or mode, where
# appropriate
# 4 points

# checking the null or missing values
Null_Counter <- apply(data, 2, function(x) length(which(is.na(x)))/length(x))
Null_Counter
# As we can see, only 'horsepower' has null values.
# And as the data type of 'horsepower' is nemric, we will go for the median value
summary(data$horsepower)
data$horsepower[is.na(data$horsepower)] <- 93.5

# 2. Identify all of the categorical variables, 
# all of the numeric variables
# Store it in the variables below.
# 2 points

numVars <- names(which(sapply(data, is.numeric)))
catVars <- names(which(sapply(data, is.factor)))

# 3. Identify the appropriate descriptive statistics and graph for this data set.
# Execute on those and use the comments to discuss relevant relationships or insights discovered.
# 2 points
summary(data)
# we will create histogram for all of the numeric variables
indices <- which(sapply(data, is.numeric))
for (i in indices){
  hist(data[,i], main = names(data[i]))
}

# we will create bar chart for 'car name' since it is categorical
?barplot
car_name_count <- as.data.frame(table(data$`car name`))

top10 <- head(car_name_count[order(car_name_count$Freq, decreasing = T),], 10)
top10
barplot(top10$Var1)

library(ggplot2)
ggplot(top10) + geom_bar(aes(x=top10$Var1))

# 4. Create a correlation matrix for all of the numeric variables.
# 2 points
indices <- which(sapply(data, is.numeric))
correlation <- cor(data[indices])
correlation

# 5. Create a box plot of mpg versus origin
# 2 points
boxplot(data$mpg~data$origin)


# 6. Divide the data into a train/test set (80% and 20% respectively) using stratified sampling
# 2 points
trainPct <- .8

library('caret')
inTrain <- createDataPartition(y = data$mpg, p = trainPct, list = FALSE)
Train <- data[inTrain,]
Test <- data[-inTrain,]

# check whether the parition is done correct
stopifnot(nrow(Train) + nrow(Test) == nrow(data))

# 7. Fit a linear model to the data using the numeric variables only. Calculate the R**2 on the test set.
# 3 points
xvars <- numVars[-1]
target_var <- 'mpg'

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = ' + ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = ' + '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(target_var, xvars, includeIntercept = FALSE)
modelForm
model <- lm(modelForm, data = Train)
summary(model)

target_var_hat <- paste0(target_var, "_hat")
# target_var_hat
Test[,target_var_hat] <- predict(model, Test)
SST <- sum((Test[,target_var] - mean(Test[,target_var]))^2)
SSR <- sum((Test[,target_var_hat] - mean(Test[,target_var]))^2)

r_squared <- SSR/SST
r_squared

# 8. Programmatically identify and remove the non-significant variables (alpha = .05). Fit a new model with those variables removed.
# Calculate the R**2 on the test set with the new model. Did this improve performance?
# 4 points
p_val <- summary(model)$coefficients[,4]

summary(model)$coefficients[,4]

req_col <- vector()
for (i in xvars){
  if (p_val[i] <= 0.05){
    req_col <- c(req_col, i)
  }
}

modelForm2 <- createModelFormula(target_var, req_col, includeIntercept = FALSE)
modelForm2
model2 <- lm(modelForm2, data = Train)
target_var_hat2 <- paste0(target_var, "_hat2")
Test[,target_var_hat2] <- predict(model2, Test)
SST2 <- sum((Test[,target_var] - mean(Test[,target_var]))^2)
SSR2 <- sum((Test[,target_var_hat2] - mean(Test[,target_var]))^2)

r_squared2 <- SSR2/SST2
r_squared2
# as We can see the new r-sqaured is lesser than the previous one. Therefore, there has been an 
# improvement in the performance


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
