# CSP571
# Homework 4


#Load in the auto mpg data set: https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data
# Note with this data set, you are trying to predict "mpg"
# Additional information is found at the link below
#, however additional interpretation may be needed from you
# https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.names

# Load Data
library(tidyr)
data <- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data',
                       header=F, sep = '\t')
data <- separate(data = data, col = V1, into = c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model_year", 'origin'), sep = "\\s+")
colnames(data)[which(names(data) == "V2")] <- "car_name"
summary(data)

# As we can see the numeric columns are loaded as characters which needs to be converted to numeric
num_col <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model_year", 'origin')
# num_col <- c("mpg", "displacement", "horsepower", "weight", "acceleration")
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
# And as the data type of 'horsepower' is numeric, we will go for the median value
summary(data$horsepower)
data$horsepower[is.na(data$horsepower)] <- median(data$horsepower, na.rm = T)
summary(data$horsepower)
# Now we see there is no 'NA' values left


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

# From the histogram plotted, we see most of the cars have an origin of 1.
# most of the cars were made in 1970
# most of the cars have an acceleration of 15 with a normal distribution with no skewness
# We see the weights variable have a right skewed normal distribution
# Horsepower is strongly right skewed
# Displacement shows no typical distribution
# Most of the cars have 4 cylinders
# And finally, the mpg shows a right skewness.
pairs(data)
# From scatterplot matrix, we see that acceleration, origin and model year show a weak positive 
# correlation with mpg while displacement, horsepower and weight show a considerable 
# strong negative correlation with mpg cylinders show a weak correlation with mpg.
# we will create bar chart for 'car name' since it is categorical

CarName <- as.data.frame(table(data$car_name))
names(CarName) <- c('car_name', 'count')
CarName <- CarName[order(CarName$count, decreasing = T),]
CarName <- head(CarName,10)

library(ggplot2)
ggplot(CarName,aes(x=car_name, y = count)) + geom_bar(stat="identity")
# As we can see, most number of cars are Ford Pinto. 
# And most of the car names in top 10 have a count of 4

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

# check whether the parition is done correct or not
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
Test[,target_var_hat] <- predict(model, Test)
SST <- sum((Test[,target_var] - mean(Test[,target_var]))^2)
SSR <- sum((Test[,target_var_hat] - mean(Test[,target_var]))^2)
r_squared <- SSR/SST
r_squared


# 8. Programmatically identify and remove the non-significant variables (alpha = .05). Fit a new model with those variables removed.
# Calculate the R**2 on the test set with the new model. Did this improve performance?
# 4 points
p_val <- summary(model)$coefficients[,4]

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
# req_col
# as We can see the new r-sqaured is slightly greater than the previous one. Therefore, there has been an 
# improvement in the performance


# 9. Attempt to fit a model on all of the relevant independent variables (including carName).
# Then calculate the R**2 on a test set. You will likely encounter an error.
# Explain why this error occurs. Fix this error.
# 4 points

varname <- names(data)
varname <- varname[-1]
modelForm3 <- createModelFormula(target_var, varname, includeIntercept = FALSE)
modelForm3
model3 <- lm(modelForm3, data = Train)
target_var_hat3 <- paste0(target_var, "_hat3")
Test[,target_var_hat3] <- predict(model3, Test)
# We are getting an High Dimensionality error in the categorical variable 'car_name'. So now, we are 
# going to encode these categories in their frequencies.
t <- sort(table(data[,'car_name']), decreasing = TRUE)
t[1:20]
CarNameFreq <- as.data.frame(t)
names(CarNameFreq) <- c('car_name', 'Count')
head(CarNameFreq)
newData <- merge(x=data, y = CarNameFreq, all.x = TRUE, by = 'car_name')
head(newData)
inTrain <- createDataPartition(y = newData$mpg, p = trainPct, list = FALSE)
Train <- newData[inTrain,]
Test <- newData[-inTrain,]
varname <- names(which(sapply(newData, is.numeric)))
varname <- varname[-1]
modelForm3 <- createModelFormula(target_var, varname, includeIntercept = FALSE)
modelForm3
model3 <- lm(modelForm3, data = Train)
target_var_hat3 <- paste0(target_var, "_hat3")
Test[,target_var_hat3] <- predict(model3, Test)
SST3 <- sum((Test[,target_var] - mean(Test[,target_var]))^2)
SSR3 <- sum((Test[,target_var_hat3] - mean(Test[,target_var]))^2)

r_squared3 <- SSR3/SST3
r_squared3
# we see there has been a further improvenment in the r**2 value.

# 10. Determine the relationship between model year and mpg.
# Interpret this relationship.
# Theorize why this relationship might occur.
# 4 points

library(ggplot2)
ggplot(data, aes(x = model_year, y = mpg)) + geom_point() +  geom_smooth()
# As we can see from the graph, there is a weak positive correlation between mpg and model year,
# which makes sense. As the years go by, the technology improves and the engines starts 
# performing better, thus giving better mpg.


# 11. Using only the variables provided, build the best linear model 
# you can (as measured by R**2 on the test data)
# Record the value obtained in the comments below. Make sure to show all your code.
# Record the best R**2 value on the test set in the comments below.
# My Best R**2 value: 
# 4 points
library(rms)
model4 <- ols(modelForm3, data = newData)
fastbw(model4, rule = "p", sls = 0.05)

model_opt <- lm(mpg~weight+model_year+origin-1, data = newData)
target_var_hat4 <- paste0(target_var, "_hat4")
Test[,target_var_hat4] <- predict(model_opt, Test)
SST4 <- sum((Test[,target_var] - mean(Test[,target_var]))^2)
SSR4 <- sum((Test[,target_var_hat4] - mean(Test[,target_var]))^2)

r_squared4 <- SSR4/SST4
r_squared4


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

# library('rvest')
# library('tidyr') 
vec <- c(sapply(strsplit(as.character(newData$car_name), " "), head, 1))
newData$Brand <- vec
car <- vector()
for (i in 1: length(newData$car_name)){
  car <- c(car, paste(strsplit(as.character(newData$car_name),split=" ")[i][[1]][2],
  strsplit(as.character(newData$car_name),split=" ")[i][[1]][3]))
}
car
library(stringdist)
newData$Brand_cleaned <- tolower(newData$Brand_cleaned)
a = names(table(newData$Brand_cleaned))
b = a
stringDist <- stringdistmatrix(a = a, b = b, method = 'lv', useNames = 'strings')

library(reshape)
stringDist2 <- melt(stringDist)
t <- stringDist2[order(stringDist2$value, decreasing = FALSE),]
t <- t[t$value >0,]
t[1:40,]

# As we can see, there are 3 names which are same but still are considered as different because of
# spelling mistake, so we remove it to make the differently considered names same.
newData$Brand_cleaned <- as.character(newData$Brand)
newData$Brand_cleaned [newData$Brand_cleaned  %in% "maxda"] <- "mazda"
newData$Brand_cleaned [newData$Brand_cleaned  %in% "toyouta"] <- "toyota"
newData$Brand_cleaned [newData$Brand_cleaned  %in% "vokswagen"] <- "volkswagen"
newData$Brand_cleaned [newData$Brand_cleaned  %in% "chevroelt"] <- "chevrolet"

inTrain <- createDataPartition(y = newData$mpg, p = trainPct, list = FALSE)
Train <- newData[inTrain,]
Test <- newData[-inTrain,]
model_opt1 <- lm(mpg~weight+model_year+origin+Brand_cleaned-1, data = newData)
target_var_hat5 <- paste0(target_var, "_hat5")
Test[,target_var_hat5] <- predict(model_opt1, Test)
SST5 <- sum((Test[,target_var] - mean(Test[,target_var]))^2)
SSR5 <- sum((Test[,target_var_hat5] - mean(Test[,target_var]))^2)

r_squared5 <- SSR5/SST5
r_squared5


