# CSP/MATH 571 Homework 2 Part 1
# Enter your email below
yourEmail<- "rnair14@hawk.iit.edu"

# Load in the Boston Housing data set using the code below.
#install.packages('mlbench')
library('mlbench')
data(BostonHousing)


# 1. Create a scatterplot matrix of all variables in the data set. Save your output.

pairs(x = BostonHousing, main = "Scatter plot matrix")



# 2. For each numeric variable in BostonHousing, create a separate boxplot using
# "Method 2" listed in the class notes. Do this programmatically; meaning do
# not simply hardcode the creation of every boxplot. Instead, loop over the
# approriate columns and create the boxplots. Save your output. Ensure your boxplots
# all have proper titles

indices <- which(sapply(BostonHousing, is.numeric))
# columns <- names(BostonHousing)[which(sapply(BostonHousing, is.numeric))]
# columns <- names(BostonHousing)[which(sapply(BostonHousing, is.numeric))]
columns <- colnames(BostonHousing[indices])
for (i in 1:length(columns)){
  print(i)
}
for (i in indices){
  boxplot(x = BostonHousing[i])
}
# for ( i in indices){
#   print(columns[i])
# }
# columns <- names(indices)
# columns
# for (i in indices){
#   print(i)
# }
# 3. Create a correlation matrix and correlation plot
# for the BostonHousing data set. Save your output.

correlation <- BostonHousing[indices]
# round(cor(correlation),2)
c <- cor(correlation)
c
library(lattice)
hor <- c(BostonHousing[indices])
ver <- paste(hor, sep="")
rgb.palette <- colorRampPalette(c("blue", "red"), space = "rgb")
levelplot(c, main="correlation plot", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))


# 4. Identify the top 3 strongest absolute correlations in the data set. Save your output.
library(tidyr)
library(tibble)
absolute <- abs(c)
absoluted_cor_mat <- absolute %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1)

# sorted_value <- sort(d2$value, decreasing = TRUE)
absoluted_cor_mat <- absoluted_cor_mat[order(d2$value, decreasing = TRUE), ]
# 
absoluted_cor_mat
top_3 <- head(absoluted_cor_mat, 3)
top_3
# 5. Create a new variable call ageGroup quartiles. Divide the age variable
# into four even sections and assign it to one quartile.





# 6. Go to the website listed below. Convert the html table into a
# dataframe with columns NO, Player, Highlights
library('rvest')
library('tidyr')
url = 'http://www.espn.com/nfl/superbowl/history/mvps'




# 7.Extract the names of the MVPs, Position and Team into columns
# MVP1, MVP2, Position, Team





# 8. Determine the 90th%, 92.5th%, 95th%, 97.5th% and 99th% confidence intervals
# for the mean of passing yards
# (as listed in "Highlights" column) for quarterbacks.
# Note that there are a few intermediate steps you'll probably want to do
# in order to accomplish this. I am purposelly leaving that up to you, so that
# you are starting to develop the types of problem solving skills you'll need
# to tackle these problems in the wild.



# 9. The following contains data on the calorie counts of four types
# of foods. Perform an ANOVA and determine the Pr(>F)
food1 <- c(164,   172,   168,   177, 	156, 	195)
food2 <- c(178,   191, 	197, 	182, 	185, 	177)
food3 <- c(175, 	193, 	178, 	171, 	163, 	176)
food4 <- c(155, 	166, 	149, 	164, 	170, 	168)


# 10. Determine how many
# Tuesdays fell on the first of the month
# during the 19th century (1 Jan 1801 to 31 Dec 1901).
