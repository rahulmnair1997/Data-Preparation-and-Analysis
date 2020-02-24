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

for (i in indices){
  boxplot(x = BostonHousing[i], main = names(BostonHousing[i]))
}

# 3. Create a correlation matrix and correlation plot
# for the BostonHousing data set. Save your output.

correlation <- BostonHousing[indices]
c <- cor(correlation)
c
library(lattice)
hor <- c(BostonHousing[indices])
ver <- paste(hor, sep="")
rgb.palette <- colorRampPalette(c("yellow", "red", "blue"), space = "rgb")
levelplot(c, main="correlation plot", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(-1,1, 0.01))


# 4. Identify the top 3 strongest absolute correlations in the data set. Save your output.
library(tidyr)
library(tibble)

absolute <- abs(c)
absoluted_cor_mat <- absolute %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1)

absoluted_cor_mat
library(dplyr)
absoluted_cor_mat<- absoluted_cor_mat[absoluted_cor_mat$value != 1.0000000, ]
absoluted_cor_mat <- absoluted_cor_mat[order(absoluted_cor_mat$value, decreasing = TRUE), ]
absoluted_cor_mat

top_3 <- data.frame(var1=character(),
                    var2=character(),
                 value=numeric())
for (i in 1:6){
  if (i %% 2 == 0){
    top_3 <- rbind(top_3, data.frame(var1 = absoluted_cor_mat$var1[i], var2 = absoluted_cor_mat$var2[i], value = absoluted_cor_mat$value[i]))
  }
}
top_3
# Therefore the top 3 strongest absolute correlatations are 'rad: tax', 'nox:dis', 'indus:nox'


# 5. Create a new variable call ageGroup quartiles. Divide the age variable
# into four even sections and assign it to one quartile.

BostonHousing <- within(BostonHousing, ageGroup <- as.integer(cut(age, quantile(age, probs=0:4/4), include.lowest=TRUE)))
# Here Q1 = 1, Q2 = 2, Q3 = 3, Q4 = 4
table(BostonHousing$ageGroup)


# 6. Go to the website listed below. Convert the html table into a
# dataframe with columns NO, Player, Highlights

library('rvest')
library('tidyr')
url = 'http://www.espn.com/nfl/superbowl/history/mvps'
web_page <- read_html(url)
SuperBowl <- html_nodes(web_page, css = 'table')
t <- SuperBowl[[1]]
sb <- html_table(t)
sb <- sb[-(1:2),]
names(sb) <- c("Number", "Player", "Highlights")
sb$Number <- 1:nrow(sb)
sb

# 7.Extract the names of the MVPs, Position and Team into columns
# MVP1, MVP2, Position, Team
sb <- separate(sb, Player, c('MVP', 'Position', 'Team')
               , sep=', ' 
               , remove=TRUE)
# head(sb, 14)
sb <- separate(sb, MVP, c('MVP 1', 'MVP 2')
               , sep=' & '
               , remove=TRUE)
sb

# 8. Determine the 90th%, 92.5th%, 95th%, 97.5th% and 99th% confidence intervals
# for the mean of passing yards
# (as listed in "Highlights" column) for quarterbacks.
# Note that there are a few intermediate steps you'll probably want to do
# in order to accomplish this. I am purposelly leaving that up to you, so that
# you are starting to develop the types of problem solving skills you'll need
# to tackle these problems in the wild.

highlight <- vector()
q <- strsplit(sb$Highlights, " ")
q
for (i in 1:length(q)){
  if (is.na(q[[i]][3]) == FALSE){
    if ((q[[i]][3] == "passing,") | (q[[i]][3] == "passing")){
      highlight <- c(highlight, q[[i]][1])
    }
  }
}
highlight
highlight <- as.numeric(highlight)
# class(highlight)
n <- length(highlight)
s <- sd(highlight)
SE <- s/sqrt(n)
xbar <- mean(highlight)

# for 90th% confidence interval
alpha_1 <- 0.1
tt_1 <- qt(1-alpha_1/2, df = 20)
# zVal_1 <- qnorm(p = 1-alpha_1/2)
E_1 <- tt_1 * SE
conf_1 <- xbar+c(-E_1,E_1)
conf_1

# for 92.5th% confidence interval
alpha_2 <- 0.075
tt_2 <- qt(1-alpha_2/2, df = 20)
E_2 <- tt_2*SE
conf_2 <- xbar+c(-E_2,E_2)
conf_2

# for 95th% confidence interval
alpha_3 <- 0.05
tt_3 <- qt(1-alpha_3/2, df = 20)
E_3 <- tt_3*SE
conf_3 <- xbar+c(-E_3,E_3)
conf_3

# for 97.5th% confidence interval
alpha_4 <- 0.025
tt_4 <- qt(1-alpha_4/2, df = 20)
E_4 <- tt_4*SE
conf_4 <- xbar+c(-E_4,E_4)
conf_4

# for 99th% confidence interval
alpha_5 <- 0.01
tt_5 <- qt(1-alpha_5/2, df = 20)
E_5 <- tt_5*SE
conf_5 <- xbar+c(-E_5,E_5)
conf_5



# 9. The following contains data on the calorie counts of four types
# of foods. Perform an ANOVA and determine the Pr(>F)
food1 <- c(164,   172,   168,   177, 	156, 	195)
food2 <- c(178,   191, 	197, 	182, 	185, 	177)
food3 <- c(175, 	193, 	178, 	171, 	163, 	176)
food4 <- c(155, 	166, 	149, 	164, 	170, 	168)
treatment <- c(
   rep('food1', 6)
  , rep('food2', 6)
  , rep('food3', 6)
  , rep('food4', 6)
  )
alpha = .05
calorie_count <- c(food1, food2, food3, food4)
df <- data.frame(treatment, calorie_count)

fit <- aov(calorie_count ~ treatment)
fit
summary(fit)
# The Pr(>F) is coming 0.00688.

# 10. Determine how many
# Tuesdays fell on the first of the month
# during the 19th century (1 Jan 1801 to 31 Dec 1901).
library('lubridate')
date <- seq(from = dmy("01-Jan-1801"), to= dmy("01-Dec-1901"), by="months")
number_of_tuesday <- (which(wday(date, label = TRUE) %in% "Tue"))
length(number_of_tuesday)
# Therefore, 173 tuesdays fell on the first of the month.

