# CSP571_HW3
# Scenario: 
# You have been tasked with analyzing ACME's Temporary Housing spend for senior management. 
# There is a hypothesis that ACME can reduce costs and increase service levels by issuing an RFP.
# ACME currently has three vendors: ‘Sherlock Homes Llc,’ ‘Keepin It Realty Inc,’ and ‘Raynor Shine Llc.’ 
# Prior to issuing an RFP, ACME requested detailed utilization data from all three vendors in Microsoft Excel 
# with the following fields:
# --> Vendor
# --> Type (Condo, Hotel, etc)
# --> # Days
#	--> Current Adjuster
# --> Move-in/Check-In Date
# --> Daily Housing Rate
# --> Claim Number
# --> Move-out/Check-Out Date
# --> Daily Admin Fee
# --> Policyholder Last Name
# --> Occupancy Status
# --> Total Housing Spend
# --> Policyholder City
# --> # ofBedrooms	

# Read in the data from the ACME Corp Spreadsheet
library('readxl')
file <-  "ACME_Corp.xlsx"
df <- read_excel(file, sheet = "Sheet1")
df <- as.data.frame(df)
summary(df)

# makning the columns such as "policy holder city", "Current Adjuster" to lower cases since the same elements 
# may have been stored as upper and lower cases.
df$`Policyholder City` <- tolower(df$`Policyholder City`)
df$`Current Adjuster` <- tolower(df$`Current Adjuster`)

# 2 points
# 1. The three vendors each use a different definition of housing type. However, ACME's official types
# are listed on Sheet2 of the Excel sheet. 
# Create a new column called 'Normalized Housing Type' based on the standardized mapping.

df_2 <- read_excel(file, sheet = "Sheet2")
df_2 <- as.data.frame(df_2)
df <- merge(df, df_2, by.x = "Housing Type (Condo, Hotel, Apartment, Single Family Home)", by.y = "Lookup Value")
names(df)[names(df) == "Clean Value"] <- "Normalized Housing Type"
table(df$`Normalized Housing Type`)

# 2 points
# 2. Compute the total housing spend by Policy holder State 
# and the total percentage of 
# total housing spend by Policy holder State
# listed in descending order by cost. Return this as a dataframe.

policy_holder_state_df  <- aggregate(df$`Total Housing Spend` ~ df$`Policy holder State`, df, sum)
names(policy_holder_state_df) <- c("Policy State Holder", "Total Housing Spending")
policy_holder_state_df$policy_holder_state_percent <- policy_holder_state_df$`Total Housing Spending`/sum(policy_holder_state_df$`Total Housing Spending`) * 100
policy_holder_state_df <- policy_holder_state_df[order(policy_holder_state_df$`Total Housing Spending`, decreasing = T), ]
policy_holder_state_df


# 2 points
# 3. Create a table that has Normalized Housing Type (NHT) on the y-axis 
# and vendor on the x-axis. At the 
# intersection, compute the 
# 'Total Housing Spend of spend for that given and vendor and NHT. Compute and display 
# on the same table, the row-wise and column-wise margins too.

samp <- df[, c("Vendor", "Normalized Housing Type", "Total Housing Spend")]
z<- df$`Total Housing Spend`
y <- df$`Normalized Housing Type`
x <- df$Vendor
tab <- xtabs(z~y+x, data=samp)
tab

# 2 points
# 4. Obtain top 20 most frequent Policyholder City and Policy holder State combos

library(dplyr)
top20 <- as.data.frame(df %>% group_by(df$`Policy holder State`, df$`Policyholder City`) %>% summarise(n = n()))
top20 <- top20[order(top20$n, decreasing = T), ]
top20_city_state_combo <- head(top20, 20)
top20_city_state_combo

# 4 points
# 5. Write a function obtains the lat lon for a given city and state
# Note: You'll propsefully need to do some research on how to obtain this.
# There are a few ways of doing this. 

#install.packages('ggmap')
library(ggmap)
key <- "AIzaSyAhqK-mFSJhGfo4Ts5ez3ArH60Nm_JutHI"
register_google(key, write = T)
city <- df$`Policyholder City`
state <- df$`Policy holder State`
cityStateLatLon <- function(x){
  geo <- geocode(as.character(x), output = "latlon")
  return(geo)
}

cityStateLatLon(city[3])

# 4 points
# 6. Using the function above, obtain the lat lons for each of the top 20 cities.

top20_cities <- count(df, df$`Policyholder City`)
names(top20_cities) <- c("cities", "freq")
top20_cities <- top20_cities[order(top20_cities$freq, decreasing = T),]
top20_cities <- head(top20_cities, 20)
top20_cities
geocodes <- cityStateLatLon(top20_cities$cities)
top20_cities$lon <- geocodes$lon
top20_cities$lat <- geocodes$lat
top20_cities

# 2 points
# 7. Write code to plot the top 20 cities on a map. Include your plot
# with your submission.

#install.packages("maps")
#install.packages("mapproj")

library(ggplot2)
library(mapproj)
library(maps)
maps <- map_data("usa")
ggplot(maps, aes(x=long, y=lat)) +
  borders("state") +
  coord_map() +
  geom_point(data=top20_cities, aes(x=top20_cities$lon, y=top20_cities$lat), color="orange")

# 4 points
# 8. There are some misspellings and other issues 
# with the "Current Adjuster" field. Leverage the text
# analysis tools and levenstein distance to clean up 
# the names properly. Put them into a new column called
# "Current Adjuster Cleaned"
# Hint: you must deal with issues of case, whitespace,
# ,name misspellings and common name differences (ie Dave vs David). 
# You will be graded on how well you complete this. 

library(stringdist)
df$`Current Adjuster` <- tolower(df$`Current Adjuster`)
a = names(table(df$`Current Adjuster`))
b = a
stringDist <- stringdistmatrix(a = a, b = b, method = 'lv', useNames = 'strings')

library(reshape)
stringDist2 <- melt(stringDist)
t <- stringDist2[order(stringDist2$value, decreasing = FALSE),]
t <- t[t$value >0,]
t[1:30,]

# As we can see, there are 2 names which are same but still are considered as different because of
# whitespace and a spelling mistake, so we remove it to make the differently considered names same.
df$`Current Adjuster Cleaned` <- as.character(df$`Current Adjuster`)
df$`Current Adjuster Cleaned`[df$`Current Adjuster Cleaned` %in% "ira  dobbins"] <- "ira dobbins"
df$`Current Adjuster Cleaned`[df$`Current Adjuster Cleaned` %in% "susan chamberlin"] <- "susan chamberlain"

# Now we apply levenstein distance on the column "Current Adjuster Cleaned" again to show that the misktake 
# has been corrected.
c <- names(table(df$`Current Adjuster Cleaned`))
d <- c
stringDist_q <- stringdistmatrix(a = c, b = d, method = "lv", useNames = 'strings')
stringDist_q <- melt(stringDist_q) 
t_q <- stringDist_q[order(stringDist_q$value, decreasing = F), ]
t_q <- t_q[t_q$value > 0,]
t_q[1:20,]

# Thus, the mistakes have been corrected.

# 4 points
# Question 9:
# You want to give your state-managers a report every month
# that shows of the top 20 adjusters (by row count) in that 
# "Policy holder State", how
# many claims they have in each "Occupancy Status". 
# For example, for a "Move-in/Check-In Date" in
# March 2015, in TX, who were the top
# n current adjusters and how many did they have in 
# "Moved-In" status, etc
# Write a function that takes in a value of state, 
# a date in the form of YYYY-MM, value of n and 
# returns the above
# report for that month.
# Run this function for the parameters below.

n = 3
state = "CA"
date = '2015-03'

library(lubridate)
library(plyr)
# df_s <- mutate(df, date = ymd(df$`Move-in/Check-In Date`), day = day(date), month = month(date), year = year(date))
# month <- month(as.Date(date))
# format(as.Date(date), "%b. %Y")
# date
d <- strsplit(date, split = "-")
d[[1]][2]
library(zoo)
z <- read.zoo(text = date, FUN = as.yearmon)
z
class(df$`Move-in/Check-In Date`)
function(n, state. date){
  m <- month(date)
}




