##%######################################################%##
#                                                          #
####             BASIC DATA MANIPULATION                ####
####             course by Ourcodingclub                ####
####            dplyr functions, 11.7.2024              ####
#                                                          #
##%######################################################%##

# Used packages
library(dplyr)

# Data from Subsetting.R used

# rename function
#  elongation_long <- rename(elongation_long, new name = old name)

str(elongation_long)

# FILTER OBSERVATIONS

# Let's keep observations from zones 2 and 3 only, and from years 2009 to 2011

elong_subset <- filter(elongation_long, zone %in% c("B", "C"), year %in% c(2009, 2010, 2011)) # you can use multiple different conditions separated by commas

# For comparison, the base R equivalent would be (not assigned to an object here):
elongation_long[elongation_long$zone %in% c(2,3) & elongation_long$year %in% c("X2009", "X2010", "X2011"), ]

# SELECT COLUMNS

# Let's ditch the zone column just as an example

elong_no.zone <- dplyr::select(elongation_long, ID, year, length)   # or alternatively
elong_no.zone <- dplyr::select(elongation_long, -zone) # the minus sign removes the column

# For comparison, the base R equivalent would be (not assigned to an object here):
elongation_long[ , -1]  # removes first column

# A nice hack! select() lets you rename and reorder columns on the fly
elong_no.zone <- dplyr::select(elongation_long, Year = year, Shrub.ID = ID, Growth = length)

# CREATE A NEW COLUMN

elong_total <- mutate(elongation, total.growth = 2007 + 2008 + 2009 + 2010 + 2011 + 2012)

# create a column with total growth per year
elong_total_year <- elongation_long %>%
  group_by(year)%>%
  summarise(total.growth = sum(length))

# GROUP DATA

elong_grouped <- group_by(elongation_long, ID)   # grouping our dataset by individual

# SUMMARISING OUR DATA

summary1 <- summarise(elongation_long, total.growth = sum(length))
summary2 <- summarise(elong_grouped, total.growth = sum(length))

summary3 <- summarise(elong_grouped, total.growth = sum(length),
                      mean.growth = mean(length),
                      sd.growth = sd(length))

# Adding data about treatments ----
# Load the treatments associated with each individual

treatments <- read.csv("EmpetrumTreatments.csv", header = TRUE, sep = ";")
head(treatments)

# Join the two data frames by ID code. The column names are spelled differently, so we need to tell the function which columns represent a match. We have two columns that contain the same information in both datasets: zone and individual ID.

# Have to change zone to factor so it is the same
treatments$Zone <- as.factor(treatments$Zone)
str(treatments)
# Change back the elongation zone levels to 2:7 so it is the same
levels(elongation_long$zone) <- c(2:7)

experiment <- left_join(elongation_long, treatments, by = c("ID" = "Indiv", "zone" = "Zone"))

# We see that the new object has the same length as our first data frame, which is what we want. And the treatments corresponding to each plant have been added!
# Can also be done with base R function merge
experiment2 <- merge(elongation_long, treatments, by.x = c("zone", "ID"), by.y = c("Zone", "Indiv"))  
# same result!

# Let's see if the treatment affects growth
boxplot(length ~ Treatment, data = experiment)