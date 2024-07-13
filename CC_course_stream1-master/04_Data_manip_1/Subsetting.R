##%######################################################%##
#                                                          #
####              BASIC DATA MANIPULATION               ####
####              course by Ourcodingclub               ####
####                Subsetting, 7.8.2024                ####
#                                                          #
##%######################################################%##

#Used packages
library(tidyr)
library(readr)

#Data import ----
setwd("C:/Users/pavla/OneDrive/Documents/GitHub/R-course/CC_course_stream1-master/04_Data_manip_1")
starting_dataframe <- read.csv("EmpetrumElongation.csv")

#This dataset represents annual increments in stem growth, measured on crowberry shrubs on a sand dune system. 
#The Zone field corresponds to distinct zones going from closest (2) to farthest (7) from the sea.

# Check import and preview data
head(starting_dataframe)   # first few observations
str(starting_dataframe)    # types of variables
starting_dataframe$Indiv   # prints out all the ID codes in the dataset
length(unique(starting_dataframe$Indiv))   # returns the number of distinct shrubs in the data

#Data subsetting ----
# Here's how we get the value in the second row and fifth column
starting_dataframe[2,5]

# Here's how we get all the info for row number 6
starting_dataframe[6, ]

# And of course you can mix it all together!
starting_dataframe[6, ]$Indiv   # returns the value in the column Indiv for the sixth observation

# Subsetting with one condition

starting_dataframe[starting_dataframe$Zone < 4, ]    # returns only the data for zones 2-3
starting_dataframe[starting_dataframe$Zone <= 4, ]   # returns only the data for zones 2-3-4


# This is completely equivalent to the last statement
starting_dataframe[!starting_dataframe$Zone >= 5, ]   # the ! means exclude


# Subsetting with two conditions
starting_dataframe[starting_dataframe$Zone == 2 | starting_dataframe$Zone == 7, ]    # returns only data for zones 2 and 7
starting_dataframe[starting_dataframe$Zone == 2 & starting_dataframe$Indiv %in% c(300:400), ]    # returns data for shrubs in zone 2 whose ID numbers are between 300 and 400

# CHANGING VARIABLE NAMES AND VALUES IN A DATA FRAME ----

# Let's create a working copy of our object
elongation <- starting_dataframe

# Now suppose you want to change the name of a column: you can use the names() function
# Used on its own, it returns a vector of the names of the columns. Used on the left side of the assign arrow, it overwrites all or some of the names to value(s) of your choice.

names(elongation)                 # returns the names of the columns

names(elongation)[1] <- "zone"    # Changing Zone to zone: we call the 1st element of the names vector using brackets, and assign it a new value
names(elongation)[2] <- "ID"      # Changing Indiv to ID: we call the 2nd element and assign it the desired value

# Now suppose there's a mistake in the data, and the value 5.1 for individual 373 in year 2008 should really be 5.7

## - option 1: you can use row and column number
elongation[1,4] <- 5.7

## - option 2: you can use logical conditions for more control
elongation[elongation$ID == 373, ]$X2008 <- 5.7   # completely equivalent to option 1

## CREATING A FACTOR----

# Let's check the classes
str(elongation)

# The zone column shows as integer data (whole numbers), but it's really a grouping factor (the zones could have been called A, B, C, etc.) Let's turn it into a factor:

elongation$zone <- as.factor(elongation$zone)        # converting and overwriting original class
str(elongation)                                  # now zone is a factor with 6 levels

## CHANGING A FACTOR'S LEVELS

levels(elongation$zone)  # shows the different factor levels

levels(elongation$zone) <- c("A", "B", "C", "D", "E", "F")   # you can overwrite the original levels with new names

# You must make sure that you have a vector the same length as the number of factors, and pay attention to the order in which they appear!


## TIDYING THE DATA 11.7.2024----
#Transform the data frame to long format ----
elongation_long <- gather(elongation, year, length,                           # in this order: data frame, key, value
                          c(X2007, X2008, X2009, X2010, X2011, X2012))        # we need to specify which columns to gather

# Here we want the lengths (value) to be gathered by year (key)

# Let's reverse! spread() is the inverse function, allowing you to go from long to wide format
elongation_wide <- spread(elongation_long, year, length)

#specify the column numbers instead of names to save time
elongation_long2 <- gather(elongation, year, length, c(3:8))

#Plot the data ----
boxplot(length ~ year, data = elongation_long,
        xlab = "Year", ylab = "Elongation (cm)",
        main = "Annual growth of Empetrum hermaphroditum")

#remove the X from year columns
names(elongation)[3:8] <- parse_number(names(elongation[ ,3:8]))
str(elongation)

#long format with years withou the X
elongation_long3 <- gather(elongation, year, length, c(3:8))
str(elongation_long3)

#remove the X from the years elongation_long
elongation_long$year <- parse_number(elongation_long$year)
