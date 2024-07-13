##%######################################################%##
#                                                          #
####         BASIC DATA MANIPULATION Challenge          ####
####    Which spice triggers the most fiery reaction?   ####
####                  And the least?                    ####
####                    11.7.2024                       ####
#                                                          #
##%######################################################%##

# Used packages----
library(dplyr)

# Import Data----
setwd("C:/Users/pavla/OneDrive/Documents/GitHub/R-course/CC_course_stream1-master/04_Data_manip_1")
fire_length <- read.csv("dragons.csv", header = TRUE)

head(fire_length)

# Correct the data frame----
# The fourth treatment wasn’t paprika at all, it was turmeric.
fire_length <- rename(fire_length, turmeric = paprika)

# There was a calibration error with the measuring device for the tabasco trial, but only for the Hungarian Horntail species. 
# All measurements are 30 cm higher than they should be.
fire_length2 <- fire_length
fire_length2[fire_length2$species == "hungarian_horntail", ]$tabasco <- fire_length2[fire_length2$species == "hungarian_horntail", ]$tabasco - 30

# Transform to a long format
fire_length_long <- gather(fire_length2, spice, fire_length, c("tabasco", "jalapeno", "wasabi", "turmeric"))

# The lengths are given in centimeters, but really it would make sense to convert them to meters.
fire_length_long$fire_length <- fire_length_long$fire_length / 100

# Create a boxplot for each species showing the effect of the spices on plume size----

# Create a subset for each species to make boxplots

str(unique(fire_length_long$species))
# Create a subset for each species to make boxplots
horntail <- filter(fire_length_long, species == 'hungarian_horntail')            # the dplyr way of filtering
green <- filter(fire_length_long, species == 'welsh_green')
shortsnout <- filter(fire_length_long, species == 'swedish_shortsnout')

#boxplot for hungarian horntail
boxplot(fire_length ~ spice, data = horntail)

#boxplot for welsh green
boxplot(fire_length ~ spice, data = green)

#boxplot for swedish shortsnout
boxplot(fire_length ~ spice, data = shortsnout)
