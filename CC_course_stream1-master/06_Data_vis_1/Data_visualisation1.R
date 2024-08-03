##%######################################################%##
#                                                          #
####                DATA VISUALISATION 1                ####
####              Course by our coding club             ####
####      Using ggplot2 tocommunicate your results      ####
####                      3.8.2024                      ####
#                                                          #
##%######################################################%##

# Set the working directory ----
setwd('C:/Users/pavla/OneDrive/Documents/GitHub/R-course/CC_course_stream1-master/06_Data_vis_1')

# Used libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

# Custom functions

# Importing and cleaning data
LPI <- read.csv('LPIdata_CC.csv')
head(LPI)
str(LPI)
dim(LPI)

# Convert from wide format to long format
LPI_long <- gather(LPI, "year", "abundance", 9:53)
