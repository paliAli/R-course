##%######################################################%##
#                                                          #
####              Data manipulation challenge              ####
####               Course by Our Coding Club               ####
####                        12.8.2024                      ####
#                                                          #
##%######################################################%##

# Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)

# Set working directory ----
setwd("C:/Users/pavla/OneDrive/Documents/GitHub/R-course/CC_course_stream2-master/01_Data_manip_2")

# Custom functions ----

theme.bar <- function(){
  theme_bw()+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                 # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               # Adding a margin
          legend.position = "none")                                    # Removing legend - not needed with only 2 factors
}

# Create a summary of the different species found within its grounds, but broken down in four quadrants (NE, NW, SE, SW)
# 1. Can you calculate the species richness (e.g. the number of different species) in each quadrant?
# starting with genus dataset
head(genus)

# Find the center coordinates that will divide the data (adding half of the range in longitude and latitude to the smallest value)

lon <- (max(genus$Easting) - min(genus$Easting))/2 + min(genus$Easting)
lat <- (max(genus$Northing) - min(genus$Northing))/2 + min(genus$Northing)

# Create the columns
genus_coord <- genus %>% 
  mutate(Quadrant = case_when(
    Easting <= lon & Northing > lat ~ 'NW',
    Easting <= lon & Northing < lat ~ 'SW',
    Easting > lon & Northing > lat ~ 'NE',
    Easting > lon & Northing < lat ~ 'SE')
  )

summary(genus_coord)
genus_coord$Quadrant <- as.factor(genus_coord$Quadrant)

# Count the number of unique common names for each quadrant
genus_richness <- genus_coord %>% 
  group_by(Quadrant) %>% 
  summarise(richness = length(unique(CommonName)))

# 2. How abundant the genus Acer is (as a % of the total number of trees) in each quadrant?

genus_coord <- genus_coord %>% 
  mutate(Total = n())

# the amount of Acer trees in each quadrant

Acer <- genus_coord %>% 
  group_by(Quadrant, Genus) %>%
  tally() %>%
  group_by(Quadrant) %>% 
  mutate(total = sum(n)) %>%
  filter(Genus == 'Acer') %>%
  mutate(percentage = total/n)

# We can make a plot representing the %

ggplot(Acer) +
  geom_col(aes(x = Quadrant, y = percentage)) +
  labs(x = 'Quadrant', y = 'Proportion of Acer') +
  theme_bw()

# For each quadrant separately, create a bar plot showing counts of Acer trees in the different age classes, ordered so they read from Young (lumping together juvenile and semi-mature trees), Middle Aged, and Mature.
unique(genus_coord$AgeGroup)
summary(genus_coord)
genus_coord$AgeGroup <- as.factor(genus_coord$AgeGroup)

genus_coord$AgeGroup <- factor(genus_coord$AgeGroup,
                         levels = c('Juvenile', 'Semi-mature', 'Middle Aged', 'Mature'),
                         labels = c('Young', 'Young', 'Middle Aged', 'Mature'))
levels(genus_coord$AgeGroup)

# Count amount of Acer trees per age group in each quadrant
acer_age <- genus_coord %>% 
  group_by(Quadrant, AgeGroup) %>% 
  tally()

acer.plots <- acer_age %>% 
  do(plots =
       ggplot(., aes(x = AgeGroup, y = n)) +
       geom_bar(position = position_dodge(), stat = "identity", aes(fill = AgeGroup)) +
       labs(title = paste("Age distribution of Acer trees in the", .$Quadrant, "quadrant", sep = " "),
            x = '\nAge group',
            y = 'Amount\n') +
       theme.bar())

acer.plots$plots[2]
