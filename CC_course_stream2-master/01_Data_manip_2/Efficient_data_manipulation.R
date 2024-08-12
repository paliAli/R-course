##%######################################################%##
#                                                          #
####             Efficient data manipulation            ####
####              Course by Our Coding Club             ####
####                      8.8.2024                      ####
#                                                          #
##%######################################################%##

# Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)

# Set working directory ----
setwd("C:/Users/pavla/OneDrive/Documents/GitHub/R-course/CC_course_stream2-master/01_Data_manip_2")

# Custom functions ----
theme_scatterplot1 <- function(){
  theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 12))
}

theme_scatterplot2 <- function(){
  theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5),
          legend.position = "bottom")
}
# Import and clean data ----
trees <- read.csv("trees.csv")

head(trees)

# how many trees of each species are found in the dataset
trees.summary <- trees %>%
  group_by(CommonName) %>%
  summarise(count = length(CommonName))

# or I can use function tally (same as summarise(count = length(CommonName)))
trees.summary <- trees %>%
  group_by(CommonName) %>%
  tally()

trees.subset  <- trees %>% 
  filter(CommonName %in% c('Common Ash', 'Rowan', 'Scots Pine')) %>% 
  group_by(CommonName, AgeGroup) %>% 
  tally()

# More dplyr functions ----
# Summarise_all
summ.all <- summarise_all(trees, mean)

# ifelse and case_when
vector <- c(4, 13, 15, 6)      # create a vector to evaluate

ifelse(vector < 10, "A", "B")  # give the conditions: if inferior to 10, return A, if not, return B

vector2 <- c("What am I?", "A", "B", "C", "D")

case_when(vector2 == "What am I?" ~ "I am the walrus",
          vector2 %in% c("A", "B") ~ "goo",
          vector2 == "C" ~ "ga",
          vector2 == "D" ~ "joob")

# Create a Genus column using mutate, case_when and grepl
unique(trees$LatinName)

genus <- trees %>% 
  mutate(Genus = case_when(
    grepl("Acer", LatinName) ~ "Acer",
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula",
    grepl("Populus", LatinName) ~ "Populus",
    grepl("Laburnum", LatinName) ~ "Laburnum",
    grepl("Aesculus", LatinName) ~ "Aesculus",
    grepl("Fagus", LatinName) ~ "Fagus",
    grepl("Prunus", LatinName) ~ "Prunus",
    grepl("Pinus", LatinName)  ~ "Pinus",
    grepl("Sambucus", LatinName) ~ "Sambucus",
    grepl("Crataegus", LatinName) ~ "Crataegus",
    grepl("Ilex", LatinName) ~ "Ilex",
    grepl("Quercus", LatinName) ~ "Quercus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Larix", LatinName) ~ "Larix",
    grepl("Salix", LatinName) ~ "Salix",
    grepl("Alnus", LatinName) ~ "Alnus")
  )

genus2 <- trees %>% 
  separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>% 
  select(- Species)

# Categorize height
unique(trees$Height)

genus <- genus %>% 
  mutate(Height.cat = case_when(
    Height %in% c("5 to 10 meters", "Up to 5 meters") ~ "Short",
    Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",
    Height == "20 to 25 meters" ~ "Tall")
  )

summary(genus)
# Need to first change Height.cat to factor
genus$Height.cat <- as.factor(genus$Height.cat)
levels(genus$Height.cat) #levels by default in alphabetical order

# Change the order to short, medium, tall so it makes sense logically
genus$Height.cat <- factor(genus$Height.cat,
                           levels = c('Short', 'Medium', 'Tall'),
                           labels = c('SHORT', 'MEDIUM', 'TALL'))
levels(genus$Height.cat)

# Advanced piping ----

# Subset dataframe
trees.five <- genus %>% 
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# Plot the data on a scatter plot and separate by genera and size
(ggplot(trees.five, aes(x = Easting, y = Northing)) +
    geom_point(aes(size = Height.cat, colour = Genus), alpha = 0.5)+
    xlab("\nEasting") +
    ylab("Northing\n") +
    theme_scatterplot1())

# Plotting a map for each genus separately
tree.plots <- trees.five %>% 
  group_by(Genus) %>% 
  do(plots =
       ggplot(data = ., aes(x = Easting, y = Northing)) +
       geom_point(aes(size = Height.cat, colour = Height.cat), alpha = 0.5) +
       scale_color_manual(values = c("#D92623", "#3B38C2", "#2FBA31")) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle\n", sep = " "),
            x = "\nEasting",
            y = "Northing\n") +
       theme_scatterplot2())

# You can view the graphs before saving them
tree.plots$plots[1]

# Saving the plots to file
tree.plots %>% 
  do(.,
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = " "),
            device = "png", height = 12, width = 16, units = "cm"))
