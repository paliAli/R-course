##%######################################################%##
#                                                          #
####    Proper coding etiquette and file layout #3rd    ####
####         lecture in R course by coding club 
####             Alena Pavlačková, 6.7.2024
#                                                          #
##%######################################################%##

# Insert a box around the introductory section of your script
install.packages("devtools")
devtools::install_github("ThinkRstat/littleboxes")

#Set working directory
setwd("C:/Users/pavla/OneDrive/Documents/GitHub/R-course/CC_course_stream1-master/03_Coding_etiquette")

#import data ----
LPI <- read.csv("LPIdata_CC.csv")

summary(LPI)

#Packages ----
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)

# Defining functions ----
# A custom ggplot2 function
theme.LPI <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(),                              
          legend.position=c(0.9, 0.9))
}

# Formatting data ----
LPI2 <- gather(LPI, "year", "abundance", 9:53)  # Transforming the data from wide to long format, some blank cells may disappear
# gather function requires tidyr package
LPI2$year <- parse_number(LPI2$year)  # Do you see awkward Xs before all the years? This gets rid of them.
names(LPI2)  # Check what the different variables are called
names(LPI2) <- tolower(names(LPI2))  # Make all variable names lower case

# When manipulating data it's always good check if the variables have stayed how we want them
# Use the str() function
str(LPI2)

# Abundance is a character variable, when it should be numeric, let's fix that
LPI2$abundance <- as.numeric(LPI2$abundance)

# Calc summary stats for each biome in the LPI database ----
list(unique(LPI2$biome))  # list all biomes

LPI_biome_summ <- LPI2 %>%  # use of pipe operator
  group_by(biome) %>%  # Group by biome
  summarise(populations = n())  # Create columns, number of populations

# Visualising the number of populations in each biome with ggplot2 package ---- 
(barplot <- ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +  
   theme.LPI() +                     # Use of personal theme function
   ylab("Number of populations") +
   xlab("Biome") +
   theme(legend.position = "none"))  # Removal of legend for simplicity

#Outputting the plot as png
png(file="biome_pop.png", width = 1000, height = 2000)
(barplot <- ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +  
    theme.LPI() +                     # Use of personal theme function
    ylab("Number of populations") +
    xlab("Biome") +
    theme(legend.position = "none"))  # Removal of legend for simplicity
dev.off()

#Outputting the plot as png using ggsave
LPI_plot <- recordPlot()
LPI_plot
ggsave("biome_pop(2).png", width = 3500, height = 7000, units = "px", dpi = 300)
