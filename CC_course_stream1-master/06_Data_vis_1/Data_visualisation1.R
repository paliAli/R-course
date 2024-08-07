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
theme.histogram <- function(){
  theme_bw()+
    theme(axis.text = element_text(size = 12),                          # Changing font size of axis labels and title
          axis.title.x = element_text(size = 14, face = "plain"),       # face="plain" is the default, you can change it to italic, bold, etc. 
          axis.title.y = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(),                                 # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
}

theme.scatter <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = c(0.85, 0.85))                                 # Setting legend position - 0 is left/bottom, 1 is top/right
}

theme.scatter2 <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right")                                 # Setting legend position - 0 is left/bottom, 1 is top/right
}

theme.boxplot <- function(){
  theme_bw()+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                 # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               # Adding a margin
          legend.position = "none")                                    # Removing legend - not needed with only 2 factors
}

# Importing and cleaning data
LPI <- read.csv('LPIdata_CC.csv')
head(LPI)
str(LPI)
dim(LPI)

# Convert from wide format to long format
LPI_long <- gather(LPI, "year", "abundance", 9:53)

LPI_long$year <- parse_number(LPI_long$year)

str(LPI_long)
LPI_long$abundance <- as.numeric(LPI_long$abundance)

# Choose one specie to focus on
unique(LPI_long$Common.Name)
butterfish <- LPI_long %>%
  filter(Common.Name == "Butterfish")

butterfish <- na.omit(butterfish)

# Plot the abundance distribution ----
# Using histogram
(butterfish_histogram <- ggplot(butterfish, aes(x = abundance))+
  geom_histogram(binwidth = 2, colour = "#8B5A00", fill = "#CD8500")+
   geom_vline(aes(xintercept = mean(abundance)),
              colour = "red", linetype = "dashed", linewidth = 1)+
   ylab("Count\n")+
   xlab("\nButterfish abundance")+
   theme.histogram())

# Scatter plot to examine change in abundance over time ----

(butterfish_scatter <- ggplot(butterfish, aes(x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", aes(fill = Country.list)) +
    scale_fill_manual(values = c("#B81D1A", "#3E96C2")) +
    scale_colour_manual(values = c("#B81D1A", "#3E96C2"),
                        labels = c("USA", "Canada")) +
    ylab("Butterfish yearly abundace\n") +
    xlab("\nYear")+
    theme.scatter())

# Box plot to examine whether butterfish abundance differs between USA and Canada ----

(butterfish_boxplot <- ggplot(data = butterfish, aes(x = Country.list, y = abundance)) +
   geom_boxplot(aes(fill = Country.list)) +
   scale_fill_manual(values = c("#B81D1A", "#3E96C2")) +
   scale_colour_manual(values = c("#B81D1A", "#3E96C2")) +
   ylab("Butterfish abundance\n")+
   xlab("\nCountry")+
   theme.boxplot())

# Use barplot to compare species richness of a few European countries

# Create a new column based on how many unique species are in a country
richness <- LPI_long %>%
  filter(Country.list %in% c("France", "Italy", "Spain", "Switzerland", "Czech Republic")) %>%
  group_by(Country.list) %>%
  mutate(richness = (length(unique(Common.Name))))

head(richness)

(richness_barplot <- ggplot(data = richness, aes(x = Country.list, y = richness))+
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#7CBA55")+
    ylab("Species richness\n")+
    xlab("\nCountry")+
    theme.boxplot())

# Using facets and creating panels ----

tit <- LPI_long %>%
  filter(Common.Name == "Coal tit") %>%
  na.omit()


(tit_all <- ggplot(data = tit, aes(x = year, y = abundance, colour = Country.list))+
    geom_point(size = 2)+
    geom_smooth(method = "lm", aes(fill = Country.list))+
    ylab("Abundace\n")+
    xlab("\nYear")+
    theme.scatter())
# too cluttered!


(tit_facet <- ggplot(data = tit, aes(x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", aes(fill = Country.list)) +
    facet_wrap(~ Country.list, scales = "free_y") +                      # THIS LINE CREATES THE FACETTING
    ylab("Abundace\n") +
    xlab("\nYear") +
    theme.scatter2())

# Arranging multiple figures together to create a panel

grid.arrange(butterfish_histogram, butterfish_scatter, butterfish_boxplot, ncol = 1)

# This doesn't look right - the graphs are too stretched, the legend and text are all messed up, the white margins are too big

# Fixing the problems - adding ylab() again overrides the previous settings

(panel <- grid.arrange(
  butterfish_histogram + ggtitle("(a)") + ylab("Count") + xlab("Abundance") +   # adding labels to the different plots
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  butterfish_boxplot + ggtitle("(b)") + ylab("Abundance") + xlab("Country") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  butterfish_scatter + ggtitle("(c)") + ylab("Abundance") + xlab("Year") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
    theme(legend.text = element_text(size = 12, face = "italic"),     
          legend.title = element_blank(),                                   
          legend.position = c(0.83, 0.83)), # changing the legend position so that it fits within the panel
  
  ncol = 1)) # ncol determines how many columns you have

ggsave(panel, filename = "butterfish_plots.png", width = 2500, height = 5500, units = "px")
