##%######################################################%##
#                                                          #
####                DATA VISUALISATION 1                ####
####              Course by our coding club             ####
####                      Challenge                     ####
####                      3.8.2024                      ####
#                                                          #
##%######################################################%##

# Set up the libraries and function ----

# Used libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

# Custom functions
theme.scatter <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 15, face = "plain"), 
          strip.text = element_text(size = 12),
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right")                                    
}

theme.boxplot <- function(){
  theme_bw()+
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 15, face = "plain"),
          strip.text = element_text(size = 12),
          panel.grid = element_blank(),                                 # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               # Adding a margin
          legend.position = "none")                                    # Removing legend - not needed with only 2 factors
}

# Same data used as in Data_visualisation1, LPI dataset

# Choose TWO species from the LPI data and display their population trends over time, using a scatterplot and a linear model fit ----
unique(LPI_long$Common.Name)
length(unique(two_species$Country.list))

two_species <- LPI_long %>% 
  filter(Common.Name %in% c("Eurasian lynx", "Grey wolf / Gray wolf")) %>%
  filter(Country.list %in% c("France", "Germany", "Finland", "Poland", "Czech Republic")) %>%
  na.omit()

(lynxwolf_scatter <- ggplot(data = two_species, aes(x = year, y = abundance)) +
    geom_point(size = 2, aes(colour = Country.list)) +
    facet_wrap(~ Common.Name, scales = "free_y") +
    geom_smooth(method = "lm", aes(fill = Country.list, colour = Country.list)) +
    scale_fill_manual(values = c("#00BFFF", "red2", "#9D60AB", "#458B00", "#8B6508"),name = 'Country') +
    scale_colour_manual(values = c("#00BFFF", "red2", "#9D60AB", "#458B00", "#8B6508"), name = 'Country') +
    ylab("Abundance\n") +
    xlab("\nYear") +
    theme.scatter()) 

lynxwolf_scatter
  
hist(two_species$abundance)
shapiro.test(two_species$abundance)

# Using the same two species, filter the data to include only records from FIVE countries of your choice ---- 
# and make a boxplot to compare how the abundance of those two species varies between the five countries
unique(two_species$Country.list)

five_countries <- two_species %>%
  filter(Country.list %in% c("France", "Germany", "Finland", "Poland", "Czech Republic"))

(five_countries_facet <- ggplot(data = five_countries, aes(x = Country.list, y = abundance)) +
  geom_boxplot(aes(colour = Country.list)) +
  ylab("Abundance\n") +
  xlab("\nCountry") +
  scale_colour_manual(values = c("#00BFFF", "red2", "#9D60AB", "#458B00", "#8B6508")) +
  theme.boxplot() + 
  facet_wrap(~ Common.Name, scales = "free_y", ncol = 2)) 

lynx_wolf <- grid.arrange(
  lynxwolf_scatter + ggtitle ("a) Population trends of Eurasian lynx and Gray wolf") + ylab("Abundance") + xlab("Year") +
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm")),
  five_countries_facet + ggtitle ("b) Variation of species abundance") + ylab("Abundance") + xlab("Country") +
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "cm"))
)

ggsave(lynx_wolf, filename = "lynx_wolf.png", width = 4000, height = 3000, units = "px")
