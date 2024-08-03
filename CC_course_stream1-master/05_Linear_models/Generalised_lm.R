##%######################################################%##
#                                                          #
####          BASICS OF STATISTICAL MODELLING           ####
####             Course by our coding club              ####
####       Practicing generalised linear models         ####
####                      27.7.2024                     ####
#                                                          #
##%######################################################%##

# Data with poisson and a binomial distribution

# Used packages ----
library(ggplot2)
library(ggthemes)
library(dplyr)

## Functions ----
theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.position = "right")
}

getwd()

shag <- read.csv("shagLPI.csv")
head(shag)
str(shag)
summary(shag)

# Making a histogram to assess data distribution
(shag.hist <- ggplot(shag, aes(pop)) +
                     geom_histogram() + 
                     theme.clean())

# count abundance data -> poisson distribution
shag.m <- glm(pop ~ year, family = poisson, data = shag)
summary(shag.m)

# Visualize the change in population throughout the years
(shag.p <- ggplot(shag, aes(x = year, y = pop)) +
    geom_point(colour = "#483D8B") +
    geom_smooth(method = glm, colour = "#483D8B", fill = "#483D8B", alpha = 0.6) +
    scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
    theme.clean() +
    labs(x = " ", y = "European Shag abundance"))


## A model with binomial distribution
weevil <- read.csv("Weevil_damage.csv", header = TRUE)
head(weevil)
str(weevil)

# Change block to factor
weevil$block <- as.factor(weevil$block)

# Run the generalized linear model
weevil.m <- glm(damage_T_F ~ block,family = binomial, data = weevil)
summary(weevil.m)

(weevil.p <- ggplot(weevil, aes(x = block, y = damage_T_F))+
  geom_jitter(aes(color = damage_T_F), height = 0.05, width = 0.2))

# show the total amount of T/F in each block
weevil_count <- weevil %>%
  group_by(block, damage_T_F) %>%
  summarise(count = n())

weevil_TRUE <- weevil_count %>%
  filter(damage_T_F == TRUE)

(ggplot(weevil_TRUE, aes(x = block, y = count))+
    geom_bar(stat = "identity", position = "dodge", fill = "white", colour = "red")+
    theme.clean() +
    labs(x = "Block", y = "Counts of damage")+
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)))
