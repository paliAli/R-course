##%######################################################%##
#                                                          #
####               Linear models Challenge              ####
####              Course by our coding club             ####
####                      2.8.2024                      ####
#                                                          #
##%######################################################%##

# Import dataset
ToothGrowth <- datasets::ToothGrowth

head(ToothGrowth)
str(ToothGrowth)

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

# Are higher doses of vitamin C beneficial for tooth growth? ----
growth_dose.m <- lm(len ~ dose, data = ToothGrowth)
summary(growth_dose.m)
# p-value > 0.05 -> the teeth grow longer with higher doses

#Check how many different doses are in the dataset
unique(ToothGrowth$dose)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

(growth_dose.p <- ggplot(ToothGrowth, aes(x = dose, y = len))+
  geom_boxplot(fill = "white", color = "orange2") + 
  theme.clean())

# Does the method of administration (orange juice, OJ, or ascorbic acid, VC) influence the effect of the dose? ----

tooth.m <- lm(len ~ dose*supp, data = ToothGrowth)
summary(tooth.m)
# dose and method explain around 77% of the variation in tooth growth

ggplot(ToothGrowth, aes(x = dose, y = len))+
  geom_boxplot(aes(colour = supp)) +
  theme.clean()
# Yes, the effect of dose on growth depends on the administration method.

# What would be the predicted tooth length of a guinea pig given 1 mg of vitamin C as ascorbic acid? ----
summary(tooth.m)
# for dose of 0.5 mg: 13.23
# dose 1 = 9.470 slope
# - 0.680 when given as ascorbic acid
# - 5.250 difference in growth linked to the ascorbic acid treatment for dose 0.5
# Total growth when given 1 mg of vitamin C as ascorbic acid: 13.23 + 9.470 - 0.68 - 5.250 = 16.77