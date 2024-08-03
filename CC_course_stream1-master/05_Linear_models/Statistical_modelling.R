##%######################################################%##
#                                                          #
####         BASICS OF STATISTICAL MODELLING            ####
####            Course by our coding club               ####
####           Linear modelling, 12.7.2024              ####
#                                                          #
##%######################################################%##

# Used packages ----
install.packages("agridat")
library(agridat)
library(ggplot2)
library(ggthemes)
library(dplyr)

# First dataset, apples -----
## Loading the dataset from agridat ----
apples <- agridat::archbold.apple
head(apples)
summary(apples)
str(apples)

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

## Plot the data ----
# Hypothesis - the closer the apple trees are to each other the lower the yield
# Only 3 different spacing values - not continuous -> transform to factor
apples$spacing2 <- as.factor(apples$spacing)

(apples.p <- ggplot(apples, aes(spacing2, yield)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "Spacing (m)", y = "Yield (kg)"))

## Create a linear model of the data ----
apples.m <- lm(yield ~ spacing2, data = apples)
summary(apples.m)

# p < 0.05 -> differences considered significant
# apple yield is indeed higher when the distance between trees is higher
# but spacing only explains 15% of change in the yield - there are some other more influencing factors

# Run ANOVA
anova(apples.m)

## Check assumptions ----

# Checking that the residuals are normally distributed
apples.resid <- resid(apples.m)  # Extracting the residuals
shapiro.test(apples.resid)                   # Using the Shapiro-Wilk test
# The null hypothesis of normal distribution is accepted: there is no significant difference (p > 0.05) from a normal distribution

# Checking for homoscedasticity
bartlett.test(apples$yield, apples$spacing2)
bartlett.test(yield ~ spacing2, data = apples)  # Note that these two ways of writing the code give the same results
# The null hypothesis of homoscedasticity is accepted

plot(apples.m)  # you will have to press Enter in the command line to view the plots

# Second dataset, sheeps ----
sheep <- agridat::ilri.sheep   # load the data
str(sheep)

sheep <- filter(sheep, ewegen == "R")   # there are confounding variables in this dataset that we don't want to take into account. We'll only consider lambs that come from mothers belonging to the breed "R".

head(sheep)  # overview of the data; we'll focus on weanwt (wean weight) and weanage

sheep.m1 <- lm(weanwt ~ weanage, data = sheep)   # run the model
summary(sheep.m1)                                # study the output
# the linear equation would be lamb weight = 2.60 + 0.08(age)

# weanage only explains 20% of the variation in weight at weaning. What if sex also influences weight gain?
sheep.m2 <- lm(weanwt ~ weanage*sex, data = sheep)
summary(sheep.m2)

## Visualize the relationship ----
(sheep.p <- ggplot(sheep, aes(x = weanage, y = weanwt)) +
   geom_point(aes(colour = sex)) +                                # scatter plot, coloured by sex
   labs(x = "Age at weaning (days)", y = "Wean weight (kg)") +
   stat_smooth(method = "lm", aes(fill = sex, colour = sex)) +    # adding regression lines for each sex
   scale_colour_manual(values = c("#FFC125", "#36648B")) +
   scale_fill_manual(values = c("#FFC125", "#36648B")) +
   theme.clean() )

