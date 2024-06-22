# Coding Club Workshop 1 - R Basics
# Learning how to import and explore data, and make graphs about Edinburgh's biodiversity
# Written by Gergana Daskalova 06/11/2016 University of Edinburgh

library(dplyr)
setwd("C:/Users/pavla/OneDrive/Documents/GitHub/R-course/CC_course_stream1-master")
edidiv <- read.csv("C:/Users/pavla/OneDrive/Documents/GitHub/R-course/CC_course_stream1-master/01_Getting_started/edidiv.csv")

str(edidiv)
head(edidiv$taxonGroup)     # Displays the first few rows of this column only
class(edidiv$taxonGroup)    # Tells you what type of variable we're dealing with: it's character now but we want it to be a factor

edidiv$taxonGroup <- as.factor(edidiv$taxonGroup)   

# More exploration
dim(edidiv)                 # Displays number of rows and columns
summary(edidiv)             # Gives you a summary of the data
summary(edidiv$taxonGroup)  # Gives you a summary of that particular variable (column) in your dataset


# Species richness --------------------------------------------------------
#total amount of different species in a given place

Beetle <- filter(edidiv, taxonGroup == "Beetle")
Bird <- filter(edidiv, taxonGroup == "Bird")
Butterfly <- filter(edidiv, taxonGroup == "Butterfly")
Dragonfly <- filter(edidiv, taxonGroup == "Dragonfly")
Flowering.Plants <- filter(edidiv, taxonGroup == "Flowering.Plants")
Fungus <- filter(edidiv, taxonGroup == "Fungus")
Hymenopteran <- filter(edidiv, taxonGroup == "Hymenopteran")
Lichen <- filter(edidiv, taxonGroup == "Lichen")
Liverwort <- filter(edidiv, taxonGroup == "Liverwort")
Mammal <- filter(edidiv, taxonGroup == "Mammal")
Mollusc <- filter(edidiv, taxonGroup == "Mollusc")

Beetle_species <- length(unique(Beetle$taxonName))
Beetle_species
Bird_species <- length(unique(Bird$taxonName))
Butterfly_species <- length(unique(Butterfly$taxonName))
Dragonfly_species <- length(unique(Dragonfly$taxonName))
Flowering.Plants_species <- length(unique(Flowering.Plants$taxonName))
Fungus_species <- length(unique(Fungus$taxonName))
Hymenopteran_species <- length(unique(Hymenopteran$taxonName))
Lichen_species <- length(unique(Lichen$taxonName))
Liverwort_species <- length(unique(Liverwort$taxonName))
Mammal_species <- length(unique(Mammal$taxonName))
Mollusc_species <- length(unique(Mollusc$taxonName))

#create a vector with the number of species and label them
biodiv <- c(Beetle_species, Bird_species, Butterfly_species, Dragonfly_species, Flowering.Plants_species, Fungus_species,
            Hymenopteran_species, Lichen_species, Liverwort_species, Mammal_species, Mollusc_species)
names(biodiv) <- c("Beetle",
                   "Bird",
                   "Butterfly",
                   "Dragonfly",
                   "Flowering.Plants",
                   "Fungus",
                   "Hymenopteran",
                   "Lichen",
                   "Liverwort",
                   "Mammal",
                   "Mollusc")
biodiv

#Plot the species richness
barplot(biodiv, xlab = "Taxa", ylab = "Number of species", ylim = c(0,600), cex.names = 1.0, cex.axis=1.3, cex.lab=1.5)

help(barplot)     # For help with the barplot() function
help(par)         # For help with plotting in general

biodiv_df <- data.frame("Number_sp" = biodiv)

#with ggplot2
library(ggplot2)
library(ggthemes)

ggplot(data = biodiv_df, aes(x = names(biodiv), y = Number_sp, fill = names(biodiv)))+
  geom_bar(stat = "identity", color = "grey")+
  scale_fill_brewer(palette = "RdYlBu")+
  labs(x = "Taxa", y = "Number of species")+
  geom_text(aes(label = Number_sp), vjust=-0.3, color="black", size=3.5)+
  theme_minimal()

#create data frame  
# Creating an object called "taxa" that contains all the taxa names
taxa <- c("Beetle",
          "Bird",
          "Butterfly",
          "Dragonfly",
          "Flowering.Plants",
          "Fungus",
          "Hymenopteran",
          "Lichen",
          "Liverwort",
          "Mammal",
          "Mollusc")
# Turning this object into a factor, i.e. a categorical variable
taxa_f <- factor(taxa)

# Combining all the values for the number of species in an object called richness
richness <- biodiv

# Creating the data frame from the two vectors
biodata <- data.frame(taxa_f, richness)

# Saving the file
write.csv(biodata, file="biodata.csv")  # it will be saved in your working directory

  
