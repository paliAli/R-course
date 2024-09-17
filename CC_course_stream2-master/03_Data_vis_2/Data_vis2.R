##%######################################################%##
#                                                          #
####                DATA VISUALISATION 2                ####
####             Course by Our Coding Club              ####
####              Customising your figures              ####
####                     13.8.2024                      ####
#                                                          #
##%######################################################%##

# Set the working directory ----
setwd('C:/Users/pavla/OneDrive/Documents/GitHub/R-course/CC_course_stream2-master/03_Data_vis_2')

# Used libraries ----
library(dplyr)
library(ggplot2)

# Custom functions ----
magic.plot <- function() {
  theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          plot.caption = element_text(face = "italic"),
          panel.grid = element_blank(),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),
          legend.title = element_text(face = "bold"),
          legend.text = element_text(size = 12),
          legend.position = "bottom",
          legend.box.background = element_rect(color = "grey", linewidth = 1))
}

# Import and clean up data ----
magic_veg <- read.csv("magic_veg.csv")

head(magic_veg)
str(magic_veg)

species_counts <- magic_veg %>% 
  group_by(land, plot) %>% 
  summarise(species_number = length(unique(species)))

# Plotting the data
(ggplot(species_counts, aes(x = plot, y = species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0,50)) +
    scale_fill_manual(values = c("#80271A", "#4BDBCD"), 
                      name = "Land of magic") +
    labs(title = "Species richness by plot",
         subtitle = "In the magical lands",
         caption = "Data from the Ministry of magic",
         x = "\nPlot number", y = "Species richness\n") +
    magic.plot())

# Save the plot
ggsave("magical_sp_richness.png", width = 20, height = 15, units = "cm", dpi = 300)

# New data frame with more lands
# Create vectors with land names and species counts
land <- factor(c("Narnia", "Hogsmeade", "Westeros", "The Shire", "Mordor", "Forbidden Forest", "Oz"))
counts <- as.numeric(c(55, 48, 37, 62, 11, 39, 51))

# Create the new data frame from the vectors
more_magic <- data.frame(land, counts)

length(levels(more_magic$land))
# There are 7 lands so I need 7 colors

# Define the color palette
magic.palette <- c("#8ecae6", "#219ebc", "#126782", "#023047", "#ffb703", "#fd9e02", "#fb8500")
# Assign the colors to factor levels
names(magic.palette) <- levels(more_magic$land)

# Create bar plot
(ggplot(data = more_magic, aes(x = land, y = counts, fill = land)) +
  geom_histogram(stat = "identity", position = "dodge", binwidth = 1) +
  scale_y_continuous(limits = c(0,65)) +
  scale_fill_manual(values = magic.palette,
                    name = "Land of magic") +
  labs(title = "Species richness in magical lands",
       x = "", y = "Species richness\n") +
  magic.plot())

# If I filter out some of the factors, the colours will remain the same
(ggplot(data = filter(more_magic, land %in% c("Hogsmeade", "Oz", "The Shire")), aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge", binwidth = 1) +
    scale_y_continuous(limits = c(0,65)) +
    scale_fill_manual(values = magic.palette,
                      name = "Land of magic") +
    labs(title = "Species richness in magical lands",
         x = "", y = "Species richness\n") +
    magic.plot())

# Box plots -----
# Create a data frame that includes the year
yearly_counts <- magic_veg %>% 
  group_by(year, land, plot) %>% 
  summarise(species_richness = length(unique(species)))

str(yearly_counts)
yearly_counts$plot <- as.factor(yearly_counts$plot)

# Plot a basic box plot
(ggplot(yearly_counts, aes(x = plot, y = species_richness, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),
                      breaks = c("Hogsmeade","Narnia"),
                      name="Land of magic",
                      labels=c("Hogsmeade", "Narnia")) +
    labs(title = "Species richness in magical lands\n",
           x = "\nPlot number", y = "Number of species\n") +
    magic.plot())

# Dot plots ----
# Create a summary containing a mean and standard deviation
summary <- species_counts %>% 
  group_by(land) %>% 
  summarise(mean = mean(species_number),
            sd = sd(species_number))

# Create dot plot
(ggplot(summary, aes(x = land, y = mean, colour = land)) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(20,50)) +
    scale_colour_manual(values = c('#CD5C5C', '#6CA6CD'), 
                        labels = c('HOGSMEADE', 'NARNIA'), 
                        name = 'Land of Magic') +                   
    labs(title = 'Average species richness', 
         x = '', y = 'Number of species \n') +
    magic.plot())

# Reordering the data so that Narnia is shown first
yearly_counts$land <- factor(yearly_counts$land,
                             levels = c("Narnia", "Hogsmeade"),
                             labels = c("Narnia", "Hogsmeade"))

(ggplot(yearly_counts, aes(x = plot, y = species_richness, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name="Land of magic",
                      labels=c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness in magical lands\n",
         x = "\nPlot number", y = "Number of species\n") +
    magic.plot())

# Reorder the plot numbers
yearly_counts$plot <- factor(yearly_counts$plot,
                             levels = c("6", "1", "2", "3", "4", "5"),
                             labels = c("6", "1", "2", "3", "4", "5"))
levels(yearly_counts$plot)

(ggplot(yearly_counts, aes(x = plot, y = species_richness, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name="Land of magic",
                      labels=c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness in magical lands\n",
         x = "\nPlot number", y = "Number of species\n") +
    magic.plot())

# Plot regression lines ----
# Create a data frame with plant heights
heights <- magic_veg %>% 
  filter(!is.na(height)) %>% 
  group_by(year, land, plot, id) %>% 
  summarise(max_height = max(height)) %>% 
  ungroup() %>% 
  group_by(year, land, plot) %>% 
  summarise(height = mean(max_height))

# Plot a simple scatter plot
(ggplot(heights, aes(x = year, y = height, colour = land)) +
    geom_point(size = 3) +
    stat_smooth(method = "lm") +
    theme_bw())

# Try quadratic fit
(ggplot(heights, aes(x = year, y = height, colour = land)) +
    geom_point(size = 3) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)) +
    theme_bw())
