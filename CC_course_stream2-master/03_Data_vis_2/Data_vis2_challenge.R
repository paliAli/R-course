##%######################################################%##
#                                                          #
####                DATA VISUALISATION 2                ####
####             Course by Our Coding Club              ####
####                     Challenge                      ####
####                     20.8.2024                      ####
#                                                          #
##%######################################################%##

# Set working directory ----
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

# Add % of endemic species to the data frame
more_magic <- more_magic %>% mutate(endemic = c(0.54, 0.32, 0.66, 0.80, 0.14, 0.24, 0.39))

head(more_magic)
str(more_magic)

# plot the species richness as a bar plot, coloured with a shade representing the % of endemism

(ggplot(more_magic, aes(x = land, y = counts, fill = endemic)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_fill_gradient(low = "#86D4E0", high = "#242D80",
                        name = "Endemism (%)\n") +
    magic.plot() +
    theme(legend.position = "right"))

arrange(more_magic, by_group = endemic)
# Rearrange the lands in growing endemism percentage

more_magic$land <- factor(more_magic$land,
                          levels = c("Mordor", "Forbidden Forest", "Hogsmeade", "Oz", "Narnia", "Westeros", "The Shire"),
                          labels = c("Mordor", "Forbidden Forest", "Hogsmeade", "Oz", "Narnia", "Westeros", "The Shire"))

(ggplot(more_magic, aes(x = land, y = counts, fill = endemic)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(limits = c(0,65)) +
    scale_fill_gradient(low = "#86D4E0", high = "#242D80",
                        name = "Endemism (%)\n") +
    labs(title = "Percentage of endemic species in selected magical lands\n",
         x = "\nMagical land", y = "Number of species\n") +
    geom_text(aes(label = endemic), vjust = -0.3, color = "#8A8585") +
    magic.plot() +
    theme(legend.position = "right"))
