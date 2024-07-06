bird_sp <- c("sparrow",
             "kingfisher",
             "eagle",
             "hummingbird",
             "sparrow",
             "kingfisher",
             "eagle",
             "hummingbird",
             "sparrow",
             "kingfisher",
             "eagle",
             "hummingbird")
summary(bird_sp)
bird_sp <- as.factor(bird_sp)
class(bird_sp)

wingspan <- c(22, 26, 195, 8, 24, 23, 201, 9, 21, 25, 185, 9)

bird_wingspan <- data.frame(bird_sp, wingspan)

mean_wingspan <- bird_wingspan %>%
  group_by(bird_sp) %>% 
  mutate(wingspan = mean(wingspan)) %>%
  distinct(.keep_all = FALSE)

mean_wingspan$wingspan <- round(mean_wingspan$wingspan, digits = 2)

library(ggplot2)
ggplot(data = mean_wingspan, aes(x = bird_sp, y = wingspan))+
  geom_bar(stat = "identity", color = "grey")+
  labs(title = "Mean wingspan of different bird species", x = "Bird species", y = "Average wingspan (cm)")+
  geom_text(aes(label = wingspan), vjust=-0.3, color="black", size=3.5)+
  theme_minimal()


# Podle kurzu -------------------------------------------------------------

# Calculate the mean wingspan for each bird species. The function to do that is simply: mean()
sparrow <- mean(22, 24, 21)
kingfisher <- mean(26, 23, 25)
eagle <- mean(195, 201, 185)
hummingbird <- mean(8, 9, 9)

# Chain them together in a vector
wingspan <- c(sparrow, kingfisher, eagle, hummingbird)

# Create a bird species vector (careful to match the order of the previous vector!)
bird_sp <- c("sparrow", "kingfisher", "eagle", "hummingbird")
# notice how we put quotation marks around the names. It's because we're creating (character) values; writing sparrow without the "" would call the object we created in the code above, which would return the value 22!

# Bird species is currently in character form, but it should be a factor. Let's fix that:
# (To be honest it does not make any difference to the output here, but it would for some other types of plot. Take good habits early!)
class(bird_sp)                      # currently character
bird_sp <- as.factor(bird_sp)       # transforming into factor
class(bird_sp)                      # now a factor! 


# Then, combine the two vectors in a data frame
wings <- data.frame(bird_sp, wingspan)

# Plot the bar plot & save it to file

png("wingspan_plot.png", width=800, height=600)
barplot(wings$wingspan, names.arg = wings$bird_sp,    # notice how we call the bird_sp column instead of typing all the names
        xlab = "Bird species", 
        ylab = "Average wingspan (cm)",               # adding axis titles
        ylim = c(0, 200),                             # setting the limits of the y axis to fit the eagle
        col = "gold"                                  # changing the colour because why not!
)
dev.off()
