library(dplyr)
library(ggplot2)

##
# semi cleaned data work
##
observations <- read_csv("~/Desktop/csv_cleaner/observations.csv")
paintings <- read_csv("~/Desktop/csv_cleaner/paintings.csv")
participants <- read_csv("~/Desktop/csv_cleaner/participants.csv")

observations <- full_join(observations, paintings,  by = c("painting_index", "painting"))



# average liking by painting
observations %>%
  group_by(painting_index, painting_category) %>%
  summarize(mean_liking = mean(liking_value)) ->
  obs_mean_liking
  
# plot average liking by painting
ggplot(obs_mean_liking, aes(x = painting_index, y = mean_liking, color = painting_category)) + geom_point()
boxplot(obs_mean_liking$mean_liking)

# plot distribution of liking by painting
ggplot(observations, aes(x = painting_index, y = liking_value, group = painting_index, color = painting_category)) +
  geom_boxplot() +
  facet_grid(. ~ painting_category)

# plot liking versus abstractness, jittered to see the points easier
ggplot(observations, aes(x = abstraction_value, y = liking_value, color = painting_category)) +
  geom_point() +
  geom_jitter()

# plot abstractness by category
ggplot(observations, aes(x = painting_category, y = abstraction_value, fill = painting_category)) + geom_boxplot()

  
