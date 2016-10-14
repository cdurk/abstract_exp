library(plyr)
library(dplyr)
library(readr)
library(stringr)

###
# Read raw CSVs and spit out tidier datasets
###

paths <- dir("~/Desktop/csv", pattern = "\\.csv$", full.names = TRUE)
names(paths) <- basename(paths)
df <- ldply(paths, read_csv)

colnames_original <- names(df)

colnames_new <- c(".id", "painting",
                  "comparison_position_1", "comparison_position_2",
                  "comparison_painting",
                  "distance_repetition", "distance_trial", "distance_index",
                  "distance_paintingindex", 
                  "liking_repetition", "liking_trial", "liking_index",
                  "liking_paintingindex",
                  "abstraction_repetition", "abstraction_trial",
                  "abstraction_index", "abstraction_paintingindex", 
                  "comparison_repetition", "comparison_trial", "comparison_index", 
                  "comparison_paintingindex",
                  "continue_1_key", "continue_1_responsetime",
                  "distance_value", "distance_responsetime", 
                  "continue_2_key", "continue_2_responsetime",
                  "liking_value", "liking_responsetime",
                  "experience_rating", "experience_responsetime",
                  "viewing_value", "viewing_responsetime",
                  "continue_3_key", "continue_3_responsetime", 
                  "abstraction_value", "abstraction_responsetime", 
                  "continue_4_key", "continue_4_responsetime", 
                  "comparison_value", "comparison_responsetime",
                  "date", "framerate", "experiment", "session", "participant",
                  "X46", "X27", "X44")

names(df) <- colnames_new

# drop useless columns
df %>%
  select(-.id, -X46, -X27, -X44) ->
  df

# participant 05 incorrectly labeled as 06
df[df$date == "2016_Jul_14_1440", ]$participant = "05"

# participant id's are not as unique as they could be
df %>%
  filter(participant != "12") %>%
  mutate(participant = ifelse(experiment == "tryagain",
                              paste0("a_", participant),
                              paste0("b_", participant))
  ) ->
  df

# function to split out each experiment into a data set
split_experiments <- function(df, filter) {
  df %>%
    rename_(.dots=setNames(paste0(filter, "_paintingindex"), "painting_index")) %>%
    select(participant, painting_index, painting, starts_with(filter)) %>%
    filter(!is.na(painting), complete.cases(.))
}

# create datasets for each question
# revalue distance with times
split_experiments(df, "distance") %>%
  mutate(distance_value = revalue(distance_value,
                                  c("left" = "tomorrow",
                                    "right" = "in a year"))) ->
  distance

liking <- split_experiments(df, "liking")
abstraction <- split_experiments(df, "abstraction")
# revalue painting_index as integer
### TODO painting_index is reset, should be fixed
split_experiments(df, "comparison") %>%
  mutate(painting_index = as.integer(painting_index)) %>%
  rename(painting_index_comparison = painting_index) ->
  comparison


full_join(distance, liking) %>%
  full_join(abstraction) %>%
  full_join(comparison) ->
  observations

write_csv(observations, "~/Desktop/csv_cleaner/observations.csv")

###
# extract participants from datasets
###
# combine rows with summarize, use min( , na.rm = T) to get rid of NA values
# revalue art knowledge variables
df %>%
  filter(!is.na(experience_rating) | !is.na(viewing_value)) %>%
  select(participant, experiment, date, session, framerate,
         starts_with("experience"), starts_with("viewing")) %>%
  mutate(date = as.POSIXct(date, format = "%Y_%b_%d_%H%M")) %>%
  group_by(participant, session, date, framerate, experiment) %>%
  summarize(experience_rating = min(experience_rating, na.rm = TRUE),
            experience_responsetime = min(experience_responsetime, na.rm = TRUE),
            viewing_value = min(viewing_value, na.rm = TRUE),
            viewing_responsetime = min(viewing_responsetime, na.rm = TRUE)) %>%
  mutate(experience_rating = revalue(experience_rating,
                                     c("a" = "novice",
                                       "b" = "enthusiast",
                                       "c" = "artist/art historian")),
         viewing_value = revalue(viewing_value,
                                     c("a" = "0",
                                       "b" = "1-4",
                                       "c" = "5 or more"))) ->
  participants

write_csv(participants, "~/Desktop/csv_cleaner/participants.csv")

###
# extract paintings from datasets
###
observations %>%
  select(starts_with("painting")) %>%
  unique() %>%
  group_by(painting, painting_index) %>%
  summarize(painting_index_comparison = min(painting_index_comparison, na.rm = TRUE)) %>%
  mutate(painter = str_extract(painting, "[A-Z][a-zA]+\\d"),
         painter = str_replace(painter, "[Aab]\\d", ""),
         painter = revalue(painter, c("Mon" = "Mondrian",
                                       "Clif" = "Clifford")),
         painting_category = str_extract(painting, "\\d"),
         painting_category = revalue(painting_category,
                                     c("3" = "abstract",
                                       "2" = "indeterminate",
                                       "1" = "concrete"))) ->
  paintings

write_csv(paintings, "~/Desktop/csv_cleaner/paintings.csv")

