library(plyr)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)

###
# Read raw CSVs and spit out tidier datasets
###

paths <- dir("mturkdata", pattern = "\\.csv$", full.names = TRUE)
names(paths) <- basename(paths)
df <- ldply(paths, read_csv)

df %>%
  filter(HitTitle != "Cla1*", HitTitle != "2tempdistmonB2") %>%
  # drop useless columns
  select(-.id, -Annotation, -HitId, -Status) %>%
  # remove annoying space from column name
  rename(distance = `Answer 1`) %>%
  # convert time strings into date-time objects to calculate task duration
  mutate(AcceptTime = as.POSIXct(AcceptTime, format = "%a %b %d %H:%M:%S PDT %Y", tz="UTC"),
         SubmitTime = as.POSIXct(SubmitTime, format = "%a %b %d %H:%M:%S PDT %Y", tz="UTC"),
         Date = as.Date(AcceptTime),
         DurationSeconds = SubmitTime - AcceptTime) %>%
  # drop now useless columns
  select(-AcceptTime, -SubmitTime) %>% 
  # extract painter, painting, category from HitTitle
  mutate(Painter = revalue(str_extract(HitTitle, "((M|m)on|(s|S)till|Cl|Rk)"),
                           c("mon" = "Mondrian",
                             "Mon" = "Mondrian",
                             "still" = "Still",
                             "Cl" = "Close",
                             "Rk" = "Rothko")),
         Painting = str_to_upper(str_extract(HitTitle,
                                             "[ABCabc]\\d(\\*|)")),
         Category = revalue(str_extract(Painting, "\\d"),
                            c("1" = "Concrete",
                              "2" = "Indeterminate",
                              "3" = "Abstract"))) ->
  df

df %>%
  filter(grepl("spat", HitTitle)) %>%
  rename(SpatialDistance = distance) %>%
  write_csv("mturk_spatial.csv")

df %>%
  # temporal data sometime
  filter(!grepl("spat", HitTitle)) %>%
  rename(TemporalDistance = distance) %>%
  write_csv("mturk_temporal.csv")