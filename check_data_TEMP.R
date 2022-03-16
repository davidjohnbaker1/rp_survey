# Look at Status of Surveys

library(dplyr)
library(magrittr)
library(ggplot2)

df <- read.csv("data/test3.csv")

df <- df %>%
  filter(recorded_at > "2021-11-22") 

View(df)

df %>%
  select(run_id, recorded_at, trial_index) %>%
  filter(recorded_at != "OS X") %>%
  mutate(trial_index = as.numeric(trial_index)) %>%
  group_by(run_id) %>%
  mutate(max_index = max(trial_index)) %>%
  select(run_id, recorded_at, max_index) %>%
  distinct() %>%
  ggplot(aes(x = recorded_at, y = max_index)) +
  geom_point() +
  coord_flip() +
  theme_minimal()


complete_table <- df %>%
  select(run_id, recorded_at, trial_index) %>%
  filter(recorded_at != "OS X") %>%
  mutate(trial_index = as.numeric(trial_index)) %>%
  group_by(run_id) %>%
  mutate(max_index = max(trial_index)) %>%
  select(run_id, recorded_at, max_index) %>%
  distinct() %>%
  filter(max_index == 64)


df %>%
  arrange(desc(run_id)) %>%
  View()

df %>%
  filter(run_id %in% c(69)) %>%
  View()
