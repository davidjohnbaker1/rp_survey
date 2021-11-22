# EDA Plot 

library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)

df <- read_csv("data/aural-skills-survey-2010-working-20211122.csv")

df %>%
  arrange(desc(recorded_at)) %>%
  View()

# Survey Launched 2021-11-21 @ 4:00 London Time 
df <- df %>%
  filter(recorded_at > "2021-11-22")
