# Assemble Tables for Analysis 

library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)

source("R/cleaning_functions.R")
source("R/assemble_functions.R")

df <- read_csv("data/2022-03-16-data-complete.csv")

# Survey Launched 2021-11-21 @ 4:00 London Time 
df <- df %>%
  filter(recorded_at > "2021-11-22")


activity_table <- assemble_activity(df)
text_response_table <- assemble_text_response_table(df)
demographic_table <- assemble_demographic_table(df)

write_csv(activity_table, file = "data/cleaned_tables/activity_table.csv")
write_csv(text_response_table, file = "data/cleaned_tables/text_response_table.csv")
write_csv(demographic_table, file = "data/cleaned_tables/demographic_table.csv")

