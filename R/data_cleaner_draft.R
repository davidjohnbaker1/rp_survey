# Clean Survey Data

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


# Extract Dempgraphic Table

# 40 - Demo Begins 

extract_rep_list <- function(df){
    df %>%
      filter(trial_index == 33) %>%
      select(run_id, response)
}

extract_intend_rep_list <- function(df){
    df %>%
      filter(trial_index == 35) %>%
      select(run_id, response)
}

extract_no_notation_list <- function(df){
    df %>%
      filter(trial_index == 36) %>%
      select(run_id, response)
}

extract_rep_freq <- function(df){
  df %>%
    filter(trial_index == 34) %>%
    select(run_id, response)
}

extract_non_notation_freq <- function(df){
  df %>%
    filter(trial_index == 37) %>%
    select(run_id, response)
}

# 35 -- text  
# 36 -- text


# Function to Extract out All five Questions + Ratings for AP and RP 

extract_rp_text_response <- function(df){
  df %>%
    filter(trial_index %in% c(12,14,16,18,20)) %>%
    select(run_id, response) %>%
    mutate(rp_activity_response_number = seq_along(run_id)) %>%
    mutate(response = str_remove_all(response, "\\{"))
}


extract_ap_text_response <- function(df){
  df %>%
    filter(trial_index %in% c(23,25,27,29,31)) %>%
    select(run_id, response) %>%
    mutate(ap_activity_response_number = seq_along(run_id)) %>%
    mutate(response = str_remove_all(response, "\\{"))
}




extract_rp_activity_ratings <- function(df){
  df %>%
    filter(trial_index %in% c(13,15,17,19,21)) %>%
    select(run_id, response) %>%    
    mutate(response = str_remove_all(response, "\\{")) %>%
    mutate(response = str_remove_all(response, "\\}")) %>%
    mutate(response = str_remove_all(response, "\\\"")) %>%
    mutate(rp_activity_response_number = seq_along(run_id)) %>%
    mutate(response = str_remove_all(response, pattern = "use_skill_freq_01:")) %>%
    mutate(response = str_remove_all(response, pattern = "easy_teach_02:")) %>%
    mutate(response = str_remove_all(response, pattern = "easy_to_learn_03:")) %>%
    mutate(response = str_remove_all(response, pattern = "helps_relative_04:")) %>%
    mutate(response = str_remove_all(response, pattern = "reps_relative_05:")) %>%
    separate(response, into = c("use_skill_rp","easy_teach_rp","easy_learn_rp","helps_foster_rp","reps_relative_rp"),sep = ",") %>%
    pivot_longer(cols = use_skill_rp:reps_relative_rp, names_to = "questions",values_to = "response") %>%
    mutate(response = as.numeric(response) + 1)
}


extract_ap_activity_ratings <- function(df){
  df %>%
    filter(trial_index %in% c(24,26,28,30,32)) %>%
    select(run_id, response) %>%
    mutate(response = str_remove_all(response, "\\{")) %>%
    mutate(response = str_remove_all(response, "\\}")) %>%
    mutate(response = str_remove_all(response, "\\\"")) %>%
    mutate(ap_activity_respone_number = seq_along(run_id)) %>%
    mutate(response = str_remove_all(response, pattern = "teach_freq_ap_01:")) %>%
    mutate(response = str_remove_all(response, pattern = "easy_teach_02:")) %>%
    mutate(response = str_remove_all(response, pattern = "eash_learn_03:")) %>%
    mutate(response = str_remove_all(response, pattern = "foster_ap_04:")) %>%
    mutate(response = str_remove_all(response, pattern = "represent_ap_05:")) %>%
    separate(response, into = c("use_skill_ap","easy_teach_ap","easy_learn_ap","helps_foster_ap","reps_absolute_ap"),sep = ",") %>%
    pivot_longer(cols = use_skill_ap:reps_absolute_ap, names_to = "questions",values_to = "response") %>%
    mutate(response = as.numeric(response) + 1)

}



extract_rpap_priority_table <- function(df){
  df %>%
    filter(trial_index == 11 ) %>%
    select(run_id, response) %>%
    mutate(response = str_remove_all(response, "\\{")) %>%
    mutate(response = str_remove_all(response, "\\}")) %>%
    mutate(response = str_remove_all(response, "\\\"")) %>% 
    separate(response, into = c("q0",
                                "q1",
                                "q2",
                                "q3"),
             sep = ",")  %>%
    pivot_longer(cols = q0:q3, names_to = "order", values_to = "response") %>%
    separate(response, into = c("id","answer"), sep = ":") %>%
    mutate(answer = as.numeric(answer) + 1) %>%
    mutate(t_id = "rpap_priority")
}




df %>%
  filter(trial_index == 10) %>%
  select(run_id, response) %>%
  mutate(t_id = "perc_class_time_ap")

df %>%
  filter(trial_index == 9) %>%
  select(run_id, response) %>%
  mutate(t_id = "perc_class_time_rp")


# Clean JSON

clean_survey_text_json <- function(df){
  df %>%    
    mutate(response = str_remove_all(response, "\\{")) %>%
    mutate(response = str_remove_all(response, "\\}")) %>%
    mutate(response = str_remove_all(response, "\\\"")) %>% 
    separate(response, into = c("q0",
                                "q1",
                                "q2",
                                "q3"),
             sep = ",")  %>%
    pivot_longer(cols = q0:q3, names_to = "order", values_to = "response") %>%
    separate(response, into = c("id","answer"), sep = ":") %>%
    mutate(answer = as.numeric(answer) + 1)
}

# Questions About People
create_people_table <- function(df){
  df %>%
    filter(trial_index == 6) %>%
    select(run_id, response) %>%
    mutate(response = str_remove_all(response, "\\{")) %>%
    mutate(response = str_remove_all(response, "\\}")) %>%
    mutate(response = str_remove_all(response, "\\\"")) %>% 
    separate(response, into = c("q0",
                                "q1",
                                "q2",
                                "q3",
                                "q4",
                                "q5",
                                "q6",
                                "q7",
                                "q8",
                                "q9",
                                "q10",
                                "q11",
                                "q12",
                                "q13") ,
             sep = ",") %>%
    pivot_longer(cols = q0:q13, names_to = "order", values_to = "response") %>%
    separate(response, into = c("id","answer"), sep = ":") %>%
    mutate(answer = as.numeric(answer) + 1) 
}


# Questions About Definitions 

create_definition_table <- function(df){
  df %>%
    filter(trial_index == 2) %>%
    select(run_id, response) %>%
    mutate(response = str_remove_all(response, "\\{")) %>%
    mutate(response = str_remove_all(response, "\\}")) %>%
    mutate(response = str_remove_all(response, "\\\"")) %>% 
    separate(response, into = c("q0",
                                "q1",
                                "q2",
                                "q3",
                                "q4",
                                "q5",
                                "q6",
                                "q7",
                                "q8",
                                "q9",
                                "q10",
                                "q11",
                                "q12",
                                "q13",
                                "q14",
                                "q15",
                                "q16",
                                "q17",
                                "q18",
                                "q19",
                                "q20",
                                "q21",
                                "q22",
                                "q23",
                                "q24",
                                "q25") ,
             sep = ",") %>%
    pivot_longer(cols = q0:q25, names_to = "order", values_to = "response") %>%
    separate(response, into = c("id","answer"), sep = ":") %>%
    mutate(answer = as.numeric(answer) + 1)
}


create_definition_table()
create_people_table()
extract_ap_activity_ratings(df)
extract_rp_activity_ratings(df)
extract_ap_text_response(df)
extract_rp_text_response(df)
extract_rep_list(df)
extract_rep_list(df)
extract_intend_rep_list(df) 
extract_no_notation_list(df)
extract_rep_freq(df)
extract_non_notation_freq(df)