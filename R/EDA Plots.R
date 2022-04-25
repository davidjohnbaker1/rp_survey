#----------------------------------------------------------------------
# EDA Plot | Poster Plots 
#----------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(ggridges)
library(stringr)
library(tidyr)
library(viridis)
#----------------------------------------------------------------------
# Import Data 

activity_table <- read_csv("data/cleaned_tables/activity_table.csv")
text_response_table <- read_csv("data/cleaned_tables/text_response_table.csv")
demographic_table <- read_csv("data/cleaned_tables/demographic_table.csv")

demo_key <- read_csv("data/demokey.csv")

def_table <- create_definition_table(df)

demo_key <- demo_key %>%
  rename(t_id = Key)

def_table_2 <- def_table %>%
  left_join(demo_key)


#------------------------------------------------------------------------------
# Agreement Plot 

agreement_plot <- def_table_2 %>%
  filter(response != 8) %>%
  filter(Question != is.na(Question)) %>%
  mutate(APRP = case_when(
    APRP == "ap" ~ "Absolute Pitch",
    APRP == "rp" ~ "Relative Pitch",
  )) %>%
  ggplot(aes(y = reorder(Question, response), x = response, fill = APRP)) +
  geom_density_ridges2() +
  scale_x_continuous(limits = c(0,7), breaks = seq(1,7,1)) +
  labs(title = "Agreeing on Definitions of \nAbsolute and Relative Pitch",
       subtitle = paste("N = ", n_total_respondents),
       y = "Question",
       x = "Density",
       fill = "Question Bank") +
  theme_bw() +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .8)
   
agreement_plot

ggsave(filename = "img/fig/agreement_plot.png", width = 10, height = 10, dpi = 300)


#----------------------------------------------------------------------------------------

aprp_time_est_plot <- activity_table %>%
  filter(t_id %in% c("perc_class_time_ap", "perc_class_time_rp")) %>%
  mutate(response = as.numeric(response)) %>%
  mutate(t_id = case_when(
    t_id == "perc_class_time_ap" ~ "Absolute Pitch",
    t_id == "perc_class_time_rp" ~ "Relative Pitch"
  )) %>%
  ggplot(aes(response, fill = t_id)) +
  geom_histogram(bins = 40) +
  facet_wrap(~t_id, nrow = 2) +
  theme_bw() +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .8) +
  labs(title = "Classroom Time Devoted to \nAbsolute Pitch vs Relative Pitch in the Classroom",
       x = "Estimated Percentage of Time",
       y = "Frequency Count in Survey",
       fill = "Question Bank")

aprp_time_est_plot

ggsave(filename = "img/fig/aprp_time_est_plot.png", width = 5, height = 5, dpi = 300)

#---------------------------------------------------------------------------------------
# People Plot 

people_table <- create_people_table(df)
people_key <- read_csv("PeopleTable.csv")

people_key <- people_key %>%
  rename(t_id = key) %>%
  mutate(t_id = str_remove_all(t_id, "\'")) %>%
  mutate(t_id = str_remove_all(t_id, "\\_[0-9]{1,2}"))

people_plot <- people_table %>%
  left_join(people_key) %>%
  filter(response != 8) %>%
  filter(Question != is.na(Question)) %>%
  mutate(APRP = case_when(
    APRP == "ap" ~ "Absolute Pitch",
    APRP == "rp" ~ "Relative Pitch"
  )) %>%
  ggplot(aes(y = reorder(Question, response), x = response, fill = APRP)) +
  geom_density_ridges2() +
  scale_x_continuous(limits = c(0,7), breaks = seq(1,7,1)) +
  labs(title = "Agreeing on Definitions of People\n with Absolute and Relative Pitch",
       subtitle = "N = 42",
       y = "Question",
       x = "Density",
       fill = "Question Bank") +
  theme_bw() +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .8)

people_plot

ggsave(filename = "img/fig/people_plot.png", width = 10, height = 10, dpi = 300)

#---------------------------------------------------------------------------------------
# Activity Data Extraction



  
#---------------------------------------------------------------------------------------
# Activity Data Extraction
main_responses <- activity_table %>%
  mutate(a.key = str_detect(string = t_id, pattern = "Activity")) %>%
  mutate(t.key = str_detect(string = response, pattern = "Q0")) %>% 
  filter(a.key == TRUE & t.key == FALSE) %>%
  select(-t.key, -a.key) %>%
  separate(col = t_id, into = c("P","Activity", "int_q_number", "Label"), sep = " ") %>%
  mutate(run_id = as.character(run_id)) %>%
  mutate(k = paste(run_id,P, Activity, int_q_number))

left_responses <- activity_table %>%
  mutate(t.key = str_detect(string = response, pattern = "Q0")) %>%
  filter(t.key == TRUE) %>%
  mutate(response = str_remove_all(response, '[[:punct:]]')) %>%
  mutate(response = str_remove_all(response, "Q0")) %>%
  select(-t.key) %>%
  mutate(run_id = as.character(run_id)) %>%
  mutate(k = paste(run_id, t_id)) %>%
  select(k, response) %>%
  mutate(text = response) %>%
  select(-response)

left_responses

temp_response_data_with_text <- main_responses %>%
  left_join(left_responses) %>%
  filter(!is.na(text)) %>%
  mutate(detector = nchar(text)) %>%
  filter(detector > 5) 

temp_response_data_with_text

write_csv(temp_response_data_with_text, file = "data/cleaned_tables/temp_text_response_data.csv")


#----------------------------------------------------------------------
df <- read_csv("data/aural-skills-survey-2010-working-20211122.csv")

df %>%
  arrange(desc(recorded_at)) %>%
  View()

# Survey Launched 2021-11-21 @ 4:00 London Time 
df <- df %>%
  filter(recorded_at > "2021-11-22")
