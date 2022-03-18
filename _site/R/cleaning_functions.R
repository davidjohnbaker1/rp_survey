# Clean Survey Data

library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
# run_id / Run ID | qid / Question ID | response / Response |  

# Extract Dempgraphic Table


assemble_demographic_table <- function(df){
  majority_teaching <- extract_majority_teaching(df)
  years_teaching <- extract_years_teaching(df)
  highest_degree_earned <- extract_highest_degree_earned(df)
  years_since_graduating <- extract_years_since_graduating(df)
  describe_institution <- extract_describe_institution(df)
  describe_teaching_role <- extract_describe_teaching_role(df)
  own_rp_sense <- extract_own_rp_sense(df)
  own_ap_sense <- extract_own_ap_sense(df)
  years_aural_taught <- extract_years_aural_taught(df) 
  system_learn_student <- extract_system_learn_student(df)
  system_where_learn_student <- extract_system_where_learn_student(df)
  system_prefer <- extract_system_prefer(df)
  system_current_inst <- extract_system_at_current_inst(df)
  num_students_per_class <- extract_num_students_in_class(df)
  aural_theory_tracking <- extract_aural_theory_tracking(df)
  inst_type <- extract_describe_institution(df)
  school_size_estimate <- extract_school_size_estimate(df)
  inst_degree_offer <- extract_inst_degree_offer(df)
  aural_skill_teacher_rank <- extract_aural_skill_teacher_rank(df)
  aural_skill_teacher_degree <- extract_aural_skill_teacher_degree(df)
  school_country <- extract_school_country(df)
  school_state <- extract_school_state(df)
  feedback <- extract_feedback(df)
  
  rbind(majority_teaching, years_teaching, highest_degree_earned,
        years_since_graduating, describe_institution, describe_teaching_role,
        own_rp_sense, own_ap_sense,
        years_aural_taught, system_learn_student, system_where_learn_student,
        system_prefer, system_current_inst,
        num_students_per_class, aural_theory_tracking,
        inst_type, school_size_estimate, 
        inst_degree_offer, aural_skill_teacher_rank,
        aural_skill_teacher_degree, school_country, school_state, feedback)

  }

#
extract_feedback <- function(df){
  df %>%
    filter(trial_index == 63) %>%
    mutate(t_id = "feedback") %>%
    select(run_id, t_id, response) %>%
    mutate(response = str_sub(response, start = 8, end = -3L))
}
#
extract_school_state <- function(df){
  df %>%
    filter(trial_index == 62) %>%
    mutate(t_id = "school_state") %>%
    select(run_id, t_id, response) %>%
    mutate(response = str_sub(response, start = 8, end = -3L))
}
#
extract_school_country <- function(df){
  df %>%
    filter(trial_index == 61) %>%
    mutate(t_id = "school_country") %>%
    select(run_id, t_id, response) %>%
    mutate(response = str_sub(response, start = 8, end = -3L))
}
#
extract_aural_skill_teacher_degree <- function(df){
  df %>%
    filter(trial_index == 60) %>%
    mutate(t_id = "aural_skill_teacher_degree") %>%
    select(run_id, t_id, response)
}
# 
extract_aural_skill_teacher_rank <- function(df){
  df %>%
    filter(trial_index == 59) %>%
    mutate(t_id = "aural_skill_teacher_rank") %>%
    select(run_id, t_id, response)
  
}
#
extract_inst_degree_offer <- function(df){
  df %>%
    filter(trial_index == 58) %>%
    mutate(t_id = "inst_degree_offer") %>%
    select(run_id, t_id, response)
}
#
extract_school_size_estimate <- function(df){
  df %>%
    filter(trial_index == 57) %>%
    mutate(t_id = "estimated_school_size") %>%
    select(run_id, t_id, response) %>%
    mutate(response = str_sub(response, start = 8, end = -3L))
}

#
extract_inst_type <- function(df){
  df %>%
    filter(trial_index == 56) %>%
    mutate(t_id = "inst_type") %>%
    select(run_id, t_id, response)
}

#
extract_aural_theory_tracking <- function(df){
  df %>%
    filter(trial_index == 55) %>%
    mutate(response = str_remove_all(response, "\\{")) %>%
    mutate(response = str_remove_all(response, "\\}")) %>%
    mutate(response = str_remove_all(response, "\\\"")) %>%
    mutate(aural_theory_tracking_number = seq_along(run_id)) %>%
    mutate(response = str_remove_all(response, pattern = "theory_track_01:")) %>%
    mutate(response = str_remove_all(response, pattern = "pitch_mem_02:")) %>%
    mutate(response = str_remove_all(response, pattern = "score_transp_03:")) %>%
    separate(response, into = c("theory_track","pitch_memory","score_transposition"),sep = ",") %>%
    pivot_longer(cols = theory_track:score_transposition, names_to = "t_id",values_to = "response")  %>%
    select(run_id, t_id, response) 
  }
#
extract_num_students_in_class <- function(df){
  df %>%
    filter(trial_index == 54) %>%
    mutate(t_id = "num_students_per_class") %>%
    select(run_id, t_id, response)
}
#
extract_system_at_current_inst <- function(df){
  df %>%
    filter(trial_index == 53) %>%
    mutate(t_id = "what_system_current_inst") %>%
    select(run_id, t_id, response)
}

#
extract_system_prefer <- function(df){
  df %>%
    filter(trial_index == 52) %>%
    mutate(t_id = "what_system_prefer") %>%
    select(run_id, t_id, response)
}
#
extract_system_where_learn_student <- function(df){
  df %>%
    filter(trial_index == 51) %>%
    mutate(t_id = "where_learn_student") %>%
    select(run_id, t_id, response)
}

#
extract_system_learn_student <- function(df){
  df %>%
    filter(trial_index == 50) %>%
    mutate(t_id = "system_learned_student") %>%
    select(run_id, t_id, response)
}

#
extract_years_aural_taught <- function(df){
  df %>%
    filter(trial_index == 49) %>%
    mutate(t_id = "describe_years_aural_taught") %>%
    select(run_id, t_id, response)
}

#
extract_own_rp_sense <- function(df){
  df %>%
    filter(trial_index == 47) %>%
    mutate(t_id = "describe_own_rp_sense") %>%
    select(run_id, t_id, response)
}

#
extract_own_ap_sense <- function(df){
  df %>%
    filter(trial_index == 46) %>%
    mutate(t_id = "describe_own_ap_sense") %>%
    select(run_id, t_id, response)
}


#
extract_describe_teaching_role <- function(df){
  df %>%
    filter(trial_index == 45) %>%
    mutate(t_id = "describe_teaching_role") %>%
    select(run_id, t_id, response)
}

#
extract_describe_institution <- function(df){
  df %>%
    filter(trial_index == 44) %>%
    mutate(t_id = "describe_institution") %>%
    select(run_id, t_id, response)
}

#
extract_years_since_graduating <- function(df){
  df %>%
    filter(trial_index == 43) %>%
    select(run_id, response) %>%
    mutate(response = str_sub(response, start = 8, end = -3L)) %>%
    mutate(t_id = "years_since_graduating") %>%
    select(run_id, t_id, response)
}

#
extract_highest_degree_earned <- function(df){
  df %>%
    filter(trial_index == 42) %>%
    mutate(t_id = "highest_degree_earned") %>%
    select(run_id, t_id, response)
}

#
extract_years_teaching <- function(df){
  df %>%
    filter(trial_index == 41) %>%
    select(run_id, response) %>%
    mutate(response = str_sub(response, start = 8, end = -3L)) %>%
    mutate(t_id = "years_teaching_music_classroom") %>%
    select(run_id, t_id, response)
}

#
extract_majority_teaching <- function(df){
  df %>%
    filter(trial_index == 40) %>%
    mutate(t_id = "majority_teaching") %>%
    select(run_id, t_id, response)
}


#------------------------------------------------------------------------------
# Text Reponses 


assemble_text_response_table <- function(df){
  personal_rp_def <- extract_personal_rp_def(df)
  personal_ap_def <- extract_personal_ap_def(df)
  rep_list <- extract_rep_list(df)
  intended_rep_list <- extract_intend_rep_list(df)
  no_notation <- extract_no_notation_list(df)
  no_notation_freq <- extract_non_notation_freq(df)
  
  rbind(personal_rp_def, personal_ap_def, rep_list,
        intended_rep_list, no_notation, no_notation_freq)
}


extract_personal_rp_def <- function(df){
  df %>%
    filter(trial_index == 3) %>%
    select(run_id, response) %>%
    mutate(response = str_sub(response, start = 8, end = -3L)) %>%
    mutate(t_id = "personal_rp_def") %>%
    select(run_id, t_id, response)
}

extract_personal_ap_def <- function(df){
  df %>%
    filter(trial_index == 4) %>%
    select(run_id, response) %>%
    mutate(response = str_sub(response, start = 8, end = -3L)) %>%
    mutate(t_id = "personal_ap_def") %>%
    select(run_id, t_id, response)
}

extract_rep_list <- function(df){
    df %>%
      filter(trial_index == 33) %>%
      select(run_id, response) %>%
      mutate(response = str_sub(response, start = 8, end = -3L)) %>%
    mutate(t_id = "rep_list_free_response") %>%
    select(run_id, t_id, response)
}


extract_intend_rep_list <- function(df){
    df %>%
      filter(trial_index == 35) %>%
      select(run_id, response) %>%
    mutate(response = str_sub(response, start = 8, end = -3L)) %>%
    mutate(t_id = "rep_list_intend_response") %>%
    select(run_id, t_id, response)
}


extract_no_notation_list <- function(df){
  df %>%
    filter(trial_index == 36) %>%
    select(run_id, response) %>%
    mutate(response = str_sub(response, start = 8, end = -3L)) %>%
    mutate(t_id = "no_notation_list") %>%
    select(run_id, t_id, response)
}


extract_rep_freq <- function(df){
  df %>%
    filter(trial_index == 34) %>%
    select(run_id, response) %>%
    mutate(t_id = "western_rep_frequency") %>%
    select(run_id, t_id, response)
}


extract_non_notation_freq <- function(df){
  df %>%
    filter(trial_index == 37) %>%
    select(run_id, response) %>%
    mutate(t_id = "non_notation_frequency") %>%
    select(run_id, t_id, response)
}

#=========================================================================================


# Function to Extract out All five Questions + Ratings for AP and RP 

extract_rp_text_response <- function(df){
  df %>%
    filter(trial_index %in% c(12,14,16,18,20)) %>%
    select(run_id, response) %>%
    mutate(t_id = seq_along(run_id)) %>%
    mutate(t_id = paste("RP Activity",t_id)) %>%
    mutate(response = str_remove_all(response, "\\{")) %>%
    select(run_id, t_id, response)
}


extract_ap_text_response <- function(df){
  df %>%
    filter(trial_index %in% c(23,25,27,29,31)) %>%
    select(run_id, response) %>%
    mutate(t_id = seq_along(run_id)) %>%
    mutate(t_id = paste("AP Activity",t_id)) %>%
    mutate(response = str_remove_all(response, "\\{")) %>%
    select(run_id, t_id, response)
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
    mutate(response = as.numeric(response) + 1) %>%
    mutate(t_id = paste("RP Activity",rp_activity_response_number,questions)) %>%
    select(run_id, t_id, response)
}


extract_ap_activity_ratings <- function(df){
  df %>%
    filter(trial_index %in% c(24,26,28,30,32)) %>%
    select(run_id, response) %>%
    mutate(response = str_remove_all(response, "\\{")) %>%
    mutate(response = str_remove_all(response, "\\}")) %>%
    mutate(response = str_remove_all(response, "\\\"")) %>%
    mutate(ap_activity_response_number = seq_along(run_id)) %>%
    mutate(response = str_remove_all(response, pattern = "teach_freq_ap_01:")) %>%
    mutate(response = str_remove_all(response, pattern = "easy_teach_02:")) %>%
    mutate(response = str_remove_all(response, pattern = "eash_learn_03:")) %>%
    mutate(response = str_remove_all(response, pattern = "foster_ap_04:")) %>%
    mutate(response = str_remove_all(response, pattern = "represent_ap_05:")) %>%
    separate(response, into = c("use_skill_ap","easy_teach_ap","easy_learn_ap","helps_foster_ap","reps_absolute_ap"),sep = ",") %>%
    pivot_longer(cols = use_skill_ap:reps_absolute_ap, names_to = "questions",values_to = "response") %>%
    mutate(response = as.numeric(response) + 1) %>%
    mutate(t_id = paste("AP Activity",ap_activity_response_number,questions)) %>%
    select(run_id, t_id, response)

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
    mutate(t_id = id) %>%
    rename(response = answer) %>%
    select(run_id, t_id, response)
}


extract_perc_class_time_ap <- function(df){
  df %>%
    filter(trial_index == 10) %>%
    select(run_id, response) %>%
    mutate(t_id = "perc_class_time_ap") %>%
    select(run_id, t_id, response)
}


extract_perc_class_time_rp <- function(df){
  df %>%
    filter(trial_index == 9) %>%
    select(run_id, response) %>%
    mutate(t_id = "perc_class_time_rp") %>%
    select(run_id, t_id, response)
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
    mutate(answer = as.numeric(answer) + 1) %>%
    rename(t_id = id, response = answer) %>%
    select(run_id, t_id, response)
}


# Questions About Definitions 
# TODO: Check Drop 
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
                                "q25",
                                "q26",
                                "q27"
                                ) ,
             sep = ",") %>%
    pivot_longer(cols = q0:q27, names_to = "order", values_to = "response") %>%
    separate(response, into = c("id","answer"), sep = ":") %>%
    mutate(answer = as.numeric(answer) + 1) %>%
    rename(t_id = id, response = answer) %>%
    select(run_id, t_id, response)
}


