# Assemble Tables 


assemble_activity <- function(df){
  perc_class_time_rp <- extract_perc_class_time_rp(df)
  perc_class_time_ap <- extract_perc_class_time_ap(df)
  rpap_priority <- extract_rpap_priority_table(df)
  ap_activity_ratings <- extract_ap_activity_ratings(df)
  rp_activity_ratings <- extract_rp_activity_ratings(df)
  rp_text_response <- extract_rp_text_response(df)
  ap_text_response <- extract_ap_text_response(df)
  
  rbind(perc_class_time_ap, perc_class_time_rp, rpap_priority, ap_activity_ratings, rp_activity_ratings,
        rp_text_response, ap_text_response)
}

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
