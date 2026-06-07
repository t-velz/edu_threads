# ---------------------------------------------------------#
#   edu 2026 proj
#   Ted, Will, Tess, Ayesha
#   create bipartite dataset
# ---------------------------------------------------------#

#          _.-^~~^^^`~-,_,,~''''''```~,''``~'``~,
#  ______,'  -o  :.  _    .          ;     ,'`,  `.
# (      -\.._,.;;'._ ,(   }        _`_-_,,    `, `,
#  ``~~~~~~'   ((/'((((____/~~~~~~'(,(,___>      `~'
# ---------------------------------------------------------#
rm(list = ls())

library(tidyverse)
library(lubridate)

# install.packages(c("ggnewscale", "patchwork"))
library(ggnewscale)
library(patchwork)

# setwd("C:/Users/welser/OneDrive - Ohio University/_R_proj_1drive/edu_2027")
# ---------------------------

# ----------------------------------------
# STEP 1: Load and clean data
# ----------------------------------------

edd_raw <- read_csv("edd15subs_indices.csv")

edd_raw <- edd_raw %>%
  mutate(
    # Handle mixed date formats (MDY, DMY, YMD)
    t_date = as.Date(parse_date_time(t_date, orders = c("mdy", "dmy", "ymd"))),

    # Subreddit group flags
    group1 = t_subreddit %in% c("Teachers", "Professors", "college"),
    group2 = t_subreddit %in% c("GradSchool", "academia", "AskAcademia")
  ) %>%
  filter(group1 | group2) %>%
  arrange(t_date)

edd_raw <- edd_raw %>%
  mutate(t_name = str_sub(t_title, 1, 40))

cat("Rows after filtering to 6 subreddits:", nrow(edd_raw), "\n")



# ----------------------------------------
# STEP 2: Split into two collection periods
# ----------------------------------------

SPLIT_DATE <- as.Date("2025-01-01")

edd_p1 <- edd_raw %>% filter(t_date < SPLIT_DATE)
edd_p2 <- edd_raw %>% filter(t_date >= SPLIT_DATE)

cat("Period 1 rows:", nrow(edd_p1), "\n")
cat("Period 2 rows:", nrow(edd_p2), "\n")


# ----------------------------------------
# STEP 3: Sample 430 per subreddit per period
# ----------------------------------------

set.seed(42)

sample_n_per_sub <- function(df, n = 430) {
  df %>%
    group_by(t_subreddit) %>%
    slice_sample(n = n, replace = FALSE) %>%
    ungroup()
}

edd_p1_samp <- sample_n_per_sub(edd_p1, 430)
edd_p2_samp <- sample_n_per_sub(edd_p2, 430)

# Combine periods back together for plotting
edd <- bind_rows(edd_p1_samp, edd_p2_samp) %>%
  arrange(t_date)

cat("Combined sample rows:", nrow(edd), "\n")

#     create time period variable

edd <- edd %>%
  mutate(time_period = case_when(
    t_date < as.Date("2025-01-01") ~ "T1 23/24",
    t_date > as.Date("2025-01-01") ~ "T2 25/26",
    TRUE ~ NA_character_
  ))
table(edd$time_period)

# ============================================================
# BIPARTITE NETWORK DATA SHAPING
# Nodes: threads & index variables | Edges: non-zero iln values
# ============================================================


library(data.table)

fwrite(edd, "sampled_edd.csv")

sedd<- fread("sampled_edd.csv")

names(sedd)    #notice t_name and time_period at end of names list

# ------------------------------------------------------------
# STEP 0: DEFINE LIST OF VARS TO DROP, MAKE SMALLER DATASET
# ------------------------------------------------------------

vars_to_drop<- c("screen_addict_rt",
"phone_addict_rt",
"digital_addict_rt",
"ipad_kid_rt",
"ipad_baby_rt",
"ipad_babies_rt",
"screenager_rt",
"device_addict_rt",
"screen_obsession_rt",
"screen_time_rt",
"chrome_book_rt",
"chromebook_rt",
"enjoy_the_process_rt",
"passion_for_teach_rt",
"intrinsic_motiv_rt",
"extrinsic_motiv_rt",
"take_ownership_rt",
"inquisitive_rt",
"curiosity_rt",
"passion_for_learning_rt",
"walk_the_halls_rt",
"skip_class_rt",
"plagiarism_rt",
"cheat_rt",
"plagiarize_rt",
"turnitin_rt",
"lock_down_browser_rt",
"low_attendance_rt",
"is_class_mandatory_rt",
"skipping_class_rt",
"is_attendance_rt",
"chat_gpt_to_write_rt",
"ai_to_write_rt",
"artificial_intelligence_rt",
"robot_rt",
"chat_gpt_rt",
"llm_rt",
"claude_rt",
"gemini_rt",
"chat_gpt_2_rt",
"grok_rt",
"anthropic_rt",
"depress_rt",
"sad_rt",
"stress_rt",
"anxious_rt",
"anxiety_rt",
"mental_health_rt",
"generation_z_rt",
"no_motivation_rt",
"not_motivated_rt",
"too_exhausted_rt",
"i_am_done_rt",
"i_cant_anymore_rt",
"burned_out_rt",
"burn_out_rt",
"i_quit_rt",
"considering_leaving_this_profession_rt",
"love_teaching_but_rt",
"why_am_i_even_here_rt",
"like_i_cant_win_rt",
"wasting_my_time_rt",
"why_do_i_even_try_rt",
"i_really_think_rt",
"i_really_believe_rt",
"i_believe_rt",
"i_do_believe_rt",
"i_feel_rt",
"i_know_rt",
"i_just_think_rt",
"i_think_rt",
"i_thought_rt",
"i_agree_rt",
"appreciate_rt",
"grateful_rt",
"thank_you_rt",
"thanks_rt",
"they_believe_rt",
"they_claim_rt",
"they_say_rt",
"they_think_rt",
"they_want_rt",
"they_dont_believe_rt",
"they_dont_think_rt",
"deleted_rt",
"gen_z_rt",
"zoomer_rt",
"gen_alpha_rt",
"nt_talk_rt",
"not_talk_rt",
"nt_answer_rt",
"not_answer_rt",
"nt_respond_rt",
"not_respond_rt",
"nt_say_rt",
"not_say_rt",
"nt_read_rt",
"not_read_rt",
"nt_multiply_rt",
"not_multiply_rt",
"nt_divide_rt",
"not_divide_rt",
"nt_draw_rt",
"not_draw_rt",
"nt_do_rt",
"not_do_rt",
"nt_know_rt",
"not_know_rt",
"nt_understand_rt",
"not_understand_rt",
"nt_digest_rt",
"not_digest_rt",
"nt_name_rt",
"not_name_rt",
"struggle_to_rt",
"struggle_with_rt",
"lack_profic_rt",
"lack_basic_rt",
"lack_fundament_rt",
"in_diapers_rt",
"potty_train_rt",
"never_develop_rt",
"stick_figure_rt",
"who_couldnt_properly_rt",
"nt_even_add_rt",
"no_idea_how_to_rt",
"at_grade_level_rt",
"below_grade_level_rt",
"nt_care_about_school_rt",
"parents_dont_care_rt",
"parents_dont_help_rt",
"parents_have_no_rt",
"parents_arent_rt",
"parents_do_not_rt",
"want_to_parent_rt",
"wants_to_parent_rt",
"nt_listen_rt",
"not_listen_rt",
"nt_pay_attention_rt",
"not_pay_attention_rt",
"nt_focus_rt",
"not_focus_rt",
"nt_engage_rt",
"not_engage_rt",
"no_engage_rt",
"tuned_out_rt",
"checked_out_rt",
"not_interested_rt",
"no_interest_rt",
"zone_out_rt",
"bored_in_class_rt",
"nt_motiva_rt",
"not_motiva_rt",
"nt_work_rt",
"not_work_rt",
"nt_try_rt",
"not_try_rt",
"nt_keep_rt",
"not_keep_rt",
"nt_apply_rt",
"not_apply_rt",
"refuse_to_rt",
"no_effort_rt",
"skate_by_rt",
"skating_by_rt",
"passive_rt",
"nt_partic_rt",
"not_partic_rt",
"just_sit_there_rt",
"never_work_rt",
"never_partic_rt",
"nt_do_anything_rt",
"do_nothing_rt",
"blank_stare_rt",
"they_just_stare_rt",
"nt_ask_question_rt",
"crickets_rt",
"t_even_listen_rt",
"sleeping_in_class_rt",
"barely_participate_rt",
"not_following_discussion_rt",
"see_me_as_a_movie_rt",
"no_question_rt",
"roll_eyes_rt",
"nt_react_rt",
"not_even_looking_rt",
"deads_down_rt",
"on_their_phone_rt",
"distracted_rt",
"playing_games_rt",
"wearing_headphones_rt",
"listening_to_headphones_rt",
"texts_rt",
"nt_turn_camera_on_rt",
"nt_turn_on_mic_rt",
"nt_type_in_chat_rt",
"no_one_responds_rt",
"muted_rt",
"talking_to_myself_rt",
"why_does_it_matter_rt",
"whatever_rt",
"blow_off_rt",
"shrug_off_rt",
"act_out_rt",
"acting_out_rt",
"interupt_rt",
"student_abruptly_state_rt",
"dismissive_remark_rt",
"doesnt_matter_rt",
"ignore_schedule_rt",
"want_preference_rt",
"should_accommodate_rt",
"demanding_rt",
"nt_try_before_rt",
"to_try_rt",
"expecting_to_be_accommodated_rt",
"vacations_during_the_semester_rt",
"zero_work_ethic_rt",
"no_work_ethic_rt",
"motivation_is_dead_rt",
"breezed_through_highschool_rt",
"concerned_with_points_rt",
"resistance_to_knowing_rt",
"resist_learning_rt",
"cant_be_bother_rt",
"teased_rt",
"be_made_fun_of_rt",
"worried_about_others_rt",
"afraid_to_try_rt",
"immediate_perfection_rt",
"they_shut_down_rt",
"cope_with_failure_rt",
"not_willing_to_try_rt",
"emotionally_safe_rt",
"lack_of_effort_rt",
"not_wanting_to_rt",
"nt_practice_rt",
"not_practice_rt",
"afraid_to_fail_rt",
"fear_of_fail_rt",
"fear_of_try_rt",
"insecur_rt",
"worse_than_last_year_rt",
"is_it_just_my_school_rt",
"dumbed_down_rt",
"many_students_didnt_understand_rt",
"not_just_a_few_students_rt",
"no_in_between_rt",
"lower_half_has_plummeted_rt",
"say_they_never_learned_rt",
"increasing_proportion_of_students_rt",
"never_did_before_rt",
"because_our_admin_rt",
"zero_consequences_rt",
"fail_them_rt",
"no_consequences_rt",
"failing_rt",
"passed_along_rt",
"accountable_for_their_failure_rt",
"they_know_they_arent_rt",
"not_fail_rt",
"nt_fail_rt",
"lack_of_consequences_rt",
"without_consequences_rt",
"consumer_model_rt",
"pleasing_the_customer_rt",
"please_the_customer_rt",
"customer_is_always_right_rt",
"consequences_are_gone_rt",
"push_for_fewer_suspensions_rt",
"admin_actively_discourage_rt",
"disregard_for_the_teacher_rt",
"administration_is_scared_rt",
"no_discipline_rt",
"public_babysitting_rt",
"inmates_run_the_asylum_rt",
"accountability_for_admin_rt",
"responsibility_but_no_power_rt",
"admin_ignores_rt",
"admin_wont_rt",
"admin_dont_rt",
"impossible_to_fail_rt",
"entitled_rt",
"parent_wants_rt",
"nt_hold_their_child_accountable_rt",
"helicopter_parent_rt",
"reply_from_her_parent_rt",
"her_childs_doing_rt",
"unfair_that_her_child_rt",
"both_parents_rt",
"with_her_mother_rt",
"with_his_mother_rt",
"email_from_the_students_rt",
"parents_along_with_them_rt",
"parental_intervention_rt",
"bring_their_parent_rt",
"no_motion_rt",
"nonchalant_rt",
"nonchalance_rt",
"apathy_rt",
"apathetic_rt",
"nt_stand_out_rt",
"nt_want_to_stand_out_rt",
"face_in_the_crowd_rt",
"no_passion_rt",
"too_cool_to_care_rt",
"too_tough_to_try_rt",
"too_tuff_to_try_rt",
"no_personality_rt",
"nt_lose_aura_rt",
"no_aura_rt",
"nt_start_a_conversation_rt",
"media_pressures_rt",
"nt_care_rt",
"no_mistakes_rt",
"nt_make_a_mistake_rt",
"nt_make_mistakes_rt",
"fear_mistakes_rt",
"nt_mess_up_rt",
"averse_to_taking_risks_rt",
"perfect_self_image_rt",
"chase_approval_rt",
"seek_approval_rt",
"chase_praise_rt",
"seek_praise_rt",
"fear_judgement_rt",
"afraid_of_judgement_rt",
"nt_want_to_be_judged_rt",
"auraless_rt",
"never_stops_hustling_rt",
"so_passionate_rt",
"i_care_deeply_rt",
"if_your_team_isnt_rt",
"grind_culture_rt",
"bosses_to_exploit_rt",
"this_isnt_satire_rt",
"theyre_locked_in_rt",
"hey_are_paid_rt",
"my_priorities_rt",
"up_by_the_bootstraps_rt",
"poser_rt",
"of_course_rt",
"the_type_of_guy_rt",
"trying_to_sell_you_rt",
"posting_thirst_traps_rt",
"performative_and_cringe_rt",
"tenk_month_rt",
"niche_a_good_product_rt",
"listing_optimization_rt",
"partner_with_rt",
"affiliates_rt",
"influencer_rt",
"cash_rides_rt",
"cant_be_verified_rt",
"safety_reasons_rt",
"fraudulent_activity_rt",
"honest_drivers_rt",
"undesirable_drivers_rt",
"scammy_rt",
"uber_doesnt_give_rt",
"to_scam_you_rt",
"lyft_driver_rt",
"uber_support_rt",
"persons_feedback_rt",
"up_the_username_rt",
"troll_rt",
"profile_shows_rt",
"rude_rt",
"abusive_rt",
"block_them_rt",
"bully_other_sellers_rt",
"off_the_platform_rt",
"the_platform_rt",
"report_them_rt",
"report_this_rt",
"d_cancel_rt",
"how_an_item_rt",
"signature_confirmation_rt",
"cancel_and_block_rt",
"red_flag_rt",
"trust_your_gut_rt",
"the_entitlement_rt",
"not_your_fault_rt",
"honest_feedback_rt",
"leave_a_neg_rt",
"just_block_rt",
"block_and_report_rt",
"threatening_legal_rt",
"worst_buyers_rt",
"negative_feedback_rt",
"move_on_rt",
"datamining_app_rt",
"laundry_machine_rt",
"crazy_expensive_rt",
"i_deleted_my_account_rt",
"demanding_updates_rt",
"smart_t_vs_rt",
"healthcare_system_rt",
"not_repairable_rt",
"your_only_option_rt",
"create_a_user_account_rt",
"promote_sponsored_result_rt",
"ai_crap_rt",
"that_garbage_rt",
"sponsored_ones_rt",
"are_worthless_rt",
"forced_to_pay_rt",
"youtube_ads_rt",
"button_unclickable_rt",
"multiple_ads_rt",
"dont_bother_watching_rt",
"appliances_that_die_rt",
"subscribe_to_use_rt",
"later_removed_rt",
"battery_life_rt",
"requirement_to_link_rt",
"required_to_link_rt",
"must_link_to_rt",
"data_will_be_sold_rt",
"users_bleed_away_rt",
"more_ads_than_content_rt",
"minutes_of_ads_rt",
"site_got_polluted_rt",
"real_names_policy_rt",
"prove_it_before_rt",
"your_real_name_rt",
"legal_name_rt",
"cess_pool_rt",
"apps_being_required_rt",
"app_required_rt",
"over_the_air_updates_rt",
"subscription_package_rt",
"impossible_to_repair_rt",
"engagement_farming_rt",
"bot_post_rt",
"spam_rt",
"repost_rt",
"cheaper_and_cheaper_rt",
"bought_out_by_rt",
"investment_firm_rt",
"equity_firm_rt",
"sucked_dry_rt",
"hollowed_out_rt",
"lower_quality_rt",
"hours_cut_rt",
"cutting_corners_rt",
"service_breakdown_rt",
"service_recovery_rt",
"making_money_rt",
"showing_my_face_rt",
"make_money_rt",
"nt_show_rt",
"not_show_rt",
"ai_rt",
"ex_rt",
"education","GenZ", "GenAlpha", "GenX", "millenials", 
"highereducation", "Parenting", "socialskills", "teenagers",
"edu_related")

sedd <-  edd %>%
  select(-all_of(vars_to_drop))

# ------------------------------------------------------------
# STEP 0: split data into two time periods
# ------------------------------------------------------------   

table(sedd$time_period)

sedd1 <- sedd %>%
  filter(time_period == "T1 23/24")

sedd2 <- sedd %>%
  filter(time_period == "T2 25/26")

# Define the list of datasets to be removed

datasets_to_remove <- c( "edd", "edd_p1",
                        "edd_p1_samp",
                        "edd_p2",
                        "edd_p2_samp",
                        "edd_raw")

# Execute the removal
rm(list = datasets_to_remove)




library(tidygraph)
library(ggraph)
library(tidyverse)

# ------------------------------------------------------------
# STEP 1: DEFINE YOUR INDEX VARIABLES
# ------------------------------------------------------------

index_vars<- c("admin_prob_iln",
"ai_iln",
"ai_to_write_iln",
"bad_behavior_iln",
"bad_parent_iln",
"burned_out_iln",
"change_is_real_iln",
"cheat_iln",
"consumer_model_iln",
"dont_attend_iln",
"extrinsic_iln",
"fall_behind_basics_iln",
"falling_behind_iln",
"helicopter_parents_iln",
"intrinsic_learning_iln",
"ipad_baby_iln",
"mental_health_iln",
"mention_genz_a_iln",
"no_conseq_iln",
"nonchalance_iln",
"not_engaged_iln",
"phone_distraction_iln",
"verbal_disengagement_iln",
"vis_distraction_iln",
"visible_disen_iln",
"zoom_disengaged_iln",
"digital_media_iln",
"feed_socials_iln",
# "i_think_iln",
 "thanks_iln",
#"they_think_iln",
"gesellschaft_iln")

# ============================================================
# INDEX NODE ATTRIBUTES — EXPANDED
# mean_iln (all), mean_iln_nonzero (conditional),
# % non-zero, raw counts
# ============================================================

# ------------------------------------------------------------
# STEP 1: CALCULATE METRICS FROM sedd1  and later from sedd2
# ------------------------------------------------------------

index_node_attrs <- sedd1 %>%
  select(all_of(index_vars)) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "name",
    values_to = "value"
  ) %>%
  group_by(name) %>%
  summarise(
    mean_iln          = mean(value,                  na.rm = TRUE),  # mean across ALL threads
    mean_iln_nonzero  = mean(value[value > 0],       na.rm = TRUE),  # mean WHERE present
    pct_nonzero       = mean(value > 0,              na.rm = TRUE) * 100,
    n_nonzero         = sum(value > 0,               na.rm = TRUE),
    n_total           = sum(!is.na(value)),
    .groups = "drop"
  )

# Diagnostic table: sorted by prevalence
index_node_attrs %>%
  arrange(desc(mean_iln_nonzero)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = 30)

###   version to time 2

index_node_attrs2 <- sedd2 %>%
  select(all_of(index_vars)) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "name",
    values_to = "value"
  ) %>%
  group_by(name) %>%
  summarise(
    mean_iln          = mean(value,                  na.rm = TRUE),  # mean across ALL threads
    mean_iln_nonzero  = mean(value[value > 0],       na.rm = TRUE),  # mean WHERE present
    pct_nonzero       = mean(value > 0,              na.rm = TRUE) * 100,
    n_nonzero         = sum(value > 0,               na.rm = TRUE),
    n_total           = sum(!is.na(value)),
    .groups = "drop"
  )

# Diagnostic table: sorted by prevalence
index_node_attrs2 %>%
  arrange(desc(mean_iln_nonzero)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = 32)





# ------------------------------------------------------------
# STEP 2: ATTACH TO INDEX NODES TABLE
# ------------------------------------------------------------

library(dplyr)

index_nodes <- tibble(
  name      = index_vars,
  node_type = "index"
) %>%
  left_join(index_node_attrs, by = "name")


# ------------------------------------------------------------
# STEP 2: BUILD EDGE LIST
# One row per thread-index pair where iln > 0
# ------------------------------------------------------------

library(dplyr)

sedd1 <- sedd1 %>% 
  mutate(t_id = as.character(case_id))

sedd2 <- sedd2 %>% 
  mutate(t_id = as.character(case_id))

edge_list <- sedd1 %>%
  # Carry along any thread-level attributes you want for profiling/viz
  select(t_id, time_period, t_comments, ang_count_rt, joy_count_rt, 
    url, topic, ang_n_count_rt, joy_n_count_rt, 
    comment, word_count, ant_count_rt, sad_count_rt, 
    t_author, sent_ave, ant_n_count_rt, sad_n_count_rt, 
    t_name, com_count, disg_count_rt, surp_count_rt, 
    t_subreddit, case_id_rt, disg_n_count_rt, surp_n_count_rt, 
    t_upvotes, sent_ave_rt, fear_count_rt, trust_count_rt, 
    t_date, profan_count_rt, fear_n_count_rt, trust_n_count_rt, 
    t_up_ratio, all_of(index_vars)) %>%
  pivot_longer(
    cols      = all_of(index_vars),
    names_to  = "index_node",
    values_to = "weight"
  ) %>%
  filter(!is.na(weight) & weight > 0) %>%
  rename(thread_node = t_id)

cat("Total edges (non-zero thread-index pairs):", nrow(edge_list), "\n")
cat("Unique threads with at least one edge:    ", n_distinct(edge_list$thread_node), "\n")
cat("Unique index nodes with at least one edge:", n_distinct(edge_list$index_node), "\n")

# ------------------------------------------------------------
# STEP 3: BUILD NODE TABLES
# Two types: "thread" and "index"
# ------------------------------------------------------------

library(dplyr)

# Thread nodes — carry metadata for visual encoding later
thread_nodes <- sedd1 %>%
  distinct(t_id, .keep_all = TRUE) %>%
  select(t_id, time_period, t_comments, ang_count_rt, joy_count_rt, 
    url, topic, ang_n_count_rt, joy_n_count_rt, 
    comment, word_count, ant_count_rt, sad_count_rt, 
    t_author, sent_ave, ant_n_count_rt, sad_n_count_rt, 
    t_name, com_count, disg_count_rt, surp_count_rt, 
    t_subreddit, case_id_rt, disg_n_count_rt, surp_n_count_rt, 
    t_upvotes, sent_ave_rt, fear_count_rt, trust_count_rt, 
    t_date, profan_count_rt, fear_n_count_rt, trust_n_count_rt, 
    t_up_ratio) %>%
  rename(name = t_id) %>%
  mutate(node_type = "thread")

# ------------------------------------------------------------
# INDEX NODES — attributes you already computed
# ------------------------------------------------------------

index_nodes <- index_node_attrs %>%
  rename(name = name) %>%        # already named 'name' from earlier
  mutate(node_type = "index") %>%
  select(name, node_type, mean_iln, mean_iln_nonzero,
         pct_nonzero, n_nonzero, n_total)



# ------------------------------------------------------------
# COMBINE — mismatched columns fill with NA automatically
# ------------------------------------------------------------
library(dplyr)

node_table <- bind_rows(thread_nodes, index_nodes)

cat("Node table dimensions:", nrow(node_table), "rows x", ncol(node_table), "cols\n")
cat("\nNode counts:\n")
print(count(node_table, node_type))

cat("\nColumn list:\n")
print(names(node_table))

# Spot check — one thread row and one index row
node_table %>% filter(node_type == "thread") %>% slice(1) %>% glimpse()
node_table %>% filter(node_type == "index")  %>% slice(1) %>% glimpse()


# ------------------------------------------------------------
# CHECK WHICH COLUMNS IN thread_nodes ARE STILL LIST-TYPE
#           NOTE  this section was caused an error
#       that resulted from package conflict related to count
#       there was no variable defined as a list, 
#       once we specified dyplyr::count the error evaporated
# ------------------------------------------------------------

thread_nodes %>%
  summarise(across(everything(), ~ class(.x)[1])) %>%
  pivot_longer(everything(), names_to = "column", values_to = "class") %>%
  filter(class == "list") %>%
  print()

# ------------------------------------------------------------
# CHECK index_nodes FOR LIST-TYPE COLUMNS
# ------------------------------------------------------------

index_nodes %>%
  summarise(across(everything(), ~ class(.x)[1])) %>%
  pivot_longer(everything(), names_to = "column", values_to = "class") %>%
  print()

# And check node_table after the bind
node_table %>%
  summarise(across(everything(), ~ class(.x)[1])) %>%
  pivot_longer(everything(), names_to = "column", values_to = "class") %>%
  filter(class == "list") %>%
  { if (nrow(.) == 0) cat("node_table: all columns atomic.\n") else print(.) }

dplyr::count(node_table, node_type)

# ------------------------------------------------------------
# STEP 4: try out network attributes
# ------------------------------------------------------------

node_table <- node_table %>%
  mutate(type = node_type == "index")   # TRUE for index, FALSE for thread

# Confirm
dplyr::count(node_table, node_type, type)

g_bipartite <- tbl_graph(
  nodes    = node_table,
  edges    = edge_list %>% rename(from = thread_node, to = index_node),
  directed = FALSE)

# Confirm index node attributes are live
g_bipartite %>%
  activate(nodes) %>%
  filter(node_type == "index") %>%
  as_tibble() %>%
  select(name, mean_iln, mean_iln_nonzero, pct_nonzero, n_nonzero, n_total) %>%
  arrange(desc(pct_nonzero)) %>%
  print()


# ------------------------------------------------------------
# STEP 4A: Filter by sub-reddit to start with smaller graphs
# ------------------------------------------------------------

# Step 1: identify thread IDs in the target subreddits
target_threads <- sedd1 %>%
  filter(t_subreddit %in% c("Teachers", "college", "Professors")) %>%
  distinct(t_id) %>%
  pull(t_id)

cat("Threads in subset:", length(target_threads), "\n")

# Step 2: filter edge list to only those threads
edge_list_sub <- edge_list %>%
  filter(thread_node %in% target_threads)

cat("Edges in subset:", nrow(edge_list_sub), "\n")

# Step 3: filter node table to only nodes that appear in the subset edge list
active_nodes <- union(edge_list_sub$thread_node, edge_list_sub$index_node)

node_table_sub <- node_table %>%
  filter(name %in% active_nodes)

cat("Nodes in subset:", nrow(node_table_sub), "\n")
dplyr::count(node_table_sub, node_type)

# Step 4: build subsetted graph
g_sub <- tbl_graph(
  nodes    = node_table_sub,
  edges    = edge_list_sub %>% rename(from = thread_node, to = index_node),
  directed = FALSE
)

# print(g_sub)   skip this boring plot


# ------------------------------------------------------------
# STEP 4B: filter on edge weight
# FILTER EDGE LIST BY MINIMUM WEIGHT BEFORE RENDERING
# ------------------------------------------------------------

# Check the distribution of edge weights first
summary(edge_list_sub$weight)
quantile(edge_list_sub$weight, probs = c(.25, .50, .75, .90, .95), na.rm = TRUE)

# Histogram to help choose a threshold
ggplot(edge_list_sub, aes(x = weight)) +
  geom_histogram(bins = 50, fill = "#4E79A7") +
  geom_vline(xintercept = c(0.5, 1.0, 1.5), 
             linetype = "dashed", colour = "red") +
  labs(title = "Distribution of Edge Weights",
       x = "Weight (iln)", y = "Count") +
  theme_minimal()

# ------------------------------------------------------------
# SET THRESHOLD AND REBUILD GRAPH
# ------------------------------------------------------------

weight_threshold <- 2.0   # adjust based on distribution above

edge_list_filtered <- edge_list_sub %>%
  filter(weight >= weight_threshold)

cat("Edges before filter:", nrow(edge_list_sub), "\n")
cat("Edges after filter: ", nrow(edge_list_filtered), "\n")

# Derive only nodes that still have edges after filtering
active_nodes_filtered <- union(edge_list_filtered$thread_node,
                               edge_list_filtered$index_node)

node_table_filtered <- node_table %>%
  filter(name %in% active_nodes_filtered)

cat("Nodes before filter:", nrow(node_table_sub), "\n")
cat("Nodes after filter: ", nrow(node_table_filtered), "\n")
dplyr::count(node_table_filtered, node_type)

# Rebuild graph on filtered data
g_filtered <- tbl_graph(
  nodes    = node_table_filtered,
  edges    = edge_list_filtered %>% rename(from = thread_node, to = index_node),
  directed = FALSE
)

print(g_filtered)

# ------------------------------------------------------------
# RENDER
# ------------------------------------------------------------

ggraph(g_filtered, layout = "stress") +
  geom_edge_link(aes(alpha = weight, width = weight), colour = "grey60") +
  geom_node_point(aes(colour = node_type, size = node_type)) +
  geom_node_text(
    aes(label = ifelse(node_type == "index", name, NA)),
    repel = TRUE, size = 3
  ) +
  scale_colour_manual(values = c(thread = "#4E79A7", index = "#F28E2B")) +
  scale_size_manual(values   = c(thread = 1.5,       index = 5)) +
  scale_edge_alpha_continuous(range = c(0.3, 0.9)) +
  scale_edge_width_continuous(range = c(0.3, 1.5)) +
  labs(title    = "Bipartite Network — Teachers, Professors & College",
       subtitle = paste("Edge weight ≥", weight_threshold)) +
  theme_graph()


# ------------------------------------------------------------
# STEP 1: ADD PLOT VARIABLES TO NODE TABLE
# ------------------------------------------------------------

summary(sedd1$t_upvotes)


# Breaks aligned to your actual distribution
# Min=8, Q1=61, Median=221, Mean=714, Q3=604, Max=28860

upvote_breaks <- c(-Inf, 60, 220, 600, 2500, Inf)
upvote_labels <- c("≤60", "61-220", "221-600", "601-2500", "2500+")

# Verify all values are caught — should show no NAs
sedd1 %>%
  mutate(
    upvote_tier = cut(t_upvotes, breaks = upvote_breaks,
                      labels = upvote_labels, right = TRUE)
  ) %>%
  dplyr::count(upvote_tier) %>%
  print()



node_table_filtered <- node_table_filtered %>%
  mutate(
    # Colour: subreddit for threads, fixed value for index nodes
    node_colour = ifelse(node_type == "thread", t_subreddit, "index"),

    # Size: ordinal upvote tier for threads, fixed value for index nodes
    upvote_tier = ifelse(
      node_type == "thread",
      as.character(cut(t_upvotes, breaks = upvote_breaks,
                       labels = upvote_labels, right = TRUE)),
      NA_character_
    ),
    upvote_tier = factor(upvote_tier, levels = upvote_labels)
  )

# Rebuild graph with updated node attributes
g_filtered <- tbl_graph(
  nodes    = node_table_filtered,
  edges    = edge_list_filtered %>% rename(from = thread_node, to = index_node),
  directed = FALSE
)

# ------------------------------------------------------------
# STEP 2: DEFINE COLOUR PALETTE
# One colour per subreddit + one for index nodes
# ------------------------------------------------------------

subreddit_colours <- c(
  "Teachers"    = "#4E79A7",
  "college"     = "#EDC948",
  "Professors"  = "#59A14F",
  "GradSchool"  = "#76B7B2",
  "academia"    = "#B07AA1",
  "AskAcademia" = "#FF9DA7",
  "index"       = "#F28E2B"   # index nodes
)

# ------------------------------------------------------------
# STEP 3: PLOT
# ------------------------------------------------------------

ggraph(g_filtered, layout = "fr") +
  geom_edge_link(aes(alpha = weight, width = weight), colour = "grey60") +
  geom_node_point(aes(colour = node_colour, size = upvote_tier),
                  na.rm = TRUE) +
  # Index nodes rendered separately at fixed size
  geom_node_point(
    data = . %>% filter(node_type == "index"),  
    aes(colour = node_colour),
    size = 5
  ) +
  geom_node_text(
    aes(label = ifelse(node_type == "index", name, NA)),
    repel = TRUE, size = 3, colour = "grey20"
  ) +
  scale_colour_manual(
    values = subreddit_colours,
    breaks = names(subreddit_colours)[names(subreddit_colours) != "index"],
    name   = "Subreddit"
  ) +
  scale_size_ordinal(
    range  = c(1.5, 7),
    name   = "Upvotes"
  ) +
  scale_edge_alpha_continuous(range = c(0.2, 0.8), guide = "none") +
  scale_edge_width_continuous(range = c(0.2, 1.2), guide = "none") +
  labs(
    title    = "Bipartite Network — Teachers, Professors & College",
    subtitle = paste("Edge weight ≥", weight_threshold,
                     " | Node colour = subreddit | Node size = upvote tier")
  ) +
  theme_graph() +
  theme(legend.position = "right")

#####################
#      fix node size issue for index nodes
#

ggraph(g_filtered, layout = "fr") +
  geom_edge_link(aes(alpha = weight, width = weight), colour = "grey60") +
  
  # Thread nodes only — sized by upvote tier
  geom_node_point(
    data = . %>% filter(node_type == "thread"),
    aes(colour = node_colour, size = upvote_tier)
  ) +
  
  # Index nodes only — fixed size, own colour
  geom_node_point(
    data = . %>% filter(node_type == "index"),
    aes(colour = node_colour),
    size = 5
  ) +
  
  geom_node_text(
    aes(label = ifelse(node_type == "index", name, NA)),
    repel = TRUE, size = 3, colour = "grey20"
  ) +
  scale_colour_manual(
    values = subreddit_colours,
    breaks = names(subreddit_colours)[names(subreddit_colours) != "index"],
    name   = "Subreddit"
  ) +
  scale_size_ordinal(range = c(1.5, 7), name = "Upvotes") +
  scale_edge_alpha_continuous(range = c(0.2, 0.8), guide = "none") +
  scale_edge_width_continuous(range = c(0.2, 1.2), guide = "none") +
  labs(
    title    = "Bipartite Network — Teachers, Professors & College",
    subtitle = paste("Edge weight ≥", weight_threshold,
                     " | Node colour = subreddit | Node size = upvote tier")
  ) +
  theme_graph() +
  theme(legend.position = "right")





# ------------------------------------------------------------
# STEP 4C: PLOT — size = % non-zero | colour = mean_iln (all)
# Shows: how broadly vs how intensely each concept appears
# ------------------------------------------------------------

ggraph(g_bipartite, layout = "bipartite") +
  geom_edge_link(aes(alpha = weight), colour = "grey60") +
  geom_node_point(aes(
    colour = ifelse(node_type == "index", mean_iln,     NA_real_),
    size   = ifelse(node_type == "index", pct_nonzero,  1.5)
  )) +
  geom_node_text(
    aes(label = ifelse(node_type == "index", name, NA)),
    repel = TRUE, size = 3
  ) +
  scale_colour_viridis_c(
    option   = "plasma",
    na.value = "#4E79A7",
    name     = "Mean iln\n(all threads)"
  ) +
  scale_size_continuous(range = c(1.5, 10), name = "% Non-zero") +
  scale_edge_alpha_continuous(range = c(0.05, 0.8), name = "Edge weight") +
  labs(
    title    = "Bipartite Network: Threads × Index Variables",
    subtitle = "Index size = % non-zero  |  colour = mean iln (all threads)"
  ) +
  theme_graph()


# ------------------------------------------------------------
# STEP 4B: PLOT — size = % non-zero | colour = mean_iln_nonzero
# Shows: intensity of signal where the concept actually appears
# ------------------------------------------------------------

ggraph(g_bipartite, layout = "bipartite") +
  geom_edge_link(aes(alpha = weight), colour = "grey60") +
  geom_node_point(aes(
    colour = ifelse(node_type == "index", mean_iln_nonzero, NA_real_),
    size   = ifelse(node_type == "index", pct_nonzero,      1.5)
  )) +
  geom_node_text(
    aes(label = ifelse(node_type == "index", name, NA)),
    repel = TRUE, size = 3
  ) +
  scale_colour_viridis_c(
    option   = "magma",
    na.value = "#4E79A7",
    name     = "Mean iln\n(non-zero only)"
  ) +
  scale_size_continuous(range = c(1.5, 10), name = "% Non-zero") +
  scale_edge_alpha_continuous(range = c(0.05, 0.8), name = "Edge weight") +
  labs(
    title    = "Bipartite Network: Threads × Index Variables",
    subtitle = "Index size = % non-zero  |  colour = mean iln (where present)"
  ) +
  theme_graph()


# ------------------------------------------------------------
# STEP 4C: SCATTERPLOT — prevalence vs intensity
# Useful diagnostic before committing to network layout
# ------------------------------------------------------------

index_node_attrs %>%
  ggplot(aes(x = pct_nonzero, y = mean_iln_nonzero, label = name)) +
  geom_point(aes(size = mean_iln), colour = "#F28E2B", alpha = 0.
             
             
             
  
# ============================================================
# VERSION A: tidygraph + ggraph
# ============================================================

# ------------------------------------------------------------
# STEP 4A: ASSEMBLE tbl_graph OBJECT
# ------------------------------------------------------------

g_bipartite <- tbl_graph(
  nodes    = node_table,
  edges    = edge_list %>% rename(from = thread_node, to = index_node),
  directed = FALSE
)

# Confirm bipartite structure
cat("\ntbl_graph summary:\n")
print(g_bipartite)

# ------------------------------------------------------------
# STEP 5A: BASIC BIPARTITE PLOT
# ------------------------------------------------------------

ggraph(g_bipartite, layout = "bipartite") +
  geom_edge_link(aes(alpha = weight), colour = "grey60") +
  geom_node_point(aes(colour = node_type, size = node_type)) +
  geom_node_text(aes(label = ifelse(node_type == "index", name, NA)),
                 repel = TRUE, size = 3) +
  scale_colour_manual(values = c(thread = "#4E79A7", index = "#F28E2B")) +
  scale_size_manual(values   = c(thread = 1.5,       index = 5)) +
  scale_edge_alpha_continuous(range = c(0.05, 0.8)) +
  labs(title    = "Bipartite Network: Threads × Index Variables",
       subtitle = "Edge weight = raw iln value",
       colour   = "Node type") +
  theme_graph()

# ------------------------------------------------------------
# STEP 6A: SUBREDDIT-FACETED VERSION
# ------------------------------------------------------------

# Add subreddit as a node attribute for faceting
g_facet <- g_bipartite %>%
  activate(nodes) %>%
  mutate(subreddit_label = ifelse(node_type == "thread",
                                  t_subreddit, "index_node"))

ggraph(g_facet, layout = "stress") +
  geom_edge_link(aes(alpha = weight), colour = "grey50") +
  geom_node_point(aes(colour = node_type)) +
  facet_nodes(~ subreddit_label, scales = "free") +
  scale_colour_manual(values = c(thread = "#4E79A7", index = "#F28E2B")) +
  scale_edge_alpha_continuous(range = c(0.05, 0.8)) +
  labs(title = "Bipartite Network by Subreddit") +
  theme_graph(base_size = 10)


# ============================================================
# VERSION B: NodeXL EXPORT
# NodeXL expects: (1) an edge list CSV, (2) a vertex CSV
# ============================================================

# ------------------------------------------------------------
# STEP 4B: EDGE LIST FOR NODEXL
# Columns: Vertex 1, Vertex 2, Weight, + any edge attributes
# ------------------------------------------------------------

nodexl_edges <- edge_list %>%
  select(
    `Vertex 1`  = thread_node,
    `Vertex 2`  = index_node,
    Weight      = weight,
    Subreddit   = t_subreddit,    # edge-level metadata
    Group       = group1          # TRUE = K12, FALSE = Higher Ed
  ) %>%
  mutate(Group = ifelse(Group, "K12", "HigherEd"))

write_csv(nodexl_edges, "nodexl_edges.csv")
cat("Saved: nodexl_edges.csv —", nrow(nodexl_edges), "edges\n")

# ------------------------------------------------------------
# STEP 5B: VERTEX (NODE) LIST FOR NODEXL
# Columns: Vertex, Type, + any node attributes
# ------------------------------------------------------------

nodexl_vertices <- node_table %>%
  rename(Vertex = name) %>%
  mutate(
    Type      = node_type,
    Subreddit = ifelse(node_type == "thread", t_subreddit, NA_character_),
    Group     = case_when(
      group1 ~ "K12",
      group2 ~ "HigherEd",
      TRUE   ~ NA_character_
    )
  ) %>%
  select(Vertex, Type, Subreddit, Group)

write_csv(nodexl_vertices, "nodexl_vertices.csv")
cat("Saved: nodexl_vertices.csv —", nrow(nodexl_vertices), "nodes\n")           
             
             

