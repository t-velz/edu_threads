#  start of new syntax file using git and sharing in github

#---------------------------------------------------------#
#   edu 2026 proj  4/9/2026
#'  Ted, Will, Tess, Ayesha
#---------------------------------------------------------#

#          _.-^~~^^^`~-,_,,~''''''```~,''``~'``~,
#  ______,'  -o  :.  _    .          ;     ,'`,  `.
# (      -\.._,.;;'._ ,(   }        _`_-_,,    `, `,
#  ``~~~~~~'   ((/'((((____/~~~~~~'(,(,___>      `~'
#---------------------------------------------------------#                     


#  need to install packages for your first use of the package
#  thereafter you need to run library at the start of each
#  new sesssion.
         

# Load packages, data


library(data.table)
library(tidyverse)
library(car)
library(psych)

rm(list = ls(all = TRUE))   # use to get rid of old object in environment

edd<- fread("all_threads_2026_rt.csv")
names(edd)
#---------------------------------------------------------#
#1-drop extra vars----


# Define the variables to be removed

vars_to_remove <- c("downvotes", "golds", "t_total_awards", "t_golds",  #these 4 drop vars added 5/22/26
                  "profan_count", "profan_ave", "ang_ave", 
                  "ang_count", "ang_n_ave", "ang_n_count", 
                  "ant_ave", "ant_count", "ant_n_ave", 
                  "ant_n_count", "disg_ave", "disg_count", 
                  "disg_n_ave", "disg_n_count", "fear_ave", 
                  "fear_count", "fear_n_ave", "fear_n_count", 
                  "joy_ave", "joy_count", "joy_n_ave", 
                  "joy_n_count", "sad_ave", "sad_count", 
                  "sad_n_ave", "sad_n_count", "surp_ave", 
                  "surp_count", "surp_n_ave", "surp_n_count", 
                  "trust_ave", "trust_count", "trust_n_ave", 
                  "trust_n_count", "profan_ave_rt", "ang_ave_rt", 
                  "ang_n_ave_rt", "ant_ave_rt", "ant_n_ave_rt", 
                  "disg_ave_rt", "disg_n_ave_rt", "fear_ave_rt", 
                  "fear_n_ave_rt", "joy_ave_rt", "joy_n_ave_rt", 
                  "sad_ave_rt", "sad_n_ave_rt", "surp_ave_rt", 
                  "surp_n_ave_rt", "trust_ave_rt", "trust_n_ave_rt"
                )


# Remove the variables from the edd dataset

edd <- edd %>% 
  select(-all_of(vars_to_remove))

#---------------------------------------------------------#
#                   2 Create vars----

names(edd)

table(edd$profan_count_rt)
hist(edd$profan_count_rt)
summary(edd$profan_count_rt)
summary(edd$i_feel_rt)
hist(edd$i_feel_rt)


#______________Start variable creation section___________#

#  create indices of conceptual variables----

#  01. admin_problem----
edd$admin_prob_iln<- log(1+ edd$because_our_admin_rt+
edd$consequences_are_gone_rt+
edd$push_for_fewer_suspensions_rt+
edd$admin_actively_discourage_rt+
edd$disregard_for_the_teacher_rt+
edd$administration_is_scared_rt+
edd$no_discipline_rt+
edd$public_babysitting_rt+
edd$inmates_run_the_asylum_rt+
edd$accountability_for_admin_rt+
edd$responsibility_but_no_power_rt+
edd$admin_ignores_rt+
edd$admin_wont_rt+
edd$admin_dont_rt+
edd$impossible_to_fail_rt)

#  02. ai----
edd$ai_iln<- log(1+edd$chat_gpt_2_rt+
edd$grok_rt+
edd$anthropic_rt+
edd$ai_rt+
edd$artificial_intelligence_rt+
edd$robot_rt+
edd$chat_gpt_rt+
edd$llm_rt+
edd$claude_rt+
edd$gemini_rt)

#  03. ai_to_write----
edd$ai_to_write_iln<- log(1+edd$chat_gpt_to_write_rt+
edd$ai_to_write_rt)

#  04. bad_behavior----

edd$bad_behavior_iln<- log(1+edd$act_out_rt+
edd$acting_out_rt+
edd$interupt_rt+
edd$student_abruptly_state_rt+
edd$dismissive_remark_rt+
edd$ignore_schedule_rt+
edd$want_preference_rt+
edd$should_accommodate_rt+
edd$demanding_rt+
edd$expecting_to_be_accommodated_rt+
edd$vacations_during_the_semester_rt)

#  05. bad_parent----
edd$bad_parent_iln<- log(1+edd$nt_care_about_school_rt+
edd$parents_dont_care_rt+
edd$parents_dont_help_rt+
edd$parents_have_no_rt+
edd$parents_arent_rt+
edd$parents_do_not_rt+
edd$want_to_parent_rt+
edd$wants_to_parent_rt)

#  06. burned_out----
edd$burned_out_iln<- log(1+edd$no_motivation_rt+
edd$not_motivated_rt+
edd$too_exhausted_rt+
edd$i_am_done_rt+
edd$i_cant_anymore_rt+
edd$burned_out_rt+
edd$burn_out_rt+
edd$i_quit_rt+
edd$considering_leaving_this_profession_rt+
edd$love_teaching_but_rt+
edd$why_am_i_even_here_rt+
edd$like_i_cant_win_rt+
edd$wasting_my_time_rt+
edd$why_do_i_even_try_rt)

#  07. change_is_real----
edd$change_is_real_iln<- log(1+edd$worse_than_last_year_rt+
edd$is_it_just_my_school_rt+
edd$dumbed_down_rt+
edd$many_students_didnt_understand_rt+
edd$not_just_a_few_students_rt+
edd$no_in_between_rt+
edd$lower_half_has_plummeted_rt+
edd$say_they_never_learned_rt+
edd$increasing_proportion_of_students_rt+
edd$never_did_before_rt)

#  08. cheating----
edd$cheat_iln<- log(1+edd$plagiarism_rt+
edd$cheat_rt+
edd$plagiarize_rt+
edd$turnitin_rt+
edd$lock_down_browser_rt)
  
#  09. consumer_model----  
edd$consumer_model_iln<- log(1+edd$consumer_model_rt+
edd$pleasing_the_customer_rt+
edd$please_the_customer_rt+
edd$customer_is_always_right_rt)

#  10. attendance----
edd$dont_attend_iln<- log(1+edd$walk_the_halls_rt+
edd$skip_class_rt+
edd$low_attendance_rt+
edd$is_class_mandatory_rt+
edd$skipping_class_rt+
edd$is_attendance_rt)

#  11. extrinsic----
edd$extrinsic_iln<- log(1+edd$concerned_with_points_rt+
edd$resistance_to_knowing_rt+
edd$teased_rt+
edd$be_made_fun_of_rt+
edd$worried_about_others_rt+
edd$afraid_to_try_rt+
edd$immediate_perfection_rt+
edd$they_shut_down_rt+
edd$cope_with_failure_rt+
edd$not_willing_to_try_rt+
edd$emotionally_safe_rt+
edd$afraid_to_fail_rt+
edd$fear_of_fail_rt+
edd$fear_of_try_rt+
edd$insecur_rt+
edd$entitled_rt+
edd$no_mistakes_rt+
edd$nt_make_a_mistake_rt+
edd$nt_make_mistakes_rt+
edd$fear_mistakes_rt+
edd$nt_mess_up_rt+
edd$averse_to_taking_risks_rt+
edd$perfect_self_image_rt+
edd$chase_approval_rt+
edd$seek_approval_rt+
edd$chase_praise_rt+
edd$seek_praise_rt+
edd$fear_judgement_rt+
edd$afraid_of_judgement_rt+
edd$nt_want_to_be_judged_rt+
edd$extrinsic_motiv_rt+
edd$not_wanting_to_rt)


#  12. fall_behind_basics----

edd$fall_behind_basics_iln<- log(1+edd$nt_multiply_rt+
edd$not_multiply_rt+
edd$nt_divide_rt+
edd$not_divide_rt+
edd$nt_draw_rt+
edd$not_draw_rt+
edd$in_diapers_rt+
edd$potty_train_rt+
edd$never_develop_rt+
edd$stick_figure_rt+
edd$who_couldnt_properly_rt+
edd$nt_even_add_rt+
edd$no_idea_how_to_rt+
edd$at_grade_level_rt+
edd$below_grade_level_rt)


#  13. falling_behind----

edd$falling_behind_iln<- log(1+edd$nt_read_rt+
edd$not_read_rt+
edd$nt_do_rt+
edd$not_do_rt+
edd$nt_know_rt+
edd$not_know_rt+
edd$nt_understand_rt+
edd$not_understand_rt+
edd$nt_digest_rt+
edd$not_digest_rt+
edd$nt_name_rt+
edd$not_name_rt+
edd$struggle_to_rt+
edd$struggle_with_rt+
edd$lack_profic_rt+
edd$lack_basic_rt+
edd$lack_fundament_rt)

#  14. helicopter_parents----

edd$helicopter_parents_iln<- log(1+edd$parent_wants_rt+
edd$nt_hold_their_child_accountable_rt+
edd$helicopter_parent_rt+
edd$reply_from_her_parent_rt+
edd$her_childs_doing_rt+
edd$unfair_that_her_child_rt+
edd$both_parents_rt+
edd$with_her_mother_rt+
edd$with_his_mother_rt+
edd$email_from_the_students_rt+
edd$parents_along_with_them_rt+
edd$parental_intervention_rt+
edd$bring_their_parent_rt)

#  15. intrinsic_learning----

edd$intrinsic_learning_iln<- log(1+edd$enjoy_the_process_rt+
edd$passion_for_teach_rt+
edd$intrinsic_motiv_rt+
edd$take_ownership_rt+
edd$inquisitive_rt+
edd$curiosity_rt+
edd$passion_for_learning_rt)

#  16. ipad_baby----

edd$ipad_baby_iln<- log(1+edd$ipad_kid_rt+
edd$ipad_baby_rt+
edd$ipad_babies_rt)

#  17. mental_health----

edd$mental_health_iln<- log(1+edd$depress_rt+
edd$sad_rt+
edd$stress_rt+
edd$anxious_rt+
edd$anxiety_rt+
edd$mental_health_rt)

#  18. mention_genz_a----

edd$mention_genz_a_iln<- log(1+edd$generation_z_rt+
edd$gen_z_rt+
edd$zoomer_rt+
edd$gen_alpha_rt)

#  19. no_consequences----

edd$no_conseq_iln<- log(1+edd$zero_consequences_rt+
edd$fail_them_rt+
edd$no_consequences_rt+
edd$failing_rt+
edd$passed_along_rt+
edd$accountable_for_their_failure_rt+
edd$they_know_they_arent_rt+
edd$not_fail_rt+
edd$nt_fail_rt+
edd$lack_of_consequences_rt+
edd$without_consequences_rt+
edd$skate_by_rt+
edd$skating_by_rt+
edd$blow_off_rt)

#  20. nonchalance----

edd$nonchalance_iln<- log(1+edd$shrug_off_rt+
edd$zero_work_ethic_rt+
edd$no_work_ethic_rt+
edd$motivation_is_dead_rt+
edd$breezed_through_highschool_rt+
edd$cant_be_bother_rt+
edd$no_motion_rt+
edd$nonchalant_rt+
edd$nonchalance_rt+
edd$apathy_rt+
edd$apathetic_rt+
edd$nt_stand_out_rt+
edd$nt_want_to_stand_out_rt+
edd$face_in_the_crowd_rt+
edd$no_passion_rt+
edd$too_cool_to_care_rt+
edd$too_tough_to_try_rt+
edd$too_tuff_to_try_rt+
edd$no_personality_rt+
edd$nt_lose_aura_rt+
edd$no_aura_rt+
edd$nt_start_a_conversation_rt+
edd$media_pressures_rt+
edd$nt_care_rt+
edd$auraless_rt)

#  21. not engaged----

edd$not_engaged_iln<- log(1+edd$nt_listen_rt+
edd$not_listen_rt+
edd$nt_pay_attention_rt+
edd$not_pay_attention_rt+
edd$nt_focus_rt+
edd$not_focus_rt+
edd$nt_engage_rt+
edd$not_engage_rt+
edd$no_engage_rt+
edd$tuned_out_rt+
edd$checked_out_rt+
edd$not_interested_rt+
edd$no_interest_rt+
edd$zone_out_rt+
edd$bored_in_class_rt+
edd$nt_motiva_rt+
edd$not_motiva_rt+
edd$nt_work_rt+
edd$not_work_rt+
edd$nt_try_rt+
edd$not_try_rt+
edd$nt_keep_rt+
edd$not_keep_rt+
edd$nt_apply_rt+
edd$not_apply_rt+
edd$refuse_to_rt+
edd$no_effort_rt+
edd$passive_rt+
edd$just_sit_there_rt+
edd$never_work_rt+
edd$never_partic_rt+
edd$nt_do_anything_rt+
edd$do_nothing_rt+
edd$nt_try_before_rt+
edd$to_try_rt+
edd$lack_of_effort_rt+
edd$nt_practice_rt+
edd$not_practice_rt+
edd$resist_learning_rt)

#  22. phone distraction----

edd$phone_distraction_iln<- log(1+edd$phone_rt+
edd$screen_addict_rt+
edd$phone_addict_rt+
edd$digital_addict_rt+
edd$screenager_rt+
edd$device_addict_rt+
edd$screen_obsession_rt+
edd$screen_time_rt+
edd$chrome_book_rt+
edd$chromebook_rt)


#  23. verbal disengagement----

edd$verbal_disengagement_iln<- log(1+edd$nt_talk_rt+
edd$not_talk_rt+
edd$nt_answer_rt+
edd$not_answer_rt+
edd$nt_respond_rt+
edd$not_respond_rt+
edd$nt_say_rt+
edd$not_say_rt)

#  24. vis_distraction----

edd$vis_distraction_iln<- log(1+edd$on_their_phone_rt+
edd$distracted_rt+
edd$playing_games_rt+
edd$wearing_headphones_rt+
edd$listening_to_headphones_rt+
edd$texts_rt)

#  25. visible_disengagement----

edd$visible_disen_iln<- log(1+edd$nt_partic_rt+
edd$not_partic_rt+
edd$blank_stare_rt+
edd$they_just_stare_rt+
edd$nt_ask_question_rt+
edd$crickets_rt+
edd$t_even_listen_rt+
edd$sleeping_in_class_rt+
edd$barely_participate_rt+
edd$not_following_discussion_rt+
edd$see_me_as_a_movie_rt+
edd$no_question_rt+
edd$roll_eyes_rt+
edd$nt_react_rt+
edd$not_even_looking_rt+
edd$deads_down_rt)

#  26. zoom_disengaged----

edd$zoom_disengaged_iln<- log(1+edd$nt_turn_camera_on_rt+
edd$nt_turn_on_mic_rt+
edd$nt_type_in_chat_rt+
edd$no_one_responds_rt+
edd$muted_rt+
edd$talking_to_myself_rt+
edd$deleted_rt)

#  27. digital_media----

edd$digital_media_iln<- log(1+edd$twitch_rt+
edd$reddit_rt+
edd$whatsapp_rt+
edd$texting_rt+
edd$groupme_rt+
edd$wikipedia_rt+
edd$email_rt)

#  28. feed_socials----

edd$feed_socials_iln<- log(1+edd$ex_rt+
edd$tik_tok_rt+
edd$instagram_rt+
edd$the_gram_rt+
edd$social_media_rt+
edd$snap_chat_rt+
edd$you_tube_rt+
edd$youtube_rt+
edd$twitter_rt+
edd$xitter_rt+
edd$facebook_rt+
edd$threads_rt)

#  29. i_think----

edd$i_think_iln<- log(1+edd$i_really_think_rt+
edd$i_really_believe_rt+
edd$i_believe_rt+
edd$i_do_believe_rt+
edd$i_feel_rt+
edd$i_know_rt+
edd$i_just_think_rt+
edd$i_think_rt+
edd$i_thought_rt+
edd$i_agree_rt)

#  30. thanks----

edd$thanks_iln<- log(1+edd$appreciate_rt+
edd$grateful_rt+
edd$thank_you_rt+
edd$thanks_rt)

#  31. they_think----

edd$they_think_iln<- log(1+edd$they_believe_rt+
edd$they_claim_rt+
edd$they_say_rt+
edd$they_think_rt+
edd$they_want_rt+
edd$they_dont_believe_rt+
edd$they_dont_think_rt)


# 32.   equity_ghouls----

edd$equity_gouls_iln<- log(1+edd$bought_out_by_rt+
edd$investment_firm_rt+
edd$equity_firm_rt+
edd$sucked_dry_rt+
edd$hollowed_out_rt)

# 33.  gesellschaft----

edd$gesellschaft_iln<- log(1+edd$poser_rt+
edd$of_course_rt+
edd$the_type_of_guy_rt+
edd$trying_to_sell_you_rt+
edd$posting_thirst_traps_rt+
edd$performative_and_cringe_rt+
edd$tenk_month_rt+
edd$niche_a_good_product_rt+
edd$listing_optimization_rt+
edd$partner_with_rt+
edd$affiliates_rt+
edd$influencer_rt+
edd$making_money_rt+
edd$make_money_rt)

34.  grind_culture----

edd$grind_culture_iln<- log(1+edd$never_stops_hustling_rt+
edd$so_passionate_rt+
edd$i_care_deeply_rt+
edd$if_your_team_isnt_rt+
edd$grind_culture_rt+
edd$bosses_to_exploit_rt+
edd$this_isnt_satire_rt+
edd$theyre_locked_in_rt+
edd$hey_are_paid_rt+
edd$my_priorities_rt+
edd$up_by_the_bootstraps_rt)


35.   platform_abuse----

edd$platform_abuse_iln<- log(1+edd$rude_rt+
edd$abusive_rt+
edd$block_them_rt+
edd$bully_other_sellers_rt+
edd$off_the_platform_rt+
edd$report_them_rt+
edd$report_this_rt+
edd$d_cancel_rt+
edd$how_an_item_rt+
edd$signature_confirmation_rt+
edd$cancel_and_block_rt+
edd$red_flag_rt+
edd$trust_your_gut_rt+
edd$the_entitlement_rt+
edd$not_your_fault_rt+
edd$honest_feedback_rt+
edd$leave_a_neg_rt+
edd$just_block_rt+
edd$block_and_report_rt+
edd$threatening_legal_rt+
edd$worst_buyers_rt+
edd$negative_feedback_rt+
edd$move_on_rt+
edd$showing_my_face_rt+
edd$nt_show_rt+
edd$not_show_rt)

36.  platform_pollution----

edd$platform_pollution_iln<- log(1+edd$more_ads_than_content_rt+
edd$minutes_of_ads_rt+
edd$site_got_polluted_rt+
edd$real_names_policy_rt+
edd$prove_it_before_rt+
edd$your_real_name_rt+
edd$legal_name_rt+
edd$cess_pool_rt+
edd$engagement_farming_rt+
edd$bot_post_rt+
edd$spam_rt+
edd$repost_rt)

# summary(edd$platform_scam)

37. platform_scam----

edd$platform_scam_iln<- log(1+edd$cash_rides_rt+
edd$cant_be_verified_rt+
edd$safety_reasons_rt+
edd$fraudulent_activity_rt+
edd$scammy_rt+
edd$to_scam_you_rt+
edd$persons_feedback_rt+
edd$up_the_username_rt+
edd$troll_rt+
edd$profile_shows_rt)

38.  ride_platform_scam----

edd$ride_platform_scam_iln<- log(1+
edd$uber_support_rt+
edd$uber_doesnt_give_rt+
edd$honest_drivers_rt+
edd$undesirable_drivers_rt+
edd$lyft_driver_rt)

39.  platform_vampire----

edd$platform_vampire_iln<- log(1+edd$datamining_app_rt+
edd$laundry_machine_rt+
edd$crazy_expensive_rt+
edd$demanding_updates_rt+
edd$smart_t_vs_rt+
edd$healthcare_system_rt+
edd$not_repairable_rt+
edd$your_only_option_rt+
edd$create_a_user_account_rt+
edd$promote_sponsored_result_rt+
edd$ai_crap_rt+
edd$that_garbage_rt+
edd$sponsored_ones_rt+
edd$are_worthless_rt+
edd$forced_to_pay_rt+
edd$youtube_ads_rt+
edd$button_unclickable_rt+
edd$multiple_ads_rt+
edd$dont_bother_watching_rt+
edd$appliances_that_die_rt+
edd$subscribe_to_use_rt+
edd$later_removed_rt+
edd$battery_life_rt+
edd$requirement_to_link_rt+
edd$required_to_link_rt+
edd$must_link_to_rt+
edd$data_will_be_sold_rt+
edd$apps_being_required_rt+
edd$app_required_rt+
edd$over_the_air_updates_rt+
edd$subscription_package_rt+
edd$impossible_to_repair_rt+
edd$cheaper_and_cheaper_rt+
edd$lower_quality_rt+
edd$hours_cut_rt+
edd$cutting_corners_rt+
edd$service_breakdown_rt)

40. platform_resist----

edd$platform_resist_iln<- log(1+edd$i_deleted_my_account_rt+
edd$users_bleed_away_rt+
  edd$service_recovery_rt)




#  create other helpful variables----
# * sub-dummies ----

table(edd$t_subreddit)

edd <- edd %>%
mutate(
Teachers = if_else(t_subreddit == "Teachers", 1, 0),
college = if_else(t_subreddit == "college", 1, 0),
AskAcademia = if_else(t_subreddit == "AskAcademia", 1, 0),
academia = if_else(t_subreddit == "academia", 1, 0),
GradSchool = if_else(t_subreddit == "GradSchool", 1, 0),
Professors = if_else(t_subreddit == "Professors", 1, 0),
education = if_else(t_subreddit == "education", 1, 0),
GenZ = if_else(t_subreddit == "GenZ", 1, 0),
GenAlpha = if_else(t_subreddit == "GenAlpha", 1, 0),
GenX = if_else(t_subreddit == "GenX", 1, 0),
millenials = if_else(t_subreddit == "millenials", 1, 0),
highereducation = if_else(t_subreddit == "highereducation", 1, 0),
Parenting = if_else(t_subreddit == "Parenting", 1, 0),
socialskills = if_else(t_subreddit == "socialskills", 1, 0),
teenagers = if_else(t_subreddit == "teenagers", 1, 0))


#---------------------------------------------------------#
#                   3 Create indicator data tables----
#---------------------------------------------------------#

# create data tables of indicators for each provisional index


# create data tables of indicators for each provisional index


#  01. admin problem----

admin_prob<- tibble( edd$because_our_admin_rt,
edd$consequences_are_gone_rt,      #not observed
edd$push_for_fewer_suspensions_rt,      #not observed
edd$admin_actively_discourage_rt,      #not observed
edd$disregard_for_the_teacher_rt,      #not observed
edd$administration_is_scared_rt,      #not observed
edd$no_discipline_rt,
edd$public_babysitting_rt,      #not observed
edd$inmates_run_the_asylum_rt,
edd$accountability_for_admin_rt,
edd$responsibility_but_no_power_rt,      #not observed
edd$admin_ignores_rt,
edd$admin_wont_rt,
edd$admin_dont_rt,
edd$impossible_to_fail_rt)

#  02. ai----
ai<- tibble(edd$chat_gpt_2_rt,
edd$grok_rt,
edd$anthropic_rt,
edd$ai_rt,
edd$artificial_intelligence_rt,
edd$robot_rt,
edd$chat_gpt_rt,
edd$llm_rt,
edd$claude_rt,
edd$gemini_rt)

#  03. ai to write----
ai_to_write<- tibble(edd$chat_gpt_to_write_rt,
edd$ai_to_write_rt)

#  04. bad behavior----

bad_behavior<- tibble(edd$act_out_rt,
edd$acting_out_rt,
edd$interupt_rt,
edd$student_abruptly_state_rt,      #not observed
edd$dismissive_remark_rt,
edd$ignore_schedule_rt,      #not observed
edd$want_preference_rt,      #not observed
edd$should_accommodate_rt,
edd$demanding_rt,
edd$expecting_to_be_accommodated_rt,      #not observed
edd$vacations_during_the_semester_rt)      #not observed

#  05. bad parent----
bad_parent<- tibble(edd$nt_care_about_school_rt,
edd$parents_dont_care_rt,
edd$parents_dont_help_rt,
edd$parents_have_no_rt,
edd$parents_arent_rt,
edd$parents_do_not_rt,
edd$want_to_parent_rt,
edd$wants_to_parent_rt)

#  06. burned out----
burned_out<- tibble(edd$no_motivation_rt,
edd$not_motivated_rt,
edd$too_exhausted_rt,
edd$i_am_done_rt,
edd$i_cant_anymore_rt,
edd$burned_out_rt,
edd$burn_out_rt,
edd$i_quit_rt,
edd$considering_leaving_this_profession_rt,      #not observed
edd$love_teaching_but_rt,
edd$why_am_i_even_here_rt,
edd$like_i_cant_win_rt,      #not observed
edd$wasting_my_time_rt,
edd$why_do_i_even_try_rt)

#  07. change is real----
change_is_real<- tibble(edd$worse_than_last_year_rt,
edd$is_it_just_my_school_rt,      #not observed
edd$dumbed_down_rt,
edd$many_students_didnt_understand_rt,      #not observed
edd$not_just_a_few_students_rt,      #not observed
edd$no_in_between_rt,
edd$lower_half_has_plummeted_rt,      #not observed
edd$say_they_never_learned_rt,      #not observed
edd$increasing_proportion_of_students_rt,      #not observed
edd$never_did_before_rt)

#  08. cheating----
cheat<- tibble(edd$plagiarism_rt,
edd$cheat_rt,
edd$plagiarize_rt,
edd$turnitin_rt,
edd$lock_down_browser_rt)
  
#  09. consumer model----  
consumer_model<- tibble(edd$consumer_model_rt,
edd$pleasing_the_customer_rt,
edd$please_the_customer_rt,
edd$customer_is_always_right_rt)

#  10. attendance----
dont_attend<- tibble(edd$walk_the_halls_rt,
edd$skip_class_rt,
edd$low_attendance_rt,
edd$is_class_mandatory_rt,      #not observed
edd$skipping_class_rt,
edd$is_attendance_rt)

#  11. extrinsic----
extrinsic<- tibble(edd$concerned_with_points_rt,      #not observed
edd$resistance_to_knowing_rt,      #not observed
edd$teased_rt,
edd$be_made_fun_of_rt,
edd$worried_about_others_rt,
edd$afraid_to_try_rt,
edd$immediate_perfection_rt,
edd$they_shut_down_rt,
edd$cope_with_failure_rt,
edd$not_willing_to_try_rt,
edd$emotionally_safe_rt,
edd$afraid_to_fail_rt,
edd$fear_of_fail_rt,
edd$fear_of_try_rt,
edd$insecur_rt,
edd$entitled_rt,
edd$no_mistakes_rt,
edd$nt_make_a_mistake_rt,
edd$nt_make_mistakes_rt,
edd$fear_mistakes_rt,
edd$nt_mess_up_rt,
edd$averse_to_taking_risks_rt,
edd$perfect_self_image_rt,      #not observed
edd$chase_approval_rt,
edd$seek_approval_rt,
edd$chase_praise_rt,      #not observed
edd$seek_praise_rt,      #not observed
edd$fear_judgement_rt,      #not observed
edd$afraid_of_judgement_rt,
edd$nt_want_to_be_judged_rt,
edd$extrinsic_motiv_rt,
edd$not_wanting_to_rt)


#  12. fall_behind_basics----

fall_behind_basics<- tibble(edd$nt_multiply_rt,
edd$not_multiply_rt,
edd$nt_divide_rt,
edd$not_divide_rt,
edd$nt_draw_rt,
edd$not_draw_rt,
edd$in_diapers_rt,
edd$potty_train_rt,
edd$never_develop_rt,
edd$stick_figure_rt,
edd$who_couldnt_properly_rt,      #not observed
edd$nt_even_add_rt,
edd$no_idea_how_to_rt,
edd$at_grade_level_rt,
edd$below_grade_level_rt)      #not observed

#  13. falling_behind----

falling_behind<- tibble(edd$nt_read_rt,
edd$not_read_rt,
edd$nt_do_rt,
edd$not_do_rt,
edd$nt_know_rt,
edd$not_know_rt,
edd$nt_understand_rt,
edd$not_understand_rt,
edd$nt_digest_rt,
edd$not_digest_rt,
edd$nt_name_rt,
edd$not_name_rt,
edd$struggle_to_rt,
edd$struggle_with_rt,
edd$lack_profic_rt,
edd$lack_basic_rt,
edd$lack_fundament_rt)

#  14. helicopter parents----

helicopter_parents<- tibble(edd$parent_wants_rt,
edd$nt_hold_their_child_accountable_rt,      #not observed
edd$helicopter_parent_rt,
edd$reply_from_her_parent_rt,      #not observed
edd$her_childs_doing_rt,      #not observed
edd$unfair_that_her_child_rt,      #not observed
edd$both_parents_rt,
edd$with_her_mother_rt,
edd$with_his_mother_rt,
edd$email_from_the_students_rt,      #not observed
edd$parents_along_with_them_rt,      #not observed
edd$parental_intervention_rt,
edd$bring_their_parent_rt)

#  15. intrinsic_learning----

intrinsic_learning<- tibble(edd$enjoy_the_process_rt,
edd$passion_for_teach_rt,
edd$intrinsic_motiv_rt,
edd$take_ownership_rt,
edd$inquisitive_rt,
edd$curiosity_rt,
edd$passion_for_learning_rt)

#  16. ipad_baby----

ipad_baby<- tibble(edd$ipad_kid_rt,
edd$ipad_baby_rt,
edd$ipad_babies_rt)

#  17. mental_health----

mental_health<- tibble(edd$depress_rt,
edd$sad_rt,
edd$stress_rt,
edd$anxious_rt,
edd$anxiety_rt,
edd$mental_health_rt)

#  18. mention_genz_a----

mention_genz_a<- tibble(edd$generation_z_rt,
edd$gen_z_rt,
edd$zoomer_rt,
edd$gen_alpha_rt)

#  19. no_consequences----

no_conseq<- tibble(edd$zero_consequences_rt,
edd$fail_them_rt,
edd$no_consequences_rt,
edd$failing_rt,
edd$passed_along_rt,
edd$accountable_for_their_failure_rt,
edd$they_know_they_arent_rt,      #not observed
edd$not_fail_rt,
edd$nt_fail_rt,
edd$lack_of_consequences_rt,
edd$without_consequences_rt,
edd$skate_by_rt,
edd$skating_by_rt,
edd$blow_off_rt)

#  20. nonchalance----

nonchalance<- tibble(edd$shrug_off_rt,
edd$zero_work_ethic_rt,
edd$no_work_ethic_rt,
edd$motivation_is_dead_rt,
edd$breezed_through_highschool_rt,
edd$cant_be_bother_rt,
edd$no_motion_rt,
edd$nonchalant_rt,
edd$nonchalance_rt,
edd$apathy_rt,
edd$apathetic_rt,
edd$nt_stand_out_rt,
edd$nt_want_to_stand_out_rt,
edd$face_in_the_crowd_rt,
edd$no_passion_rt,
edd$too_cool_to_care_rt,
edd$too_tough_to_try_rt,      #not observed
edd$too_tuff_to_try_rt,      #not observed
edd$no_personality_rt,
edd$nt_lose_aura_rt,      #not observed
edd$no_aura_rt,
edd$nt_start_a_conversation_rt,
edd$media_pressures_rt,
edd$nt_care_rt,
edd$auraless_rt)      #not observed

#  21. not engaged----

not_engaged<- tibble(edd$nt_listen_rt,
edd$not_listen_rt,
edd$nt_pay_attention_rt,
edd$not_pay_attention_rt,
edd$nt_focus_rt,
edd$not_focus_rt,
edd$nt_engage_rt,
edd$not_engage_rt,
edd$no_engage_rt,
edd$tuned_out_rt,
edd$checked_out_rt,
edd$not_interested_rt,
edd$no_interest_rt,
edd$zone_out_rt,
edd$bored_in_class_rt,
edd$nt_motiva_rt,
edd$not_motiva_rt,
edd$nt_work_rt,
edd$not_work_rt,
edd$nt_try_rt,
edd$not_try_rt,
edd$nt_keep_rt,
edd$not_keep_rt,
edd$nt_apply_rt,
edd$not_apply_rt,
edd$refuse_to_rt,
edd$no_effort_rt,
edd$passive_rt,
edd$just_sit_there_rt,
edd$never_work_rt,
edd$never_partic_rt,
edd$nt_do_anything_rt,
edd$do_nothing_rt,
edd$nt_try_before_rt,      #not observed
edd$to_try_rt,
edd$lack_of_effort_rt,
edd$nt_practice_rt,
edd$not_practice_rt,
edd$resist_learning_rt)

#  22. phone distraction----

phone_distraction<- tibble(edd$phone_rt,
edd$screen_addict_rt,
edd$phone_addict_rt,
edd$digital_addict_rt,
edd$screenager_rt,
edd$device_addict_rt,
edd$screen_obsession_rt,
edd$screen_time_rt,
edd$chrome_book_rt,
edd$chromebook_rt)

#  23. verbal disengagement----

verbal_disengagement<- tibble(edd$nt_talk_rt,
edd$not_talk_rt,
edd$nt_answer_rt,
edd$not_answer_rt,
edd$nt_respond_rt,
edd$not_respond_rt,
edd$nt_say_rt,
edd$not_say_rt)

#  24. vis_distraction----

vis_distraction<- tibble(edd$on_their_phone_rt,
edd$distracted_rt,
edd$playing_games_rt,
edd$wearing_headphones_rt,
edd$listening_to_headphones_rt,
edd$texts_rt)

#  25. visible_disengagement----

visible_disen<- tibble(edd$nt_partic_rt,
edd$not_partic_rt,
edd$blank_stare_rt,
edd$they_just_stare_rt,
edd$nt_ask_question_rt,
edd$crickets_rt,
edd$t_even_listen_rt,
edd$sleeping_in_class_rt,
edd$barely_participate_rt,
edd$not_following_discussion_rt,      #not observed
edd$see_me_as_a_movie_rt,
edd$no_question_rt,
edd$roll_eyes_rt,
edd$nt_react_rt,
edd$not_even_looking_rt,
edd$deads_down_rt)

#  26. zoom_disengaged----

zoom_disengaged<- tibble(edd$nt_turn_camera_on_rt,      #not observed
edd$nt_turn_on_mic_rt,      #not observed
edd$nt_type_in_chat_rt,      #not observed
edd$no_one_responds_rt,     
edd$muted_rt,
edd$talking_to_myself_rt,
edd$deleted_rt)

#  27. digital_media----

digital_media<- tibble(edd$twitch_rt,
edd$reddit_rt,
edd$whatsapp_rt,
edd$texting_rt,
edd$groupme_rt,
edd$wikipedia_rt,
edd$email_rt)

#  28. feed_socials----

feed_socials<- tibble(edd$ex_rt,
edd$tik_tok_rt,
edd$instagram_rt,
edd$the_gram_rt,
edd$social_media_rt,
edd$snap_chat_rt,
edd$you_tube_rt,
edd$youtube_rt,
edd$twitter_rt,
edd$xitter_rt,
edd$facebook_rt,
edd$threads_rt)

#  29. i_think----

i_think<- tibble(edd$i_really_think_rt,
edd$i_really_believe_rt,
edd$i_believe_rt,
edd$i_do_believe_rt,
edd$i_feel_rt,
edd$i_know_rt,
edd$i_just_think_rt,
edd$i_think_rt,
edd$i_thought_rt,
edd$i_agree_rt)

#  30. thanks----

thanks<- tibble(edd$appreciate_rt,
edd$grateful_rt,
edd$thank_you_rt,
edd$thanks_rt)

#  31. they_think----

they_think<- tibble(edd$they_believe_rt,
edd$they_claim_rt,
edd$they_say_rt,
edd$they_think_rt,
edd$they_want_rt,
edd$they_dont_believe_rt,
edd$they_dont_think_rt)


#---------------------------------------------------------#
#             4   Evaluate correlation of index indicators
#                      with the index itself
#---------------------------------------------------------#



round(cor(admin_prob, edd$admin_prob_iln),digits = 3)
round(cor(ai, edd$ai_iln),digits = 3)
round(cor(ai_to_write, edd$ai_to_write_iln),digits = 3)
round(cor(bad_behavior, edd$bad_behavior_iln),digits = 3)
round(cor(bad_parent, edd$bad_parent_iln),digits = 3)
round(cor(burned_out, edd$burned_out_iln),digits = 3)
round(cor(change_is_real, edd$change_is_real_iln),digits = 3)
round(cor(cheat, edd$cheat_iln),digits = 3)
round(cor(consumer_model, edd$consumer_model_iln),digits = 3)
round(cor(dont_attend, edd$dont_attend_iln),digits = 3)
round(cor(extrinsic, edd$extrinsic_iln),digits = 3)
round(cor(fall_behind_basics, edd$fall_behind_basics_iln),digits = 3)
round(cor(falling_behind, edd$falling_behind_iln),digits = 3)
round(cor(helicopter_parents, edd$helicopter_parents_iln),digits = 3)
round(cor(intrinsic_learning, edd$intrinsic_learning_iln),digits = 3)
round(cor(ipad_baby, edd$ipad_baby_iln),digits = 3)
round(cor(mental_health, edd$mental_health_iln),digits = 3)
round(cor(mention_genz_a, edd$mention_genz_a_iln),digits = 3)
round(cor(no_conseq, edd$no_conseq_iln),digits = 3)
round(cor(nonchalance, edd$nonchalance_iln),digits = 3)
round(cor(not_engaged, edd$not_engaged_iln),digits = 3)
round(cor(phone_distraction, edd$phone_distraction_iln),digits = 3)
round(cor(verbal_disengagement, edd$verbal_disengagement_iln),digits = 3)
round(cor(vis_distraction, edd$vis_distraction_iln),digits = 3)
round(cor(visible_disen, edd$visible_disen_iln),digits = 3)
round(cor(zoom_disengaged, edd$zoom_disengaged_iln),digits = 3)
round(cor(digital_media, edd$digital_media_iln),digits = 3)
round(cor(feed_socials, edd$feed_socials_iln),digits = 3)
round(cor(i_think, edd$i_think_iln),digits = 3)
round(cor(thanks, edd$thanks_iln),digits = 3)
round(cor(they_think, edd$they_think_iln),digits = 3)




  # edu_related is 1 if the thread comes from any of the listed subreddits,
  # 0 otherwise.  %in% checks membership in the vector in one step, which is
  # cleaner than chaining 15 separate if_else() conditions with | operators.

# write edu 15 dataset to csv---- 
edd <- edd %>% 
mutate(
  edu_related = if_else(t_subreddit %in% c(
    "Teachers",
    "college",
    "AskAcademia",
    "academia",
    "GradSchool",
    "Professors",
    "education",
    "GenZ",
    "GenAlpha",
    "GenX",
    "millenials",
    "highereducation",
    "Parenting",
    "socialskills",
    "teenagers"
  ), 1, 0)
)

edd |>
  filter(edu_related == 1)  -> edu_threads

fwrite(edu_threads, "edd15subs_indices.csv")

