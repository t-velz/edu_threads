#---------------------------------------------------------#
#   edu 2026 proj  4/9/2026
#'  Ted, Will, Tess, Ayesha
#---------------------------------------------------------#

#          _.-^~~^^^`~-,_,,~''''''```~,''``~'``~,
#  ______,'  -o  :.  _    .          ;     ,'`,  `.
# (      -\.._,.;;'._ ,(   }        _`_-_,,    `, `,
#  ``~~~~~~'   ((/'((((____/~~~~~~'(,(,___>      `~'
#---------------------------------------------------------#                     


# need to install packages for your first use of the package
#  thereafter you need to run library at the start of each
#  new session.
                     
# Load packages, data



library(data.table)
library(tidyverse)
library(car)
library(psych)

edd<- fread("edd_2026_updated.csv")
names(edd)
#---------------------------------------------------------#
#1-drop extra vars----


# Define the variables to be removed
vars_to_remove <- c("profan_count", "profan_ave", "ang_ave", 
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
                    "surp_n_ave_rt", "trust_ave_rt", "trust_n_ave_rt")


# Remove the variables from the edd dataset

edd <- edd %>% 
  select(-all_of(vars_to_remove))

#---------------------------------------------------------#
#                   2 Create vars----

names(edd)
table(edd$t_subreddit)

#______________Start variable creation section___________#

#  create indices of conceptual variables----

# *1 no consequences----

edd$no_conseq_ind <- (
  edd$zero_consequences_rt+
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
    edd$no_consequences)

edd$no_conseq_iln  <- log(1+edd$no_conseq_ind) 

hist(edd$no_conseq_ind)

hist(edd$no_conseq_iln)

plot(edd$no_conseq_ind, edd$no_conseq_iln)

#  *2 visible disengagement----

edd$visible_disen_ind <- (  
  edd$sleeping_in_class_rt +                  
    edd$barely_participate_rt +                
    edd$not_following_discussion_rt +         
    edd$see_me_as_a_movie_rt+                  
    edd$no_question_rt +                       
    edd$roll_eyes_rt   +                       
    edd$nt_react_rt   +                        
    edd$not_even_looking_rt  +                 
    edd$deads_down_rt)        

hist(edd$visible_disen_ind)

hist(edd$sad_count_rt)



edd$bad_parent_ind <- ( 
  edd$parents_dont_care_rt+                  
    edd$parents_dont_help_rt+                  
    edd$parents_have_no_rt+                    
    edd$parents_arent_rt+                      
    edd$parents_do_not_rt+                     
    edd$want_to_parent_rt+                     
    edd$wants_to_parent_rt) 

###    new additions for 3/26/2026


edd$visible_disen_iln <- log(1 + edd$visible_disen_ind)


# *3 bad parent----

edd$bad_parent_ind<- (
  edd$nt_care_about_school_rt+
    edd$parents_dont_care_rt+
    edd$parents_dont_help_rt+
    edd$parents_have_no_rt+
    edd$parents_arent_rt+
    edd$parents_do_not_rt+
    edd$want_to_parent_rt+
    edd$wants_to_parent_rt)

edd$bad_parent_iln  <-   log(1+ edd$bad_parent_ind)


# *4 helicopter parents----

edd$helicopter_parents_ind <-(edd$parent_wants_rt+
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


edd$helicopter_parents_iln   <- log(1 + edd$helicopter_parents_ind )


summary(edd$helicopter_parents_iln)

hist(edd$helicopter_parents_ind)


# *5 visible distraction----

edd$vis_distraction_ind <-(edd$on_their_phone_rt+
     edd$distracted_rt+
     edd$playing_games_rt+
     edd$wearing_headphones_rt+
     edd$listening_to_headphones_rt+
     edd$texts_rt)

edd$vis_distraction_iln   <- log(1 + edd$vis_distraction_ind )


edd$zoom_disengaged_ind <-(edd$nt_turn_camera_on_rt+
     edd$nt_turn_on_mic_rt+
     edd$nt_type_in_chat_rt+
     edd$no_one_responds_rt+
     edd$muted_rt)

edd$zoom_disengaged_iln   <- log(1 + edd$zoom_disengaged_ind )

# *6 feed socials----

edd$feed_socials_ind <-(
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


max_ex_rt_case <- edd %>% 
  filter(ex_rt == max(ex_rt, na.rm = TRUE))

# Extract the thread title into case_name
case_name <- max_ex_rt_case$t_title


edd$feed_socials_iln   <- log(1 + edd$feed_socials_ind )

hist(edd$feed_socials_ind)

hist(edd$feed_socials_iln)

# *7 phone_distraction----

edd$phone_distraction_ind<-(
  edd$phone_rt+
    edd$screen_addict_rt+
    edd$phone_addict_rt+
    edd$digital_addict_rt+
    edd$screenager_rt+
    edd$device_addict_rt+
    edd$screen_obsession_rt+
    edd$screen_time_rt+
    edd$chrome_book_rt+
    edd$chromebook_rt)

edd$phone_distraction_iln   <- log(1 + edd$phone_distraction_ind )

hist(edd$phone_distraction_iln)

plot(edd$feed_socials_iln, edd$phone_distraction_iln)

# *8 ai----

edd$ai_ind <-(
  edd$ai_rt+
    edd$artificial_intelligence_rt+
    edd$chat_gpt_rt+
    edd$llm_rt+
    edd$claude_rt+
    edd$gemini_rt+
    edd$chat_gpt_2_rt+
    edd$grok_rt+
    edd$anthropic_rt)

edd$ai_iln   <- log(1 + edd$ai_ind )


# *9 burned our index----

edd$burned_out_ind<-(
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

edd$burned_out_iln <- log(1 + edd$burned_out_ind)

names(edd)

# *10 falling behind----

edd$falling_behind_ind<- (
  edd$lack_basic_rt+
    edd$below_grade_level_rt+
    edd$struggle_with_rt+
    edd$lack_fundament_rt+
    edd$lack_profic_rt+
    edd$struggle_to_rt+
    edd$no_idea_how_to_rt+
    edd$nt_digest_rt+
    edd$not_digest_rt+
    edd$nt_name_rt+
    edd$not_name_rt+
    edd$struggle_to_rt+
    edd$struggle_with_rt)

summary(edd$falling_behind_ind)

edd$falling_behind_iln <- log(1+ edd$falling_behind_ind)

# *11 lack discipline----

edd$lack_discipline_ind<- (
  edd$nt_care_about_school_rt+
    edd$checked_out_rt+
    edd$not_interested_rt+
    edd$not_wanting_to_rt+
    edd$skipping_class_rt+
    edd$chat_gpt_to_write_rt)

edd$lack_discipline_iln <-  log(1+ edd$lack_discipline_ind)


# *12 bad behavior----

edd$bad_behavior_ind<-(
  edd$nt_listen_rt+
    edd$nt_care_about_school_rt+
    edd$nt_pay_attention_rt+
    edd$not_pay_attention_rt+
    edd$no_effort_rt+
    edd$nt_partic_rt+
    edd$acting_out_rt+
    edd$act_out_rt+
    edd$refuse_to_rt+
    edd$never_partic_rt)


edd$bad_behavior_iln <- log(1+ edd$bad_behavior_ind)

# *13  fall behind basics----

edd$fall_behind_basics_ind <- (
    edd$nt_read_rt+
    edd$not_read_rt+
    edd$nt_multiply_rt+    # 
    edd$not_multiply_rt+    #
    edd$nt_divide_rt+
    edd$not_divide_rt+      #
    edd$nt_draw_rt+
    edd$not_draw_rt+
    edd$nt_do_rt+
    edd$not_do_rt+
    edd$nt_know_rt+
    edd$not_know_rt+
    edd$nt_understand_rt+
    edd$not_understand_rt+
      edd$in_diapers_rt+
      edd$potty_train_rt+
      edd$never_develop_rt+
      edd$stick_figure_rt+               #
      edd$who_couldnt_properly_rt+
      edd$nt_even_add_rt+               #
      edd$no_idea_how_to_rt+
      edd$at_grade_level_rt+
      edd$below_grade_level_rt) 
      
summary(edd$fall_behind_basics_ind)      

edd$fall_behind_basics_iln <- log(1+ edd$fall_behind_basics_ind)

# *14  learned helplessness ----

edd$learned_helplessness_ind <- (     #edited
    edd$motivation_is_dead_rt +
    edd$afraid_to_try_rt +
    edd$not_willing_to_try_rt +
    edd$lack_of_effort_rt +
    edd$fear_of_try_rt +
    edd$no_effort_rt +
    edd$lack_of_effort_rt +
    edd$just_sit_there_rt+
      edd$nt_try_rt+
      edd$not_try_rt+
      edd$nt_keep_rt+
      edd$not_keep_rt+
      edd$nt_apply_rt+
      edd$not_apply_rt+
      edd$refuse_to_rt+
      edd$nt_focus_rt+
      edd$not_focus_rt)

summary(edd$learned_helplessness_ind)

edd$learned_helplessness_iln <-   log(1+ edd$learned_helplessness_ind)


# *15 giant lack of engagement or effort----

edd$giant_no_engagement_ind <- (
  edd$nt_listen_rt+
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
    edd$skate_by_rt+
    edd$skating_by_rt+
    edd$passive_rt+
    edd$nt_partic_rt+
    edd$not_partic_rt+
    edd$just_sit_there_rt+
    edd$never_work_rt+
    edd$never_partic_rt+
    edd$nt_do_anything_rt+
    edd$do_nothing_rt)

edd$giant_no_engagement_iln <- log(1+ edd$giant_no_engagement_ind)


# *16 tech reliance----

edd$tech_reliance_iln <- log(
  (1+ edd$ai_ind)*
  (1+edd$phone_distraction_ind)
  )


hist(edd$tech_reliance_iln)



#  *17 ai_to_write_ind----

edd$ai_to_write_ind <-
  (edd$chat_gpt_to_write_rt+
  edd$ai_to_write_rt+
  edd$ai_crap_rt)



edd$ai_to_write_iln <- log(1+edd$ai_to_write_ind) 

# *18 demanding----

edd$demanding_ind <-(
  edd$dismissive_remark_rt+
    edd$ignore_schedule_rt+
    edd$want_preference_rt+
    edd$should_accommodate_rt+
    edd$demanding_rt+
    edd$expecting_to_be_accommodated_rt+
    edd$vacations_during_the_semester_rt+
    edd$entitled_rt)

edd$demanding_iln <- log(1+edd$demanding_ind)

# *19 fear_mockery----

edd$fear_mockery_ind <-(
  edd$teased_rt+
    edd$be_made_fun_of_rt+
    edd$worried_about_others_rt+
    edd$afraid_to_try_rt+
    edd$immediate_perfection_rt+
    edd$they_shut_down_rt+
    edd$cope_with_failure_rt+
    edd$not_willing_to_try_rt+
    edd$emotionally_safe_rt)

edd$fear_mockery_iln <-  log(1+ edd$fear_mockery_ind)

# *20  consumer_model_ind----

edd$consumer_model_ind <-(
  edd$consumer_model_rt+
  edd$pleasing_the_customer_rt+
  edd$please_the_customer_rt+
  edd$customer_is_always_right_rt)

edd$consumer_model_iln <-log(1+ edd$consumer_model_ind)

# *21 admin_problems----

edd$admin_prob_ind<-(edd$because_our_admin_rt+edd$push_for_fewer_suspensions_rt+
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

edd$admin_prob_iln  <- log (1+ edd$admin_prob_ind)

# *22 nonchalance----
edd$nonchalance_ind <-(edd$nonchalant_rt+
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
                         edd$auraless_rt)

edd$nonchalance_iln <-   log(1+edd$nonchalance_ind)

# *23 extrinsic----

edd$extrinsic_ind<-(edd$no_mistakes_rt+
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
                  edd$nt_want_to_be_judged_rt)

edd$extrinsic_iln <- log(1+  edd$extrinsic_ind)

# *24 grind ideology----

edd$grind_ideology_ind <- (
  edd$so_passionate_rt+
    edd$i_care_deeply_rt+
    edd$if_your_team_isnt_rt+
    edd$grind_culture_rt)

edd$grind_ideology_iln   <- log(1 + edd$grind_ideology_ind)

# *25 enshittification----

edd$enshittification_ind <- (
  edd$bought_out_by_rt+
    edd$off_the_platform_rt+
    edd$cheaper_and_cheaper_rt+
    edd$bought_out_by_rt+
    edd$investment_firm_rt+
    edd$equity_firm_rt+
    edd$sucked_dry_rt+
    edd$hollowed_out_rt+
    edd$lower_quality_rt+
    edd$hours_cut_rt+
    edd$cutting_corners_rt+
    edd$service_breakdown_rt+
    edd$required_to_link_rt+
    edd$must_link_to_rt+
    edd$data_will_be_sold_rt+
    edd$users_bleed_away_rt+
    edd$more_ads_than_content_rt+
    edd$minutes_of_ads_rt+
    edd$site_got_polluted_rt+
    edd$cess_pool_rt+
    edd$apps_being_required_rt+
    edd$app_required_rt+
    edd$over_the_air_updates_rt+
    edd$subscription_package_rt+
    edd$impossible_to_repair_rt+
    edd$engagement_farming_rt)

edd$enshittification_iln <- log(1+ edd$enshittification_ind)

# *26 bot_spam----

edd$bot_spam_ind <- (
    edd$bot_post_rt+
    edd$spam_rt)

# *27 negative sanctions----

edd$negative_sanctions_ind <- (
  edd$honest_feedback_rt+
    edd$leave_a_neg_rt+
    edd$just_block_rt+
    edd$block_and_report_rt+
    edd$threatening_legal_rt+
    edd$worst_buyers_rt+
    edd$negative_feedback_rt)

edd$negative_sanctions_iln <- log(1+ edd$negative_sanctions_ind)

# *28 Mental health ----


edd$mental_health_ind <- (
edd$depress_rt+
  edd$sad_rt+
  edd$stress_rt+
  edd$anxious_rt+
  edd$anxiety_rt+
  edd$mental_health_rt)

edd$mental_health_iln <- log(1+edd$mental_health_ind)

# *29 sub-dummies ----

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

# 30 admin_fail_policy_ind----

edd$admin_fail_ind <-
(edd$fail_them_rt+
edd$failing_rt+
edd$passed_along_rt+
edd$accountable_for_their_failure_rt+
edd$they_know_they_arent_rt+
edd$not_fail_rt+
edd$nt_fail_rt+
edd$lack_of_consequences_rt+
edd$impossible_to_fail_rt)

edd$admin_fail_iln <- log (1 + edd$admin_fail_ind)

# 31 vis_phone_distract_ind----

edd$vis_distraction_ind <-(
edd$on_their_phone_rt+
edd$distracted_rt+
edd$playing_games_rt+
edd$wearing_headphones_rt+
edd$listening_to_headphones_rt+   #
edd$texts_rt)

edd$vis_distraction_iln <- log(1+ edd$vis_distraction_ind)

# 32 attend issues----

edd$attend_issues_iln<- log(1+edd$is_attendance_rt + 
                              edd$is_class_mandatory_rt+
                              edd$low_attendance_rt+
                              edd$skipping_class_rt)

#   add additional new var creating sections below here.





#   MESSY code below!   

#  rapid exploration work for NCSA presentation,   
#  see project google drive for google slides

#'  NOTE  the following plots should be modified to explore core concepts
#'  from the study of student disengagement
#'  
#'  these are from my grad class focused on online platforms and social change
#'  so, you will need to explore other indices to use
#'  
#'   
# Calculate mean burnout and enshittification index by subreddit and plot burnout
edd %>%
  group_by(t_subreddit) %>%
  summarise(mean_burnout = mean(burned_out_iln, na.rm = TRUE),
            mean_enshittification = mean(enshittification_iln, na.rm = TRUE)) %>%
  arrange(desc(mean_burnout)) %>%
  ggplot(aes(x = reorder(t_subreddit, -mean_burnout), y = mean_burnout, fill = mean_enshittification)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "white", high = "saddlebrown", name = "Avg Enshittification") +
  labs(title = "Mean Burnout Index by Subreddit (High to Low)",
       x = "Subreddit",
       y = "Mean Burnout Index") 
 # theme_minimal()

# Create a boxplot of mental health index by subreddit
ggplot(edd, aes(x = t_subreddit, y = mental_health_ind, fill = t_subreddit)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Mental Health Index by Subreddit",
       x = "Subreddit",
       y = "Mental Health Index") +
  theme_minimal() +
  theme(legend.position = "none")

library(tidyverse)
# Create a boxplot of up per comment by subreddit
ggplot(edd, aes(x = t_subreddit, y = up_per_comment, fill = t_subreddit)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Upvotes per Comment by Subreddit",
       x = "Subreddit",
       y = "Upvotes per Comment") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(edd, aes(x = t_subreddit, y = burned_out_iln, fill = t_subreddit)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Upvotes per Comment by Subreddit",
       x = "Subreddit",
       y = "Upvotes per Comment") +
  theme_minimal() +
  theme(legend.position = "none")


# Calculate the mean of burned_out_iln by subreddit and create a bar plot
edd %>%
  group_by(t_subreddit) %>%
  summarise(mean_burned_out = mean(burned_out_iln, na.rm = TRUE)) %>%
  arrange(desc(mean_burned_out)) %>%
  ggplot(aes(x = reorder(t_subreddit, -mean_burned_out), y = mean_burned_out, fill = t_subreddit)) +
  geom_col() +
  coord_flip() +
  labs(title = "Mean Burned Out Index by Subreddit",
       x = "Subreddit",
       y = "Mean Burned Out Index") +
  theme_minimal() +
  theme(legend.position = "none")

# 100 3key vars----


mygroup <- tibble(
edd$no_conseq_iln,
edd$nonchalance_iln,
edd$giant_no_engagement_iln,
edd$ai_iln)

round(cor(mygroup), digits = 3)

install.packages("psych")
library(psych)

pairs.panels(mygroup)    # this is fast way to make scatter plots
#   end of Video 1. start of video 2.

summary(mygroup)

 


 


# your dummy variables----

library(tidyverse) # Ensure the tidyverse package (which includes dplyr) is loaded

#------------------------- 1 ------------------#
# edd$no_conseq_iln  # Mean   :0.2369


edd <- edd %>%
mutate(
myvar1_dummy = case_when(
no_conseq_iln  > 0.2369   ~ 1,     # replace varname and 0.??? with mean of your var1
TRUE ~ 0
))

edd %>%
count(myvar1_dummy)


#------------------------ 2 -------------------#
# edd$nonchalance_iln  # Mean   :0.04467

edd <- edd %>%
mutate(
myvar2_dummy = case_when(
nonchalance_iln > 0.04467 ~ 1,    #replace .49 with mean of your var2
TRUE ~ 0
))

edd %>%
count(myvar2_dummy)

#------------------------ 3 -------------------#
# edd$giant_no_engagement_iln   # Mean   :0.7042
edd <- edd %>%
mutate(
myvar3_dummy = case_when(
giant_no_engagement_iln > 0.7042 ~ 1,
TRUE ~ 0
))


edd %>%
count(myvar3_dummy)

#-------------------------------------------#
# categorical scatterplot----

# 1. add up the three dummy vars----

edd$key_combo <-  (
edd$myvar1_dummy +
edd$myvar2_dummy +
edd$myvar3_dummy )

edd$key_combo_factor <-  as.factor(edd$key_combo)
table(edd$key_combo)

#   create categorical scatterplot

install.packages("ggthemes")
library(ggthemes)

# edit plot titles, x variable----

ggplot(
data = edd,
mapping = aes(x = ai_iln, y = key_combo,
color = key_combo_factor, shape = key_combo_factor),  )+
geom_jitter() +
geom_smooth(method = "lm")+
labs(
title = "AIs index & co-occurance of key variables",
subtitle = "We focus on threads on the upper right quadrant",
x = "AI Index",
y = "Key Combo"
) +
scale_color_colorblind()


#   create cutpoint variable----
edd <- edd %>%
mutate(
hi_x_var = if_else(ai_iln > .75
, 1, 0) )

edd <- edd %>%
mutate(
combo_3 = if_else(key_combo == 3
, 1, 0) )

table(edd$combo_3)

table(edd$hi_x_var)

# confirm number of cases for your dataset----

edd$combo_both <- edd$combo_3 * edd$hi_x_var

table(edd$combo_both)

# create mydataset----

edd %>%
filter(key_combo == 3) %>%
mutate() %>%
as_tibble  ->  big3

edd %>%
filter(combo_both == 1) %>%
mutate() %>%
as_tibble  ->  big4

table(mydata$t_subreddit)

write.csv(mydata, file = "big_three.csv")
write.csv(big4, file = "big_4.csv")

#           _.-^~~^^^`~-,_,,~''''''```~,''``~'``~,
#   ______,'  -o  :.  _    .          ;     ,'`,  `.
# (      -\.._,.;;'._ ,(   }        _`_-_,,    `, `,
# ``~~~~~~'   ((/'((((____/~~~~~~'(,(,___>      `~'


 edd %>%
  group_by(t_subreddit) %>%
  summarize(avg_order = mean(no_conseq_iln, na.rm = TRUE),
            avg_color = mean(nonchalance_iln, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(t_subreddit, avg_order), 
             y = avg_order, 
             fill = avg_color)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "white",
                      high = "purple",
                      name = "Nonchalance") +
  labs(title = 
      "All threads:  No consequences and Nonchalance",
       x = "Discussion space",
       y = "No consequences") 

 
   edd %>%
  group_by(t_subreddit) %>%
  summarize(avg_order = mean(giant_no_engagement_iln, na.rm = TRUE),
            avg_color = mean(nonchalance_iln, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(t_subreddit, avg_order), 
             y = avg_order, 
             fill = avg_color)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "white",
                      high = "blue",
                      name = "Nonchalance") +
  labs(title = 
      "All threads:  Disengagement and Nonchalance",
       x = "Discussion space",
       y = "Disengagement") 
 

     edd %>%
  group_by(t_subreddit) %>%
  summarize(avg_order = mean(ai_iln, na.rm = TRUE),
            avg_color = mean(nonchalance_iln, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(t_subreddit, avg_order), 
             y = avg_order, 
             fill = avg_color)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "white",
                      high = "darkgreen",
                      name = "Nonchalance") +
  labs(title = 
      "AI and Nonchalance",
       x = "Discussion space",
       y = "AI") 
 
 
  mydata %>%
  group_by(t_subreddit) %>%
  summarize(avg_order = mean(giant_no_engagement_iln, na.rm = TRUE),
            avg_color = mean(nonchalance_iln, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(t_subreddit, avg_order), 
             y = avg_order, 
             fill = avg_color)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "white",
                      high = "darkgreen",
                      name = "Nonchalance") +
  labs(title = 
      "High relevance threads:  Disengagment and Nonchalance",
       x = "Discussion space",
       y = "Disengagement") 
 
 

  mydata %>%
  group_by(t_subreddit) %>%
  summarize(avg_order = mean(giant_no_engagement_iln, na.rm = TRUE),
            avg_color = mean(nonchalance_iln, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(t_subreddit, avg_order), 
             y = avg_order, 
             fill = avg_color)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "white",
                      high = "blue",
                      name = "Nonchalance") +
  labs(title = 
      "High relevance threads:  Disengagment and Nonchalance",
       x = "Discussion space",
       y = "Disengagement") 

  mydata %>%
  group_by(t_subreddit) %>%
  summarize(avg_order = mean(no_conseq_iln, na.rm = TRUE),
            avg_color = mean(nonchalance_iln, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(t_subreddit, avg_order), 
             y = avg_order, 
             fill = avg_color)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "white",
                      high = "purple",
                      name = "Nonchalance") +
  labs(title = 
      "High relevance threads:  No consequences and Nonchalance",
       x = "Discussion space",
       y = "No consequences") 
