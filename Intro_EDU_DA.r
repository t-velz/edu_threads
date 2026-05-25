##____________________________________________________________________________________________________
# Intro_EDU_DA.R
# Author: Will Hoffman
# Date: 2026-05-24
##___________________________________________________________________________________________________

#Gathering Libraries and Data ----
library(readr)
library(dplyr)
library(tidyverse)
library(pscl)

edd <- read_csv("desktop/R_Things/edd15subs_indices.csv")

sum(edd$t_comments)
table(edd$t_subreddit)

#______________________________________________________________________________________________________________
# Time Vars Creation
library(corrplot)

edd <- edd %>%
  mutate(
    t_date_parsed = coalesce(
      ymd(t_date, quiet = TRUE),
      mdy(t_date, quiet = TRUE),
      dmy(t_date, quiet = TRUE)
    )
  ) %>%
  mutate(
    days_since = as.numeric(difftime(t_date_parsed, min(t_date_parsed, na.rm = TRUE), units = "days")),
    time_decimal = lubridate::decimal_date(t_date_parsed),
    time_rank = dplyr::dense_rank(t_date_parsed)
  )


library(dplyr)

#______________________________________________________________________________________________________________
# Whole sample correlations ----
group_vars <- edd |>
  dplyr::select(
    not_engaged_iln,
    nonchalance_iln,
    no_conseq_iln,
    bad_parent_iln,
    phone_distraction_iln,
    mental_health_iln,
    helicopter_parents_iln,
    extrinsic_iln,
    admin_prob_iln)

M <- stats::cor(group_vars, use = "complete.obs")
par(mar = c(2, 2, 2, 2))

# rounding my matrix to 2 decimals for cleaner numbers
M2 <- round(M, 2)

M2

# plot with larger numbers and label text
corrplot::corrplot(
  M2,
  method = "number",
  type = "lower",
  tl.col = "black",      
  number.cex = 0.5,    
  number.font = 2
)
mtext("Correlations Among Indices (Whole Sample)", at=2.5, line=-0.5, cex=2)

cor(edd$not_engaged_iln, edd$no_conseq_iln)

#______________________________________________________________________________________________________________
#Teacher and Professor Subsamples
teachers_only <- filter(edd, Teachers == 1)

group_vars1 <- teachers_only |>
  dplyr::select(
    not_engaged_iln,
    nonchalance_iln,
    no_conseq_iln,
    bad_parent_iln,
    phone_distraction_iln,
    mental_health_iln,
    helicopter_parents_iln,
    extrinsic_iln,
    admin_prob_iln)

T <- stats::cor(group_vars1, use = "complete.obs")
par(mar = c(2, 2, 2, 2))

# rounding my matrix to 2 decimals for cleaner numbers
T2 <- round(T, 2)

T2

# plot with larger numbers and label text
corrplot::corrplot(
  T2,
  method = "number",
  type = "lower",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 1.0,        
  number.cex = 0.5,    
  number.font = 2,     
  cl.cex = 0.8         
)
mtext("Correlations Among Indices (Teacher Subsample)", at=2.5, line=-0.5, cex=2)

#______________________________________________________________________________________________________________
#Professor only subsample
professors_only <- filter(edd, Professors == 1)

group_vars2 <- professors_only |>
  dplyr::select(
    not_engaged_iln,
    nonchalance_iln,
    no_conseq_iln,
    bad_parent_iln,
    phone_distraction_iln,
    mental_health_iln,
    helicopter_parents_iln,
    extrinsic_iln,
    admin_prob_iln)

P <- stats::cor(group_vars2, use = "complete.obs")
par(mar = c(2, 2, 2, 2))  

# rounding my matrix to 2 decimals for cleaner numbers
P2 <- round(P, 2)
P2

# plot with larger numbers and label text

corrplot::corrplot(
  P2,
  method = "number",
  type = "lower",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 1.0,        
  number.cex = 0.5,    
  number.font = 2,     
  cl.cex = 0.8         
)
mtext("Correlations Among Indices (Professor Subsample)", at=2.5, line=-0.5, cex=2)

#______________________________________________________________________________________________________________
# Sample Correlations With Conversation + Temporal Variables

full_model_vars <- edd |>
  dplyr::select(
    not_engaged_iln,
    nonchalance_iln,
    no_conseq_iln,
    bad_parent_iln,
    phone_distraction_iln,
    mental_health_iln,
    Teachers,
    Professors,
    college,
    education,
    score,
    t_comments,
    days_since
    )

F <- stats::cor(full_model_vars, use = "complete.obs")
par(mar = c(2, 2, 2, 2))

F2 <- round(F, 2)
F2

corrplot::corrplot(
  F2,
  method = "number",
  type = "lower",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.8,        
  number.cex = 0.5,    
  number.font = 2,     
  cl.cex = 0.8         
)
mtext("Correlations of Full Sample ", at=2.5, line=-0.5, cex=2)

#______________________________________________________________________________________________________________
#Regressions ---- 
#______________________________________________________________________________________________________________

summary(lm(not_engaged_iln ~
   nonchalance_iln +
   no_conseq_iln +
   bad_parent_iln +
   phone_distraction_iln +
   mental_health_iln +
   Teachers +
   Professors +
   college +
   education +
   score +
   t_comments + days_since, data = edd))

#______________________________________________________________________________________________________________
#Graphing Results
#Full Sample Graphs
plot_dd <- edd %>%
  select(not_engaged_iln, nonchalance_iln, no_conseq_iln, 
    mental_health_iln, phone_distraction_iln) %>%
  pivot_longer(cols = c(nonchalance_iln, no_conseq_iln, mental_health_iln, phone_distraction_iln),
               names_to = "predictor",
               values_to = "value")

d <- ggplot(plot_dd, aes(x = value, y = not_engaged_iln)) +
  geom_point(alpha = 0.45, size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~predictor, scales = "free_x", labeller = labeller(
    predictor = c(nonchalance_iln = "Nonchalant Index (r = .18)",
                  no_conseq_iln = "No Consequences Index (r = .23)",
                  mental_health_iln = "Mental Health Index (r = .27)",
                  phone_distraction_iln = "Phone Distraction Index (r = .18)")
  )) +
  labs(title = "Disengagement vs Predictors in the Full Sample",
       x = "Predictor value (log-scaled indices)",
       y = "Disengagement Index") +
  theme_minimal()

d

#______________________________________________________________________________________________________________
# Subsample Graphs

Edu_specific <- edd %>%
  filter(Teachers == 1 | Professors == 1 | college == 1)

table(Edu_specific$t_subreddit)

plot_df <- Edu_specific %>%
  select(not_engaged_iln, nonchalance_iln, no_conseq_iln, 
    mental_health_iln, phone_distraction_iln) %>%
  pivot_longer(cols = c(nonchalance_iln, no_conseq_iln, mental_health_iln, phone_distraction_iln),
               names_to = "predictor",
               values_to = "value")

f <- ggplot(plot_df, aes(x = value, y = not_engaged_iln)) +
  geom_point(alpha = 0.45, size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~predictor, scales = "free_x", labeller = labeller(
    predictor = c(nonchalance_iln = "Nonchalant Index (r = .20)",
                  no_conseq_iln = "No Consequences Index (r = .25)",
                  mental_health_iln = "Mental Health Index (r = .24)",
                  phone_distraction_iln = "Phone Distraction Index (r = .18)")
  )) +
  labs(title = "Disengagement vs Predictors in Education Subsample (Teachers, Professors, & College)",
       x = "Predictor value (log-scaled indices)",
       y = "Disengagement Index") +
  theme_minimal()

f

summary(lm(not_engaged_iln ~
   nonchalance_iln +
   no_conseq_iln +
   bad_parent_iln +
   phone_distraction_iln +
   mental_health_iln +
   score +
   t_comments + 
  days_since +
  profan_count_rt, data = Edu_specific))



