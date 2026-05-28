
---------------------------------------------------------#
# Creating TWO waffle plots using reddit data (one per data collection period)
# https://materialui.co/colors
#
# original code in 2025 by: Miracle Sammons
# updated in 2026 by Ayesha Akbar and Ted Welser
##-----------------------------------------------------#
#---------------------------------------------------------#

#          _.-^~~^^^`~-,_,,~''''''```~,''``~'``~,
#  ______,'  -o  :.  _    .          ;     ,'`,  `.
# (      -\.._,.;;'._ ,(   }        _`_-_,,    `, `,
#  ``~~~~~~'   ((/'((((____/~~~~~~'(,(,___>      `~'
#---------------------------------------------------------#                     

# add as needed to list
install.packages(c("ggnewscale", "patchwork", "pscl"))

#Load necessary libraries for data wrangling and plotting, etc

library(tidyverse)     # Data manipulation and visualization
library(lubridate)     # Handling date/time
library(ggnewscale)    # Allows multiple fill/color scales in a ggplot
library(patchwork)     # Combines multiple plots into one
library(pscl)          # Political Science Computational Laboratory
library(data.table)    # fast read and write

#   Load data from local

edd<- fread("edd15subs_indices.csv")

sum(edd$t_comments)
table(edd$t_subreddit)


##-------------------------------------------------------#
# Creating a waffle plot using reddit data
#          https://materialui.co/colors
#
#  original code in 2025 by: Miracle Sammons
#  updated in 2026 by Ayesha Akbar and Ted Welser
##-----------------------------------------------------#



# ----------------------------------------
# STEP 0: For updated version, 2026
# use filter to create two datasets by dividing
# at gap between the two ~ year long data collection
# periods.  We will make two versions of the plot
# one for each time period that are otherwise 
# similar.   The API changed and reduced the #
# of threads allowed per year period to ~500 per
# subreddit, rather than 1000.  So we want to
# avoid conflating sampling change with empirical change
# After splitting into the
# samples we should explore making a random sample
# of 480 per each subreddit.       
# ----------------------------------------


#   first time through try running the code as is
#    notice if it throws an error, I remember there
#  was a depracated funtion that would still work after
#  the error that should be updated.  




# Load necessary libraries
library(tidyverse)
library(lubridate)

# NOTE: if ggnewscale or patchwork are not installed yet, run:
# install.packages(c("ggnewscale", "patchwork"))
library(ggnewscale)
library(patchwork)


# ----------------------------------------
# STEP 0: Context for the 2026 update
# The combined dataset (edd_combined.csv) spans two data collection periods:
#   Period 1 (2025): ~Feb 2025 – ~Aug 2025  (original ~1000/subreddit API cap)
#   Period 2 (2026): ~Sep 2025 – ~Feb 2026  (reduced ~500/subreddit API cap)
# We split at the midpoint gap (~Sep 2025), then take a random sample of
# 480 threads per subreddit per period to make the two plots comparable.
# We produce one waffle plot per period, then combine side-by-side.
# ----------------------------------------

names(edd)

class(edd$date)


# ----------------------------------------
# STEP 1: clean the data
# ----------------------------------------

edd<- fread("edd15subs_indices.csv")


#_______________________________________________________________#
#          make needed changes to variables, additions          #
#_______________________________________________________________#

#   fix date var using lubridate  #
library(lubridate)

sum(grepl("deleted", edd$date, ignore.case = TRUE))
sum(grepl("NA", edd$date, ignore.case = TRUE))


edd$date_top_comment <- as.Date(parse_date_time(edd$date, orders = c('mdy', 'ymd')))
edd$date_thread <- as.Date(parse_date_time(edd$t_date, orders = c('mdy', 'ymd')))

# Check for NA or deleted cases
sum(is.na(edd$date_top_comment))
sum(is.na(edd$date_thread))

# Check the range
range(edd$date_top_comment)
range(edd$date_thread)

class(edd$date_top_comment)
class(edd$date_thread)

sum(edd$t_comments)
table(edd$t_subreddit)

edd_raw <- edd |>
  filter(t_subreddit %in% c("Professors", "Teachers", "college"),
         !is.na(date_thread))

sum(edd_raw$t_comments)
table(edd_raw$t_subreddit)


#   make the split into two datasets on Jan 1 2025

# NOTE on column changes from 2025 version:
#   - Subreddit booleans (sub_teachers, sub_professors, sub_college) are no longer
#     pre-built; we now derive them from t_subreddit
#   - Disengagement columns are now visible_disen_iln and visible_disen_ind
#     (replacing disengage.iln and disengage.i)

# ----------------------------------------
# STEP 0.5:   Filter to include relevant subreddits only
# ----------------------------------------

edd_raw <- edd_raw %>%
  mutate(
    date_thread = as.Date(date_thread),

    # Disengagement: average of the two measures (same as original)
    # Derive subreddit booleans from t_subreddit (not pre-built in local CSV)
    sub_teachers   = t_subreddit == "Teachers",
    sub_professors = t_subreddit == "Professors",
    #sub_professors = t_subreddit %in% c("Professors", "AskAcademia", "GradSchool"),
    sub_college    = t_subreddit == "college",

    # Disengagement columns (local CSV uses these names)
    disengage_combined = rowMeans(select(., not_engaged_iln, visible_disen_iln), na.rm = TRUE)
  ) %>%
  filter(!is.na(disengage_combined)) %>%
  arrange(date_thread)

plot(edd_raw$date_thread, edd_raw$ai_rt)
plot(edd_raw$date_thread, edd_raw$disengage_combined)

# ----------------------------------------
# STEP 2: Split into two data collection periods
# ----------------------------------------
# Period 1: Feb 2025 – Aug 2025 (original API limit ~1000/sub)
# Period 2: Sep 2025 – Feb 2026 (reduced API limit ~500/sub)
# Cutoff chosen at Sep 1 2025 based on the sampling change noted in Step 0


SPLIT_DATE <- as.Date("2025-01-01")

edd_p1 <- edd_raw %>% filter(date_thread < SPLIT_DATE)
edd_p2 <- edd_raw %>% filter(date_thread >= SPLIT_DATE)

cat("Period 1 rows:", nrow(edd_p1), "\n")
cat("Period 2 rows:", nrow(edd_p2), "\n")


# ----------------------------------------
# STEP 3: Random sample of 480 per subreddit per period
# ----------------------------------------
# This equalizes sampling across the API-cap change so we can compare.

set.seed(42)  # for reproducibility

sample_480 <- function(df) {
  df %>%
    group_by(t_subreddit) %>%
    slice_sample(n = 480, replace = FALSE) %>%
    ungroup()
}

# Only sample if period has enough rows; otherwise take all
edd_p1_samp <- if (nrow(edd_p1) >= 480) sample_480(edd_p1) else edd_p1
edd_p2_samp <- if (nrow(edd_p2) >= 480) sample_480(edd_p2) else edd_p2

cat("Period 1 sample rows:", nrow(edd_p1_samp), "\n")
cat("Period 2 sample rows:", nrow(edd_p2_samp), "\n")


plot(edd_p1_samp$date_thread, edd_p1_samp$disengage_combined)
plot(edd_p2_samp$date_thread, edd_p2_samp$disengage_combined)
# ----------------------------------------
# STEP 4: Month abbreviation helper
# ----------------------------------------

month_abbrev <- function(dates) {
  abbr <- format(dates, "%b")
  abbr <- str_replace_all(abbr, c(
    "Jan" = "Ja", "Feb" = "Fb", "Mar" = "Mr", "Apr" = "Ap",
    "May" = "My", "Jun" = "Jn", "Jul" = "Jl", "Aug" = "Au",
    "Sep" = "Sp", "Oct" = "Oc", "Nov" = "Nv", "Dec" = "De"
  ))
  paste0(abbr, format(dates, "%d"))
}


# ----------------------------------------
# STEP 5: Prepare a single period's data for plotting
# (deciles, time labels, quartile categories)
# ----------------------------------------

prepare_period <- function(df) {

  df <- df %>%
    arrange(date_thread) %>%
    mutate(
      row_id = row_number(),
      decile = ntile(row_id, 10)
    )

  # Build decile start-date labels
  decile_labels <- df %>%
    group_by(decile) %>%
    summarize(start_date = min(date_thread), .groups = "drop") %>%
    mutate(
      time_slice_label = month_abbrev(start_date),
      decile = as.integer(decile)
    )

  df <- df %>%
    mutate(decile = as.integer(decile)) %>%
    left_join(decile_labels, by = "decile")

  stopifnot("time_slice_label" %in% colnames(df))

  df <- df %>%
    mutate(time_slice = factor(time_slice_label, levels = decile_labels$time_slice_label))

  # Quartile categories (based on positive disengagement values only)
  qtiles <- quantile(
    df$disengage_combined[df$disengage_combined > 0],
    probs = c(0.25, 0.5, 0.75),
    na.rm = TRUE
  )

  df <- df %>%
    mutate(
      dis_cat = case_when(
        disengage_combined == 0             ~ "None",
        disengage_combined <= qtiles[1]     ~ "Q1",
        disengage_combined <= qtiles[2]     ~ "Q2",
        disengage_combined <= qtiles[3]     ~ "Q3",
        disengage_combined >  qtiles[3]     ~ "Q4"
      ),
      dis_cat = factor(dis_cat, levels = c("None", "Q1", "Q2", "Q3", "Q4"))
    )

  list(df = df, decile_labels = decile_labels)
}

prep_p1 <- prepare_period(edd_p1_samp)
prep_p2 <- prepare_period(edd_p2_samp)


# ----------------------------------------
# STEP 6: Function to build waffle grid data per group
# ----------------------------------------

make_waffle_df <- function(df, subreddit_column, group_label) {
  df %>%
    filter(.data[[subreddit_column]] == TRUE) %>%
    group_by(time_slice) %>%
    arrange(dis_cat, date_thread) %>%
    mutate(
      id    = row_number(),
      x     = (id - 1) %% 4,
      y     = floor((id - 1) / 4),
      group = group_label
    ) %>%
    ungroup()
}

build_all_waffle <- function(df) {
  df_teachers   <- make_waffle_df(df, "sub_teachers",   "Teachers")
  df_professors <- make_waffle_df(df, "sub_professors", "Professors")
  df_college    <- make_waffle_df(df, "sub_college",    "College")

  bind_rows(df_teachers, df_professors, df_college) %>%
    mutate(group = factor(group, levels = c("Teachers", "Professors", "College")))
}

all_waffle_p1 <- build_all_waffle(prep_p1$df)
all_waffle_p2 <- build_all_waffle(prep_p2$df)


# ----------------------------------------
# STEP 7: Color palettes
# LINK FOR COLOR COMBOS: https://materialui.co/colors
# ----------------------------------------

pal_teachers <- c("None" = "#f6f5fb", "Q1" = "#cab2d6", "Q2" = "#9e9ac8", "Q3" = "#6a51a3", "Q4" = "#3f007d")
pal_profs    <- c("None" = "#f7fcf5", "Q1" = "#c7e9c0", "Q2" = "#74c476", "Q3" = "#238b45", "Q4" = "#00441b")
pal_college  <- c("None" = "#f0f7fb", "Q1" = "#bdd7e7", "Q2" = "#6baed6", "Q3" = "#2171b5", "Q4" = "#08306b")


# ----------------------------------------
# STEP 8: Legend plot (shared across both panels)
# ----------------------------------------

grey_pal <- c("None" = "#f0f0f0", "Q1" = "#bdbdbd", "Q2" = "#737373", "Q3" = "#404040", "Q4" = "#0d0d0d")

legend_df <- tibble(
  dis_cat = factor(c("None", "Q1", "Q2", "Q3", "Q4"), levels = c("None", "Q1", "Q2", "Q3", "Q4")),
  x = 1:5,
  y = 1
)

legend_plot <- ggplot() +
  geom_text(aes(x = 0, y = 1, label = "Quartile Range:"), hjust = 1, size = 3, fontface = "bold") +
  geom_tile(
    data = legend_df,
    aes(x = x, y = y, fill = dis_cat),
    color = "black", linewidth = 0.3, width = 0.6, height = 0.6  # linewidth replaces deprecated size
  ) +
  geom_text(data = legend_df, aes(x = x, y = y - 0.7, label = dis_cat), color = "black", size = 2) +
  scale_fill_manual(values = grey_pal) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(5, 5, 5, 5)) +
  coord_fixed(expand = FALSE, xlim = c(0, 6), ylim = c(0.1, 1.5))


# ----------------------------------------
# STEP 9: Function to build a single waffle plot panel
# ----------------------------------------

make_waffle_plot <- function(all_waffle, title_label) {

  df_t <- all_waffle %>% filter(group == "Teachers")
  df_p <- all_waffle %>% filter(group == "Professors")
  df_c <- all_waffle %>% filter(group == "College")

  ggplot() +
    # Teachers layer
    geom_tile(data = df_t, aes(x = x, y = y, fill = dis_cat),
              color = "black", linewidth = 0.1, width = 0.9, height = 0.9) +
    scale_fill_manual(values = pal_teachers, drop = FALSE) +
    ggnewscale::new_scale_fill() +

    # Professors layer
    geom_tile(data = df_p, aes(x = x, y = y, fill = dis_cat),
              color = "black", linewidth = 0.1, width = 0.9, height = 0.9) +
    scale_fill_manual(values = pal_profs, drop = FALSE) +
    ggnewscale::new_scale_fill() +

    # College layer
    geom_tile(data = df_c, aes(x = x, y = y, fill = dis_cat),
              color = "black", linewidth = 0.1, width = 0.9, height = 0.9) +
    scale_fill_manual(values = pal_college, drop = FALSE) +

    facet_grid(group ~ time_slice, switch = "x") +
    coord_fixed() +
    scale_y_continuous(
      breaks = seq(0, 25, by = 5),
      labels = function(y) y * 4,
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(title = title_label) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid      = element_blank(),
      axis.text.x     = element_blank(),
      axis.title.x    = element_blank(),
      axis.title.y    = element_blank(),
      axis.text.y     = element_text(size = 8),
      axis.ticks.y    = element_line(),
      legend.position = "none",
      strip.background = element_blank(),
      strip.placement  = "outside",
      strip.text.x     = element_text(size = 7, face = "bold", margin = margin(t = 2, b = 0)),
      strip.text.y     = element_blank(),
      plot.title       = element_text(size = 11, face = "bold", hjust = 0.5)
    )
}

waffle_p1 <- make_waffle_plot(all_waffle_p1, "2025 Collection Period (n=480/subreddit)")
waffle_p2 <- make_waffle_plot(all_waffle_p2, "2026 Collection Period (n=480/subreddit)")


# ----------------------------------------
# STEP 10: Combine legend + two plots
# ----------------------------------------

final_plot <- legend_plot / (waffle_p1 | waffle_p2) +
  plot_layout(heights = c(1, 20))


# ----------------------------------------
# STEP 11: Display
# ----------------------------------------

print(final_plot)

# Optional: save to file
# ggsave("waffle_2026_combined.png", final_plot, width = 20, height = 10, dpi = 150)
