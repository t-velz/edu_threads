
# ---------------------------------------------------------#
#   edu 2026 proj
#   Ted, Will, Tess, Ayesha
# ---------------------------------------------------------#

#          _.-^~~^^^`~-,_,,~''''''```~,''``~'``~,
#  ______,'  -o  :.  _    .          ;     ,'`,  `.
# (      -\.._,.;;'._ ,(   }        _`_-_,,    `, `,
#  ``~~~~~~'   ((/'((((____/~~~~~~'(,(,___>      `~'
# ---------------------------------------------------------#
##-------------------------------------------------------#
# Creating TWO waffle plots using reddit data
#          https://materialui.co/colors
#
#  original code in 2025 by: Miracle Sammons
#  updated in 2026 by Ayesha Akbar and Ted Welser
##-----------------------------------------------------#

# Load necessary libraries
library(tidyverse)
library(lubridate)

# NOTE: if ggnewscale or patchwork are not installed, run:
# install.packages(c("ggnewscale", "patchwork"))
library(ggnewscale)
library(patchwork)


# ----------------------------------------
# STEP 0: For updated version, 2026
# use filter to create two datasets by dividing
# at gap between the two ~year long data collection
# periods. We will make two versions of the plot
# one for each time period that are otherwise
# similar. The API changed and reduced the #
# of threads allowed per year period to ~500 per
# subreddit, rather than 1000. So we want to
# avoid conflating sampling change with empirical change.
# After splitting into the samples we should explore
# making a random sample of 480 per each subreddit.
#
# 2026 update notes (Ayesha):
# - Dataset: edd15subs_indices.csv (Jan 2024 - Sep 2025, 15 subreddits)
# - Split at Jan 1 2025: Period 1 = Jan-Dec 2024, Period 2 = Jan-Sep 2025
# - Period 1 has 6 subreddits (~500-1000/sub), Period 2 has 15 (~430-500/sub)
# - Sample 430/subreddit per period (limited by smallest Period 2 sub)
# - Dates in M/D/YYYY format, parsed with mdy()
# - Disengagement: average of visible_disen_iln and verbal_disengagement_iln
# - Subreddit boolean columns derived from t_subreddit
# - size= updated to linewidth= in geom_tile() (ggplot2 deprecation fix)
# ----------------------------------------


# ----------------------------------------
# STEP 1: Load and clean the data
# ----------------------------------------

# Load the 2026 combined dataset
edd_raw <- read_csv("edd15subs_indices.csv")

edd_raw <- edd_raw %>%
  mutate(
    # Dates are in M/D/YYYY format in this dataset
    t_date = mdy(t_date),

    # Derive subreddit boolean columns from t_subreddit
    sub_teachers   = t_subreddit == "Teachers",
    sub_professors = t_subreddit %in% c("Professors", "AskAcademia", "GradSchool", "academia"),
    sub_college    = t_subreddit == "college",

    # Disengagement: average of two measures
    disengage_combined = rowMeans(
      select(., visible_disen_iln, verbal_disengagement_iln),
      na.rm = TRUE
    )
  ) %>%
  filter(!is.na(disengage_combined)) %>%
  arrange(t_date)


# ----------------------------------------
# STEP 2: Split into two data collection periods
# ----------------------------------------
# Period 1: Jan 2024 - Dec 2024 (original ~1000/sub API limit, 6 subreddits)
# Period 2: Jan 2025 - Sep 2025 (reduced ~500/sub API limit, 15 subreddits)

SPLIT_DATE <- as.Date("2025-01-01")

edd_p1 <- edd_raw %>% filter(t_date < SPLIT_DATE)
edd_p2 <- edd_raw %>% filter(t_date >= SPLIT_DATE)

cat("Period 1 rows:", nrow(edd_p1), "\n")
cat("Period 2 rows:", nrow(edd_p2), "\n")


# ----------------------------------------
# STEP 3: Random sample of 430 per subreddit per period
# ----------------------------------------
# 430 chosen as the sample size because the smallest
# subreddit in Period 2 (highereducation) has ~134 rows,
# so we sample up to 430 where available (replace=FALSE)

set.seed(42)

sample_n_per_sub <- function(df, n = 430) {
  df %>%
    group_by(t_subreddit) %>%
    slice_sample(n = n, replace = FALSE) %>%
    ungroup()
}

edd_p1_samp <- sample_n_per_sub(edd_p1, 430)
edd_p2_samp <- sample_n_per_sub(edd_p2, 430)

cat("Period 1 sample rows:", nrow(edd_p1_samp), "\n")
cat("Period 2 sample rows:", nrow(edd_p2_samp), "\n")


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
# ----------------------------------------

prepare_period <- function(df) {
  df <- df %>%
    arrange(t_date) %>%
    mutate(
      row_id = row_number(),
      decile = ntile(row_id, 10)
    )

  decile_labels <- df %>%
    group_by(decile) %>%
    summarize(start_date = min(t_date), .groups = "drop") %>%
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

  qtiles <- quantile(
    df$disengage_combined[df$disengage_combined > 0],
    probs = c(0.25, 0.5, 0.75),
    na.rm = TRUE
  )

  df <- df %>%
    mutate(
      dis_cat = case_when(
        disengage_combined == 0         ~ "None",
        disengage_combined <= qtiles[1] ~ "Q1",
        disengage_combined <= qtiles[2] ~ "Q2",
        disengage_combined <= qtiles[3] ~ "Q3",
        disengage_combined >  qtiles[3] ~ "Q4"
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
    arrange(dis_cat, t_date) %>%
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
    color = "black", linewidth = 0.3, width = 0.6, height = 0.6
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
    geom_tile(data = df_t, aes(x = x, y = y, fill = dis_cat),
              color = "black", linewidth = 0.1, width = 0.9, height = 0.9) +
    scale_fill_manual(values = pal_teachers, drop = FALSE) +
    ggnewscale::new_scale_fill() +

    geom_tile(data = df_p, aes(x = x, y = y, fill = dis_cat),
              color = "black", linewidth = 0.1, width = 0.9, height = 0.9) +
    scale_fill_manual(values = pal_profs, drop = FALSE) +
    ggnewscale::new_scale_fill() +

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
      panel.grid       = element_blank(),
      axis.text.x      = element_blank(),
      axis.title.x     = element_blank(),
      axis.title.y     = element_blank(),
      axis.text.y      = element_text(size = 8),
      axis.ticks.y     = element_line(),
      legend.position  = "none",
      strip.background = element_blank(),
      strip.placement  = "outside",
      strip.text.x     = element_text(size = 7, face = "bold", margin = margin(t = 2, b = 0)),
      strip.text.y     = element_blank(),
      plot.title       = element_text(size = 11, face = "bold", hjust = 0.5)
    )
}

waffle_p1 <- make_waffle_plot(all_waffle_p1, "2024 Collection Period (n=430/subreddit)")
waffle_p2 <- make_waffle_plot(all_waffle_p2, "2025 Collection Period (n=430/subreddit)")


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
