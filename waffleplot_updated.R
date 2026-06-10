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
# Creating waffle plots for two subreddit groups across
# four disengagement-related variables
#          https://materialui.co/colors
#
#  original code in 2025 by: Miracle Sammons
#  updated in 2026 by Ayesha Akbar and Ted Welser
##-----------------------------------------------------#

library(tidyverse)
library(lubridate)

# install.packages(c("ggnewscale", "patchwork"))
library(ggnewscale)
library(patchwork)


# ----------------------------------------
# STEP 0: Overview
# Two subreddit groups, four variables each.
# Group 1: Teachers, Professors, College
# Group 2: GradSchool, Academia, AskAcademia
# Variables: not_engaged_iln, no_conseq_iln,
#            falling_behind_iln, mental_health_iln
# Data split at Jan 1 2025, sampled at 430/subreddit.
# ----------------------------------------


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
# STEP 5: Prepare data for a given variable and subreddit group
# ----------------------------------------

prepare_group <- function(df, subreddits, var_col) {

  df <- df %>%
    filter(t_subreddit %in% subreddits) %>%
    rename(index_var = !!sym(var_col)) %>%
    filter(!is.na(index_var)) %>%
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
    left_join(decile_labels, by = "decile") %>%
    mutate(time_slice = factor(time_slice_label, levels = decile_labels$time_slice_label))

  qtiles <- quantile(
    df$index_var[df$index_var > 0],
    probs = c(0.25, 0.5, 0.75),
    na.rm = TRUE
  )

  df <- df %>%
    mutate(
      dis_cat = case_when(
        index_var == 0          ~ "None",
        index_var <= qtiles[1]  ~ "Q1",
        index_var <= qtiles[2]  ~ "Q2",
        index_var <= qtiles[3]  ~ "Q3",
        index_var >  qtiles[3]  ~ "Q4"
      ),
      dis_cat = factor(dis_cat, levels = c("None", "Q1", "Q2", "Q3", "Q4"))
    )

  df
}


# ----------------------------------------
# STEP 6: Build waffle grid data per subreddit
# ----------------------------------------

make_waffle_df <- function(df, sub_name) {
  df %>%
    filter(t_subreddit == sub_name) %>%
    group_by(time_slice) %>%
    arrange(dis_cat, t_date) %>%
    mutate(
      id    = row_number(),
      x     = (id - 1) %% 4,
      y     = floor((id - 1) / 4),
      group = sub_name
    ) %>%
    ungroup()
}


# ----------------------------------------
# STEP 7: Color palettes — one per subreddit
# ----------------------------------------

pal_teachers   <- c("None" = "#f6f5fb", "Q1" = "#cab2d6", "Q2" = "#9e9ac8", "Q3" = "#6a51a3", "Q4" = "#3f007d")
pal_professors <- c("None" = "#f7fcf5", "Q1" = "#c7e9c0", "Q2" = "#74c476", "Q3" = "#238b45", "Q4" = "#00441b")
pal_college    <- c("None" = "#f0f7fb", "Q1" = "#bdd7e7", "Q2" = "#6baed6", "Q3" = "#2171b5", "Q4" = "#08306b")
pal_gradschool <- c("None" = "#fff5eb", "Q1" = "#fdd0a2", "Q2" = "#fd8d3c", "Q3" = "#d94801", "Q4" = "#7f2704")
pal_academia   <- c("None" = "#f7f7f7", "Q1" = "#cccccc", "Q2" = "#737373", "Q3" = "#252525", "Q4" = "#000000")
pal_askacad    <- c("None" = "#fff5f0", "Q1" = "#ffc09e", "Q2" = "#fc6e51", "Q3" = "#c0392b", "Q4" = "#7b241c")

# Map subreddit name to palette
pal_map <- list(
  "Teachers"    = pal_teachers,
  "Professors"  = pal_professors,
  "college"     = pal_college,
  "GradSchool"  = pal_gradschool,
  "academia"    = pal_academia,
  "AskAcademia" = pal_askacad
)


# ----------------------------------------
# STEP 8: Legend plot
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
# STEP 9: Function to build one waffle plot for a group of subreddits
# ----------------------------------------

make_group_waffle <- function(df, subreddits, var_col, title_label) {

  prepped <- prepare_group(df, subreddits, var_col)

  # Build waffle df for each subreddit
  waffle_list <- lapply(subreddits, function(s) make_waffle_df(prepped, s))

  # Start ggplot
  p <- ggplot()

  for (i in seq_along(subreddits)) {
    sub <- subreddits[[i]]
    pal <- pal_map[[sub]]
    wdf <- waffle_list[[i]]

    if (i == 1) {
      p <- p +
        geom_tile(data = wdf, aes(x = x, y = y, fill = dis_cat, group = group),
                  color = "white", linewidth = 0.05, width = 1, height = 1) +
        scale_fill_manual(values = pal, drop = FALSE)
    } else {
      p <- p +
        ggnewscale::new_scale_fill() +
        geom_tile(data = wdf, aes(x = x, y = y, fill = dis_cat, group = group),
                  color = "white", linewidth = 0.05, width = 1, height = 1) +
        scale_fill_manual(values = pal, drop = FALSE)
    }
  }

  # Combine all waffle data for faceting
  all_waffle <- bind_rows(waffle_list) %>%
    mutate(group = factor(group, levels = subreddits))

  p +
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
      strip.text.y     = element_text(size = 8, face = "bold"),
      plot.title       = element_text(size = 11, face = "bold", hjust = 0.5)
    )
}


# ----------------------------------------
# STEP 10: Define groups and variables
# ----------------------------------------

group1_subs <- c("Teachers", "Professors", "college")
group2_subs <- c("GradSchool", "academia", "AskAcademia")

variables <- list(
  "not_engaged_iln"    = "Not Engaged",
  "no_conseq_iln"      = "No Consequences",
  "falling_behind_iln" = "Falling Behind",
  "mental_health_iln"  = "Mental Health"
)


# ----------------------------------------
# STEP 11: Build and display all 8 plots
# ----------------------------------------

for (var_col in names(variables)) {
  var_label <- variables[[var_col]]

  p1 <- make_group_waffle(edd, group1_subs, var_col,
                          paste0(var_label, " — Teachers / Professors / College"))
  p2 <- make_group_waffle(edd, group2_subs, var_col,
                          paste0(var_label, " — GradSchool / Academia / AskAcademia"))

  final <- legend_plot / p1 / p2 +
    plot_layout(heights = c(1, 20, 20))

  print(final)
  cat("Displayed:", var_label, "\n")
}


# ----------------------------------------
# STEP 11: Build and display all 8 plots
# ----------------------------------------

for (var_col in names(variables)) {
  var_label <- variables[[var_col]]

  p1 <- make_group_waffle(edd, group1_subs, var_col,
                          paste0(var_label, " — College / Professors / Teachers"))
  p2 <- make_group_waffle(edd, group2_subs, var_col,
                          paste0(var_label, " — Academia / AskAcademia / GradSchool"))

  final <- legend_plot / p1 / p2 +
    plot_layout(heights = c(1, 60, 60))

  print(final)
  cat("Displayed:", var_label, "\n")
}



# Optional: save each plot
# for (var_col in names(variables)) {
#   var_label <- variables[[var_col]]
#   p1 <- make_group_waffle(edd, group1_subs, var_col, paste0(var_label, " — Group 1"))
#   p2 <- make_group_waffle(edd, group2_subs, var_col, paste0(var_label, " — Group 2"))
#   final <- legend_plot / p1 / p2 + plot_layout(heights = c(1, 20, 20))
#   ggsave(paste0("waffle_", var_col, ".png"), final, width = 20, height = 16, dpi = 150)
# }


# Optional: save each plot
 for (var_col in names(variables)) {
   var_label <- variables[[var_col]]
   p1 <- make_group_waffle(edd, group1_subs, var_col, paste0(var_label, " — Group 1"))
   p2 <- make_group_waffle(edd, group2_subs, var_col, paste0(var_label, " — Group 2"))
   final <- legend_plot / p1 / p2 + plot_layout(heights = c(1, 20, 20))
   ggsave(paste0("waffle_", var_col, ".png"), final, width = 20, height = 16, dpi = 150)
 }
