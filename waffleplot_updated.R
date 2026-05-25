
---------------------------------------------------------#
#   edu 2026 proj  4/9/2026
#'  Ted, Will, Tess, Ayesha
#---------------------------------------------------------#

#          _.-^~~^^^`~-,_,,~''''''```~,''``~'``~,
#  ______,'  -o  :.  _    .          ;     ,'`,  `.
# (      -\.._,.;;'._ ,(   }        _`_-_,,    `, `,
#  ``~~~~~~'   ((/'((((____/~~~~~~'(,(,___>      `~'
#---------------------------------------------------------#                     
##-------------------------------------------------------#
# Creating a waffle plot using reddit data
#          https://materialui.co/colors
#
#  original code in 2025 by: Miracle Sammons
#  updated in 2026 by Ayesha Akbar and Ted Welser
##-----------------------------------------------------#

#Load necessary libraries for data wrangling and plotting

library(tidyverse)     #Data manipulation and visualization
library(lubridate)     #Handling date/time
install.packages(c("ggnewscale", "patchwork"))

library(ggnewscale)    #Allows multiple fill/color scales in a ggplot
library(patchwork)     #Combines multiple plots into one


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



# ----------------------------------------
# STEP 1: Prepare and clean the data
# ----------------------------------------



#load data, no authorization needed
edd <- read_csv("https://docs.google.com/spreadsheets/d/1LgoGLwwuuWhVnF21DmxNTe6mCS3iON2YUWPjLSC-cZY/export?format=csv")

edd <- edd %>%
  mutate(
    #Convert the t_date column to actual Date format
    t_date = as.Date(t_date),
    
    #Create a new column: average of two disengagement measures (ignores missing values)
    disengage_combined = rowMeans(select(., disengage.iln, disengage.i), na.rm = TRUE)
  ) %>%
  #Remove rows where disengage_combined is missing
  filter(!is.na(disengage_combined)) %>%
  #Sort data by date
  arrange(t_date) %>%
  #Add row numbers and divide data into 10 equal-sized time chunks ("deciles")
  mutate(row_id = row_number(),
         decile = ntile(row_id, 10))

# ----------------------------------------
# STEP 2: Create abbreviated month labels
# ----------------------------------------

#This function shortens month names for compact labeling
month_abbrev <- function(dates) {
  abbr <- format(dates, "%b")  #Get 3-letter month abbreviation
  abbr <- str_replace_all(abbr, c(
    "Jan" = "Ja", "Feb" = "Fb", "Mar" = "Mr", "Apr" = "Ap",
    "May" = "My", "Jun" = "Jn", "Jul" = "Jl", "Aug" = "Au",
    "Sep" = "Sp", "Oct" = "Oc", "Nov" = "Nv", "Dec" = "De"
  ))
  paste0(abbr, format(dates, "%d"))  #ex, "Ja01"
}

# ----------------------------------------
# STEP 3: Create decile labels based on start date (had to rerun)
# ----------------------------------------

decile_labels <- edd %>%
  group_by(decile) %>%
  summarize(start_date = min(t_date), .groups = "drop") %>%
  mutate(time_slice_label = month_abbrev(start_date),
         decile = as.integer(decile))  #Ensure decile is numeric

#Join time_slice labels back into the main dataset
edd <- edd %>%
  mutate(decile = as.integer(decile)) %>%
  left_join(decile_labels, by = "decile")

#Make sure the label column was added successfully
stopifnot("time_slice_label" %in% colnames(edd))

# ----------------------------------------
# STEP 4: Set time slice as a factor for plotting
# ----------------------------------------

edd <- edd %>%
  mutate(time_slice = factor(time_slice_label, levels = decile_labels$time_slice_label))

# ----------------------------------------
# STEP 5: Create quartile categories for disengagement
# ----------------------------------------

#Find the 25th, 50th, and 75th percentiles for positive disengagement values
qtiles <- quantile(edd$disengage_combined[edd$disengage_combined > 0], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

#Categorize disengagement into "None", Q1–Q4 based on quartile thresholds
edd <- edd %>%
  mutate(
    dis_cat = case_when(
      disengage_combined == 0 ~ "None",
      disengage_combined <= qtiles[1] ~ "Q1",
      disengage_combined <= qtiles[2] ~ "Q2",
      disengage_combined <= qtiles[3] ~ "Q3",
      disengage_combined > qtiles[3] ~ "Q4"
    ),
    #Set the order of categories for plotting (lightest to darkest)
    dis_cat = factor(dis_cat, levels = c("None", "Q1", "Q2", "Q3", "Q4"))
  )

# ----------------------------------------
# STEP 6: Function to create data for waffle plots
# ----------------------------------------

#Each user is placed in a 4-column waffle grid by time slice and disengagement level
make_waffle_df <- function(df, subreddit_column, group_label) {
  df %>%
    #Filter for users in the given subreddit
    filter(.data[[subreddit_column]] == TRUE) %>%
    group_by(time_slice) %>%
    arrange(dis_cat, t_date) %>%  #Order by disengagement then date
    mutate(
      id = row_number(),
      x = (id - 1) %% 4,           #X-position in 4-column layout
      y = floor((id - 1) / 4),     #Y-position
      group = group_label
    ) %>%
    ungroup()
}

#Apply the function to different subgroups
df_teachers <- make_waffle_df(edd, "sub_teachers", "Teachers")
df_professors <- make_waffle_df(edd, "sub_professors", "Professors")
df_college <- make_waffle_df(edd, "sub_college", "College")

#Combine all waffle data and reorder groups for plotting
all_waffle <- bind_rows(df_teachers, df_professors, df_college) %>%
  mutate(group = factor(group, levels = c("Teachers", "Professors", "College")))

#Split back into separate groups
df_teachers <- all_waffle %>% filter(group == "Teachers")
df_professors <- all_waffle %>% filter(group == "Professors")
df_college <- all_waffle %>% filter(group == "College")

# ----------------------------------------
# STEP 7: Define color palettes for each group
# LINK FOR COLOR COMBOS: https://materialui.co/colors 
# ----------------------------------------

pal_teachers <- c("None" = "#f6f5fb", "Q1" = "#cab2d6", "Q2" = "#9e9ac8", "Q3" = "#6a51a3", "Q4" = "#3f007d")
pal_profs <- c("None" = "#f7fcf5", "Q1" = "#c7e9c0", "Q2" = "#74c476", "Q3" = "#238b45", "Q4" = "#00441b")
pal_college <- c("None" = "#f0f7fb", "Q1" = "#bdd7e7", "Q2" = "#6baed6", "Q3" = "#2171b5", "Q4" = "#08306b")

# ----------------------------------------
# STEP 8: Create a legend showing disengagement levels
# ----------------------------------------

grey_pal <- c("None" = "#f0f0f0", "Q1" = "#bdbdbd", "Q2" = "#737373", "Q3" = "#404040", "Q4" = "#0d0d0d")

#Simple data frame for legend tiles
legend_df <- tibble(
  dis_cat = factor(c("None", "Q1", "Q2", "Q3", "Q4"), levels = c("None", "Q1", "Q2", "Q3", "Q4")),
  x = 1:5,
  y = 1
)

#Create the legend plot
legend_plot <- ggplot() +
  geom_text(aes(x = 0, y = 1, label = "Quartile Range:"), hjust = 1, size = 3, fontface = "bold") + 
  geom_tile(data = legend_df, aes(x = x, y = y, fill = dis_cat), color = "black", size = 0.3, width = 0.6, height = 0.6) +
  geom_text(data = legend_df, aes(x = x, y = y - 0.7, label = dis_cat), color = "black", size = 2) +
  scale_fill_manual(values = grey_pal) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  ) +
  coord_fixed(expand = FALSE, xlim = c(0,6), ylim = c(0.1, 1.5))

# ----------------------------------------
# STEP 9: Create the waffle plot
# ----------------------------------------

waffle_plot <- ggplot() +
  # Teachers layer
  geom_tile(data = df_teachers, aes(x = x, y = y, fill = dis_cat),
            color = "black", size = 0.1, width = 0.9, height = 0.9) +
  scale_fill_manual(values = pal_teachers, drop = FALSE) +
  ggnewscale::new_scale_fill() +
  
  # Professors layer
  geom_tile(data = df_professors, aes(x = x, y = y, fill = dis_cat),
            color = "black", size = 0.1, width = 0.9, height = 0.9) +
  scale_fill_manual(values = pal_profs, drop = FALSE) +
  ggnewscale::new_scale_fill() +
  
  # College layer
  geom_tile(data = df_college, aes(x = x, y = y, fill = dis_cat),
            color = "black", size = 0.1, width = 0.9, height = 0.9) +
  scale_fill_manual(values = pal_college, drop = FALSE) +
  
  facet_grid(group ~ time_slice, switch = "x") +  #One row per group, one column per time_slice
  coord_fixed() +  # Equal aspect ratio
  
  scale_y_continuous(
    breaks = seq(0, 25, by = 5),             
    labels = function(y) y * 4,               
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),        
    axis.text.y = element_text(size = 8),
    axis.ticks.y = element_line(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 7, face = "bold", margin = margin(t = 2, b = 0)),
    strip.text.y = element_blank()
  )

# ----------------------------------------
# STEP 10: Combine legend and waffle plot
# ----------------------------------------

final_plot <- legend_plot / waffle_plot + plot_layout(heights = c(1, 20))  #Legend smaller than plot

# ----------------------------------------
# STEP 11: Display the final plot
# ----------------------------------------

print(final_plot)


