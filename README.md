# dsci-100-project_template
Template project repository for DSCI-100

library(tidyverse)
library(repr)
library(tidymodels)
library(tidyclust)

#1. Read in the Data
getwd()
list.files()
players <- read_csv("data/players (1).csv")
head(players)

sessions <- read_csv("data/sessions (1).csv")
head(sessions)

#2. Summarize sessions by player
player_sessions_summary_tbl <- sessions |>
  group_by(hashedEmail) |>
  summarise(
    num_sessions = n(),                                                       
    total_play_hours = sum((original_end_time - original_start_time) / 1000/60/60),  
    .groups = "drop")
player_sessions_summary_tbl
