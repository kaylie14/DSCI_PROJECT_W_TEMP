# dsci-100-project_template
Template project repository for DSCI-100

library(tidyverse)
library(repr)
library(tidymodels)
library(tidyclust)

Frank Wood’s Minecraft research server records every player’s activity, from each login and logout to the duration of every session. Knowing which users generate the most data, measured by session count and total playtime, supports better capacity planning, more effective community outreach, and fair resource allocation. This analysis examines whether simple profile attributes: experience level, subscription status, gender, and age, can predict which players place the highest demands on server resources. 

To answer the question "Which "kinds" of players are most likely to contribute a large amount of data so that we can target those players in our recruiting efforts," the following code starts by loading the two CSV files, one with player profiles and one with session logs. It then groups the session data by each player’s hashed email and uses a count and a sum of session durations (converted from milliseconds to hours) to create per-player metrics for number of sessions and total play hours. Those metrics are merged back into the players table with a left join, and any missing values (for players with no sessions) are replaced with zeros. Finally, the code builds three separate summary tables that calculate the average sessions or average playtime per player, broken out by experience level, subscription status, and gender, and orders each result from highest to lowest.

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

Session duration is calculated by subtracting the start timestamp from the end timestamp and converting the difference from milliseconds into hours. Grouping these durations by hashed email produces two per-player metrics: num_sessions, which counts all sessions per user, and total_play_hours, which sums all session durations. These metrics are merged back into the players table using a left join on the hashed email key, preserving every registered account and replacing missing values with zero. This ensures that calculated averages reflect the entire roster of registered users rather than only those who actively logged in.

# 3. Join those summaries back to the players table
players_with_metrics_tbl <- players |>
  left_join(player_sessions_summary_tbl, by = "hashedEmail") |>
  mutate(
    num_sessions = replace_na(num_sessions, 0),     # zero for players with no sessions
    total_play_hours = replace_na(total_play_hours, 0))
players_with_metrics_tbl











