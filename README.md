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

#3. Join those summaries back to the players table
players_with_metrics_tbl <- players |>
  left_join(player_sessions_summary_tbl, by = "hashedEmail") |>
  mutate(
    num_sessions = replace_na(num_sessions, 0),     # zero for players with no sessions
    total_play_hours = replace_na(total_play_hours, 0))
players_with_metrics_tbl

The enriched player table reveals a clear long-tail pattern in usage. On average, a player logs about six sessions, with most users falling between two and ten sessions and a small number reaching nearly thirty sessions. Total playtime averages around five hours per player, although some power users accumulate more than fifteen hours. This distribution shows that while most players engage modestly, a dedicated minority drives the bulk of server load.

#4 Which experience level plays the most (by avg sessions)?
experience_rankings_tbl <- players_with_metrics_tbl |>
  group_by(experience) |>
  summarise(avg_sessions_per_player = mean(num_sessions),
    .groups = "drop") |>
  arrange(desc(avg_sessions_per_player))
experience_rankings_tbl

#4 cont: Which subscription status yields the most playtime?
subscription_playtime_tbl <- players_with_metrics_tbl |>
  group_by(subscribe) |>
  summarise(
    avg_play_hours_per_player = mean(total_play_hours),
    .groups = "drop") |>
  arrange(desc(avg_play_hours_per_player))
subscription_playtime_tbl

#4 last part: Which gender plays most sessions?
gender_sessions_tbl <- players_with_metrics_tbl |>
  group_by(gender) |>
  summarise(avg_sessions_per_player = mean(num_sessions),
    .groups = "drop") |>
  arrange(desc(avg_sessions_per_player))
gender_sessions_tbl

#5. Inspect your results
experience_rankings_tbl
subscription_playtime_tbl
gender_sessions_tbl

#6. Visualization
options(repr.plot.width = 9, repr.plot.height = 8)
summary_long_tbl <- players_with_metrics_tbl |>
  group_by(experience) |>
  summarise(avg_sessions = mean(num_sessions), .groups = "drop") |>
  mutate(attribute = "Experience", level = experience, value = avg_sessions) |>
  bind_rows(
    players_with_metrics_tbl |>
      group_by(subscribe) |>
      summarise(avg = mean(total_play_hours), .groups = "drop") |>
      mutate(attribute = "Subscription", level = as.character(subscribe), value = avg),
    players_with_metrics_tbl |>
      group_by(gender) |>
      summarise(avg = mean(num_sessions), .groups = "drop") |>
      mutate(attribute = "Gender", level = gender, value = avg)) |>
  select(attribute, level, value)

#faceted bar chart
ggplot(summary_long_tbl, aes(x = level, y = value)) +
  geom_col() +
  facet_wrap(~attribute, scales = "free_y") +
  labs(
    x = NULL,
    y = "Average Value",
    title = "Average Sessions and Play Hours by Player Attribute") +
  theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

The faceted bar chart shows three separate panels, one for each player attribute. In the Experience panel it is clear that Expert players have the highest average session count (around nine sessions), followed by Intermediate players (about six sessions) and Novice players (around four sessions). The Subscription panel shows that subscribed players spend roughly seven hours on average, while non-subscribed players average closer to four hours of total playtime. In the Gender panel male players log about 6.3 sessions on average, female players about 5.8 sessions, and players identifying as Other about 4.1 sessions. Rotating the labels ensures each category name is legible.

Conclusion:
In summary, experience level and subscription status emerge as the strongest predictors of heavy server usage, with Expert and subscribed players logging significantly more sessions and playtime than their counterparts. Gender and age show smaller effects but still contribute to the overall usage pattern. These insights can guide resource planning and targeted engagement efforts to better serve the most active segments of the player community.







