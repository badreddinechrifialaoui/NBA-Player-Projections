# NBA Player Projections Model
# Clear workspace
rm(list = ls())
cat("\014")

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(hoopR, dplyr, tidyr, lubridate, knitr, writexl, stringr, ranger, zoo, tibble, rvest)

# CONFIGURATION
target_date <- as.character(Sys.Date())     # Use for TODAY
# target_date <- "2026-01-26"                   # Use for TESTING/TOMORROW
MIN_AVG_MINUTES <- 15.0

cat(paste("TARGET DATE:", target_date, "\n"))


# Fetch schedule and filter for target date
cat("Fetching schedule...\n")
full_schedule <- hoopR::load_nba_schedule(seasons = 2026) %>%
  mutate(game_id = as.character(game_id),
         game_date = as.character(game_date))

games_today <- full_schedule %>% filter(game_date == target_date)

if (nrow(games_today) == 0) {
  stop(paste("ERROR: No games found for", target_date))
}

teams_playing <- unique(c(games_today$home_abbreviation, games_today$away_abbreviation))
cat(paste("TEAMS PLAYING ON", target_date, ":\n"))
print(teams_playing)

# Fetch player stats
cat("\nFetching player stats...\n")
nba_stats <- hoopR::load_nba_player_box(seasons = 2026) %>%
  mutate(game_id = as.character(game_id))

# Process base statistics
cat("Calculating features...\n")
base_stats <- nba_stats %>%
  left_join(full_schedule %>% select(game_id, home_abbreviation, away_abbreviation), by = "game_id") %>%
  mutate(
    game_date = as.Date(game_date),
    min_numeric = sapply(strsplit(as.character(minutes), ":"), function(x) {
      as.numeric(x[1]) + (as.numeric(x[2]) / 60)
    }),
    min_numeric = ifelse(is.na(min_numeric), as.numeric(minutes), min_numeric),
    is_home = ifelse(team_abbreviation == home_abbreviation, 1, 0),
    opponent = ifelse(is_home == 1, away_abbreviation, home_abbreviation),
    Pos = case_when(
      grepl("C", athlete_position_abbreviation) ~ "C",
      grepl("F", athlete_position_abbreviation) ~ "F",
      grepl("G", athlete_position_abbreviation) ~ "G",
      TRUE ~ "F"
    )
  ) %>%
  arrange(athlete_display_name, game_date)

# Calculate player season averages for injury impact
player_quality <- base_stats %>%
  filter(did_not_play == FALSE) %>%
  group_by(athlete_display_name, team_abbreviation) %>%
  summarise(
    Avg_PTS = mean(points, na.rm = TRUE),
    Avg_Mins = mean(min_numeric, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate historical injury impact
historical_injuries <- base_stats %>%
  select(game_id, team_abbreviation, athlete_display_name, did_not_play) %>%
  left_join(player_quality, by = c("athlete_display_name", "team_abbreviation")) %>%
  group_by(game_id, team_abbreviation) %>%
  summarise(
    Missing_Production = sum(Avg_PTS[did_not_play == TRUE & Avg_Mins > 15], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Missing_Production = ifelse(is.na(Missing_Production), 0, Missing_Production))

stats_with_context <- base_stats %>%
  filter(did_not_play == FALSE) %>%
  left_join(historical_injuries, by = c("game_id", "team_abbreviation"))

# Calculate rest days between games
team_schedule <- full_schedule %>%
  select(game_date, home_abbreviation, away_abbreviation) %>%
  mutate(game_date = as.Date(game_date)) %>%
  pivot_longer(cols = c(home_abbreviation, away_abbreviation), values_to = "team") %>%
  distinct() %>%
  arrange(team, game_date) %>%
  group_by(team) %>%
  mutate(days_rest = as.numeric(as.Date(game_date) - lag(as.Date(game_date))) - 1,
         days_rest = ifelse(is.na(days_rest), 3, ifelse(days_rest > 5, 5, days_rest))) %>%
  ungroup()

# Calculate rolling averages and trends
enhanced_stats <- stats_with_context %>%
  left_join(team_schedule, by = c("game_date", "team_abbreviation" = "team")) %>%
  group_by(athlete_display_name) %>%
  mutate(
    L5_PTS  = lag(rollmean(points, k = 5, fill = NA, align = "right"), 1),
    L5_REB  = lag(rollmean(rebounds, k = 5, fill = NA, align = "right"), 1),
    L5_AST  = lag(rollmean(assists, k = 5, fill = NA, align = "right"), 1),
    L5_Mins = lag(rollmean(min_numeric, k = 5, fill = NA, align = "right"), 1),
    L5_FGA  = lag(rollmean(field_goals_attempted, k = 5, fill = NA, align = "right"), 1),
    Season_PTS = cummean(points),
    Delta_PTS = L5_PTS - lag(Season_PTS, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(L5_Mins))

# Train Random Forest models
cat("Training models...\n")
vars <- c("points", "rebounds", "assists", 
          "L5_PTS", "L5_REB", "L5_AST", "L5_Mins", "L5_FGA", "Delta_PTS",
          "opponent", "is_home", "days_rest", "Pos", "Missing_Production")

train_data <- enhanced_stats %>%
  select(all_of(vars)) %>%
  mutate(opponent = as.factor(opponent), Pos = as.factor(Pos)) %>%
  na.omit()

rf_pts <- ranger(points ~ ., data = train_data %>% select(-rebounds, -assists), num.trees = 100)
rf_reb <- ranger(rebounds ~ ., data = train_data %>% select(-points, -assists), num.trees = 100)
rf_ast <- ranger(assists ~ ., data = train_data %>% select(-points, -rebounds), num.trees = 100)

# Prepare prediction data (only use data before target date)
cat(paste("Generating projections for:", target_date, "...\n"))

historical_cutoff <- base_stats %>%
  filter(game_date < as.Date(target_date))

latest_info <- historical_cutoff %>%
  group_by(athlete_display_name) %>%
  arrange(desc(game_date)) %>%
  slice(1) %>%
  select(athlete_display_name, team_abbreviation, Pos)

current_form <- historical_cutoff %>%
  group_by(athlete_display_name) %>%
  arrange(desc(game_date)) %>%
  slice_head(n = 5) %>%
  summarise(
    L5_PTS = mean(points), L5_REB = mean(rebounds), L5_AST = mean(assists),
    L5_Mins = mean(min_numeric), L5_FGA = mean(field_goals_attempted),
    .groups = "drop"
  )

season_avgs <- historical_cutoff %>%
  group_by(athlete_display_name) %>%
  summarise(Season_PTS = mean(points))


# Scrapping
cat("Fetching injury report from ESPN...\n")

injury_url <- "https://www.espn.com/nba/injuries"

injury_report <- tryCatch({
  read_html(injury_url) %>%
    html_table() %>%
    bind_rows() %>%
    select(NAME, STATUS) %>%
    rename(athlete_display_name = NAME, status = STATUS) %>%
    filter(grepl("Out|Surgery|Indefinitely", status, ignore.case = TRUE)) %>%
    distinct()
}, error = function(e) {
  cat("Warning: Could not fetch injury report. Proceeding without injury filter.\n")
  return(data.frame(athlete_display_name = character(), status = character()))
})

cat(paste("Found", nrow(injury_report), "injured players to exclude.\n"))

# Build prediction dataset for players in today's games
predict_input <- latest_info %>%
  inner_join(current_form, by = "athlete_display_name") %>%
  inner_join(season_avgs, by = "athlete_display_name") %>%
  mutate(Delta_PTS = L5_PTS - Season_PTS) %>%
  inner_join(games_today %>% select(home_abbreviation, away_abbreviation), 
             by = c("team_abbreviation" = "home_abbreviation")) %>%
  mutate(is_home = 1, opponent = away_abbreviation) %>%
  select(-away_abbreviation) %>%
  bind_rows(
    latest_info %>%
      inner_join(current_form, by = "athlete_display_name") %>%
      inner_join(season_avgs, by = "athlete_display_name") %>%
      mutate(Delta_PTS = L5_PTS - Season_PTS) %>%
      inner_join(games_today %>% select(home_abbreviation, away_abbreviation), 
                 by = c("team_abbreviation" = "away_abbreviation")) %>%
      mutate(is_home = 0, opponent = home_abbreviation) %>%
      select(-home_abbreviation)
  ) %>%
  mutate(join_date = as.character(target_date)) %>%
  left_join(team_schedule %>% mutate(game_date = as.character(game_date)), 
            by = c("join_date" = "game_date", "team_abbreviation" = "team")) %>%
  select(-join_date) %>%
  mutate(opponent = as.factor(opponent), Pos = as.factor(Pos), Missing_Production = 0) %>%
  filter(opponent %in% levels(train_data$opponent),
         L5_Mins >= MIN_AVG_MINUTES,
         # EXCLUDE INJURED PLAYERS
         !athlete_display_name %in% injury_report$athlete_display_name) %>%
  ungroup()

# Generate predictions
preds_pts <- predict(rf_pts, data = predict_input)$predictions
preds_reb <- predict(rf_reb, data = predict_input)$predictions
preds_ast <- predict(rf_ast, data = predict_input)$predictions

# Format final output
final_projections <- predict_input %>%
  mutate(
    Proj_PTS = round(preds_pts, 1),
    Proj_REB = round(preds_reb, 1),
    Proj_AST = round(preds_ast, 1),
    L5_PTS = round(L5_PTS, 1),
    L5_REB = round(L5_REB, 1),
    L5_AST = round(L5_AST, 1)
  ) %>%
  select(
    athlete_display_name,
    team_abbreviation,
    opponent,
    Proj_PTS, L5_PTS,
    Proj_REB, L5_REB,
    Proj_AST, L5_AST
  ) %>%
  arrange(desc(Proj_PTS))

# DYNAMIC PATH FOR GITHUB ACTIONS
current_dir <- getwd()

# Note: I use file.path to ensure it works on both Windows and Linux
output_dir <- file.path(current_dir, "NBAWeb", "data_feed")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

output_file <- file.path(output_dir, "projections.csv")
write.csv(final_projections, output_file, row.names = FALSE)

cat(paste("\nSUCCESS! Data sent to:", output_file, "\n"))