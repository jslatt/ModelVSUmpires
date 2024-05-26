remove(list = ls())

library(tidyverse)

setwd("./")

# Import datasets
gamelog_df <- read.csv("gl2023.txt",
                       header = FALSE)

statcast_df <- read.csv("statcast_data.csv")

team_xwalk <- read.csv("team_xwalk.csv")

# Clean gamelog data
gamelog_df <- gamelog_df %>% 
  rename(game_date = V1,
         game_num = V2,
         away_team = V4,
         home_team = V7,
         home_ump_name = V79) %>% 
  select(game_date, game_num, away_team, home_team, home_ump_name) %>% 
  mutate(game_date = paste0(substr(game_date, 1, 4), "-",
                            substr(game_date, 5, 6), "-",
                            substr(game_date, 7, 8))) %>% 
  mutate(game_num = case_when(game_num == 0 ~ 1,
                              TRUE ~ game_num))

# Join on statcast team names
ump_xwalk <- gamelog_df %>% 
  left_join(team_xwalk, join_by(away_team == gl_name)) %>% 
  select(-away_team) %>% 
  rename(away_team = statcast_name) %>% 
  left_join(team_xwalk, join_by(home_team == gl_name)) %>% 
  select(-home_team) %>% 
  rename(home_team = statcast_name)

# Create game numbers for statcast data
statcast_game_num_xwalk <- statcast_df %>% 
  select(game_date, away_team, home_team, game_pk) %>% 
  distinct() %>% 
  arrange(game_date, away_team, home_team, game_pk) %>% 
  group_by(game_date, away_team, home_team) %>% 
  mutate(game_num = row_number()) %>% 
  ungroup()

# Crosswalk for ump names
ump_statcast_xwalk <- statcast_game_num_xwalk %>% 
  left_join(ump_xwalk) %>% 
  select(game_pk, home_ump_name)

# Join umps onto statcast
statcast_with_ump <- statcast_df %>% 
  left_join(ump_statcast_xwalk)

# Calculate strike dummy
left_plate_edge <- -(17/2)/12

right_plate_edge <- (17/2)/12

statcast_with_ump <- statcast_with_ump %>% 
  mutate(strike_dummy = case_when(
    ((plate_x >= left_plate_edge & plate_x <= right_plate_edge) &
       (plate_z >= sz_bot & plate_z <= sz_top)) ~ 1,
    TRUE ~ 0
  ))

test <- statcast_with_ump %>% 
  select(pitch_type, game_date, release_speed, release_pos_x, release_pos_z,
         pfx_x, pfx_z, plate_x, plate_z, sz_top, sz_bot, description, type, strike_dummy)
