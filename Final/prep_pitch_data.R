library(glue)
library(lubridate)
library(tidyverse)
library(igraph)
library(fastDummies)

setwd("~/Documents/Booth/Classes/Spring 24/Big Data/Final/")

source("combine_pitch_ump.R")
## outputs: 
############# combo (pitch data with umpire names)
############# umps_with_game_ids (ump data for networks)

### DELETE THIS
#combo <- sample_n(combo, 100000)

# Pitch location columns
left_plate_edge <- -(17/2)/12
right_plate_edge <- (17/2)/12

combo <- combo %>% 
  mutate(strike_dummy = case_when(
    ((plate_x >= left_plate_edge & plate_x <= right_plate_edge) &
       (plate_z >= sz_bot & plate_z <= sz_top)) ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(
    center_z = (sz_bot + sz_top) / 2,
    dist_z = plate_z - center_z,
    distance_from_center = sqrt((plate_x - 0)^2 + (dist_z)^2)
  ) %>% 
  mutate(
    is_above_zone = plate_z > sz_top,
    is_below_zone = plate_z < sz_bot,
    is_left_zone = plate_x < left_plate_edge,
    is_right_zone = plate_x > right_plate_edge,
    is_above_right = is_above_zone * is_right_zone,
    is_above_left = is_above_zone * is_left_zone,
    is_below_right = is_below_zone * is_right_zone,
    is_below_left = is_below_zone * is_left_zone,
  )

# Create other outcome flags
combo <- combo %>% 
  dummy_cols(select_columns = "events") %>% 
  #dummy_cols(select_columns = c("ump_name_0b")) %>% 
  mutate(is_discretionary_pitch = description %in% c("ball", "called_strike"),
         discretionary_call = case_when(description == "called_strike" ~ "strike",
                                        description == "ball" ~ "ball"),
         is_called_strike = description=="called_strike",
         is_called_ball = description=="ball",
         is_correct_call = case_when(is_discretionary_pitch ~ (is_called_strike==T & strike_dummy==1) | (is_called_ball & strike_dummy==0)),
         is_noncall = is_called_strike + is_called_ball == 0)

# Date/game flags
combo <- combo %>% 
  mutate(month = factor(month(game_date)),
         day_of_week = factor(wday(game_date))) # 1 == Sunday, 2 == Monday, etc

# Previous pitch columns
## need to group within games, wont be able to use first pitch of each game

# Ump level data
df_merge <- combo %>% 
  filter(!is.na(ump_name_0b),
         is_discretionary_pitch) %>% 
  select(pitch_id, ump_name_0b, is_correct_call, game_year) %>% 
  group_by(game_year, ump_name_0b) %>% 
  arrange(pitch_id) %>% 
  mutate(ump_season_pitches_called = row_number(),
         ump_season_correct_cml = cummean(is_correct_call)) %>% 
  mutate(pitch_id = lead(pitch_id)) %>% 
  ungroup %>% 
  filter(!is.na(pitch_id)) %>% 
  select(pitch_id, ump_season_pitches_called, ump_season_correct_cml) 

combo <- combo %>% 
  left_join(df_merge, "pitch_id")

# Pitcher level data
df_merge <- combo %>% 
  select(pitch_id, pitcher, strike_dummy, game_year) %>% 
  group_by(game_year, pitcher) %>% 
  arrange(pitch_id) %>% 
  mutate(pitcher_season_pitches_thrown = row_number(),
         pitcher_season_strike_zone_cml = cummean(strike_dummy)) %>% 
  mutate(pitch_id = lead(pitch_id)) %>% 
  ungroup %>% 
  filter(!is.na(pitch_id)) %>% 
  select(pitch_id, pitcher_season_pitches_thrown, pitcher_season_strike_zone_cml) 

combo <- combo %>% 
  left_join(df_merge, "pitch_id")

# Batter level data
### What should we add here?

# Add umpire network data here

### Categorizing columns
df_coltypes <- read_csv("colnames.csv")

## MODEL DATA
df_model <- combo %>% 
  filter(is_discretionary_pitch) %>% 
  select(pitch_id,
         df_coltypes %>% filter(xvar=="X") %>% pull(col),
         df_coltypes %>% filter(xfe=="X") %>% pull(col),
         df_coltypes %>% filter(yvar=="X") %>% pull(col))

## Save to R file
saveRDS(df_model, "~/Spring 2024/Big Data/Final Project/model_data.rds")

# Running a few regressions
# test <- combo %>% 
#   filter(is_discretionary_pitch)
# 
# summary(lm(is_called_strike ~ strike_dummy, test))
# summary(lm(is_called_strike ~ strike_dummy + distance_from_center, test))
# summary(lm(is_called_strike ~ is_above_zone, test))
# summary(lm(is_called_strike ~ is_below_zone, test))
# summary(lm(is_called_strike ~ is_right_zone, test))
# summary(lm(is_called_strike ~ is_left_zone, test))
# summary(lm(is_called_strike ~ is_above_right, test))
# summary(lm(is_called_strike ~ is_below_right, test))
# summary(lm(is_called_strike ~ is_above_left, test))
# summary(lm(is_called_strike ~ is_below_left, test))



  
