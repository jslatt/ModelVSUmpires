# Packages
library(tidyverse)
library(gamlr)

# Load data
df_model <- readRDS("../model_data.rds")

# Convert characters to factors
df_to_matrix <- df_model

df_to_matrix[sapply(df_to_matrix, is.character)] <- lapply(df_to_matrix[sapply(df_to_matrix, is.character)], 
                                                           as.factor)

df_to_matrix[sapply(df_to_matrix, is.logical)] <- lapply(df_to_matrix[sapply(df_to_matrix, is.logical)], 
                                                         as.numeric)

# Replace some missing values
df_to_matrix <- df_to_matrix %>% 
  mutate(count = as.factor(paste0(balls, "-", strikes)),
         runner_on_1b = case_when(is.na(on_1b) == FALSE ~ 1,
                                  TRUE ~ 0),
         runner_on_2b = case_when(is.na(on_2b) == FALSE ~ 1,
                                  TRUE ~ 0),
         runner_on_3b = case_when(is.na(on_3b) == FALSE ~ 1,
                                  TRUE ~ 0),
         outs_when_up = as.factor(outs_when_up),
         inning = as.factor(inning),
         fielder_2 = as.factor(fielder_2),
         zone = as.factor(zone)) %>% 
  rename(in_strike_zone = strike_dummy)

# Remove unwanted variables
df_to_matrix <- df_to_matrix %>% 
  select(-c(spin_dir, balls, strikes, on_1b, on_2b, on_3b,
            center_z:is_below_left, batter, pitcher,
            hit_location, bb_type, hc_x, hc_y,
            estimated_ba_using_speedangle:launch_speed_angle,
            bat_speed, swing_length, events_catcher_interf:events_NA,
            pitch_id, events, type, delta_run_exp, delta_home_win_exp,
            plate_x, plate_z))

# Remove missing data
df_to_matrix <- na.omit(df_to_matrix)

# Pull only x variables
x_vars <- df_to_matrix %>% 
  select(-c(is_correct_call, is_called_strike, is_called_ball, zone))

x_vars_no_desc <- x_vars %>% 
  select(-description)

x_vars_no_sz <- x_vars %>% 
  select(-in_strike_zone)

x_vars_no_sz_desc <- x_vars %>% 
  select(-c(in_strike_zone, description))

# Pull y variable
y_correct_call <- df_to_matrix %>% 
  pull(is_correct_call)

y_called_strike <- df_to_matrix %>% 
  pull(is_called_strike)

y_called_ball <- df_to_matrix %>% 
  pull(is_called_ball)

y_in_strike_zone <- df_to_matrix %>% 
  pull(in_strike_zone)

# Create Matrix
x_cat <- sparse.model.matrix(~., data = x_vars)

x_cat_no_desc <- sparse.model.matrix(~., data = x_vars_no_desc)

x_cat_no_sz <- sparse.model.matrix(~., data = x_vars_no_sz)

x_cat_no_sz_desc <- sparse.model.matrix(~., data = x_vars_no_sz_desc)
