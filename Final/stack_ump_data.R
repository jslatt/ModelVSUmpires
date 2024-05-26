library(glue)
library(lubridate)
library(tidyverse)

setwd("~/Documents/Booth/Classes/Spring 24/Big Data/Final/")

loadUmps <- function(this_year) {
  
  df <- read.csv(glue("gl{this_year}.txt"), header = FALSE) %>% 
    rename(game_date = V1,
           game_num = V2,
           away_team = V4,
           home_team = V7,
           ump_name_0b = V79,
           ump_name_1b = V81,
           ump_name_2b = V83,
           ump_name_3b = V85) %>% 
    select(game_date, game_num, away_team, home_team, starts_with("ump")) %>% 
    mutate(game_date = paste0(substr(game_date, 1, 4), "-",
                              substr(game_date, 5, 6), "-",
                              substr(game_date, 7, 8))) %>% 
    mutate(game_num = case_when(game_num == 0 ~ 1,
                                TRUE ~ game_num))
  
}

df_umps <- bind_rows(loadUmps(2021),
                     loadUmps(2022),
                     loadUmps(2023))

feather::write_feather(df_umps, "ump_data_21_22_23.feather")
