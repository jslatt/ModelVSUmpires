df_pitches <- read_csv("pitch_data_21_22_23.csv")
df_umps <- feather::read_feather("ump_data_21_22_23.feather")
df_xwalk <- read_csv("team_xwalk.csv")

ump_xwalk <- df_umps %>% 
  left_join(df_xwalk, join_by(away_team == gl_name)) %>% 
  select(-away_team) %>% 
  rename(away_team = statcast_name) %>% 
  left_join(df_xwalk, join_by(home_team == gl_name)) %>% 
  select(-home_team) %>% 
  rename(home_team = statcast_name) %>% 
  mutate(game_date = ymd(game_date))

# Merge ump names onto game ID from pitch data
umps_with_game_ids <- df_pitches %>% 
  select(game_date, away_team, home_team, game_pk) %>% 
  distinct() %>% 
  arrange(game_date, away_team, home_team, game_pk) %>% 
  group_by(game_date, away_team, home_team) %>% 
  mutate(game_num = row_number()) %>% 
  ungroup() %>% 
  left_join(ump_xwalk) %>% 
  select(game_pk, starts_with("ump")) %>% 
  mutate_at(vars(starts_with("ump")), ~ na_if(., "(none)"))
  
# Join umps onto statcast
combo <- df_pitches %>% 
  left_join(umps_with_game_ids) %>% 
  mutate(missing_umps = is.na(ump_name_0b)) %>% 
  mutate(row_id = row_number()) %>% 
  arrange(desc(row_id)) %>% 
  mutate(pitch_id = row_number()) %>% 
  select(-row_id)
