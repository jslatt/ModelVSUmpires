remove(list = ls())
options(scipen = 999)

# Packges
library(tidyverse)
library(gamlr)

source("create_matrix.R")

# Create x variables without year
x_vars_year <- x_vars_no_sz_desc %>% 
  mutate(rule_change_yr = case_when(game_year == 2023 ~ 1,
                                    TRUE ~ 0)) %>% 
  select(-game_year)

x_vars_no_year <- x_vars_year %>% 
  select(-rule_change_yr)

# Create matrix
x_cat_year <- sparse.model.matrix(~., data = x_vars_year)

x_cat_no_year <- sparse.model.matrix(~., data = x_vars_no_year)

# Create year vector
rule_change_yr <- x_vars_year %>% 
  pull(rule_change_yr)

# Check relationship between strike zone and year
yr_model <- glm(y_in_strike_zone ~ rule_change_yr, family = binomial)

summary(yr_model)

odds_change <- exp(yr_model$coefficients[2]) - 1

# Run treatment LASSO
treat_lasso <- gamlr(x_cat_no_year, y = rule_change_yr, standardize = FALSE, family = "binomial",
                     lambda.min.ratio = 1e-3)

x_hat <- predict(treat_lasso, x_cat_no_year, type = "response")
x_hat <- drop(x_hat)

plot(treat_lasso)

# Run Causal Lasso
causal_lasso <- gamlr(cbind(rule_change_yr, x_hat, x_cat_no_year), y = y_in_strike_zone,
                      standardize = FALSE, family = "binomial", lambda.min.ration = 1e-3)

plot(causal_lasso)

causal_rule_change_yr <- coef(causal_lasso)["rule_change_yr",]

causal_lasso_min_segment = which.min(AICc(causal_lasso))

causal_lasso_coef <- coef(causal_lasso, select = causal_lasso_min_segment)

coef_names <- unlist(causal_lasso_coef@Dimnames[1])

causal_lasso_coef_df <- as.data.frame(summary(causal_lasso_coef)) %>% 
  mutate(coef_name = coef_names[i]) %>% 
  rename(coef_value = x) %>% 
  select(coef_name, coef_value)

causal_lasso_intercept <- causal_lasso_coef_df %>% 
  filter(coef_name == 'intercept')

causal_lasso_ordered_coef_df <- causal_lasso_coef_df %>% 
  filter(coef_name != 'intercept') %>% 
  arrange(desc(abs(coef_value)))

causal_lasso_coef_df <- rbind(causal_lasso_intercept, causal_lasso_ordered_coef_df) %>% 
  mutate(odds_value = exp(coef_value))

remove(causal_lasso_intercept, causal_lasso_ordered_coef_df)

# Run Naive Lasso
naive_lasso <- gamlr(cbind(rule_change_yr, x_cat_no_year), y = y_in_strike_zone,
                     standardize = FALSE, family = "binomial", lambda.min.ratio = 1e-3)

plot(naive_lasso)

naive_rule_change_yr <- coef(naive_lasso)["rule_change_yr",]

naive_lasso_min_segment = which.min(AICc(naive_lasso))

naive_lasso_coef <- coef(naive_lasso, select = naive_lasso_min_segment)

coef_names <- unlist(naive_lasso_coef@Dimnames[1])

naive_lasso_coef_df <- as.data.frame(summary(naive_lasso_coef)) %>% 
  mutate(coef_name = coef_names[i]) %>% 
  rename(coef_value = x) %>% 
  select(coef_name, coef_value)

naive_lasso_intercept <- naive_lasso_coef_df %>% 
  filter(coef_name == 'intercept')

naive_lasso_ordered_coef_df <- naive_lasso_coef_df %>% 
  filter(coef_name != 'intercept') %>% 
  arrange(desc(abs(coef_value)))

naive_lasso_coef_df <- rbind(naive_lasso_intercept, naive_lasso_ordered_coef_df) %>% 
  mutate(odds_value = exp(coef_value))

remove(naive_lasso_intercept, naive_lasso_ordered_coef_df)
