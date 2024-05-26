remove(list = ls())
options(scipen = 999)

# Packges
library(tidyverse)
library(gamlr)

source("create_matrix.R")

# Called Ball LASSO
# Without description
lasso1 <- gamlr(x_cat_no_desc, 	y=y_called_ball, standardize=FALSE, family="binomial",
                lambda.min.ratio=1e-3)

par(mfrow=c(1,1))

plot(lasso1, main="Called Ball Full Model")

lasso1_min_segment = which.min(AICc(lasso1))

lasso1_coef <- coef(lasso1, select = lasso1_min_segment)

coef_names <- unlist(lasso1_coef@Dimnames[1])

lasso1_coef_df <- as.data.frame(summary(lasso1_coef)) %>% 
  mutate(coef_name = coef_names[i]) %>% 
  rename(coef_value = x) %>% 
  select(coef_name, coef_value)

lasso1_intercept <- lasso1_coef_df %>% 
  filter(coef_name == 'intercept')

lasso1_ordered_coef_df <- lasso1_coef_df %>% 
  filter(coef_name != 'intercept') %>% 
  arrange(desc(abs(coef_value)))

lasso1_coef_df <- rbind(lasso1_intercept, lasso1_ordered_coef_df) %>% 
  mutate(odds_value = exp(coef_value))

remove(lasso1_intercept, lasso1_ordered_coef_df)

# CROSS-VALIDATION

cv.lasso1 <- cv.gamlr(x_cat_no_desc, 	y=y_called_ball,
                      standardize=FALSE, family="binomial", lambda.min.ratio=1e-3)

coef(cv.lasso1) ## 1se rule; see ?cv.gamlr

coef(cv.lasso1, select="min") ## min cv selection

## plot them together

par(mfrow=c(1,2))

plot(cv.lasso1)

plot(cv.lasso1$gamlr) ## cv.gamlr includes a gamlr object

# Get minimum OOS Deviance
cv.lasso1_coef_oos <- coef(cv.lasso1, select="min")

# Determine coefficient names
cv.coef_names_oos <- unlist(cv.lasso1_coef_oos@Dimnames[1])

# Calculate number of coefficients that are non-zero
cv.lasso1_coef_oos_df <- as.data.frame(summary(cv.lasso1_coef_oos)) %>%
  mutate(coef_name = cv.coef_names_oos[i]) %>%
  rename(coef_value = x) %>%
  select(coef_name, coef_value)

cv.lasso1_intercept <- cv.lasso1_coef_oos_df %>% 
  filter(coef_name == 'intercept')

cv.lasso1_ordered_coef_df <- cv.lasso1_coef_oos_df %>% 
  filter(coef_name != 'intercept') %>% 
  arrange(desc(abs(coef_value)))

cv.lasso1_coef_oos_df <- rbind(cv.lasso1_intercept, cv.lasso1_ordered_coef_df) %>% 
  mutate(odds_value = exp(coef_value))

remove(cv.lasso1_intercept, cv.lasso1_ordered_coef_df)

# Get minimum 1se Deviance
cv.lasso1_coef_1se <- coef(cv.lasso1, select="1se")

# Determine coefficient names
cv.coef_names_1se <- unlist(cv.lasso1_coef_1se@Dimnames[1])

# Calculate number of coefficients that are non-zero
cv.lasso1_coef_1se_df <- as.data.frame(summary(cv.lasso1_coef_1se)) %>%
  mutate(coef_name = cv.coef_names_1se[i]) %>%
  rename(coef_value = x) %>%
  select(coef_name, coef_value)

cv.lasso1_intercept <- cv.lasso1_coef_1se_df %>% 
  filter(coef_name == 'intercept')

cv.lasso1_ordered_coef_df <- cv.lasso1_coef_1se_df %>% 
  filter(coef_name != 'intercept') %>% 
  arrange(desc(abs(coef_value)))

cv.lasso1_coef_1se_df <- rbind(cv.lasso1_intercept, cv.lasso1_ordered_coef_df) %>% 
  mutate(odds_value = exp(coef_value))

remove(cv.lasso1_intercept, cv.lasso1_ordered_coef_df)

# Without strike zone and description
lasso2 <- gamlr(x_cat_no_sz_desc, 	y=y_called_ball, standardize=FALSE, family="binomial",
                lambda.min.ratio=1e-3)

par(mfrow=c(1,1))

plot(lasso2, main="Called Ball w/o Strike Zone")

lasso2_min_segment = which.min(AICc(lasso2))

lasso2_coef <- coef(lasso2, select = lasso2_min_segment)

coef_names <- unlist(lasso2_coef@Dimnames[1])

lasso2_coef_df <- as.data.frame(summary(lasso2_coef)) %>% 
  mutate(coef_name = coef_names[i]) %>% 
  rename(coef_value = x) %>% 
  select(coef_name, coef_value)

lasso2_intercept <- lasso2_coef_df %>% 
  filter(coef_name == 'intercept')

lasso2_ordered_coef_df <- lasso2_coef_df %>% 
  filter(coef_name != 'intercept') %>% 
  arrange(desc(abs(coef_value)))

lasso2_coef_df <- rbind(lasso2_intercept, lasso2_ordered_coef_df) %>% 
  mutate(odds_value = exp(coef_value))

remove(lasso2_intercept, lasso2_ordered_coef_df)

# CROSS-VALIDATION

cv.lasso2 <- cv.gamlr(x_cat_no_sz_desc, 	y=y_called_ball,
                      standardize=FALSE, family="binomial", lambda.min.ratio=1e-3)

coef(cv.lasso2) ## 1se rule; see ?cv.gamlr

coef(cv.lasso2, select="min") ## min cv selection

## plot them together

par(mfrow=c(1,2))

plot(cv.lasso2)

plot(cv.lasso2$gamlr) ## cv.gamlr includes a gamlr object

# Get minimum OOS Deviance
cv.lasso2_coef_oos <- coef(cv.lasso2, select="min")

# Determine coefficient names
cv.coef_names_oos <- unlist(cv.lasso2_coef_oos@Dimnames[1])

# Calculate number of coefficients that are non-zero
cv.lasso2_coef_oos_df <- as.data.frame(summary(cv.lasso2_coef_oos)) %>%
  mutate(coef_name = cv.coef_names_oos[i]) %>%
  rename(coef_value = x) %>%
  select(coef_name, coef_value)

cv.lasso2_intercept <- cv.lasso2_coef_oos_df %>% 
  filter(coef_name == 'intercept')

cv.lasso2_ordered_coef_df <- cv.lasso2_coef_oos_df %>% 
  filter(coef_name != 'intercept') %>% 
  arrange(desc(abs(coef_value)))

cv.lasso2_coef_oos_df <- rbind(cv.lasso2_intercept, cv.lasso2_ordered_coef_df) %>% 
  mutate(odds_value = exp(coef_value))

remove(cv.lasso2_intercept, cv.lasso2_ordered_coef_df)

# Get minimum 1se Deviance
cv.lasso2_coef_1se <- coef(cv.lasso2, select="1se")

# Determine coefficient names
cv.coef_names_1se <- unlist(cv.lasso2_coef_1se@Dimnames[1])

# Calculate number of coefficients that are non-zero
cv.lasso2_coef_1se_df <- as.data.frame(summary(cv.lasso2_coef_1se)) %>%
  mutate(coef_name = cv.coef_names_1se[i]) %>%
  rename(coef_value = x) %>%
  select(coef_name, coef_value)

cv.lasso2_intercept <- cv.lasso2_coef_1se_df %>% 
  filter(coef_name == 'intercept')

cv.lasso2_ordered_coef_df <- cv.lasso2_coef_1se_df %>% 
  filter(coef_name != 'intercept') %>% 
  arrange(desc(abs(coef_value)))

cv.lasso2_coef_1se_df <- rbind(cv.lasso2_intercept, cv.lasso2_ordered_coef_df) %>% 
  mutate(odds_value = exp(coef_value))

remove(cv.lasso2_intercept, cv.lasso2_ordered_coef_df)

write.csv(lasso1_coef_df, "~/Spring 2024/Big Data/Final Project/Lasso/called_ball/lasso1.csv")
write.csv(cv.lasso1_coef_oos_df, "~/Spring 2024/Big Data/Final Project/Lasso/called_ball/cv.lasso1_oos.csv")
write.csv(cv.lasso1_coef_1se_df, "~/Spring 2024/Big Data/Final Project/Lasso/called_ball/cv.lasso1_1se.csv")
write.csv(lasso2_coef_df, "~/Spring 2024/Big Data/Final Project/Lasso/called_ball/lasso2.csv")
write.csv(cv.lasso2_coef_oos_df, "~/Spring 2024/Big Data/Final Project/Lasso/called_ball/cv.lasso2_oos.csv")
write.csv(cv.lasso2_coef_1se_df, "~/Spring 2024/Big Data/Final Project/Lasso/called_ball/cv.lasso2_1se.csv")
