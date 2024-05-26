
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(glmnet)
library(caret)


load('combo.RData')





# Descriptive Statistics and Trends
# Summary statistics for pitch speeds
summary(combo$release_speed)
ggplot(combo, aes(x = release_speed)) +
  geom_histogram(binwidth = 1, fill = 'blue', color = 'black') +
  facet_wrap(~ pitch_type) +
  labs(title = 'Distribution of Pitch Speeds by Pitch Type', x = 'Release Speed (mph)', y = 'Count')

# Average pitch speeds over time
combo$game_date <- as.Date(combo$game_date, format="%m/%d/%Y")
avg_speed_time <- combo %>%
  group_by(game_date) %>%
  summarise(avg_speed = mean(release_speed, na.rm = TRUE))

ggplot(avg_speed_time, aes(x = game_date, y = avg_speed)) +
  geom_line(color = 'black') +
  labs(title = 'Average Pitch Speed Over Time', x = 'Date', y = 'Average Speed (mph)')


# Calculate average release speed by inning
avg_speed_inning <- combo %>%
  group_by(inning) %>%
  summarise(avg_speed = mean(release_speed, na.rm = TRUE))

# Plot average release speed by inning
ggplot(avg_speed_inning, aes(x = inning, y = avg_speed)) +
  geom_line(color = 'black') +
  geom_point(color = 'black') +
  labs(title = 'Average Release Speed by Inning', x = 'Inning', y = 'Average Speed (mph)')


ggplot(combo, aes(x = as.factor(is_called_strike), y = release_speed)) +
  geom_boxplot(fill = 'blue', color = 'black') +
  labs(title = 'Release Speed by Strike or Not', x = 'Is it called a strike (True or False).', y = 'Release Speed (mph)')





# Clustering
# Select relevant features for clustering
pitch_data <- data1 %>%
  select(release_speed, release_pos_x, release_pos_z, spin_axis) %>%
  na.omit()

# Scale the data
pitch_data_scaled <- scale(pitch_data)

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(pitch_data_scaled, centers = 3)

# Add cluster assignment to the original data
data1$cluster <- kmeans_result$cluster

# Visualize clusters
ggplot(data1, aes(x = release_pos_x, y = release_pos_z, color = as.factor(cluster))) +
  geom_point(alpha = 0.5) +
  labs(title = 'Pitch Clusters', x = 'Release Position X', y = 'Release Position Z', color = 'Cluster')

# Lasso Regression
# Prepare the data for modeling
model_data <- data1 %>%
  mutate(strikeout = ifelse(events == 'strikeout', 1, 0)) %>%
  select(strikeout, release_speed, release_pos_x, release_pos_z, spin_axis, delta_home_win_exp, delta_run_exp) %>%
  na.omit()

# Define predictor and response variables
x <- model.matrix(strikeout ~ ., model_data)[, -1]
y <- model_data$strikeout

# Perform Lasso regression
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = 'binomial')

# Best lambda value
best_lambda <- cv.lasso$lambda.min

# Fit the final model
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda, family = 'binomial')

# Display coefficients
coef(lasso_model)

# Predict and evaluate the model
preds <- predict(lasso_model, x, type = 'response')
pred_labels <- ifelse(preds > 0.5, 1, 0)
conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(y))
conf_matrix

