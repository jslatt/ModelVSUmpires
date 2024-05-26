library(tree)
library(randomForest)

setwd("~/Documents/Booth/Classes/Spring 24/Big Data/Final/")

source("LASSO/create_matrix.R")

# Simple tree
xset <- x_vars_no_desc %>% 
  select(-player_name, -ump_name_0b, -fielder_2)

tree_full <- tree(as.factor(y_correct_call) ~ ., 
                  data = xset)

plot(tree_full)
text(tree_full, cex=.8, font=2)
title(main = "Decision Tree: Y = Correct Call (data includes strike zone)")

# Random Forest
runRFmodel <- function(yvar, includes_sz=T, nsize) {
  
  # yvar = "y_called_ball"
  # includes_sz = T
  # nsize = 5000
  
  if (includes_sz==T) {
    xset <- x_vars_no_desc
  } else{
    xset <- x_vars_no_sz_desc
  }
  
  ydata <- get(yvar)
  
  set.seed(26783)
  train_rows <- sample(1:nrow(xset), nsize, replace = F)
  test_rows <- setdiff(1:nrow(xset), train_rows)
  
  # training split  
  x_train <- xset %>% 
    select(-player_name, -ump_name_0b, -fielder_2) %>% # dropping categoricals with >53 levels
    slice(train_rows)
  
  y_train <- ydata[train_rows]
  
  # test split  
  x_test <- xset %>% 
    select(-player_name, -ump_name_0b, -fielder_2) %>% # dropping categoricals with >53 levels
    slice(test_rows)
  
  y_test <- ydata[test_rows]
  
  tictoc::tic()
  rf_model <- randomForest(as.factor(y_train) ~ ., 
                           data=x_train) # not including "importance" to speed up run time
  model_time <- tictoc::toc()

  # feature importance
  # importance_df <- as.data.frame(importance(rf_model)) %>% 
  #   arrange(desc(`%IncMSE`))
  # 
  # importance_df$Feature <- rownames(importance_df) 
  
  #varImpPlot(rf_model, type=1)
  #plot(rf_model)
  
  # oos accuracy
  yhat <- predict(rf_model, x_test)
  
  mean_oos <- tibble(pred = yhat,
                     true = y_test) %>% 
    mutate(correct = pred==true) %>% 
    pull(correct) %>% 
    mean
  
  # export
  tibble(yvar, includes_sz, nsize, model_time$callback_msg, mean_oos)
  
}

# sample run
# a <- runRFmodel("y_correct_call", includes_sz = T, nsize = 1000)
# b <- runRFmodel("y_correct_call", includes_sz = T, nsize = 250000)

# all model inputs
crosslist <- crossing(yvar = c("y_correct_call",
                               "y_called_ball",
                               "y_called_strike",
                               "y_in_strike_zone"),
                      includes_sz = c(T, F),
                      nsize = c(1000, 10000, 100000)) %>% 
  filter(!(yvar=="y_in_strike_zone" & includes_sz==T))

stack_results <- tibble()
for (i in 1:nrow(crosslist)) {
  
  params <- crosslist %>% slice(i)
  
  results <- runRFmodel(params$yvar, params$includes_sz, params$nsize)
  
  write_csv(results, glue::glue("RandomForest/rf_results_{i}.csv"))
  
  stack_results <- bind_rows(stack_results, results)
  
}

stack_results

#############
# Importance measures for model w/out location data
xset <- x_vars_no_sz_desc

ydata <- y_correct_call

set.seed(26783)
train_rows <- sample(1:nrow(xset), 10000, replace = F)
test_rows <- setdiff(1:nrow(xset), 10000, train_rows)

# training split  
x_train <- xset %>% 
  select(-player_name, -ump_name_0b, -fielder_2) %>% # dropping categoricals with >53 levels
  slice(train_rows)

y_train <- ydata[train_rows]

tictoc::tic()
rf_model <- randomForest(as.factor(y_train) ~ ., 
                         data=x_train,
                         importance=T)
model_time <- tictoc::toc()

varImpPlot(rf_model, type=1, 
           main = "Feature Importance: Y = Correct Call \n(Model excludes location data)")

# Importance measures for model w/ location data
xset <- x_vars_no_desc

ydata <- y_correct_call

set.seed(26783)
train_rows <- sample(1:nrow(xset), 10000, replace = F)
test_rows <- setdiff(1:nrow(xset), 10000, train_rows)

# training split  
x_train <- xset %>% 
  select(-player_name, -ump_name_0b, -fielder_2) %>% # dropping categoricals with >53 levels
  slice(train_rows)

y_train <- ydata[train_rows]

tictoc::tic()
rf_model <- randomForest(as.factor(y_train) ~ ., 
                         data=x_train,
                         importance=T)
model_time <- tictoc::toc()

varImpPlot(rf_model, type=1, 
           main = "Feature Importance: Y = Correct Call \n(Model includes location data)")
#plot(rf_model)



