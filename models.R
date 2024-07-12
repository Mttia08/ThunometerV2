train_test_split <- function(data, train_size = 0.8, seed = FALSE) {
  train_size <- floor(train_size * nrow(data))
  
  if (seed) {
    set.seed(123)
  }
  train_ind <- sample(seq_len(nrow(data)), size = train_size)
  
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  
  return(list(train = train, test = test))
}

linear_regression <- function(pp, data) {
  cores <- makeCluster(detectCores())
  registerDoParallel(cores)
  
  mod_lm <- caret::train(pp, data = drop_na(data), method = "lm", metric = "RMSE")
  
  stopCluster(cores)
  
  return(mod_lm)
}

knn <- function(pp, data, k = 5) {
  cores <- makeCluster(detectCores())
  registerDoParallel(cores)
  
  mod_knn <- caret::train(pp, data = drop_na(data), method = "knn", metric = "RMSE",
                          trControl = caret::trainControl(method = "cv", number = 3, savePredictions = "final"), 
                          tundGrid = data.frame(k = k))
  
  stopCluster(cores)
  
  return(mod_knn)
}

random_forest <- function(pp, data, tuning = FALSE){
  cores <- makeCluster(detectCores())
  registerDoParallel(cores)
  
  pred_count <- length(pp$var_info$variable)
  if (tuning == FALSE) {
    print('The model is currently being generated with mtry = 16 and min.node.size = 3. Please be patient...')
    grid <- expand.grid(
      .mtry = pred_count/3.5,
      .min.node.size = 3,
      .splitrule = "variance"
    )
  } else {
    print('The model is now in the tuning process. Please be patient...')
    grid <- expand.grid(
      .mtry = c(pred_count/2,pred_count/2.5,pred_count/3,pred_count/3.5,pred_count/4),
      .min.node.size = c(3,5,8,10),
      .splitrule = "variance"
    )
  }
  
  mod_rf <- caret::train(pp, data = drop_na(data), method = "ranger", metric = "RMSE",
                         trControl = caret::trainControl(method = "cv", number = 3, savePredictions = "final"),
                         tuneGrid = grid, replace = FALSE, sample.fraction = 0.5, num.trees = 100, seed = 1982)
  
  stopCluster(cores)
  
  return(mod_rf)
}