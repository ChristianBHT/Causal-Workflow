library(xgboost)
library(parallel)
library(dplyr)
data <- data.frame(simuData(200))
data <- data %>% select(Y, trt, X)  # The columns must be in this ordering, first outcome, then treatment indicator then the adjustment set
# Creating a function to calculate bootstrapped statistics 
T_learner <- function(data,
                      indices = NULL,
                      objective = "reg:squarederror",
                      max_depth = c(2,3,4,5,6),
                      eta = 0.1,
                      early_stopping = 10,
                      nfold_t = 5,
                      nfold_c = 5,
                      nrounds = 500,
                      num_search_rounds = 10,
                      print_every_n = 100,
                      verbose = FALSE,
                      nthread = parallel::detectCores()-1,
                      ...){

  if(any(sapply(data, is.factor))) {
    stop("There are factor variables in the data. Please recode to dummies.")
  } 
  
  if (is.null(indices)) {
    data = data
  } else {
    data <- data[indices, ]
  }
  
  outcome <- data[1]
  trt <- data[2]
  adjustment <-  data.frame(data[, 3:ncol(data)])
  
  # Check if treatment is correctly encoded
  if (all(trt == 0 | trt == 1) == FALSE) {
    stop("Treatment must be a 0 1 indicator")
  }
  
  x_t <- adjustment[which(trt == 1),]
  y_t <- outcome[which(trt == 1),]
  x_c <- adjustment[which(trt == 0),] 
  y_c <- outcome[which(trt == 0),]
  
  data_matrix_t <- xgboost::xgb.DMatrix(data = as.matrix(x_t), label = as.matrix(y_t))
  data_matrix_c <- xgboost::xgb.DMatrix(data = as.matrix(x_c), label = as.matrix(y_c))
  
  # Hyperparameters
  params <- list(
    objective = "reg:squarederror",
    eta = eta,
    max_depth = max_depth,
    nthread = nthread
  )
  
  # Use xgb.cv for cross validation 
  t_1_cv <- xgboost::xgb.cv(
    data = data_matrix_t,
    params = params,
    nfold = nfold_t, 
    nrounds = nrounds, 
    print_every_n = print_every_n,
    early_stopping_rounds = early_stopping,
    verbose = verbose
  )
  # Extracting the best nrounds
  optimal_nrounds <- t_1_cv$best_iteration
  

  # Train the final model 
  t_1_final <- xgboost::xgb.train(
    params = params,
    data = data_matrix_t,  
    nrounds = optimal_nrounds,  # Optimal number of rounds determined from CV
    verbose = verbose,
    print_every_n = print_every_n
    )
  
  t_0_cv <- xgboost::xgb.cv(
    data = data_matrix_c,
    params = params,
    nfold = nfold_c, 
    nrounds = nrounds, 
    print_every_n = print_every_n,
    early_stopping_rounds = early_stopping,
    verbose = verbose
  )
  
  optimal_nrounds <- t_0_cv$best_iteration
  
  # Train the final model 
  t_0_final <- xgboost::xgb.train(
    params = params,
    data = data_matrix_c,  
    nrounds = optimal_nrounds,
    verbose = verbose,
    print_every_n = print_every_n
  )
  
  full_data_matrix <- xgb.DMatrix(data = as.matrix(adjustment))
  y_1_hat <- predict(t_1_final, newdata = full_data_matrix)
  y_0_hat <- predict(t_0_final, newdata = full_data_matrix) 
  tau_hat <- y_1_hat - y_0_hat
  
  ATE <- mean(tau_hat)
  Rsquared_t <- cor(outcome, y_1_hat)^2
  Rsquared_c <- cor(outcome, y_0_hat)^2
  
  
  result <- c(ATE, Rsquared_t, Rsquared_c) 
  
  return(result)
}




X_learner <- function(data,
                      indices = NULL,
                      objective = "reg:squarederror",
                      max_depth = c(2,3,4,5,6),
                      eta = 0.1,
                      early_stopping = 10,
                      nfold_t = 5,
                      nfold_c = 5,
                      nrounds = 500,
                      num_search_rounds = 10,
                      print_every_n = 100,
                      verbose = FALSE,
                      nthread = parallel::detectCores()-1,
                      ...){
  
  if(any(sapply(data, is.factor))) {
    stop("There are factor variables in the data. Please recode to dummies.")
  } 
  
  if (is.null(indices)) {
    data = data
  } else {
    data <- data[indices, ]
  }
  
  outcome <- data[1]
  trt <- data[2]
  adjustment <-  data.frame(data[, 3:ncol(data)])
  
  # Check if treatment is correctly encoded
  if (all(trt == 0 | trt == 1) == FALSE) {
    stop("Treatment must be a 0 1 indicator")
  }
  
  x_t <- adjustment[which(trt == 1),]
  y_t <- outcome[which(trt == 1),]
  x_c <- adjustment[which(trt == 0),] 
  y_c <- outcome[which(trt == 0),]
  
  data_matrix_t <- xgboost::xgb.DMatrix(data = as.matrix(x_t), label = as.matrix(y_t))
  data_matrix_c <- xgboost::xgb.DMatrix(data = as.matrix(x_c), label = as.matrix(y_c))
  
  # Hyperparameters
  params <- list(
    objective = "reg:squarederror",
    eta = eta,
    max_depth = max_depth,
    nthread = nthread
  )
  
  # Use xgb.cv for cross validation 
  t_1_cv <- xgboost::xgb.cv(
    data = data_matrix_t,
    params = params,
    nfold = nfold_t, 
    nrounds = nrounds, 
    print_every_n = print_every_n,
    early_stopping_rounds = early_stopping,
    verbose = verbose
  )
  # Extracting the best nrounds
  optimal_nrounds <- t_1_cv$best_iteration
  
  
  # Train the final model 
  t_1_final <- xgboost::xgb.train(
    params = params,
    data = data_matrix_t,  
    nrounds = optimal_nrounds,  # Optimal number of rounds determined from CV
    verbose = verbose,
    print_every_n = print_every_n
  )
  
  t_0_cv <- xgboost::xgb.cv(
    data = data_matrix_c,
    params = params,
    nfold = nfold_c, 
    nrounds = nrounds, 
    print_every_n = print_every_n,
    early_stopping_rounds = early_stopping,
    verbose = verbose
  )
  
  optimal_nrounds <- t_0_cv$best_iteration
  
  # Train the final model 
  t_0_final <- xgboost::xgb.train(
    params = params,
    data = data_matrix_c,  
    nrounds = optimal_nrounds,
    verbose = verbose,
    print_every_n = print_every_n
  )
  
  full_data_matrix <- xgb.DMatrix(data = as.matrix(adjustment))
  y_1_hat <- predict(t_1_final, newdata = full_data_matrix)
  y_0_hat <- predict(t_0_final, newdata = full_data_matrix) 
  tau_hat <- y_1_hat - y_0_hat
  
  ATE <- mean(tau_hat)
  Rsquared_t <- cor(outcome, y_1_hat)^2
  Rsquared_c <- cor(outcome, y_0_hat)^2
  
  
  result <- c(ATE, Rsquared_t, Rsquared_c) 
  
  return(result)
}
