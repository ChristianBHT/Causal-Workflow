library(xgboost)
library(parallel)
library(dplyr)
library(ggplot2)
library(stringr)
library(ipred)
library(forcats)
library(boot)
library(tidyr)
# Creating a function to calculate bootstrapped statistics 

data <- simuData(500)
formula <- Y ~ X + F1 + F2
treatment = 'trt'

T_learner_boost <- function(data, 
                            index = NULL, 
                            formula,
                            treatment,
                            p = 0.8,
                            nfold_t = 5,
                            nfold_c = 5,
                            nrounds_t = 100, 
                            nrounds_c = 100, 
                            eta_t = 0.05, 
                            eta_c = 0.05, 
                            max_depth_t = 5, 
                            max_depth_c = 5,
                            verbose = 0,
                            early_stopping = 5,
                            n_jobs = 1) { 
  if (is.null(index)) {
    data = data
  } else {
    data <- data[index, ]
  }
  
  treatment <- data[treatment]
  
  data_t <- data[which(treatment == 1),]
  data_c <- data[which(treatment == 0),] 
  
  label_t <- subset(data_t, select = c( as.character(update(formula, . ~ .)[[2]])))
  label_c <- subset(data_c, select = c( as.character(update(formula, . ~ .)[[2]])))
  
  independent_vars <- all.vars(formula)[-1]
  data_t <- data_t %>% select(all_of(independent_vars))
  data_c <- data_c %>% select(all_of(independent_vars))
  full_data <- data %>% select(all_of(independent_vars))
  
  if (any(sapply(full_data, is.factor))) {
    factor_vars <- names(full_data)[sapply(full_data, is.factor)]
   
    features_t <- cbind(data_t[!sapply(data_t, is.factor)], model.matrix(~ . - 1, data = data_t[factor_vars])) 
    features_c <- cbind(data_c[!sapply(data_c, is.factor)], model.matrix(~ . - 1, data = data_c[factor_vars]))
    full_data <- cbind(full_data[!sapply(full_data, is.factor)], model.matrix(~ . - 1, data = full_data[factor_vars]))
    
    data_matrix_t <- xgb.DMatrix(data = as.matrix(features_t), label = as.matrix(label_t))
    data_matrix_c <- xgb.DMatrix(data = as.matrix(features_c), label = as.matrix(label_c))
    data_matrix <- xgb.DMatrix(data = as.matrix(full_data))

  } else {
    features_t <- data_t %>% select(all_of(independent_vars))
    features_c <- data_c %>% select(all_of(independent_vars))
    
    data_matrix_t <- xgb.DMatrix(data = as.matrix(features_t), label = as.matrix(label_t))
    data_matrix_c <- xgb.DMatrix(data = as.matrix(features_c), label = as.matrix(label_c))
    data_matrix <- xgb.DMatrix(data = as.matrix(full_data))
  }
  
  
  params_t <- list(
    objective = "reg:squarederror", 
    eta = eta_t,                      
    max_depth = max_depth_t,
    n_jobs = n_jobs
  )
  params_c <- list(
    objective = "reg:squarederror", 
    eta = eta_c,                      
    max_depth = max_depth_c,
    n_jobs = n_jobs
  )

  model_t_cv <- xgboost::xgb.cv(
    data = data_matrix_t,
    params = params_t,
    nfold = nfold_t, 
    nrounds = nrounds_t, 
    early_stopping_rounds = early_stopping,
    verbose = verbose
  )
  
  model_c_cv <- xgboost::xgb.cv(
    data = data_matrix_c,
    params = params_c,
    nfold = nfold_c, 
    nrounds = nrounds_c, 
    early_stopping_rounds = early_stopping,
    verbose = verbose
  )
  
  optimal_nrounds_t <- model_t_cv$best_iteration
  optimal_nrounds_c <- model_c_cv$best_iteration
  
  model_t <- xgboost::xgb.train(params = params_t,
                                data = data_matrix_t,  
                                nrounds = optimal_nrounds_t,
                                verbose = verbose)
  model_c <- xgboost::xgb.train(params = params_c,
                                data = data_matrix_c,  
                                nrounds = optimal_nrounds_c,
                                verbose = verbose)
  
  y_1 <- predict(model_t, newdata = data_matrix)
  y_0 <- predict(model_c, newdata = data_matrix) 
  
  ITE <- y_1-y_0
  ATE <- mean(ITE)
  mu_0 <- mean(y_0)
  mu_1 <- mean(y_1)
  
  return(c(ATE, mu_1, mu_0))
}


T_learner_old <- function(data,
                      indices = NULL,
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
  
  outcome <- data.frame(data[,1])
  trt <- data[,2]
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
                      max_depth = 6,
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
  
  outcome <- data.frame(data[,1])
  trt <- data[,2]
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

  optimal_nrounds <- t_1_cv$best_iteration
  
  
  t_1 <- xgboost::xgb.train(
    params = params,
    data = data_matrix_t,  
    nrounds = optimal_nrounds,  
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
  
  t_0 <- xgboost::xgb.train(
    params = params,
    data = data_matrix_c,  
    nrounds = optimal_nrounds,
    verbose = verbose,
    print_every_n = print_every_n
  )
  
  full_data_matrix <- xgb.DMatrix(data = as.matrix(adjustment))
  
  mu_1_hat <- predict(t_1, newdata = full_data_matrix)
  mu_0_hat <- predict(t_0, newdata = full_data_matrix) 
  
  d_t <- y_t - mu_0_hat[trt == 1] #treatment group
  d_c <- mu_0_hat[trt == 0] - y_c #control group
  
  #Model imputed ITE as a function of treatment group adjustment set 
  data_matrix_dt <- xgb.DMatrix(data = as.matrix(x_t), label = as.matrix(d_t))
  
  x_1_cv <- xgboost::xgb.cv(
    data = data_matrix_dt,
    params = params,
    nfold = nfold_t, 
    nrounds = nrounds, 
    print_every_n = print_every_n,
    early_stopping_rounds = early_stopping,
    verbose = verbose
  )
  
  optimal_nrounds <- x_1_cv$best_iteration
  
  x_1_fit <- xgboost::xgb.train(
    params = params,
    data = data_matrix_dt,  
    nrounds = optimal_nrounds,  
    verbose = verbose,
    print_every_n = print_every_n
  )
  
  #Model imputed ITE as a function of CONTROL group adjustment set 
  data_matrix_dc <- xgb.DMatrix(data = as.matrix(x_c), label = as.matrix(d_c))
  
  x_0_cv <- xgboost::xgb.cv(
    data = data_matrix_dc,
    params = params,
    nfold = nfold_t, 
    nrounds = nrounds, 
    print_every_n = print_every_n,
    early_stopping_rounds = early_stopping,
    verbose = verbose
  )
  
  optimal_nrounds <- x_0_cv$best_iteration
  
  x_0_fit <- xgboost::xgb.train(
    params = params,
    data = data_matrix_dc,  
    nrounds = optimal_nrounds,  
    verbose = verbose,
    print_every_n = print_every_n
  )
  
  tau_1_pred = predict(x_1_fit, newdata = full_data_matrix)
  tau_0_pred = predict(x_0_fit, newdata = full_data_matrix)
  
  #Calculate propensity score
  params <- list(
    objective = "binary:logistic",
    eta = eta,
    max_depth = max_depth,
    nthread = nthread
  )
  
  probs_matrix <- xgb.DMatrix(data = as.matrix(adjustment), label = as.matrix(trt))
  
  trt_cv <- xgboost::xgb.cv(
    data = probs_matrix,
    params = params,
    nfold = nfold_t, 
    nrounds = nrounds, 
    print_every_n = print_every_n,
    early_stopping_rounds = early_stopping,
    verbose = verbose
  )
    
  optimal_nrounds <- trt_cv$best_iteration
  
  trt_fit <- xgboost::xgb.train(
    params = params,
    data = probs_matrix,  
    nrounds = optimal_nrounds,  
    verbose = verbose,
    print_every_n = print_every_n
  )
  
  p_hat <- predict(trt_fit, newdata = probs_matrix)
  
  tau_hat <- tau_1_pred*(1-p_hat) + tau_0_pred*(p_hat) 
  
  ATE <- mean(tau_hat)
  
  result <- c(ATE) 
  
  return(result)
}

