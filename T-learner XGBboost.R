library(xgboost)
library(dplyr)
library(stringr)

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

