xgboost_test <- function(formula = NULL,
                         data = NULL,
                         indices,
                         p = NULL,
                         objective = "reg:squarederror",
                         early_stopping = 10,
                         nrounds = 60,
                         eta = 0.1,
                         max_depth = c(3,4,5),
                         n_folds = 5,
                         ...) {
  if (is.null(indices)) {
    resample <- data
  } else {
    resample <- data[indices, ]
  }
  
  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]]
  # Check if there are factor variables in the feature set
  if (any(sapply(resample, is.factor))) {
    features <- resample[independent]
    label <- resample[[dependent]]
    # create data matrix
    features <- model.matrix(~ . - 1, data = features)
    # Create the data matrix for the XGBoost function
    data_matrix <- xgboost::xgb.DMatrix(data = as.matrix(features), label = as.matrix(label))
  } else {
    # Create the data matrix for the XGBoost function without factor variables in the feature set
    features <- resample[independent]
    label <- resample[[dependent]]
    data_matrix <- xgboost::xgb.DMatrix(data = as.matrix(features), label = as.matrix(label))
  }
  # Hyperparameters search
  nrounds_values <- nrounds
  best_max_depth <- NULL
  best_nrounds <- NULL
  best_test <- Inf
  best_iteration <- NULL
  # Perform grid search over hyperparameters
  for (depth in max_depth) {
    params <- list(
      eta = eta,
      max_depth = depth
    )
    for (fold in 1:n_folds) {
      fold_size <- floor(nrow(data_matrix) / n_folds)
      test_indices <- ((fold - 1) * fold_size + 1):(fold * fold_size)
      train_indices <- setdiff(1:nrow(data_matrix), test_indices)
      train_data <- data_matrix[train_indices, ]
      test_data <- data_matrix[test_indices, ]
      watchlist <- list(train = train_data, test = test_data)
      model <- xgboost::xgb.train(
        data = watchlist$train,
        objective = objective,
        max_depth = depth,
        eta =eta,
        nrounds = nrounds,
        early_stopping_rounds = early_stopping,
        nthread = 1,
        watchlist = watchlist,
        verbose = FALSE
      )
      if (inherits(objective, c("binary:logistic", "multi:softmax"))) {
        best_iteration <-  which.min(model$evaluation_log$test_logloss)
        min_cv <- model$evaluation_log$test_logloss[best_iteration]
      } else {
        best_iteration <-  which.min(model$evaluation_log$test_rmse)
        min_cv <- model$evaluation_log$test_rmse[best_iteration]
      }
      # Check if this combination has a lower RMSE than the previous best
      if (min_cv < best_test) {
        best_test <- min_cv
        best_max_depth <- depth
        best_nrounds <- best_iteration
      }
    }
  }
  
  # Split the data
  inTraining <- sample(1:nrow(resample), size = floor(p * nrow(resample)))
  training <- resample[inTraining, ]
  testing <- resample[-inTraining, ]
  
  # Check if there are factor variables in the feature set
  if (any(sapply(training, is.factor))) {
    train_features <- training[independent]
    train_label <- training[[dependent]]
    test_features <- testing[independent]
    test_label <- testing[[dependent]]
    # create data matrix
    train_features <- model.matrix(~ . - 1, data = train_features)
    test_features <- model.matrix(~ . - 1, data = test_features)
    # Create the data matrix for the XGBoost function
    train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgboost::xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
  } else {
    # Create the data matrix for the XGBoost function without factor variables in the feature set
    train_features <- training[independent]
    train_label <- training[[dependent]]
    test_features <- testing[independent]
    test_label <- testing[[dependent]]
    train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgboost::xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
  }
  
  # Model1
  # Set best max depth
  if (inherits(objective, "multi:softmax")) {
    params <- list(
      eta = eta,
      max_depth = best_max_depth,
      num_class = num_class
    )
  } else {
    params <- list(
      eta = eta,
      max_depth = best_max_depth
    )
  }
  
  # Train the XGBoost models
  model1 <- xgboost::xgboost(data = train_matrix,
                             objective = objective,
                             params = params,
                             nrounds = best_nrounds,
                             verbose=0,
                             nthread = 1)
  # Get performance score model 1 if statement for separating classification and regression
  if (inherits(objective, c("binary:logistic", "multi:softmax"))) {
    # Predict on test set using model 1
    predictions <- predict(model1, test_matrix)
    # Predictions of binary outcome are probabilities
    if (inherits(objective, "binary:logistic")) {
      predictions <- ifelse(predictions > 0.5, 1, 0)
    }
    # Confusion Matrix
    conf_matrix <- caret::confusionMatrix(as.factor(predictions), as.factor(test_label))
    # Extract accuracy
    mod1_metric1 <- conf_matrix$overall[1]
    # Extract Kappa score
    mod1_metric2 <- conf_matrix$overall[2]
  } else {
    # Predict on test set using model 1
    predictions <- predict(model1, newdata = test_matrix)
    # Calculate RMSE
    mod1_metric1 <- Metrics::rmse(test_label, predictions)
    # Calculate R2
    mod1_metric2 <- cor(predictions, test_label)^2
  }
  # Replacing the variable with the reshuffled variable
  training[independent[[1]]] <- sample(training[[independent[1]]]) # Se på R()
  # Creating new feature set, same steps as above
  if (any(sapply(training, is.factor))) {
    model2_train_features <- training[independent]
    model2_train_features <- model.matrix(~ . - 1, data = model2_train_features)
    model2_train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(model2_train_features), label = as.matrix(train_label))
  } else {
    # Create the data matrix for the XGBoost function without factor variables in the feature set
    model2_train_features <- training[independent]
    model2_train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(model2_train_features), label = as.matrix(train_label))
  }
  
  model2 <- xgboost::xgboost(data = model2_train_matrix,
                             objective = objective,
                             params = params,
                             nrounds = best_nrounds,
                             verbose=0,
                             nthread = 1)
  
  if (inherits(objective, c("binary:logistic", "multi:softmax"))) {
    predictions <- predict(model2, test_matrix)
    if (inherits(objective, "binary:logistic")) {
      predictions <- ifelse(predictions > 0.5, 1, 0)
    }
    conf_matrix <- caret::confusionMatrix(as.factor(predictions), as.factor(test_label))
    mod2_metric1 <- conf_matrix$overall[1]
    mod2_metric2 <- conf_matrix$overall[2]
  } else {
    predictions <- predict(model2, newdata = test_matrix)
    mod2_metric1 <- Metrics::rmse(test_label, predictions)
    mod2_metric2 <- cor(predictions, test_label)^2
  }
  
  if (inherits(objective, c("binary:logistic", "multi:softmax"))) {
    result <- c(mod1_metric1 - mod2_metric1, mod1_metric2 - mod2_metric2)
    names(result) <- c("Difference Accuracy", "Difference Kappa score")
  } else {
    result <- c(mod1_metric1 - mod2_metric1, mod1_metric2 - mod2_metric2)
    names(result) <- c("Difference RMSE", "Difference R-squared")
  }
  return(result)
}



bagging_test <- function(formula = NULL,
                         data = NULL,
                         indices,
                         p = NULL,
                         nbagg = 50,
                         n_folds = 10,
                         ...) {
  
  if (is.null(indices)) {
    resample <- data
  } else {
    resample <- data[indices, ]
  }
  
  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]]
  # Split the data
  inTraining <- sample(1:nrow(resample), size = floor(p * nrow(resample)))
  training <- resample[inTraining, ]
  testing <- resample[-inTraining, ]
  # Model1
  model1 <- ipred::bagging(formula = formula, data = training, coob = TRUE, nbagg = nbagg)
  # Check if outcome is a factor variable (binary variables must be factor var.))
  if (inherits(data[[dependent]], "factor")) { #inherit
    # Predict on test set using model 1
    predictions <- predict(model1, newdata=testing)
    # Confusion Matrix
    conf_matrix <- caret::confusionMatrix(predictions, testing[[dependent]])
    # Extract accuracy
    mod1_metric1 <- conf_matrix$overall[1]
    # Extract Kappa score
    mod1_metric2 <- conf_matrix$overall[2]
  } else {
    # Predict on test set using model 1
    predictions <- predict(model1, newdata=testing)
    # Calculate RMSE
    mod1_metric1 <- Metrics::rmse(testing[[dependent]], predictions)
    # Calculate R2
    if (sd(predictions)==0) {
      mod1_metric2 <- 0
    } else {
      mod1_metric2 <- cor(predictions, testing[[dependent]])^2
    }
  }
  # Replacing the variable with the reshuffled variable
  training[independent[[1]]] <- sample(training[[independent[1]]])
  # model2
  model2 <- ipred::bagging(formula = formula, data = training, coob = TRUE)
  if (inherits(data[[dependent]], "factor")) {
    # Predict on test set using model 1
    predictions <- predict(model2, newdata=testing)
    # Confusion Matrix
    conf_matrix <- caret::confusionMatrix(predictions, testing[[dependent]])
    # Extract accuracy
    mod2_metric1 <- conf_matrix$overall[1]
    # Extract Kappa score
    mod2_metric2 <- conf_matrix$overall[2]
  } else {
    # Predict on test set using model 2
    predictions <- predict(model2, newdata=testing)
    # Calculate RMSE
    mod2_metric1 <- Metrics::rmse(testing[[dependent]], predictions)
    # Calculate R2
    if (sd(predictions)==0) {
      mod2_metric2 <- 0
    } else {
      mod2_metric2 <- cor(predictions, testing[[dependent]])^2
    }
  }
  
  if (inherits(data[[dependent]], "factor")) {
    result <- c(mod1_metric1 - mod2_metric1, mod1_metric2 - mod2_metric2)
    names(result) <- c("Difference Accuracy", "Difference Kappa score")
  } else {
    result <- c(mod1_metric1 - mod2_metric1, mod1_metric2 - mod2_metric2)
    names(result) <- c("Difference RMSE", "Difference R-squared")
  }
  return(result)
}



BootyTest <- function(formula = NULL, data = data, statistic, nboot = 25,
                      bootstrap_sample = TRUE, bayes = TRUE, p = NULL,
                      alpha = c(rep(1,nrow(data))), dag = NULL, dag_cond = NULL,
                      parallel = NULL, metric  = c('RMSE', 'R2', 'Accuracy', 'Kappa score'), ...){
  if (is.null(data)) {
    stop("No data, no testing")
  }
  if (is.null(p)) {
    stop("Please provide the parameter p (size of training set)")
  }
  if (p <= 0 || p >= 1) {
    stop("You silly you, p must be between 0 and 1")
  }
  if (is.null(formula) & is.null(dag)) {
    stop("Please provide a R-formula for the condition to be tested, i.e. Y _||_ X | Z -> Y ~ X + Z.
         Or provide a dagitty DAG with the option: dag = 'your DAG' and dag_cond = '# of the condition in need of testing'")
  } else if (!inherits(formula, "formula") & !is.null(formula)) {
    formula <- as.formula(formula)
  } else if (is.null(formula) & !is.null(dag)) {
    # Use provided DAG and extract the testable condition
    if (!inherits(dag, 'dagitty')) {
      stop('The provided DAG needs to be a dagitty object')
    }
    characters <- unlist(dagitty::impliedConditionalIndependencies(dag)[dag_cond])
    formula_str <- paste(characters[2:length(characters)], collapse = " + ")
    formula_dep <- characters[1]
    formula <- as.formula(paste(formula_dep ,"~", formula_str))
  }
  
  if (is.null(parallel)){
    if (bootstrap_sample) {
      if (bayes) {
        if (!is.null(alpha)) {
          # Check length of alpha
          if (length(alpha) != nrow(data)) {
            stop("Error: the prior alpha should have the same number of entries as the rows in data")
          }
        }
        weights <- DirichletReg::rdirichlet(nboot, alpha)
        output <- boot::boot(data = data,
                             statistic = function(data, indices, formula,...) {statistic(formula, data, indices, p, ...)},
                             R = rep(1,nboot),
                             formula = formula,
                             weights = weights)
      } else {
        output <- boot::boot(data = data,
                             statistic = function(data, indices, formula, ...) statistic(formula, data, indices, p, ...),
                             R = nboot,
                             formula = formula)
      }
    } else {
      if (identical(statistic, bagging_test)){
        output <- bagging_test(formula, data, indices = NULL, p, nboot)
      } else if (identical(statistic, xgboost_test)){
        output <- xgboost_test(formula, data, indices = NULL,  p)
      }
    }
  } else if (!is.null(parallel)){
    if (bootstrap_sample) {
      if (bayes) {
        if (!is.null(alpha)) {
          # Check length of alpha
          if (length(alpha) != nrow(data)) {
            stop("Error: the prior alpha should have the same number of entries as the rows in data")
          }
        }
        weights <- DirichletReg::rdirichlet(nboot, alpha)
        # Applying the parallel = "snow" option, which is only valid for windows computers
        output <- boot::boot(data = data,
                             statistic = function(data, indices, formula, ...) {statistic(formula, data, indices, p, ...)},
                             R = rep(1,nboot),
                             weights = weights,
                             formula = formula,
                             parallel = 'snow')
      } else {
        output <- boot::boot(data = data, statistic = function(data, indices, formula, ...) statistic(formula, data, indices, p, ...),
                             R = nboot,
                             formula = formula,
                             parallel = 'snow')
      }
    } else { # Case with no bootstrapping
      if (identical(statistic, bagging_test)){
        output <- bagging_test(data, indices = NULL, formula, p, nboot,...)
      } else if (identical(statistic, xgboost_test)){
        output <- xgboost_test(data, indices = NULL, formula, p,...)
      }
    }
  }
  
  return(output)
}





plot.booty <- function(your_booty, metric = 1, binwidth = 0.001) {
  
  Metric <- names(your_booty$t0[metric])
  results <- your_booty$t[,metric]
  df <- data.frame(Value = results)
  boots <- your_booty$R
  
  ggplot2::ggplot(df, ggplot2::aes(x = Value)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), binwidth = binwidth, fill = "blue", color = "black") +
    ggplot2::geom_density(ggplot2::aes(color = "Density"), linewidth = 0.5, color = 'black') +
    ggplot2::labs(
      title = paste("", sum(boots), " bootstraps"),
      x = paste("",Metric),
      y = "Density"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 12)
    )
}

