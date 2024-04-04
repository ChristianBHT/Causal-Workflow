n <- 100

simuData <- function(n) {
  F1 <- sample(c(1:4), size = n, replace = T)
  F2 <- sample(c(1:4), size = n, replace = T)
  X <- rnorm(n,0,1)
  p <- 1 / (1 + exp(-X))
  trt <- rbinom(n = n, size = 1, prob = p)
  Y <- 2.3*trt + X^2 + rnorm(n,0,1) 
  return(data.frame(cbind(Y, trt, X, F1, F2)))
}

data <- simuData(1000)
data$F1 <- as.factor(data$F1)
data$F2 <- as.factor(data$F2)

# T_learner_boost needs data formula where Y ~ AdjustmentSet, and a string as the name of treatment variable
# The T_learner_boost also handles factor variables
tboost <- T_learner_boost(data = data, formula = Y ~ X + F1 + F2, treatment = 'trt')
tboost

# Perform bootstrapping
boot_result <- boot(
  data = data,
  formula = Y ~ X + F1 + F2,
  treatment = 'trt',
  statistic = T_learner_boost,
  R = 100  
)

mean(boot_result$t[,1]) 
sd(boot_result$t[,1])
# hist(boot_result$t[,1], breaks = 10)
summary(lm <- lm(formula = Y ~ trt + X + F1 + F2, data = data))

data$trt <- sample(data$trt, replace = TRUE)

tboost <- T_learner_boost(data = data, formula = Y ~ X + F1 + F2, treatment = 'trt')
tboost

# Perform bootstrapping
boot_result <- boot(
  data = data,
  formula = Y ~ X + F1 + F2,
  treatment = 'trt',
  statistic = T_learner_boost,
  R = 500  
)

mean(boot_result$t[,1]) 
sd(boot_result$t[,1])

hist(boot_result$t[,1], breaks = 10)
summary(lm <- lm(formula = Y ~ trt + X + F1 + F2, data = data))




