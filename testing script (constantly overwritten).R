n <- 100
simuData <- function(n) {
  X <- rnorm(n,0,1)
  p <- 1 / (1 + exp(-X))
  trt <- rbinom(n = n, size = 1, prob = p)
  Y <- 2*trt + X + rnorm(n,0,1) #Treatment effect is 2
  return(cbind(Y, trt, X))
}
data <- simuData(3000)
Ttest <- T_learner(data)
Xtest <- X_learner(data)
lm <- lm(formula = Y ~ trt + X, data = data.frame(data))
Ttest
Xtest
lm$coefficients
summary(lm)

# Perform bootstrapping
boot_result <- boot(
  data = data,
  statistic = X_learner,
  R = 100  
)

mean(boot_result$t[,1]) 
sd(boot_result$t[,1])
hist(boot_result$t[,1], breaks = 10)
summary(lm(formula = Y ~ trt + X, data = data))

