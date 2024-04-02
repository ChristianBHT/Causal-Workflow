n <- 100
simuData <- function(n) {
  X <- rnorm(n,0,1)
  p <- 1 / (1 + exp(-X))
  trt <- rbinom(n = n, size = 1, prob = p)
  Y <- 2*trt + X + rnorm(n,0,1)
  return(cbind(X, trt, Y))
}
dat <- simuData(10)


# Perform bootstrapping
boot_result <- boot(
  data = data,
  statistic = T_learner,
  R = 100  
)

mean(boot_result$t[,1]) 
sd(boot_result$t[,1])

summary(lm(formula = Y ~ trt + X, data = data))

