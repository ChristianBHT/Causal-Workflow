rm(list = ls())

library(dagitty)
library(boot)
library(tidyverse)
library(xgboost)
library(dplyr)
library(margins)
library(fastDummies)
# Extract marginal effects function
extract_ame <- function(margin_effect) {
  summary_df <- summary(margin_effect)
  return(data.frame(AME = summary_df$AME, Lower = summary_df$lower, Upper = summary_df$upper))
}

setwd("C:/Causal-Workflow/")
# load("data/analysis_wide_data.Rda")
# 
# write.csv(wide_data, "data.csv", row.names = FALSE)
# # Manually fix the names!
# wide_data <- read.csv("data.csv")
# save(wide_data, file="data/data_for_causal_estimation.Rda")
load(file="data/data_for_causal_estimation.Rda")
wide_data <- wide_data %>%
  filter(id_slaughterhouse != -1) 

wide_data$ascites_prev <- 1000*wide_data$ascites/wide_data$chicken_last_day
wide_data <- wide_data[wide_data$feed_name != "Miljofor" & wide_data$feed_name != "Testfor", ]

data <- subset(wide_data, select = c('feed_name', 
                                     'ascites', 
                                     'frequent_month', 
                                     'average_out_hum', 
                                     'average_out_temp', 
                                     'id_slaughterhouse', 
                                     'ascites_prev',
                                     'growth',
                                     'sqr_growth',
                                     'average_water',
                                     'bird_density',
                                     'average_Hmax',
                                     'average_Hmin'))
 
levels = 4   
data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "Other")
data$feed_group <- as.factor(data$feed_group)

#########################################################################

data$feed_group <- as.factor(data$feed_group)
data$id_slaughterhouse <- as.factor(data$id_slaughterhouse)
data$frequent_month <- as.factor(data$frequent_month)
data$treatment_1 <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0) #Feed 1 obs: 997
data$treatment_2 <-  ifelse(data$feed_group == "Kromat Kylling 2 Laag u/k", 1, 0) #Feed 2 obs:937  
data$treatment_3 <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0) #Feed 3 obs: 632
data$treatment_4 <-  ifelse(data$feed_group == "Harmoni Kylling Ressurs", 1, 0) #Feed 4: 594
data <- subset(data, id_slaughterhouse != -1)

#------------------#
#  Total Effect    #
#------------------#
#-----------------------------------------------------------------------------
# Total effect treatment_1
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_2 == 1 | treatment_3 == 1 | treatment_4 == 1))

df <- subset(df, select = c('ascites_prev', 'treatment_1', 'frequent_month', 'average_out_hum', 'average_out_temp', 'id_slaughterhouse'))
df <- na.omit(df)

T_learner_boot_trt1 <-  boot(
  data = df,
  formula = ascites_prev ~ average_out_temp + average_out_hum + frequent_month + id_slaughterhouse ,
  treatment = 'treatment_1',
  statistic = T_learner_boost,
  R = 500  
)

hist(T_learner_boot_trt1$t[,1])
save(T_learner_boot_trt1,file="C:/Causal-Workflow/Results/total_effect_T_learner_boot_trt1.Rda")

#-----------------------------------------------------------------------------
# Total effect treatment_2
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_3 == 1 | treatment_4 == 1))

df <- subset(df, select = c('ascites_prev', 'treatment_2', 'average_out_temp', 'average_out_hum', 'frequent_month', 'id_slaughterhouse'))
df <- na.omit(df)

T_learner_boot_trt2 <-  boot(
  data = df,
  formula =  ascites_prev ~ average_out_temp + average_out_hum + frequent_month + id_slaughterhouse ,
  treatment = 'treatment_2',
  statistic = T_learner_boost,
  R = 500  
)

hist(T_learner_boot_trt2$t[,1])
save(T_learner_boot_trt2,file="C:/Causal-Workflow/Results/total_effect_T_learner_boot_trt2.Rda")


#-----------------------------------------------------------------------------
# Total effect treatment_3
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_4 == 1))

df <- subset(df, select = c('ascites_prev', 'treatment_3', 'average_out_temp', 'average_out_hum', 'frequent_month', 'id_slaughterhouse'))
df <- na.omit(df)

T_learner_boot_trt3 <-  boot(
  data = df,
  formula = ascites_prev ~ average_out_temp + average_out_hum + frequent_month + id_slaughterhouse ,
  treatment = 'treatment_3',
  statistic = T_learner_boost,
  R = 500  
)
hist(T_learner_boot_trt3$t[,1])
save(T_learner_boot_trt3,file="C:/Causal-Workflow/Results/total_effect_T_learner_boot_trt3.Rda")

#-----------------------------------------------------------------------------
# Total effect treatment_4
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_3 == 1))

df <- subset(df, select = c('ascites_prev', 'treatment_4', 'average_out_temp', 'average_out_hum', 'frequent_month', 'id_slaughterhouse'))
df <- na.omit(df)

T_learner_boot_trt4 <-  boot(
  data = df,
  formula = ascites_prev ~ average_out_temp + average_out_hum + frequent_month + id_slaughterhouse ,
  treatment = 'treatment_4',
  statistic = T_learner_boost,
  R = 500  
)
hist(T_learner_boot_trt4$t[,1])
save(T_learner_boot_trt4,file="C:/Causal-Workflow/Results/total_effect_T_learner_boot_trt4.Rda")


#--------------------------------------------------------------------------------
# Linear model
#--------------------------------------------------------------------------------
data$feed_group <- factor(data$feed_group, 
                                levels = c("Other",
                                           "Toppkylling Netto",
                                           "Kromat Kylling 2 Laag u/k",
                                           "Kromat Kylling 2 Enkel u/k", 
                                           "Harmoni Kylling Ressurs"))
lm_model <- lm(ascites_prev ~ feed_group +  average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, data = data) 
# Calculate marginal effects for the treatment variable
marginal_effects_treatment <- margins(lm_model, variables = "feed_group", method = "delta")
marginal_total <- extract_ame(marginal_effects_treatment)

save(marginal_total,file = "C:/Causal-Workflow/Results/total_effect_marginal_lm.Rda")

#------------------------------------
# Placebo treatment and dummy outcome
#-----------------------------------

# Placebo treatment
data$dummy_trt <- rbinom(nrow(data), size = 1, p = 0.5)
df <- subset(data, select = c('ascites_prev', 'average_out_temp', 'average_out_hum', 'frequent_month', 'id_slaughterhouse'))
df <- na.omit(df)

output <- list()
R = 500

for (j in 1:R) {
  df$dummy_trt <- rbinom(nrow(df),1,0.5)
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'dummy_trt')
  cat(sprintf("Sample: %d\r", j))
  flush.console()
}

T_learner_boot_placebo <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_placebo) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_placebo$ATE)
save(T_learner_boot_placebo,file="C:/Causal-Workflow/Results/total_effect_T_learner_boot_placebo.Rda")

lm_dummy <- lm(ascites_prev ~ dummy_trt + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, data = data) 

placebo_treatment_effects_lm <- margins(lm_dummy, variables = "dummy_trt")
save(placebo_treatment_effects_lm,file="C:/Causal-Workflow/Results/total_effect_placebo_treatment_effects_lm.Rda")

#-----------------------------------------------------------------------------
# Total effect dummy outcome Overall
#-----------------------------------------------------------------------------

data$treatment <- ifelse(data$treatment_1 == 1 | data$treatment_2 == 1 | data$treatment_3 == 1 | data$treatment_4 == 1, 1, 0)
df <- subset(data, select = c('ascites_prev', 'treatment', 'average_out_temp', 'average_out_hum', 'frequent_month', 'id_slaughterhouse'))
df <- na.omit(df)
output <- list()
R = 500
for (j in 1:R) {
  df$ascites_prev <-  abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'treatment')
}

T_learner_boot_dummyoutcome_all <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_dummyoutcome_all) <- c("ATE", "MU1", "MU0")

save(T_learner_boot_dummyoutcome_all,file="C:/Causal-Workflow/Results/total_effect_T_learner_boot_dummyoutcome_all.Rda")

#For linear regression
output <- list()
for (j in 1:R) {
  df$ascites_prev <-  abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse)$coefficient[2]
}

linear_boot_dummyoutcome_all <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_dummyoutcome_all$treatment)
save(linear_boot_dummyoutcome_all,file="C:/Causal-Workflow/Results/total_effect_linear_boot_dummyoutcome_all.Rda")

#-----------------------------------------------------------------------------
# Total effect dummy outcome treatment_1
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_2 == 1 | treatment_3 == 1 | treatment_4 == 1))

df <- subset(df, select = c('treatment_1', 'average_out_temp', 'average_out_hum', 'frequent_month', 'id_slaughterhouse'))
df <- na.omit(df)
output <- list()

for (j in 1:R) {
  df$ascites_prev <- abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'treatment_1')
}

T_learner_boot_dummyoutcome_1 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_dummyoutcome_1) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_dummyoutcome_1$ATE)

save(T_learner_boot_dummyoutcome_1,file="C:/Causal-Workflow/Results/total_effect_T_learner_boot_dummyoutcome_1.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-  abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_1 + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_dummyoutcome_1 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_dummyoutcome_1$treatment_1)
save(linear_boot_dummyoutcome_1,file="C:/Causal-Workflow/Results/total_effect_linear_boot_dummyoutcome_1.Rda")

#-----------------------------------------------------------------------------
# Total effect dummy outcome treatment_2
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_3 == 1 | treatment_4 == 1))
df <- subset(df, select = c('treatment_2', 'average_out_temp', 'average_out_hum', 'frequent_month', 'id_slaughterhouse'))
df <- na.omit(df)
output <- list()

for (j in 1:R) {
  df$ascites_prev <-  abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'treatment_2')
}
T_learner_boot_dummyoutcome_2 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_dummyoutcome_2) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_dummyoutcome_2$ATE)

save(T_learner_boot_dummyoutcome_2,file="C:/Causal-Workflow/Results/total_effect_T_learner_boot_dummyoutcome_2.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-  abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_2 + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_dummyoutcome_2 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_dummyoutcome_2$treatment_2)
save(linear_boot_dummyoutcome_2,file="C:/Causal-Workflow/Results/total_effect_linear_boot_dummyoutcome_2.Rda")

#-----------------------------------------------------------------------------
# Total effect dummy outcome treatment_3
#-----------------------------------------------------------------------------

df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_4 == 1))
df <- subset(df, select = c('treatment_3', 'average_out_temp', 'average_out_hum', 'frequent_month', 'id_slaughterhouse'))
df <- na.omit(df)
output <- list()

for (j in 1:R) {
  df$ascites_prev <-   abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'treatment_3')
}
T_learner_boot_dummyoutcome_3 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_dummyoutcome_3) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_dummyoutcome_3$ATE)
mean(T_learner_boot_dummyoutcome_3$ATE)
save(T_learner_boot_dummyoutcome_3,file="C:/Causal-Workflow/Results/total_effect_T_learner_boot_dummyoutcome_3.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-   abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_3 + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_dummyoutcome_3 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_dummyoutcome_3$treatment_3)
save(linear_boot_dummyoutcome_3,file="C:/Causal-Workflow/Results/total_effect_linear_boot_dummyoutcome_3.Rda")

#-----------------------------------------------------------------------------
# Total effect dummyoutcome treatment_4
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_3 == 1))
df <- subset(df, select = c('treatment_4', 'average_out_temp', 'average_out_hum', 'frequent_month', 'id_slaughterhouse'))
df <- na.omit(df)
output <- list()

for (j in 1:R) {
  df$ascites_prev <- abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'treatment_4')
}
T_learner_boot_dummyoutcome_4 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_dummyoutcome_4) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_dummyoutcome_4$ATE)
mean(T_learner_boot_dummyoutcome_4$ATE)
save(T_learner_boot_dummyoutcome_4,file="C:/Causal-Workflow/Results/total_effect_T_learner_boot_dummyoutcome_4.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-    abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_4 + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_dummyoutcome_4 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_dummyoutcome_4$treatment_4)
save(linear_boot_dummyoutcome_4,file="C:/Causal-Workflow/Results/total_effect_linear_boot_dummyoutcome_4.Rda")

#-----------------------------------------------------------------------------------------------------------------
###################################################################################################################
#-----------------------------------------------------------------------------------------------------------------

#------------------#
#  Direct Effect    #
#------------------#

#-----------------------------------------------------------------------------
# Direct effect treatment_1
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_2 == 1 | treatment_3 == 1 | treatment_4 == 1))

df <- subset(df, select = c('ascites_prev', 
                            'treatment_1', 
                            'growth', 
                            'sqr_growth',
                            'average_water',
                            'bird_density',
                            'average_Hmax',
                            'average_Hmin',
                            'frequent_month', 
                            'average_out_hum', 
                            'average_out_temp', 
                            'id_slaughterhouse'))
df <- na.omit(df)
T_learner_boot_trt1 <-  boot(
  data = df,
  formula = ascites_prev ~ growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse ,
  treatment = 'treatment_1',
  statistic = T_learner_boost,
  R = 500  
)

hist(T_learner_boot_trt1$t[,1])
save(T_learner_boot_trt1,file="C:/Causal-Workflow/Results/direct_effect_T_learner_boot_trt1.Rda")

#-----------------------------------------------------------------------------
# Direct effect treatment_2
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_3 == 1 | treatment_4 == 1))

df <- subset(df, select = c('ascites_prev', 
                            'treatment_2', 
                            'growth', 
                            'sqr_growth',
                            'average_water',
                            'bird_density',
                            'average_Hmax',
                            'average_Hmin',
                            'frequent_month', 
                            'average_out_hum', 
                            'average_out_temp', 
                            'id_slaughterhouse'))
df <- na.omit(df)

T_learner_boot_trt2 <-  boot(
  data = df,
  formula = ascites_prev ~ growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse ,
  treatment = 'treatment_2',
  statistic = T_learner_boost,
  R = 500  
)
hist(T_learner_boot_trt2$t[,1])
save(T_learner_boot_trt2,file="C:/Causal-Workflow/Results/direct_effect_T_learner_boot_trt2.Rda")


#-----------------------------------------------------------------------------
# Direct effect treatment_3
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_4 == 1))

df <- subset(df, select = c('ascites_prev', 
                            'treatment_3', 
                            'growth', 
                            'sqr_growth',
                            'average_water',
                            'bird_density',
                            'average_Hmax',
                            'average_Hmin',
                            'frequent_month', 
                            'average_out_hum', 
                            'average_out_temp', 
                            'id_slaughterhouse'))
df <- na.omit(df)

T_learner_boot_trt3 <-  boot(
  data = df,
  formula = ascites_prev ~ growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse ,
  treatment = 'treatment_3',
  statistic = T_learner_boost,
  R = 500  
)
hist(T_learner_boot_trt3$t[,1])
save(T_learner_boot_trt3,file="C:/Causal-Workflow/Results/direct_effect_T_learner_boot_trt3.Rda")

#-----------------------------------------------------------------------------
# Direct effect treatment_4
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_3 == 1))

df <- subset(df, select = c('ascites_prev', 
                            'treatment_4', 
                            'growth', 
                            'sqr_growth',
                            'average_water',
                            'bird_density',
                            'average_Hmax',
                            'average_Hmin',
                            'frequent_month', 
                            'average_out_hum', 
                            'average_out_temp', 
                            'id_slaughterhouse'))
df <- na.omit(df)

T_learner_boot_trt4 <-  boot(
  data = df,
  formula = ascites_prev ~ growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse ,
  treatment = 'treatment_4',
  statistic = T_learner_boost,
  R = 500  
)
hist(T_learner_boot_trt4$t[,1])
save(T_learner_boot_trt4,file="C:/Causal-Workflow/Results/direct_effect_T_learner_boot_trt4.Rda")


#--------------------------------------------------------------------------------
# Linear model
#--------------------------------------------------------------------------------
data$feed_group <- factor(data$feed_group, 
                          levels = c("Other",
                                     "Toppkylling Netto",
                                     "Kromat Kylling 2 Laag u/k",
                                     "Kromat Kylling 2 Enkel u/k", 
                                     "Harmoni Kylling Ressurs"))
lm_model <- lm(formula = ascites_prev ~ feed_group +  growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, data = data) 
summary(lm_model)
# Calculate marginal effects for the treatment variable
marginal_effects_treatment <- margins(lm_model, variables = "feed_group")
save(marginal_effects_treatment,file="C:/Causal-Workflow/Results/direct_effect_marginal_effects_linear_model.Rda")
plot(marginal_effects_treatment)

#------------------------------------
# Placebo treatment and dummy outcome
#-----------------------------------

# Placebo treatment
data$dummy_trt <- rbinom(nrow(data), size = 1, p = 0.5)
df <- subset(data, select = c('ascites_prev', 
                              'growth', 
                              'sqr_growth',
                              'average_water',
                              'bird_density',
                              'average_Hmax',
                              'average_Hmin',
                              'frequent_month', 
                              'average_out_hum', 
                              'average_out_temp', 
                              'id_slaughterhouse'))
df <- na.omit(df)

output <- list()
R = 500

for (j in 1:R) {
  df$dummy_trt <- rbinom(nrow(df),1,0.5)
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'dummy_trt')
}

T_learner_boot_placebo <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_placebo) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_placebo$ATE)
save(T_learner_boot_placebo,file="C:/Causal-Workflow/Results/direct_effect_T_learner_boot_placebo.Rda")

lm_dummy <- lm(ascites_prev ~ dummy_trt + growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, data = data) 
# summary(lm_dummy)

placebo_treatment_effects_lm <- margins(lm_dummy, vce = 'delta')
save(placebo_treatment_effects_lm,file="C:/Causal-Workflow/Results/direct_effect_placebo_treatment_effects_lm.Rda")

#-----------------------------------------------------------------------------
# Direct effect dummy outcome Overall
#-----------------------------------------------------------------------------

data$treatment <- ifelse(data$treatment_1 == 1 | data$treatment_2 == 1 | data$treatment_3 == 1 | data$treatment_4 == 1, 1, 0)
df <- subset(data, select = c('treatment', 
                              'growth', 
                              'sqr_growth',
                              'average_water',
                              'bird_density',
                              'average_Hmax',
                              'average_Hmin',
                              'frequent_month', 
                              'average_out_hum', 
                              'average_out_temp', 
                              'id_slaughterhouse'))
df <- na.omit(df)
output <- list()
R = 500
for (j in 1:R) {
  df$ascites_prev <-  abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'treatment')
}

T_learner_boot_dummyoutcome_all <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_dummyoutcome_all) <- c("ATE", "MU1", "MU0")

save(T_learner_boot_dummyoutcome_all,file="C:/Causal-Workflow/Results/direct_effect_T_learner_boot_dummyoutcome_all.Rda")

#For linear regression
output <- list()
for (j in 1:R) {
  df$ascites_prev <-  abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment + growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse)$coefficient[2]
}

linear_boot_dummyoutcome_all <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_dummyoutcome_all$treatment)
save(linear_boot_dummyoutcome_all,file="C:/Causal-Workflow/Results/direct_effect_linear_boot_dummyoutcome_all.Rda")

#-----------------------------------------------------------------------------
# Direct effect dummy outcome treatment_1
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_2 == 1 | treatment_3 == 1 | treatment_4 == 1))

df <- subset(df, select = c('treatment_1', 
                            'growth', 
                            'sqr_growth',
                            'average_water',
                            'bird_density',
                            'average_Hmax',
                            'average_Hmin',
                            'frequent_month', 
                            'average_out_hum', 
                            'average_out_temp', 
                            'id_slaughterhouse'))
df <- na.omit(df)
output <- list()

for (j in 1:R) {
  df$ascites_prev <- abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'treatment_1')
}

T_learner_boot_dummyoutcome_1 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_dummyoutcome_1) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_dummyoutcome_1$ATE)

save(T_learner_boot_dummyoutcome_1,file="C:/Causal-Workflow/Results/direct_effect_T_learner_boot_dummyoutcome_1.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-  abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_1 + growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_dummyoutcome_1 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_dummyoutcome_1$treatment_1)
save(linear_boot_dummyoutcome_1,file="C:/Causal-Workflow/Results/direct_effect_linear_boot_dummyoutcome_1.Rda")

#-----------------------------------------------------------------------------
# Direct effect dummyoutcome treatment_2
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_3 == 1 | treatment_4 == 1))
df <- subset(df, select = c('treatment_2', 
                            'growth', 
                            'sqr_growth',
                            'average_water',
                            'bird_density',
                            'average_Hmax',
                            'average_Hmin',
                            'frequent_month', 
                            'average_out_hum', 
                            'average_out_temp', 
                            'id_slaughterhouse'))
df <- na.omit(df)
output <- list()

for (j in 1:R) {
  df$ascites_prev <-  abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'treatment_2')
}
T_learner_boot_dummyoutcome_2 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_dummyoutcome_2) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_dummyoutcome_2$ATE)

save(T_learner_boot_dummyoutcome_2,file="C:/Causal-Workflow/Results/direct_effect_T_learner_boot_dummyoutcome_2.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-  abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_2 + growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_dummyoutcome_2 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_dummyoutcome_2$treatment_2)
save(linear_boot_dummyoutcome_2,file="C:/Causal-Workflow/Results/direct_effect_linear_boot_dummyoutcome_2.Rda")

#-----------------------------------------------------------------------------
# Direct effect dummyoutcome treatment_3
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_4 == 1))
df <- subset(df, select = c('treatment_3', 
                            'growth', 
                            'sqr_growth',
                            'average_water',
                            'bird_density',
                            'average_Hmax',
                            'average_Hmin',
                            'frequent_month', 
                            'average_out_hum', 
                            'average_out_temp', 
                            'id_slaughterhouse'))
df <- na.omit(df)
output <- list()

for (j in 1:R) {
  df$ascites_prev <-   abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'treatment_3')
}
T_learner_boot_dummyoutcome_3 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_dummyoutcome_3) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_dummyoutcome_3$ATE)
mean(T_learner_boot_dummyoutcome_3$ATE)
save(T_learner_boot_dummyoutcome_3,file="C:/Causal-Workflow/Results/direct_effect_T_learner_boot_dummyoutcome_3.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-   abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_3 + growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_dummyoutcome_3 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_dummyoutcome_3$treatment_3)
save(linear_boot_dummyoutcome_3,file="C:/Causal-Workflow/Results/direct_effect_linear_boot_dummyoutcome_3.Rda")

#-----------------------------------------------------------------------------
# Direct effect dummyoutcome treatment_4
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_3 == 1))
df <- subset(df, select = c('treatment_4', 
                            'growth', 
                            'sqr_growth',
                            'average_water',
                            'bird_density',
                            'average_Hmax',
                            'average_Hmin',
                            'frequent_month', 
                            'average_out_hum', 
                            'average_out_temp', 
                            'id_slaughterhouse'))
df <- na.omit(df)
output <- list()

for (j in 1:R) {
  df$ascites_prev <- abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse, 
                                 treatment = 'treatment_4')
}
T_learner_boot_dummyoutcome_4 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_dummyoutcome_4) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_dummyoutcome_4$ATE)
mean(T_learner_boot_dummyoutcome_4$ATE)
save(T_learner_boot_dummyoutcome_4,file="C:/Causal-Workflow/Results/direct_effect_T_learner_boot_dummyoutcome_4.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-    abs(rnorm(nrow(df), 0, 1))
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_4 + growth + sqr_growth + bird_density + average_water + average_Hmax + average_Hmin + average_out_temp + average_out_hum + frequent_month + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_dummyoutcome_4 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_dummyoutcome_4$treatment_4)
save(linear_boot_dummyoutcome_4, file = "C:/Causal-Workflow/Results/direct_effect_linear_boot_dummyoutcome_4.Rda")



