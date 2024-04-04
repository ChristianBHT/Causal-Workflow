rm(list = ls())

library(dagitty)
library(boot)
library(tidyverse)
library(xgboost)
library(parallel)
library(dplyr)

library(margins)

setwd("C:/Causal-Workflow/")
load("data/analysis_wide_data.Rda")
DAG <- dagitty('dag {
bb="0,0,1,1"
"Bird density" [pos="0.419,0.295"]
"CO2-level" [latent,pos="0.496,0.467"]
"Feed type" [exposure,pos="0.202,0.321"]
"Food cons." [pos="0.304,0.113"]
"Month of Year" [pos="0.309,0.542"]
"Outdoor humidity" [pos="0.439,0.752"]
"Outdoor temp." [pos="0.631,0.679"]
"Water cons." [pos="0.542,0.149"]
Ascites [outcome,pos="0.419,0.446"]
Geography [latent,pos="0.234,0.674"]
Growth [pos="0.309,0.253"]
Humidity [pos="0.470,0.256"]
Temp. [pos="0.538,0.343"]
Unknown [latent,pos="0.445,0.050"]
"Bird density" -> "Food cons."
"Bird density" -> Ascites
"Bird density" -> Growth
"CO2-level" -> Ascites
"Feed type" -> "Food cons."
"Feed type" -> Ascites
"Feed type" -> Growth
"Food cons." -> Growth
"Month of Year" -> "CO2-level"
"Month of Year" -> "Feed type"
"Month of Year" -> "Outdoor humidity"
"Month of Year" -> "Outdoor temp."
"Month of Year" -> Ascites
"Month of Year" -> Growth
"Outdoor humidity" -> "CO2-level"
"Outdoor humidity" -> Ascites
"Outdoor humidity" -> Growth
"Outdoor humidity" -> Humidity
"Outdoor temp." -> "CO2-level"
"Outdoor temp." -> "Outdoor humidity"
"Outdoor temp." -> "Water cons."
"Outdoor temp." -> Ascites
"Outdoor temp." -> Humidity
"Outdoor temp." -> Temp.
"Water cons." -> "Food cons."
"Water cons." -> Ascites
"Water cons." -> Growth
Geography -> "Feed type"
Geography -> "Outdoor humidity"
Geography -> "Outdoor temp."
Growth -> Ascites
Humidity -> Ascites
Humidity -> Growth
Temp. -> "Water cons."
Temp. -> Ascites
Temp. -> Humidity
Unknown -> "Feed type"
Unknown -> Temp.
}')
# plot(DAG)
wide_data$ascites_prev <- 1000*wide_data$ascites/wide_data$chicken_last_day
sd(wide_data$ascites_prev)
#------------------#
#  Total Effect    #
#------------------#
adjust_set_total <- adjustmentSets(DAG, effect = "total")
adjust_set_total
data <- subset(wide_data, select = c('feed_name', 'ascites', 'frequent_month', 'average_out_hum', 'average_out_temp', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse', 'ascites_prev'))

data$id_slaughterhouse <- as.factor(data$id_slaughterhouse)
data$frequent_month <- as.factor(data$frequent_month)

levels = 4  # Set the number of levels other than other
data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "Other")
data$feed_group <- as.factor(data$feed_group)
# Treatment 
data$treatment_1 <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0) #Feed 1 obs: 997
data$treatment_2 <-  ifelse(data$feed_group == "Kromat Kylling 2 Laag u/k", 1, 0) #Feed 2 obs:937  
data$treatment_3 <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0) #Feed 3 obs: 632
data$treatment_4 <-  ifelse(data$feed_group == "Harmoni Kylling Ressurs", 1, 0) #Feed 4: 594


#-----------------------------------------------------------------------------
# Total effect treatment_1
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_2 == 1 | treatment_3 == 1 | treatment_4 == 1))

df <- subset(df, select = c('ascites_prev', 'treatment_1', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse'))
df <- na.omit(df)

T_learner_boot_trt1 <-  boot(
  data = df,
  formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse ,
  treatment = 'treatment_1',
  statistic = T_learner_boost,
  R = 300  
)

hist(T_learner_boot_trt1$t[,1])
save(T_learner_boot_trt1,file="C:/Causal-Workflow/Results/T_learner_boot_trt1.Rda")

#-----------------------------------------------------------------------------
# Total effect treatment_2
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_3 == 1 | treatment_4 == 1))

df <- subset(df, select = c('ascites_prev', 'treatment_2', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse'))
df <- na.omit(df)

T_learner_boot_trt2 <-  boot(
  data = df,
  formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse ,
  treatment = 'treatment_2',
  statistic = T_learner_boost,
  R = 500  
)
hist(T_learner_boot_trt2$t[,1])
save(T_learner_boot_trt2,file="C:/Causal-Workflow/Results/T_learner_boot_trt2.Rda")


#-----------------------------------------------------------------------------
# Total effect treatment_3
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_4 == 1))

df <- subset(df, select = c('ascites_prev', 'treatment_3', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse'))
df <- na.omit(df)
org_effect <- T_learner_boost(data = df, formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin, treatment = 'treatment_3')
org_effect
T_learner_boot_trt3 <-  boot(
  data = df,
  formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse ,
  treatment = 'treatment_3',
  statistic = T_learner_boost,
  R = 500  
)
hist(T_learner_boot_trt3$t[,1])
save(T_learner_boot_trt3,file="C:/Causal-Workflow/Results/T_learner_boot_trt3.Rda")

#-----------------------------------------------------------------------------
# Total effect treatment_4
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_3 == 1))

df <- subset(df, select = c('ascites_prev', 'treatment_4', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse'))
df <- na.omit(df)
org_effect <- T_learner_boost(data = df, formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin, treatment = 'treatment_4')
org_effect
T_learner_boot_trt4 <-  boot(
  data = df,
  formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin ,
  treatment = 'treatment_4',
  statistic = T_learner_boost,
  R = 500  
)
hist(T_learner_boot_trt4$t[,1])
save(T_learner_boot_trt4,file="C:/Causal-Workflow/Results/T_learner_boot_trt4.Rda")


#--------------------------------------------------------------------------------
# Linear model
#--------------------------------------------------------------------------------
total_data$feed_group <- factor(total_data$feed_group, 
                                levels = c("Other",
                                           "Toppkylling Netto",
                                           "Kromat Kylling 2 Laag u/k",
                                           "Kromat Kylling 2 Enkel u/k", 
                                           "Harmoni Kylling Ressurs"))
lm_model <- lm(ascites_prev ~ feed_group +  average_out_temp + average_out_hum + average_Tmax + average_Tmin, data = total_data) # Fit the model on the resample
summary(lm_model)
# Calculate marginal effects for the treatment variable
marginal_effects_treatment <- margins(lm_model, variables = "feed_group")
save(marginal_effects_treatment,file="C:/Causal-Workflow/Results/marginal_effects_linear_model.Rda")


#-------------------------------
# Heterogenous effect over month
#-------------------------------

#------------------------------------
# Placebo treatment and dummy outcome
#-----------------------------------

# Placebo treatment
org_effect <- T_learner_boost(data = data, 
                              formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse, 
                              treatment = 'dummy_trt')
org_effect

T_learner_boot_placebo <-  boot(
  data = data,
  formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse ,
  treatment = 'dummy_trt',
  statistic = T_learner_boost,
  R = 100  
)
hist(T_learner_boot_placebo$t[,1])
save(T_learner_boot_placebo,file="C:/Causal-Workflow/Results/T_learner_boot_placebo.Rda")


lm_dummy <- lm(ascites_prev ~ dummy_trt +  average_out_temp + average_out_hum + average_Tmax + average_Tmin, data = data) # Fit the model on the resample
summary(lm_dummy)

dummy_effects_treatment <- margins(lm_dummy, vce = 'delta')
save(dummy_effects_treatment,file="C:/Causal-Workflow/Results/dummy_effects_treatment.Rda")


#-----------------------------------------------------------------------------
# Total effect dummy treatment_1
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_2 == 1 | treatment_3 == 1 | treatment_4 == 1))

df <- subset(df, select = c('ascites_prev', 'treatment_1', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse'))
df <- na.omit(df)
output <- list()
R = 300

for (j in 1:R) {
  df$ascites_prev <-  sample(df$ascites_prev, replace = FALSE)
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse, 
                                 treatment = 'treatment_1')
}

T_learner_boot_placebo_1 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_placebo_1) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_placebo_1$ATE)
save(T_learner_boot_placebo_1,file="C:/Causal-Workflow/Results/T_learner_boot_placebo_1.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-  sample(df$ascites_prev, replace = FALSE)
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_1 + average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_placebo_1 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_placebo_1$treatment_1)
save(linear_boot_placebo_1,file="C:/Causal-Workflow/Results/linear_boot_placebo_1.Rda")

#-----------------------------------------------------------------------------
# Total effect dummy treatment_2
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_3 == 1 | treatment_4 == 1))
df <- subset(df, select = c('ascites_prev', 'treatment_2', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse'))
df <- na.omit(df)
output <- list()
R = 300
for (j in 1:R) {
  df$ascites_prev <-  sample(df$ascites_prev, replace = FALSE)
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse, 
                                 treatment = 'treatment_2')
}
T_learner_boot_placebo_2 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_placebo_2) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_placebo_2$ATE)
save(T_learner_boot_placebo_2,file="C:/Causal-Workflow/Results/T_learner_boot_placebo_2.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-  sample(df$ascites_prev, replace = FALSE)
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_2 + average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_placebo_2 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_placebo_2$treatment_2)
save(linear_boot_placebo_2,file="C:/Causal-Workflow/Results/linear_boot_placebo_2.Rda")

#-----------------------------------------------------------------------------
# Total effect dummy treatment_3
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_4 == 1))
df <- subset(df, select = c('ascites_prev', 'treatment_3', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse'))
df <- na.omit(df)
output <- list()
R = 300
for (j in 1:R) {
  df$ascites_prev <-  sample(df$ascites_prev, replace = FALSE)
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse, 
                                 treatment = 'treatment_3')
}
T_learner_boot_placebo_3 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_placebo_3) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_placebo_3$ATE)
save(T_learner_boot_placebo_3,file="C:/Causal-Workflow/Results/T_learner_boot_placebo_2.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-  sample(df$ascites_prev, replace = FALSE)
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_3 + average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_placebo_3 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_placebo_3$treatment_3)
save(linear_boot_placebo_3,file="C:/Causal-Workflow/Results/linear_boot_placebo_3.Rda")

#-----------------------------------------------------------------------------
# Total effect dummy treatment_4
#-----------------------------------------------------------------------------
df <- subset(data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_3 == 1))
df <- subset(df, select = c('ascites_prev', 'treatment_4', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse'))
df <- na.omit(df)
output <- list()
R = 300
for (j in 1:R) {
  df$ascites_prev <-  sample(df$ascites_prev, replace = FALSE)
  output[[j]] <- T_learner_boost(data = df, 
                                 formula = ascites_prev ~ average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse, 
                                 treatment = 'treatment_4')
}
T_learner_boot_placebo_4 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
names(T_learner_boot_placebo_4) <- c("ATE", "MU1", "MU0")
hist(T_learner_boot_placebo_4$ATE)
save(T_learner_boot_placebo_4,file="C:/Causal-Workflow/Results/T_learner_boot_placebo_4.Rda")

output <- list()
for (j in 1:R) {
  df$ascites_prev <-  sample(df$ascites_prev, replace = FALSE)
  output[[j]] <- lm(data = df, formula = ascites_prev ~ treatment_4 + average_out_temp + average_out_hum + average_Tmax + average_Tmin + id_slaughterhouse)$coefficients[2]
  
}

linear_boot_placebo_4 <- do.call(rbind, lapply(output, function(x) data.frame(t(unlist(x)))))
hist(linear_boot_placebo_4$treatment_4)
save(linear_boot_placebo_4,file="C:/Causal-Workflow/Results/linear_boot_placebo_4.Rda")


