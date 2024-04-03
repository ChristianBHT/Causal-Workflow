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
#------------------#
#  Total Effect    #
#------------------#
adjust_set_total <- adjustmentSets(DAG, effect = "total")
adjust_set_total
total_data <- subset(wide_data, select = c('feed_name', 'ascites', 'frequent_month', 'average_out_hum', 'average_out_temp', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse', 'ascites_prev'))

total_data$feed_name <- as.factor(total_data$feed_name)
total_data$id_slaughterhouse <- as.factor(total_data$id_slaughterhouse)
total_data$frequent_month <- as.factor(total_data$frequent_month)


levels = 4  # Set the number of levels other than other
total_data$feed_group = fct_lump_n(total_data$feed_name, n = levels, other_level = "Other")
total_data$feed_group <- as.factor(total_data$feed_group)
table(total_data$feed_group)
# Treatment 
total_data$treatment_1 <-  ifelse(total_data$feed_group == "Toppkylling Netto", 1, 0) #Feed 1 obs: 997
total_data$treatment_2 <-  ifelse(total_data$feed_group == "Kromat Kylling 2 Laag u/k", 1, 0) #Feed 2 obs:937  
total_data$treatment_3 <-  ifelse(total_data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0) #Feed 3 obs: 632
total_data$treatment_4 <-  ifelse(total_data$feed_group == "Harmoni Kylling Ressurs", 1, 0) #Feed 4: 594


#-----------------------------------------------------------------------------
# Total effect treatment_1
#-----------------------------------------------------------------------------
data <- subset(total_data, !(treatment_2 == 1 | treatment_3 == 1 | treatment_4 == 1 ))

dummy_slaughter <- model.matrix(~ id_slaughterhouse - 1, data = data)
dummy_month <- model.matrix(~ frequent_month - 1, data = data)
data <- subset(data, select = c('ascites_prev', 'treatment_1', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin'))
data <- cbind(data, dummy_month)
data <- cbind(data, dummy_slaughter)

data <- na.omit(data)
org_effect <- T_learner(data, nfold_t = 10, nfold_c = 10)
org_effect
T_learner_boot_trt1 <-  boot(data = data, statistic = T_learner, R = 250)
save(T_learner_boot_trt1,file="C:/Causal-Workflow/Results/T_learner_boot_trt1.Rda")
X_learner_boot_trt1 <-  boot(data = data, statistic = X_learner, R = 250)
save(X_learner_boot_trt1,file="C:/Causal-Workflow/Results/X_learner_boot_trt1.Rda")

#-----------------------------------------------------------------------------
# Total effect treatment_2
#-----------------------------------------------------------------------------
data <- subset(total_data, !(treatment_1 == 1 | treatment_3 == 1 | treatment_4 == 1))

dummy_slaughter <- model.matrix(~ id_slaughterhouse - 1, data = data)
dummy_month <- model.matrix(~ frequent_month - 1, data = data)
data <- subset(data, select = c('ascites_prev', 'treatment_2', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin'))
data <- cbind(data, dummy_month)
data <- cbind(data, dummy_slaughter)

data <- na.omit(data)
org_effect <- T_learner(data, nfold_t = 10, nfold_c = 10)
org_effect
T_learner_boot_trt2 <-  boot(data = data, statistic = T_learner, R = 250)
save(T_learner_boot_trt2,file="C:/Causal-Workflow/Results/T_learner_boot_trt2.Rda")
X_learner_boot_trt2 <-  boot(data = data, statistic = X_learner, R = 250)
save(X_learner_boot_trt2,file="C:/Causal-Workflow/Results/X_learner_boot_trt2.Rda")


#-----------------------------------------------------------------------------
# Total effect treatment_3
#-----------------------------------------------------------------------------
data <- subset(total_data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_4 == 1))

dummy_slaughter <- model.matrix(~ id_slaughterhouse - 1, data = data)
dummy_month <- model.matrix(~ frequent_month - 1, data = data)
data <- subset(data, select = c('ascites_prev', 'treatment_3', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin'))
data <- cbind(data, dummy_month)
data <- cbind(data, dummy_slaughter)

data <- na.omit(data)
org_effect <- T_learner(data, nfold_t = 10, nfold_c = 10)
org_effect
T_learner_boot_trt3 <-  boot(data = data, statistic = T_learner, R = 250)
save(T_learner_boot_trt3,file="C:/Causal-Workflow/Results/T_learner_boot_trt3.Rda")
X_learner_boot_trt3 <-  boot(data = data, statistic = X_learner, R = 250)
save(X_learner_boot_trt3,file="C:/Causal-Workflow/Results/X_learner_boot_trt3.Rda")

#-----------------------------------------------------------------------------
# Total effect treatment_4
#-----------------------------------------------------------------------------
data <- subset(total_data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_3 == 1 ))

dummy_slaughter <- model.matrix(~ id_slaughterhouse - 1, data = data)
dummy_month <- model.matrix(~ frequent_month - 1, data = data)
data <- subset(data, select = c('ascites_prev', 'treatment_4', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin'))
data <- cbind(data, dummy_month)
data <- cbind(data, dummy_slaughter)

data <- na.omit(data)
org_effect <- T_learner(data, nfold_t = 10, nfold_c = 10)
org_effect
T_learner_boot_trt4 <-  boot(data = data, statistic = T_learner, R = 250)
save(T_learner_boot_trt4,file="C:/Causal-Workflow/Results/T_learner_boot_trt4.Rda")
X_learner_boot_trt4 <-  boot(data = data, statistic = X_learner, R = 250)
save(X_learner_boot_trt4,file="C:/Causal-Workflow/Results/X_learner_boot_trt4.Rda")


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
# Dummy treatment and random outcome
#-----------------------------------

# Dummy treatment
data <- subset(wide_data, select = c('ascites', 'frequent_month', 'average_out_hum', 'average_out_temp', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse', 'ascites_prev'))
n <- nrow(data)
data$dummy_trt <- rbinom(n = n, size = 1, prob = 0.4)

dummy_slaughter <- model.matrix(~ id_slaughterhouse - 1, data = data)
dummy_month <- model.matrix(~ frequent_month - 1, data = data)
data <- subset(data, select = c('ascites_prev', 'dummy_trt', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin'))
data <- cbind(data, dummy_month)
data <- cbind(data, dummy_slaughter)

data <- na.omit(data)
org_effect <- T_learner(data, nfold_t = 10, nfold_c = 10)
org_effect
T_learner_boot_dummy <-  boot(data = data, statistic = T_learner, R = 250)
save(T_learner_boot_dummy,file="C:/Causal-Workflow/Results/T_learner_boot_dummy.Rda")
X_learner_boot__dummy <-  boot(data = data, statistic = X_learner, R = 250)
save(X_learner_boot__dummy,file="C:/Causal-Workflow/Results/X_learner_boot_dummy.Rda")

lm_dummy <- lm(ascites_prev ~ dummy_trt +  average_out_temp + average_out_hum + average_Tmax + average_Tmin, data = total_data) # Fit the model on the resample
summary(lm_dummy)
# Calculate marginal effects for the treatment variable
dummy_effects_treatment <- margins(lm_dummy, vce = 'delta')
save(dummy_effects_treatment,file="C:/Causal-Workflow/Results/dummy_effects_treatment.Rda")






