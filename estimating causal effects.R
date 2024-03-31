devtools::install_github("junyzhou10/MetaLearners")
rm(list = ls())
library(tidyverse)
library(dagitty)
library(MetaLearners)
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
Humidity -> "Food cons."
Humidity -> Ascites
Humidity -> Growth
Temp. -> "Water cons."
Temp. -> Ascites
Temp. -> Humidity
Unknown -> "Feed type"
Unknown -> Temp.
}')
plot(DAG)
adjustmentSets(DAG, effect = "total")
adjustmentSets(DAG, effect = "direct")

setwd("C:/Causal-Workflow/")
load("data/analysis_wide_data.Rda")
wide_data$feed_name <- as.factor(wide_data$feed_name)
wide_data$id_slaughterhouse <- as.factor(wide_data$id_slaughterhouse)
wide_data$LeverandoerNr <- as.factor(wide_data$LeverandoerNr)
wide_data$frequent_month <- as.factor(wide_data$frequent_month)
wide_data$ascites_prev <- 1000*wide_data$ascites/wide_data$chicken_last_day

levels = 5  # Set the number of levels other than other
wide_data$feed_group = fct_lump_n(wide_data$feed_name, n = levels, other_level = "Other")
wide_data$feed_group <- as.factor(wide_data$feed_group)
table(wide_data$feed_group)

# Treatment 
wide_data$treatment_1 <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0) #Feed 1 obs: 997
wide_data$treatment_2 <-  ifelse(data$feed_group == "Kromat Kylling 2 Laag u/k", 1, 0) #Feed 2 obs:937  
wide_data$treatment_3 <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0) #Feed 3 obs: 632
wide_data$treatment_4 <-  ifelse(data$feed_group == "Harmoni Kylling Ressurs", 1, 0) #Feed 4: 594
wide_data$treatment_5 <-  ifelse(data$feed_group == "Kromat Kylling 2 Hog uten narasin", 1, 0) #Feed 5: 291

wide_data$treatment  <- ifelse(wide_data$treatment_2 == 1 | wide_data$treatment_3 == 1 , NA, ifelse(data$treatment_1 == 1,1,0))
model_1 <- lm(ascites_prev ~ treatment +  average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight, data = data) # Fit the model on the resample
summary(model_1)
data$treatment  <- ifelse(data$treatment_1 == 1 | data$treatment_3 == 1 , NA, ifelse(data$treatment_2 == 1,1,0))
model_2 <- lm(ascites_prev ~ treatment +  average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight, data = data) # Fit the model on the resample
summary(model_2)
data$treatment  <- ifelse(data$treatment_1 == 1 | data$treatment_2 == 1 , NA, ifelse(data$treatment_3 == 1,1,0))
model_3 <- lm(ascites_prev ~ treatment +  average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight, data = data) # Fit the model on the resample
summary(model_3)

# Linear model
linear_model <- lm(ascites_prev <- )

# Meta Learners
Data <- data.frame(subset(wide_data, select = c('ascites_prev', 'feed_group', 'frequent_month', 'average_out_hum', 'average_out_temp', 'average_Tmin', 'average_Tmax', 'id_slaughterhouse')))
Data <- na.omit(Data)

Data$treatment_1 <-  ifelse(Data$feed_group == "Toppkylling Netto", 1, ifelse(Data$feed_group == "Kromat Kylling 2 Laag u/k", 2, 0)) #Feed 1 obs: 997
dummy_matrix1 <- model.matrix(~ frequent_month - 1, data = testData)
dummy_matrix2 <- model.matrix(~ id_slaughterhouse - 1, data = testData)

# Data$treatment_2 <-  ifelse(Data$feed_group == "Kromat Kylling 2 Laag u/k", 1, 0) #Feed 2 obs:937  
# Data$treatment_3 <-  ifelse(Data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0) #Feed 3 obs: 632
# Data$treatment_4 <-  ifelse(Data$feed_group == "Harmoni Kylling Ressurs", 1, 0) #Feed 4: 594
# Data$treatment_5 <-  ifelse(Data$feed_group == "Kromat Kylling 2 Hog uten narasin", 1, 0) #Feed 5: 291
Trt <- as.matrix(Data$treatment_1 + 1)
outcome <- as.matrix(Data$ascites_prev)
adjustSet <- as.matrix(subset(Data, select = c('average_out_hum', 'average_out_temp', 'average_Tmin')))
T_learner <- MetaLearners(X = adjustSet, Y = outcome, Trt = Trt, algorithm = 'RF', Learners = "S")
