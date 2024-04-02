rm(list = ls())

library(dagitty)
library(boot)
library(xgboost)
library(parallel)
library(dplyr)
library(ggplot2)
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
plot(DAG)
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


levels = 5  # Set the number of levels other than other
total_data$feed_group = fct_lump_n(total_data$feed_name, n = levels, other_level = "Other")
total_data$feed_group <- as.factor(total_data$feed_group)
table(total_data$feed_group)
# Treatment 
total_data$treatment_1 <-  ifelse(total_data$feed_group == "Toppkylling Netto", 1, 0) #Feed 1 obs: 997
total_data$treatment_2 <-  ifelse(total_data$feed_group == "Kromat Kylling 2 Laag u/k", 1, 0) #Feed 2 obs:937  
total_data$treatment_3 <-  ifelse(total_data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0) #Feed 3 obs: 632
total_data$treatment_4 <-  ifelse(total_data$feed_group == "Harmoni Kylling Ressurs", 1, 0) #Feed 4: 594
total_data$treatment_5 <-  ifelse(total_data$feed_group == "Kromat Kylling 2 Hog uten narasin", 1, 0) #Feed 5: 291


#-----------------------------------------------------------------------------
# Total effect treatment_1
#-----------------------------------------------------------------------------
data <- subset(total_data, !(treatment_2 == 1 | treatment_3 == 1 | treatment_4 == 1 | treatment_5 == 1))

dummy_slaughter <- model.matrix(~ id_slaughterhouse - 1, data = data)
dummy_month <- model.matrix(~ frequent_month - 1, data = data)
data <- subset(data, select = c('ascites_prev', 'treatment_1', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin'))
data <- cbind(data, dummy_month)
data <- cbind(data, dummy_slaughter)

#T-learner
data <- na.omit(data)
org_effect <- T_learner(data, nfold_t = 10, nfold_c = 10)
org_effect
T_learner_boot_trt1 <-  boot(data = data, statistic = T_learner, R = 500)
save(T_learner_boot_trt1,file="C:/Causal-Workflow/Results/T_learner_boot_trt1.Rda")
X_learner_boot_trt1 <-  boot(data = data, statistic = X_learner, R = 500)
save(X_learner_boot_trt1,file="C:/Causal-Workflow/Results/X_learner_boot_trt1.Rda")

#-----------------------------------------------------------------------------
# Total effect treatment_2
#-----------------------------------------------------------------------------
data <- subset(total_data, !(treatment_1 == 1 | treatment_3 == 1 | treatment_4 == 1 | treatment_5 == 1))

dummy_slaughter <- model.matrix(~ id_slaughterhouse - 1, data = data)
dummy_month <- model.matrix(~ frequent_month - 1, data = data)
data <- subset(data, select = c('ascites_prev', 'treatment_2', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin'))
data <- cbind(data, dummy_month)
data <- cbind(data, dummy_slaughter)

#T-learner
data <- na.omit(data)
org_effect <- T_learner(data, nfold_t = 10, nfold_c = 10)
org_effect
T_learner_boot_trt2 <-  boot(data = data, statistic = T_learner, R = 500)
save(T_learner_boot_trt2,file="C:/Causal-Workflow/Results/T_learner_boot_trt2.Rda")
X_learner_boot_trt2 <-  boot(data = data, statistic = X_learner, R = 500)
save(X_learner_boot_trt2,file="C:/Causal-Workflow/Results/X_learner_boot_trt2.Rda")


#-----------------------------------------------------------------------------
# Total effect treatment_3
#-----------------------------------------------------------------------------
data <- subset(total_data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_4 == 1 | treatment_5 == 1))

dummy_slaughter <- model.matrix(~ id_slaughterhouse - 1, data = data)
dummy_month <- model.matrix(~ frequent_month - 1, data = data)
data <- subset(data, select = c('ascites_prev', 'treatment_3', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin'))
data <- cbind(data, dummy_month)
data <- cbind(data, dummy_slaughter)

#T-learner
data <- na.omit(data)
org_effect <- T_learner(data, nfold_t = 10, nfold_c = 10)
org_effect
T_learner_boot_trt3 <-  boot(data = data, statistic = T_learner, R = 500)
save(T_learner_boot_trt3,file="C:/Causal-Workflow/Results/T_learner_boot_trt3.Rda")
X_learner_boot_trt3 <-  boot(data = data, statistic = X_learner, R = 500)
save(X_learner_boot_trt3,file="C:/Causal-Workflow/Results/X_learner_boot_trt3.Rda")

#-----------------------------------------------------------------------------
# Total effect treatment_4
#-----------------------------------------------------------------------------
data <- subset(total_data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_3 == 1 | treatment_5 == 1))

dummy_slaughter <- model.matrix(~ id_slaughterhouse - 1, data = data)
dummy_month <- model.matrix(~ frequent_month - 1, data = data)
data <- subset(data, select = c('ascites_prev', 'treatment_4', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin'))
data <- cbind(data, dummy_month)
data <- cbind(data, dummy_slaughter)

#T-learner
data <- na.omit(data)
org_effect <- T_learner(data, nfold_t = 10, nfold_c = 10)
org_effect
T_learner_boot_trt4 <-  boot(data = data, statistic = T_learner, R = 250)
save(T_learner_boot_trt4,file="C:/Causal-Workflow/Results/T_learner_boot_trt4.Rda")
X_learner_boot_trt4 <-  boot(data = data, statistic = X_learner, R = 250)
save(X_learner_boot_trt4,file="C:/Causal-Workflow/Results/X_learner_boot_trt4.Rda")

#-----------------------------------------------------------------------------
# Total effect treatment_5
#-----------------------------------------------------------------------------
data <- subset(total_data, !(treatment_1 == 1 | treatment_2 == 1 | treatment_3 == 1 | treatment_4 == 1))

dummy_slaughter <- model.matrix(~ id_slaughterhouse - 1, data = data)
dummy_month <- model.matrix(~ frequent_month - 1, data = data)
data <- subset(data, select = c('ascites_prev', 'treatment_5', 'average_out_temp', 'average_out_hum', 'average_Tmax', 'average_Tmin'))
data <- cbind(data, dummy_month)
data <- cbind(data, dummy_slaughter)

#T-learner
data <- na.omit(data)
org_effect <- T_learner(data, nfold_t = 10, nfold_c = 10)
org_effect
T_learner_boot_trt5 <-  boot(data = data, statistic = T_learner, R = 250)
save(T_learner_boot_trt5,file="C:/Causal-Workflow/Results/T_learner_boot_trt5.Rda")
X_learner_boot_trt5 <-  boot(data = data, statistic = X_learner, R = 250)
save(X_learner_boot_trt5,file="C:/Causal-Workflow/Results/X_learner_boot_trt5.Rda")



#--------------------------------------------------------------------------------
# Linear model
#--------------------------------------------------------------------------------
data$treatment  <- ifelse(data$treatment_2 == 1 | data$treatment_3 == 1 , NA, ifelse(data$treatment_1 == 1,1,0))
model_1 <- lm(ascites_prev ~ treatment +  average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight, data = data) # Fit the model on the resample
summary(model_1)
data$treatment  <- ifelse(data$treatment_1 == 1 | data$treatment_3 == 1 , NA, ifelse(data$treatment_2 == 1,1,0))
model_2 <- lm(ascites_prev ~ treatment +  average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight, data = data) # Fit the model on the resample
summary(model_2)
data$treatment  <- ifelse(data$treatment_1 == 1 | data$treatment_2 == 1 , NA, ifelse(data$treatment_3 == 1,1,0))
model_3 <- lm(ascites_prev ~ treatment +  average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight, data = data) # Fit the model on the resample
summary(model_3)

# Calculate marginal effects for the treatment variable
marginal_effects_treatment_1 <- margins(model_1, variables = "treatment")
marginal_effects_treatment_2 <- margins(model_2, variables = "treatment")
marginal_effects_treatment_3 <- margins(model_3, variables = "treatment")

extract_ame <- function(margin_effect) {
  summary_df <- summary(margin_effect)
  return(data.frame(AME = summary_df$AME, Lower = summary_df$lower, Upper = summary_df$upper))
}


ame_1 <-extract_ame(marginal_effects_treatment_1)
ame_2 <- extract_ame(marginal_effects_treatment_2)
ame_3 <- extract_ame(marginal_effects_treatment_3)

# Combine into a single data frame
ames <- rbind(ame_1, ame_2, ame_3)
ames$Treatment <- factor(c("Feed 1", "Feed 2", "Feed 3"))

# Plot using ggplot2
ggplot(ames, aes(x=Treatment, y=AME)) +
  geom_point() +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2) +
  labs(y="Average Marginal Effect", x="", title="Marginal Effects of Treatment Variables") +
  theme_minimal()


#------------------------------------------------
# Creating boxplots of the distributions
#-----------------------------------------------
treat1 <- data.frame(T_total_boot_feed1[,1])
treat2 <- data.frame(T_total_boot_feed2[,1])
treat3 <- data.frame(T_total_boot_feed3[,1])
treat1$feed <- "Feed 1"
colnames(treat1) <- c('values', 'feed')
treat2$feed <- "Feed 2"
colnames(treat2) <- c('values', 'feed')
treat3$feed <- "Feed 3"
colnames(treat3) <- c('values', 'feed')
df <- rbind(treat1, treat2, treat3)

plot_total <- ggplot(data = df, aes(x = feed, y = values)) +
  geom_boxplot() +
  xlab("Growth Feed Type") +
  ylab("Treatment effect on the prevalence (1/1000) ascites") +
  ggtitle("T-learner: Direct causal effect") + 
  theme(
    plot.title = element_text(size = 16),         # Enlarge the plot title
    axis.title.x = element_text(size = 14),       # Enlarge the x-axis label
    axis.title.y = element_text(size = 14),       # Enlarge the y-axis label
    axis.text.x = element_text(size = 12),        # Enlarge the x-axis text
    axis.text.y = element_text(size = 12)         # Enlarge the y-axis text
  )
plot_total

data$treatment_1 <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0)
data$treatment_2 <-  ifelse(data$feed_group == "Kromat Kylling 2 Laag u/k", 1, 0)
data$treatment_3 <-  ifelse(data$feed_group == "Harmoni Kylling Ressurs", 1, 0)
data <- data %>% mutate(new_var1 = ifelse(feed_group == 'Toppkylling Netto', 'Feed 1', 
                                          ifelse(feed_group == 'Kromat Kylling 2 Laag u/k', 'Feed 2', 
                                                 ifelse(feed_group == 'Harmoni Kylling Ressurs', 'Feed 3', 
                                                        ifelse(feed_group == 'other', 'Control group', NA)))))
means <- data %>%
  group_by(new_var1) %>%
  summarise(mean_ascites_prev = mean(ascites_prev, na.rm = TRUE))

# Create the boxplot and add mean values
ggplot(data, aes(x = new_var1, y = ascites_prev)) +
  geom_boxplot() +
  geom_text(data = means, aes(label = round(mean_ascites_prev, 2), 
                              y = mean_ascites_prev), 
            position = position_dodge(width = 0.75), 
            vjust = -0, color = "blue") +
  ylab("Prevalence (1/1000) ascites") +
  xlab("Growth Feed Type") +
  theme(
    plot.title = element_text(size = 16),         # Enlarge the plot title
    axis.title.x = element_text(size = 14),       # Enlarge the x-axis label
    axis.title.y = element_text(size = 14),       # Enlarge the y-axis label
    axis.text.x = element_text(size = 12),        # Enlarge the x-axis text
    axis.text.y = element_text(size = 12)         # Enlarge the y-axis text
  )
