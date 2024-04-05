library(ggplot2)
library(tidyverse)

#Causal Effect T learner
#-----------------

load("C://Causal-Workflow/Results/T_learner_boot_trt1.Rda")
load("C://Causal-Workflow/Results/T_learner_boot_trt2.Rda")
load("C://Causal-Workflow/Results/T_learner_boot_trt3.Rda")
load("C://Causal-Workflow/Results/T_learner_boot_trt4.Rda")

t_learner_data <- data.frame(cbind(T_learner_boot_trt1$t[,1], 
                                   T_learner_boot_trt2$t[,1], 
                                   T_learner_boot_trt3$t[,1], 
                                   T_learner_boot_trt4$t[,1]))

df_long <- t_learner_data %>%
  as.data.frame() %>%
  rownames_to_column(var = "id") %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

df_percentiles <- df_long %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value),
    lower_perc = quantile(value, probs = 0.025),
    upper_perc = quantile(value, probs = 0.975)
  )

ggplot() +
  geom_point(data = df_percentiles, aes(x = variable, y = mean), color = "black", size = 3) +
  geom_errorbar(data = df_percentiles, aes(x = variable, ymin = lower_perc, ymax = upper_perc), color = "black", width = 0.2) +
  scale_x_discrete(labels = c('X1' = 'Toppkylling', 'X2' = 'Kromat Low', 'X3' = 'Kromat Simple', 'X4' = 'Harmoni')) +
  theme_bw() +
  labs(title = " ",
       x = "", 
       y = "Change in prevalence per 1000") +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1, 1.5), limits = c(-1.25, 2)) +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  coord_flip()

#------------------------------------

#####################Causal Effect Linear Regression


load("C://Causal-Workflow/Results/marginal_effects_linear_model.Rda")

extract_ame <- function(margin_effect) {
  summary_df <- summary(margin_effect)
  return(data.frame(AME = summary_df$AME, Lower = summary_df$lower, Upper = summary_df$upper))
}


ame_1 <-extract_ame(marginal_effects_treatment)


ame_1$Treatment <- factor(c("Harmoni", "Kromat Simple", "Kromat Low", "Toppkylling"))


# Convert 'Treatment' to a factor with levels in the current order of the data frame
ame_1$Treatment <- factor(ame_1$Treatment, levels = c('Toppkylling',  'Kromat Low', 'Kromat Simple', 'Harmoni'))
levels(ame_1$Treatment)
 
ggplot(ame_1, aes(x=Treatment, y=AME)) +
  geom_point( color = "black", size = 3) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2) +
  theme_bw() +
  labs(y = "Average Marginal Effect", x="", title = "Marginal Effects of Treatment Variables") +
  labs(title = " ",
                       x = "Feed", 
                       y = "Change in prevalence per 1000") +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1, 1.5), limits = c(-1.25, 1.75)) +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  coord_flip()

    
#Placebo treatment

load("C:/Causal-Workflow/Results/placebo_treatment_effects_lm.Rda")

extract_ame <- function(margin_effect) {
  summary_df <- summary(margin_effect)
  return(data.frame(AME = summary_df$AME, Lower = summary_df$lower, Upper = summary_df$upper))
}


ame_1 <-extract_ame(placebo_treatment_effects_lm)
reg_placebo <- ame_1['dummy_trt',]

load("C:/Causal-Workflow/Results/T_learner_boot_placebo.Rda")
T_learner_boot_placebo
T_placebo <- c(mean = mean(T_learner_boot_placebo$ATE), 
               Lower = quantile(T_learner_boot_placebo$ATE, probs = 0.025), 
               Upper = quantile(T_learner_boot_placebo$ATE, probs = 0.975))

placebo_data <- rbind(T_placebo, reg_placebo)
rownames(placebo_data) <- c("T-learner", "Linear reg.")
placebo_data$model <- NA
placebo_data$model[1] <- 'T-learner'
placebo_data$model[2] <- 'Linear reg.'


ggplot(placebo_data, aes(x = model, y=AME)) +
  geom_point( color = "black", size = 3) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2) +
  theme_bw() +
  labs(y = "Average Marginal Effect", x="", title = "Marginal Effects of Treatment Variables") +
  labs(title = " ",
       x = "Model", 
       y = "Change in prevalence per 1000") +
  scale_y_continuous(breaks = c(-0.75, -0.25, 0, 0.25, 0.75), limits = c(-0.75, 0.75)) +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  coord_flip()



load("C://Causal-Workflow/Results/T_learner_boot_dummyoutcome_all.Rda")
load("C://Causal-Workflow/Results/T_learner_boot_dummyoutcome_1.Rda")
load("C://Causal-Workflow/Results/T_learner_boot_dummyoutcome_2.Rda")
load("C://Causal-Workflow/Results/T_learner_boot_dummyoutcome_3.Rda")
load("C://Causal-Workflow/Results/T_learner_boot_dummyoutcome_4.Rda")

TL_all <- c(mean = mean(T_learner_boot_dummyoutcome_all$ATE), 
                           Lower = quantile(T_learner_boot_dummyoutcome_all$ATE, probs = 0.025), 
                           Upper = quantile(T_learner_boot_dummyoutcome_all$ATE, probs = 0.975))
            
TL_1 <-  c(mean = mean(T_learner_boot_dummyoutcome_1$ATE), 
           Lower = quantile(T_learner_boot_dummyoutcome_1$ATE, probs = 0.025), 
           Upper = quantile(T_learner_boot_dummyoutcome_1$ATE, probs = 0.975))

TL_2 <-  c(mean = mean(T_learner_boot_dummyoutcome_2$ATE), 
           Lower = quantile(T_learner_boot_dummyoutcome_2$ATE, probs = 0.025), 
           Upper = quantile(T_learner_boot_dummyoutcome_2$ATE, probs = 0.975))

TL_3 <-  c(mean = mean(T_learner_boot_dummyoutcome_3$ATE), 
           Lower = quantile(T_learner_boot_dummyoutcome_3$ATE, probs = 0.025), 
           Upper = quantile(T_learner_boot_dummyoutcome_3$ATE, probs = 0.975))

TL_4 <-  c(mean = mean(T_learner_boot_dummyoutcome_4$ATE), 
           Lower = quantile(T_learner_boot_dummyoutcome_4$ATE, probs = 0.025), 
           Upper = quantile(T_learner_boot_dummyoutcome_4$ATE, probs = 0.975))


TL_dummy_outcome <- data.frame(rbind(TL_all, TL_1, TL_2, TL_3, TL_4))
TL_dummy_outcome$tests <- NA
TL_dummy_outcome$tests[1] <- 'Overall'
TL_dummy_outcome$tests[2] <- 'Toppkylling'
TL_dummy_outcome$tests[3] <- 'Kromat Low'
TL_dummy_outcome$tests[4] <- 'Kromat Simple'
TL_dummy_outcome$tests[5] <- 'Harmoni'
TL_dummy_outcome$tests <- factor(TL_dummy_outcome$tests, levels = rev(c('Overall', 'Toppkylling', 'Kromat Low', 'Kromat Simple', 'Harmoni')))

ggplot() +
  geom_point(data = TL_dummy_outcome, aes(x = tests, y = mean), color = "black", size = 3) +
  geom_errorbar(data = TL_dummy_outcome, aes(x = tests, ymin = Lower.2.5., ymax = Upper.97.5.), color = "black", width = 0.2) +
  scale_x_discrete() +
  theme_bw() +
  labs(title = " ",
       x = "", 
       y = "Change in prevalence per 1000") +
  scale_y_continuous(breaks = c(-0.20, -0.1,0,0.1,0.20), limits = c(-0.25, 0.25)) +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  coord_flip()


load("C:/Causal-Workflow/Results/linear_boot_dummyoutcome_all.Rda")
load("C:/Causal-Workflow/Results/linear_boot_dummyoutcome_1.Rda")
load("C:/Causal-Workflow/Results/linear_boot_dummyoutcome_2.Rda")
load("C:/Causal-Workflow/Results/linear_boot_dummyoutcome_3.Rda")
load("C:/Causal-Workflow/Results/linear_boot_dummyoutcome_4.Rda")

LM_all <-  c(mean = mean(linear_boot_dummyoutcome_all$treatment), 
           Lower = quantile(linear_boot_dummyoutcome_all$treatment, probs = 0.025), 
           Upper = quantile(linear_boot_dummyoutcome_all$treatment, probs = 0.975))

LM_1 <-  c(mean = mean(linear_boot_dummyoutcome_1$treatment), 
           Lower = quantile(linear_boot_dummyoutcome_1$treatment, probs = 0.025), 
           Upper = quantile(linear_boot_dummyoutcome_1$treatment, probs = 0.975))

LM_2 <-  c(mean = mean(linear_boot_dummyoutcome_2$treatment), 
           Lower = quantile(linear_boot_dummyoutcome_2$treatment, probs = 0.025), 
           Upper = quantile(linear_boot_dummyoutcome_2$treatment, probs = 0.975))

LM_3 <-  c(mean = mean(linear_boot_dummyoutcome_3$treatment), 
           Lower = quantile(linear_boot_dummyoutcome_3$treatment, probs = 0.025), 
           Upper = quantile(linear_boot_dummyoutcome_3$treatment, probs = 0.975))

LM_4 <-  c(mean = mean(linear_boot_dummyoutcome_4$treatment), 
           Lower = quantile(linear_boot_dummyoutcome_4$treatment, probs = 0.025), 
           Upper = quantile(linear_boot_dummyoutcome_4$treatment, probs = 0.975))


LM_dummy_outcome <- data.frame(rbind(LM_all, LM_1, LM_2, LM_3, LM_4))
LM_dummy_outcome$tests <- NA
LM_dummy_outcome$tests[1] <- 'Overall'
LM_dummy_outcome$tests[2] <- 'Toppkylling'
LM_dummy_outcome$tests[3] <- 'Kromat Low'
LM_dummy_outcome$tests[4] <- 'Kromat Simple'
LM_dummy_outcome$tests[5] <- 'Harmoni'
LM_dummy_outcome$tests <- factor(LM_dummy_outcome$tests, levels = rev(c('Overall', 'Toppkylling', 'Kromat Low', 'Kromat Simple', 'Harmoni')))

ggplot() +
  geom_point(data = LM_dummy_outcome, aes(x = tests, y = mean), color = "black", size = 3) +
  geom_errorbar(data = LM_dummy_outcome, aes(x = tests, ymin = Lower.2.5., ymax = Upper.97.5.), color = "black", width = 0.2) +
  scale_x_discrete() +
  theme_bw() +
  labs(title = " ",
       x = "", 
       y = "Change in prevalence per 1000") +
  scale_y_continuous(breaks = c( -0.1,0,0.1), limits = c(-0.15, 0.15)) +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  coord_flip()


#Data from estimatiomn script
data <- subset(wide_data, select = c('feed_name', 'ascites', 'frequent_month', 'average_out_hum', 'average_out_temp', 'average_Tmax', 'average_Tmin', 'id_slaughterhouse', 'ascites_prev'))

data$id_slaughterhouse <- as.factor(data$id_slaughterhouse)
data$frequent_month <- as.factor(data$frequent_month)

levels = 4  # Set the number of levels other than other
data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "Other")
data$feed_group <- as.factor(data$feed_group)
table(data$feed_group)

data$feed_group <- as.character(data$feed_group)

data$feed_group[data$feed_group == "Harmoni Kylling Ressurs"] <- "Harmoni"
data$feed_group[data$feed_group == "Kromat Kylling 2 Enkel u/k"] <- "Kromat Simple"
data$feed_group[data$feed_group == "Kromat Kylling 2 Laag u/k"] <- "Kromat Low"
data$feed_group[data$feed_group == "Toppkylling Netto"] <- "Toppkylling"


table(data$feed_group)
data$feed_group <- factor(data$feed_group, levels = rev(c('Other', 'Toppkylling', 'Kromat Low', 'Kromat Simple', 'Harmoni')))


observation_counts <- data %>%
  group_by(feed_group) %>%
  summarise(n = n(), .groups = 'drop')  # 'n' is the count of observations per group

# Merge the counts back into the original data frame if necessary
data <- merge(data, observation_counts, by = "feed_group", all.x = TRUE)

ggplot(data, aes(x = feed_group, y = ascites_prev)) +
  geom_boxplot() +
  geom_text(aes(label = n, y = -3.5), position = position_dodge(width = 0.75)) +
  labs(title = "Ascites Prevalence per 1000 by Feed",
       x = "Feed",
       y = "Ascites Prevalence 1/1000") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  coord_flip()




