library(ggplot2)
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
  scale_x_discrete(labels = NULL) +
  theme_bw() +
  labs(title = " ",
       x = "", 
       y = "Change in prevalence per 1000") +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1, 1.5), limits = c(-1.25, 1.75)) +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  coord_flip()



load("C://Causal-Workflow/Results/marginal_effects_linear_model.Rda")

extract_ame <- function(margin_effect) {
  summary_df <- summary(margin_effect)
  return(data.frame(AME = summary_df$AME, Lower = summary_df$lower, Upper = summary_df$upper))
}


ame_1 <-extract_ame(marginal_effects_treatment)

# Combine into a single data frame
ame_1$Treatment <- factor(c("Harmoni", "Kromat Simple", "Kromat Low", "Toppkylling"))


# Convert 'Treatment' to a factor with levels in the current order of the data frame
ame_1$Treatment <- factor(ame_1$Treatment, levels = c('Toppkylling',  'Kromat Low', 'Kromat Simple', 'Harmoni'))
levels(ame_1$Treatment)
# Plot using ggplot2
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

load("C:/Causal-Workflow/Results/dummy_effects_treatment.Rda")

extract_ame <- function(margin_effect) {
  summary_df <- summary(margin_effect)
  return(data.frame(AME = summary_df$AME, Lower = summary_df$lower, Upper = summary_df$upper))
}


ame_1 <-extract_ame(dummy_effects_treatment)
reg_placebo <- ame_1["dummy_trt",]
load("C:/Causal-Workflow/Results/T_learner_boot_dummy.Rda")
T_boot_placebo <- T_learner_boot_dummy$t[,1]
load("C:/Causal-Workflow/Results/X_learner_boot_dummy.Rda")
X_boot_placebo <- X_learner_boot__dummy$t[,1]


T_placebo <- c(mean = mean(T_boot_placebo), 
               Lower = quantile(T_boot_placebo, probs = 0.025), 
               Upper = quantile(T_boot_placebo, probs = 0.975))
X_placebo <- c(mean = mean(X_boot_placebo), 
               Lower = quantile(X_boot_placebo, probs = 0.025), 
               Upper = quantile(X_boot_placebo, probs = 0.975))

placebo_data <- rbind(T_placebo, X_placebo, reg_placebo)











