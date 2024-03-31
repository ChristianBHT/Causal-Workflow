rm(list = ls())
library(dplyr)
library(tidyr)
library(lubridate)
library(janitor)
if (!requireNamespace("lavaan", quietly = TRUE)) {
  install.packages("lavaan")
}
library(lavaan)
library(ggplot2)

setwd("C:/Causal-Workflow/")

load(file="data/clean_data.Rda")


long_df <-  subset(data, select = c(id_batch, age, weight))
long_df <- long_df %>%
  clean_names()

long_df <- long_df %>%
  group_by(id_batch) %>%
  filter(sum(!is.na(weight)) >= 20)

#Reshape data
wide_df <- pivot_wider(long_df, 
                      names_from = age, 
                      values_from = weight, 
                      id_cols = id_batch,
                      names_prefix = "weight_")

#Keeping variables which have many observed
wide_df <- data.frame(wide_df[c("id_batch", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                     "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", 
                     "weight_13", "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", 
                     "weight_19", "weight_20", "weight_21", "weight_22", "weight_23", "weight_24", 
                     "weight_25", "weight_26", "weight_27")])

#Removing rows with missing values, i.e. we are only modeling on those with a complete set of weight observations
wide_df <- na.omit(wide_df)
#Specifying a growth model
model <- 'intercept =~   1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27
           growth =~   1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26 + 27*weight_27 
            sqr_growth =~   1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27' 

fit <- growth(model, data=wide_df)
# summary(fit)
# Predict the latent variables using the fitted model and the observed data
parameters <- predict(fit, wide_df)
wide_df <- cbind(wide_df, parameters)
wide_df$id <- seq(nrow(wide_df))
growth <- wide_df %>% select(id_batch, id, intercept, growth, sqr_growth)


#Reshape to long data for plotting
long <- pivot_longer(
  data = wide_df, 
  cols = starts_with("weight_"), 
  names_to = "age", 
  values_to = "weight",
  names_prefix = "weight_"  # Optional: remove "var" prefix from column names in 'age'
)
long$day <- as.numeric(long$age)
long <- select(long, c('id', 'id_batch', 'day', 'weight'))

plot_data <- merge(long, growth, by = "id")

plot_data <- select(plot_data, c("id", "id_batch.y", "day", "weight", "intercept", "growth", "sqr_growth"))
plot_data$age <- plot_data$day
plot_data$estimate_weight <- plot_data$intercept + plot_data$growth*plot_data$age + plot_data$sqr_growth*plot_data$age^2

#plot 
i <- sample(plot_data$id_batch.y, 1)
obs_df <- filter(plot_data, id_batch.y == i)
obs_df <- obs_df[order(obs_df$day), ]
rows <- nrow(obs_df)
obs_df$x <- seq(0, rows, length.out = rows)
obs_df$y <- obs_df$intercept[1] + obs_df$growth[1]*obs_df$x + obs_df$sqr_growth[1]*obs_df$x^2

#plot 1 with ggplot2
rows <- nrow(obs_df)
# Compute x and y values 
obs_df$x <- seq(0, rows, length.out = rows)
obs_df$y <- obs_df$intercept[1] + obs_df$growth[1]*obs_df$x + obs_df$sqr_growth[1]*obs_df$x^2

p1 <- ggplot(data = obs_df, aes(x = age, y = weight)) + 
  geom_line(lwd = 1, color = "black", linetype = "dashed") +
  geom_point(shape = 21, fill = "white", size = 3) +
  ylim(0, 2000) +
  xlab("Age") + 
  ylab("Weight")

p1 <- p1 + geom_line(data=obs_df, aes(x,y)) + ggtitle(paste0("Batch id = ",i, ". Foredlingskylling"))
p1

#Add on growth data
data <- left_join(data, growth, by = 'id_batch')

names(data)[names(data) == "aceties"] <- "ascites"

long_data <- subset(data, select = c('id_batch',
                                     'feed_name', #Treatment 
                                     'feed_firm', #Feed firm code, case we need it
                                     'ascites', #Outcome
                                     'average_out_temp', #Outdoor temperature
                                     'average_out_hum', #Outdoor humidity
                                     'growth', #Growth_linear
                                     'sqr_growth', #Growth_sqr
                                     'average_Tmax', #Indoor Temperature
                                     'average_Tmin', #Indoor Temperature
                                     'average_Hmax', #Indoor Humidity
                                     'average_Hmin', #Indoor Humidity
                                     'frequent_month', #Month
                                     'id_slaughterhouse', #Slaughterhouse
                                     'average_food', #Food consumption
                                     'average_water', #Water consumption
                                     'bird_density', #Birds p m-sqr
                                     'chicken_last_day',
                                     'LeverandoerNr')) #For farm effect  

# Reshape from long to wide 
wide_data <-  distinct(long_data, id_batch, .keep_all = TRUE)
setwd("C:/Causal-Workflow/")
save(wide_data,file = "data/analysis_wide_data.Rda")
save(data,file = "data/analysis_data.Rda")

write.csv(wide_data, file = "data/analysis_wide_data.csv", row.names = FALSE)
#MANUALLY CHANGE THE ENCODING
library(readr)
wide_data <- read_csv("data/analysis_wide_data.csv", locale = locale(encoding = "UTF-8"))

table(wide_data$feed_name)

wide_data <- wide_data %>%
  mutate(feed_name = case_when(
    feed_name == "Harmoni Kylling  Margin" ~ "Harmoni Kylling Margin",
    feed_name == "Harmoni Kylling  Ressurs" ~ "Harmoni Kylling Ressurs",
    feed_name == "Harmoni Kylling Ressurs Mc Donalds m/ Clostat" ~ "Harmoni Kylling Ressurs Mc Donalds m/Clostat",
    feed_name == "Harmoni Kylling Vekst " ~ "Harmoni Kylling Vekst",
    feed_name == "Harmoni kylling  Ressurs" ~ "Harmoni Kylling Ressurs",
    feed_name == "Harmoni kylling  Spurt" ~ "Harmoni Kylling Spurt",
    feed_name == "Harmoni kylling Spurt" ~ "Harmoni Kylling Spurt",
    feed_name == "Harmoni kylling Vekst" ~ "Harmoni Kylling Vekst",
    TRUE ~ feed_name  # Default case to leave other names unchanged
  ))


save(wide_data,file = "data/analysis_wide_data.Rda")
