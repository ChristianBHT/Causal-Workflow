library(dplyr)
library(lubridate)
library(janitor)
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
wide_df <- reshape(long_df, idvar = "id_batch", timevar = "age", direction = "wide")
all_columns <- colnames(wide_df)

# Identify columns to keep (weight.1 to weight.29)
# This pattern matches "weight." followed by a number from 1 to 29
columns_to_keep <- c('id_batch', grep("weight\\.(2[0-8]|1[0-9]|[1-9])$", all_columns, value = TRUE))
wide_df <- wide_df[, columns_to_keep]

#Removing rows with missing values, i.e. we are only modeling on those with a complete set of weight observations
wide_df <- na.omit(wide_df)
#Specifying a growth model
model <- ' intercept =~   1*weight.1 + 1*weight.2 + 1*weight.3 + 1*weight.4 + 1*weight.5 + 1*weight.6 + 1*weight.7 + 1*weight.8 + 1*weight.9 + 1*weight.10 + 1*weight.11 + 1*weight.12 + 1*weight.13 + 1*weight.14 + 1*weight.15 + 1*weight.16 + 1*weight.17 + 1*weight.18 + 1*weight.19 + 1*weight.20 + 1*weight.21 + 1*weight.22 + 1*weight.23 +  1*weight.24 + 1*weight.25 + 1*weight.26 + 1*weight.27 + 1*weight.28
           growth =~   1*weight.1 + 2*weight.2 + 3*weight.3 + 4*weight.4 + 5*weight.5 + 6*weight.6 + 7*weight.7 + 8*weight.8 + 9*weight.9 + 10*weight.10 + 11*weight.11 + 12*weight.12 + 13*weight.13 + 14*weight.14 + 15*weight.15 + 16*weight.16 + 17*weight.17 + 18*weight.18 + 19*weight.19 + 20*weight.20 + 21*weight.21 + 22*weight.22 + 23*weight.23 +24*weight.24 + 25*weight.25 + 26*weight.26 + 27*weight.27 + 28*weight.28
            sqr_growth =~   1*weight.1 + 4*weight.2 + 9*weight.3 + 16*weight.4 + 25*weight.5 + 36*weight.6 + 49*weight.7 + 64*weight.8 + 81*weight.9 + 100*weight.10 + 121*weight.11 + 144*weight.12 + 169*weight.13 + 196*weight.14 + 225*weight.15 + 256*weight.16 + 289*weight.17 + 324*weight.18 + 361*weight.19 + 400*weight.20 + 441*weight.21 + 484*weight.22 + 529*weight.23 +  576*weight.24 + 625*weight.25 + 676*weight.26 + 729*weight.27 + 784*weight.28'

fit <- growth(model, data=wide_df)
summary(fit)
# Predict the latent variables using the fitted model and the observed data
parameters <- predict(fit, wide_df)
wide_df <- cbind(wide_df, parameters)
wide_df$id <- seq(nrow(wide_df))
growth <- wide_df %>% select(id_batch, id, intercept, growth, sqr_growth)


#Reshape to long data for plotting
long <- reshape(wide_df,
                idvar = "id",
                timevar = "day",
                varying = c("weight.1", "weight.2", "weight.3", "weight.4","weight.5", "weight.6", "weight.7","weight.8",
                            "weight.9", "weight.10","weight.11", "weight.12", "weight.13", "weight.14","weight.15", "weight.16",
                            "weight.17", "weight.18","weight.19", "weight.20", "weight.21", "weight.22","weight.23", "weight.24",
                            "weight.25", "weight.26","weight.27", "weight.28"),
                v.names = c("weight"),
                direction = "long")
long <- select(long, c('id', 'id_batch', 'day', 'weight'))

plot_data <- merge(long, growth, by = "id")

plot_data <- select(plot_data, c("id", "id_batch.y", "day", "weight", "intercept", "growth", "sqr_growth"))
plot_data$age <- plot_data$day
plot_data$estimate_weight <- plot_data$intercept + plot_data$growth*plot_data$age + plot_data$sqr_growth*plot_data$age^2

#plot 
i <- sample(plot_data$id_batch.y, 1)
obs_df <- filter(plot_data, id_batch.y == 86473)
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

p1 <- p1 + geom_line(data=obs_df, aes(x,y)) + ggtitle(paste0("Batch id = ", i, ". Foredlingskylling"))
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
                                     'LeverandoerNr')) #For farm effect  

# Reshape from long to wide 
wide_data <-  distinct(long_data, id_batch, .keep_all = TRUE)
setwd("C:/Causal-Workflow/")
save(wide_data,file="data/analysis_wide_data.Rda")
save(data,file="data/analysis_data.Rda")











