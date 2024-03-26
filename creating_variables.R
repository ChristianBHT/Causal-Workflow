rm(list = ls())

setwd("C:/broiler_acites/Causal Workflow with Broiler case")
load("data/data.Rda")

load("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Foreldredyr.Rdata")
parents <- subset(Foreldredyr, select = c('FK_Innsett_Dim', 'KodeForeldredyr', 'AlderForeldredyr', 'Antall', 'VektDaggamle'))
colnames(parents) <-  c('id_batch', 'code_parents', 'age_parents', 'birds_from_hatchery', 'weight_day_old')

birds <- parents %>%
  group_by(id_batch) %>%
  summarise(birds_from_hatchery = sum(birds_from_hatchery))

data <- left_join(data, birds, by = 'id_batch')

#Making a month variable
data$month <- month(data$date)

#Get the most frequent month as the month appearing in final data
freq_month <- data %>% 
  group_by(id_batch) %>%
  summarise(frequent_month = names(which.max(table(month))))

# Converting month into a factor variable
freq_month$frequent_month <- as.factor(freq_month$frequent_month)

# Create water_per_bird
data$chickens <- data$birds_from_hatchery - data$total_dead

data$water_per_bird <- data$water_consump/data$chickens

water <- data %>%
  group_by(id_batch) %>%
  filter(age > 3 & age < 30) %>%
  summarise(average_water = mean(water_per_bird))

data$food_per_bird <- data$total_food_used/data$chickens

food <- data %>%
  group_by(id_batch) %>%
  filter(age > 3 & age < 30) %>%
  summarise(average_food = mean(food_per_bird))

data$bird_per_m <- data$chickens/data$area





# Add on month variable to data
data <- left_join(data, freq_month, by = 'id_batch')
data <- left_join(data, water, by = 'id_batch')
data <- left_join(data, food, by = 'id_batch')
