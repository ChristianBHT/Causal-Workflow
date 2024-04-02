library(dplyr)
library(lubridate)
library(janitor)
library(lavaan)
library(ggplot2)

rm(list = ls())

setwd("C:/broiler_acites/Causal Workflow with Broiler case")
load("data/data.Rda")
selected_rows <- data %>% filter(id_batch == 15450)

load('C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Husinfo.Rdata')
#Getting feed names from the forblanding table
load('C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Forblanding.Rdata')

#Slaughter table (not used)
load('C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Utslaktning.Rdata')
slaughter <- subset(Utslaktning, select = c("FK_Innsett_Dim", "LevendeAntall", "InnsatteKyllinger"))
colnames(slaughter) <- c("id_batch", "alive_slaughter", "chickens_from_h")
# Finding duplicates 
duplicated <- slaughter[duplicated(slaughter) | duplicated(slaughter, fromLast = TRUE), ]
slaughter <- slaughter[!duplicated(slaughter), ]

feed <- subset(Forblanding, select = c('PK_Forblanding_Dim', 'Forblanding', 'FK_Forfirma_Dim', 'FK_Fortype_Dim'))
feed <- filter(feed, FK_Fortype_Dim == 2)
colnames(feed) <- c('growth_feed', 'feed_name', 'feed_firm')

#Get Hybrid information
load("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/DaggamleKyllinger.Rdata")
hybrid <- subset(DaggamleKyllinger, select = c('FK_Innsett_Dim', 'Hybrid'))
colnames(hybrid) <- c('id_batch', 'hybrid')
# Load data from the hatchery
load("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Foreldredyr.Rdata")
parents <- subset(Foreldredyr, select = c('FK_Innsett_Dim', 'KodeForeldredyr', 'AlderForeldredyr', 'Antall', 'VektDaggamle'))
colnames(parents) <-  c('id_batch', 'code_parents', 'age_parents', 'birds_from_hatchery', 'weight_day_old')

# Get number of birds from hatchery 
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

# Calculating how many alive chicken there is, data from slaughterhouse is wrongly registered with many double counts
data <- data %>%
  group_by(id_batch) %>%                  # Group by id_batch
  arrange(age, .by_group = TRUE) %>%  # Ensure data is ordered by age within each group
  mutate(cum_dead = cumsum(total_dead))   # Calculate cumulative sum of dead chicken


data$chickens <- data$birds_from_hatchery - data$cum_dead #Calculating number of chickens

# The minimum alive chicken is the number delivered to slaughterhouse
min_chicken <- data %>%
  group_by(id_batch) %>%
  summarise(chicken_last_day = min(chickens))

data$water_per_bird <- data$water_consump/data$chickens

water <- data %>%
  group_by(id_batch) %>%
  filter(age > 3 & age < 30) %>%
  summarise(average_water = mean(water_per_bird))

# Create food_per_bird
data$food_per_bird <- data$total_food_used/data$chickens

food <- data %>%
  group_by(id_batch) %>%
  filter(age > 3 & age < 30) %>%
  summarise(average_food = mean(food_per_bird))

data$bird_per_m <- data$chickens/data$area

bird_density <- data %>%
  group_by(id_batch) %>%
  summarise(bird_density = mean(bird_per_m))

# Create temp minimum variable
temperatur_aggregate <- data %>%
  group_by(id_batch) %>%
  summarise(average_Tmin = mean(temp_min), 
            average_Tmax = mean(temp_max),
            average_Hmin = mean(humidity_min), 
            average_Hmax = mean(humidity_max))

#Outdoor temp and outdoor humidity
data$out_temp <- as.numeric(data$out_temp)

climate_data <- data %>%
                group_by(id_batch) %>%
                summarise(average_out_temp = mean(out_temp), 
                          average_out_hum = mean(out_humidity))


#Add on hybrid
data <- left_join(data, hybrid, by = 'id_batch')
#Add on chickens
data <- left_join(data, min_chicken, by = 'id_batch')
#Add on month variable to data
data <- left_join(data, freq_month, by = 'id_batch')
#Add on water consumption 
data <- left_join(data, water, by = 'id_batch')
#Add on food consumption
data <- left_join(data, food, by = 'id_batch')
#Add on bird density
data <- left_join(data, bird_density, by = 'id_batch')
#Add on bird indoor temp and humidity
data <- left_join(data, temperatur_aggregate, by = 'id_batch')
#Add on climate data
data <- left_join(data, climate_data, by = 'id_batch')
#Add on feed data
data <- merge(data, feed, by = 'growth_feed')


# Modelling growth curves
# Select id age and weights
data <- subset(data, hybrid == "Ross 308")
setwd("C:/Causal-Workflow/")

save(data,file="data/unclean_analysis_data.Rda")



