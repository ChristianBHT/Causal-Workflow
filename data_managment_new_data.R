library(lubridate)
library(dplyr)
library(naniar)
rm(list = ls())

load("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Produksjonsdata.Rdata")
# The last date in original data set is 2021-06-10
load('C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Temperature_update.Rdata')
load("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Humidity_update.Rdata")

load("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/DaggamleKyllinger.Rdata")

day_data <- data.frame(Produksjonsdata$PK_Produksjonsdata_Fak,
                       Produksjonsdata$FK_Innsett_Dim,
                       Produksjonsdata$FK_Forfirma_Dim,
                       Produksjonsdata$FK_Slakteri_Dim,
                       Produksjonsdata$FK_Rugeri_Dim,
                       Produksjonsdata$FK_TypeProduksjon_Dim,
                       Produksjonsdata$InnsatteKyllinger,
                       ymd(Produksjonsdata$Dato), 
                       Produksjonsdata$Alder, 
                       Produksjonsdata$VektDyr,
                       Produksjonsdata$AntallSelvdoede, 
                       Produksjonsdata$AvlivedeBen, 
                       Produksjonsdata$AvlivedeMisvekst, 
                       Produksjonsdata$AvlivedeSkaderHakking, 
                       Produksjonsdata$AvlivedeAndre, 
                       Produksjonsdata$Doede,
                       Produksjonsdata$Vannforbruk, 
                       Produksjonsdata$Forforbruk,
                       Produksjonsdata$TempMin, 
                       Produksjonsdata$TempMaks, 
                       Produksjonsdata$LuftfuktighetMin, 
                       Produksjonsdata$LuftfuktighetMaks, 
                       Produksjonsdata$Lysstyrke, 
                       Produksjonsdata$Lystimer,
                       Produksjonsdata$Vektvariasjon,
                       Produksjonsdata$DagligTilvekst,
                       Produksjonsdata$VannDeltPaaFor)  

colnames(day_data) <- c('id_farmday', 
                        'id_batch', 
                        'id_feedfirm', 
                        'id_slaughterhouse', 
                        'id_hatchery', 
                        'prod_type', 
                        'N_of_chicken',
                        'date',
                        'age',
                        'weight',
                        'dead_self',
                        'killed_legs',
                        'killed_stunted',
                        'killed_pecking',
                        'killed_other',
                        'total_dead',
                        'water_consump',
                        'feed_consump',
                        'temp_min',
                        'temp_max',
                        'humidity_min',
                        'humidity_max',
                        'light_strength',
                        'light_hours',
                        'weight_var',
                        'day_growth',
                        'water_by_food')


temperature_data$date <- as.character(temperature_data$date)

#Find duplications and select those with best quality (temperature)
temperature_data <- subset(temperature_data, timeOffset == 'PT0H')

temp_data_selected <- temperature_data %>%
  group_by(id_batch, date) %>% # Group by id_batch and date
  slice_min(qualityCode) # Filter to keep only the best quality

uniqe_temp_data <- distinct(temp_data_selected, id_batch, date, .keep_all = TRUE) #Removing duplicated entries  (if equal quality)
temp_df <- subset(uniqe_temp_data, select = c('id_batch', 'date', 'temperature'))
colnames(temp_df) <-  c('id_batch', 'date', 'out_temp')

#I am using merge in such a way that we are only keeping observations with observations in both data set Y and X!
analysis_df <- merge(day_data, temp_df, by = c('id_batch', 'date'))
humi_data_selected <- humidity_data %>%
  group_by(id_batch, date) %>% # Group by id_batch and date
  slice(which.min(qualityCode)) # Filter to keep only the best quality

dups <- duplicated(humidity_data[, c("id_batch", "date")]) | 
  duplicated(humidity_data[, c("id_batch", "date")], fromLast = TRUE)

non_dups <- !dups
uniqe_humi_data <- humidity_data[non_dups, ]

humi_df <- data.frame(rbind(humi_data_selected, uniqe_humi_data))
humi_df$id_batch <- as.numeric(humi_df$id_batch)
humi_df$date <- as.Date(humi_df$date)
humi_df$value <- as.numeric(humi_df$value)
humi_df <- subset(humi_df, select = c('id_batch', 'date', 'value'))
colnames(humi_df) <-  c('id_batch', 'date', 'out_humidity')
#I am using merge in such a way that we are only keeping observations with observations in both data set Y and X!
analysis_df <- merge(day_data, temp_df, by = c('id_batch', 'date'))
analysis_df <- merge(analysis_df, humi_df, by = c('id_batch', 'date'))



save(analysis_df, file = "data/data_without_feed_type.Rdata")
# Koble på innsett informasjon, blant annet fortype
load("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Innsett.Rdata")
batch_df <- subset(Innsett, select = c('PK_Innsett_Dim', 'Areal',  'Aceties', 'FK_TypeProduksjon_Dim', 'ForforbrukTotalt', 'LeverandoerNr'))
colnames(batch_df) <- c('id_batch', 'area', 'aceties', 'type_of_prod', 'total_food_used', 'LeverandoerNr')
analysis_df <- merge(analysis_df, batch_df, by = 'id_batch')
rm(temperature_data)

# Koble på fortype-navn
load('C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Fortype.Rdata')
load('C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Forblanding.Rdata')
handling <- load('C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Handling.Rdata')
handling <- subset(Handling, select = c('FK_Innsett_Dim', 'FK_Fortype_Dim', 'FK_Forblanding_Dim', "Mengde"))
colnames(handling) <- c('id_batch', 'feed_type', 'feed_mix', "mengde")
Forblanding <- subset(Forblanding, select = c('PK_Forblanding_Dim', 'Forblanding', 'FK_Fortype_Dim'))
colnames(Forblanding) <- c('feed_mix', 'feed', 'feed_type') 
feed_type <- subset(Forblanding, feed_type==2)
# write.csv2(feed_type, "C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/feed_type.csv")
# # Manuell manipulering av data set slik at teite tegn endres til noe normalt
# feed_type <- read.csv2(file = "C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/feed_type.csv")
feed_type <- subset(feed_type, select = c("feed_mix", "feed", "feed_type"))

feed <- subset(handling, feed_type == 2)
result <- feed %>%
  group_by(id_batch, feed_mix) %>%
  summarise(total_amount = sum(mengde))
# Only keep those who use one kind of growth feed
unique_groups <- result %>%
  group_by(id_batch) %>%
  filter(n() == 1)

feed <- unique_groups
feed <- merge(feed, feed_type, by = 'feed_mix')
colnames(feed) <- c('growth_feed', 'id_batch', 'total_amount', 'feed', 'feed_type')
feed <- subset(feed, select = c('id_batch', 'growth_feed', 'total_amount'))
# Check number of observations (426949)
#load("data_without_feed_type.Rdata")
df <- merge(analysis_df, feed, by = 'id_batch')
df$age <- as.numeric(df$age)

# Check that everyhing is correct
df <- df[!duplicated(df), ]

# hybrid <- subset(DaggamleKyllinger, select = c("FK_Innsett_Dim", "Hybrid"))
# colnames(hybrid) <- c("id_batch", "hybrid")

# Number of observations (162271)
# df <- merge(df, hybrid, by = 'id_batch')

save(df,file="data/analysis_df_newdata.Rda")

