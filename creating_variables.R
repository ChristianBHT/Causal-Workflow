rm(list = ls())

setwd("C:/broiler_acites/Causal Workflow with Broiler case")
load("data/data.Rda")

#Making a month variable
data$month <- month(data$date)

#Get the most frequent month as the month appearing in final data
freq_month <- data %>% 
  group_by(id_batch) %>%
  summarise(frequent_month = names(which.max(table(month))))

freq_month$frequent_month <- as.factor(freq_month$frequent_month)
data <- merge(data, freq_month, by = 'id_batch')
