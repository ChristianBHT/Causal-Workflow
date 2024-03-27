rm(list = ls())
setwd("C:/Causal-Workflow/")

load(file="data/unclean_analysis_data.Rda")
data$chicken_type <-  ifelse(data$type_of_prod == 4, "Processed", 
                             ifelse(data$type_of_prod == 6, "Grill", 
                                    ifelse(data$type_of_prod == 8, "Land",
                                           ifelse(data$type_of_prod == 12, "Organic",
                                                  ifelse(data$type_of_prod == 14, "Liveche",
                                                         ifelse(data$type_of_prod == 17, "McDonalds",
                                                                ifelse(data$type_of_prod == 21, "Hubbard",
                                                                       ifelse(data$type_of_prod == 22, "Kyllinggaarden",
                                                                              ifelse(data$type_of_prod == 29, "Unknown", NA)))))))))
table(data$chicken_type)  

grill_data <- subset(data, chicken_type == "Grill")
grill_data <- subset(grill_data, age <= 30)

grill_data$weight[grill_data$weight >= 100 & grill_data$age == 0] <- NA
grill_data$weight[grill_data$weight <= 34 & grill_data$age == 0] <- NA

for (i in 0:30){
  grill_data$weight[grill_data$weight == 0 & grill_data$age == i]  <- NA
  mean <- mean(grill_data$weight[grill_data$age == i], na.rm = T)
  outlier <- 4*sd(grill_data$weight[grill_data$age == i], na.rm = T)
  grill_data$weight[(grill_data$weight >= (mean + outlier) | grill_data$weight <= (mean - outlier) ) & grill_data$age == i]  <- NA
}

proces_data <- subset(data, chicken_type == "Processed")
proces_data <- subset(proces_data, age <= 30) 
proces_data$weight[proces_data$weight >= 100 & proces_data$age == 0] <- NA
proces_data$weight[proces_data$weight <= 25 & proces_data$age == 0] <- NA

for (i in 0:30){
  proces_data$weight[proces_data$weight == 0 & proces_data$age == i]  <- NA
  mean <- mean(proces_data$weight[proces_data$age == i], na.rm = T)
  outlier <- 4*sd(proces_data$weight[proces_data$age == i], na.rm = T)
  proces_data$weight[(proces_data$weight >= (mean + outlier) | proces_data$weight <= (mean - outlier) ) & proces_data$age == i]  <- NA
}

#########################################
###Cleaning data for McDonalds chicken###
#########################################
mcdonald_data <- subset(data, chicken_type == "McDonalds")
mcdonald_data <- subset(mcdonald_data, age <= 30)
mcdonald_data$weight[mcdonald_data$weight >= 100 & mcdonald_data$age == 0] <- NA
mcdonald_data$weight[mcdonald_data$weight <= 25 & mcdonald_data$age == 0] <- NA

for (i in 0:30){
  mcdonald_data$weight[mcdonald_data$weight == 0 & mcdonald_data$age == i]  <- NA
  mean <- mean(mcdonald_data$weight[mcdonald_data$age == i], na.rm = T)
  outlier <- 4*sd(mcdonald_data$weight[mcdonald_data$age == i], na.rm = T)
  mcdonald_data$weight[(mcdonald_data$weight >= (mean + outlier) | mcdonald_data$weight <= (mean - outlier) ) & mcdonald_data$age == i]  <- NA
}

##############################################
###Cleaning data for unknown               ###
##############################################

unknown_data <- subset(data, chicken_type == "Unknown")
unknown_data <- subset(unknown_data, age <= 30)

unknown_data$weight[unknown_data$weight >= 100 & unknown_data$age == 0] <- NA
unknown_data$weight[unknown_data$weight <= 25 & unknown_data$age == 0] <- NA

for (i in 0:30){
  unknown_data$weight[unknown_data$weight == 0 & unknown_data$age == i]  <- NA
  mean <- mean(unknown_data$weight[unknown_data$age == i], na.rm = T)
  outlier <- 4*sd(unknown_data$weight[unknown_data$age == i], na.rm = T)
  unknown_data$weight[(unknown_data$weight >= (mean + outlier) | unknown_data$weight <= (mean - outlier) ) & unknown_data$age == i]  <- NA
}


##############################################
###Cleaning data for Kyllinggaarden chicken###
##############################################
kyllinggaarden_data <- subset(data, chicken_type == "Kyllinggaarden")
kyllinggaarden_data <- subset(kyllinggaarden_data, age <= 30)

kyllinggaarden_data$weight[kyllinggaarden_data$weight >= 100 & kyllinggaarden_data$age == 0] <- NA
kyllinggaarden_data$weight[kyllinggaarden_data$weight <= 25 & kyllinggaarden_data$age == 0] <- NA

for (i in 0:30){
  kyllinggaarden_data$weight[kyllinggaarden_data$weight == 0 & kyllinggaarden_data$age == i]  <- NA
  mean <- mean(kyllinggaarden_data$weight[kyllinggaarden_data$age == i], na.rm = T)
  outlier <- 4*sd(kyllinggaarden_data$weight[kyllinggaarden_data$age == i], na.rm = T)
  kyllinggaarden_data$weight[(kyllinggaarden_data$weight >= (mean + outlier) | kyllinggaarden_data$weight <= (mean - outlier) ) & kyllinggaarden_data$age == i]  <- NA
}


#Data cleaning line by line
data <- rbind(grill_data, proces_data, mcdonald_data, kyllinggaarden_data, unknown_data)

data$temp_max[data$temp_max == -99.0] <- NA
data$temp_max[data$temp_max == 99.0] <- NA
data$temp_max[data$temp_min == -99.0] <- NA
data$temp_max[data$temp_min == 99.0] <- NA
data$temp_min[data$temp_min >= 80] <- NA
data$temp_max[data$temp_max >= 80] <- NA
data$humidity_min[data$humidity_min > 100] <- NA
data$humidity_max[data$humidity_max > 100] <- NA
data$humidity_min[data$humidity_min < 1] <- NA
data$humidity_max[data$humidity_max < 1] <- NA


save(data, file="data/clean_data.Rda")

