library(dagitty)
library(GeneralisedCovarianceMeasure)
rm(list = ls())
setwd("C:/Causal-Workflow/")
load("data/analysis_data.Rda")

DAG <- dagitty('dag {
bb="0,0,1,1"
"Bird density" [pos="0.376,0.316"]
"CO2-level" [latent,pos="0.496,0.467"]
"Feed type" [exposure,pos="0.202,0.321"]
"Food cons." [pos="0.304,0.113"]
"Month of Year" [pos="0.309,0.542"]
"Outdoor humidity" [pos="0.436,0.544"]
"Outdoor temp." [pos="0.631,0.679"]
"Water cons." [pos="0.542,0.149"]
Ascites [outcome,pos="0.419,0.446"]
Geography [latent,pos="0.234,0.674"]
Growth [pos="0.301,0.298"]
Humidity [pos="0.440,0.311"]
Temp. [pos="0.545,0.257"]
"Bird density" -> "Food cons."
"Bird density" -> Ascites
"Bird density" -> Growth
"CO2-level" -> Ascites
"Feed type" -> "Food cons."
"Feed type" -> Ascites
"Feed type" -> Growth
"Food cons." -> Growth
"Month of Year" -> "Feed type"
"Month of Year" -> "Outdoor humidity"
"Month of Year" -> "Outdoor temp."
"Month of Year" -> Ascites
"Outdoor humidity" -> Ascites
"Outdoor temp." -> "CO2-level"
"Outdoor temp." -> "Outdoor humidity"
"Outdoor temp." -> Ascites
"Outdoor temp." -> Humidity
"Outdoor temp." -> Temp.
"Water cons." -> "Food cons."
"Water cons." -> Growth
Geography -> "Feed type"
Geography -> "Outdoor humidity"
Geography -> "Outdoor temp."
Growth -> Ascites
Humidity -> Ascites
Temp. -> "Water cons."
Temp. -> Ascites
Temp. -> Humidity
}
')
plot(wide_data$growth)
plot(DAG)
# Get the adjustment set for total effect
impliedConditionalIndependencies(DAG)

# Creating a feed variable
frequency <- wide_data %>%
  count(feed_name) %>%
  arrange(desc(n))

# Identify the ten most common feed types
top_ten <- frequency$feed_name[1:10]

wide_data <- wide_data %>%
  mutate(feed = if_else(feed_name %in% top_ten, feed_name, "Other"))

wide_data$feed <- as.factor(wide_data$feed)
wide_data$frequent_month <- as.factor(wide_data$frequent_month)

#Feed type ⊥ Water cons. | Temp.
testData <- subset(wide_data, select = c(feed, average_water, average_Tmax))
testData <- na.omit(testData)

test1 <- gcm.test(Y = testData$feed, X = testData$average_water, Z = testData$average_Tmax, alpha = 0.01)
test1

#Feed type ⊥ Water cons. | Outdoor temp.
testData <- subset(wide_data, select = c(feed, average_water, average_out_temp))
testData <- na.omit(testData)

test2 <- gcm.test(Y = testData$feed, X = testData$average_water, Z = testData$average_out_temp, alpha = 0.01)
test2

#Feed type ⊥ Humidity | Outdoor temp.
testData <- subset(wide_data, select = c(feed, average_Hmax, average_out_temp))
testData <- na.omit(testData)
test3 <- gcm.test(Y = testData$feed, X = testData$average_Hmax, Z = testData$average_out_temp, alpha = 0.01)
test3

#Feed type ⊥ Temp. | Outdoor temp.
testData <- subset(wide_data, select = c(feed, average_Tmax, average_out_temp))
testData <- na.omit(testData)
test4 <- gcm.test(Y = testData$feed, X = testData$average_Tmax, Z = testData$average_out_temp, alpha = 0.01)
test4 #Reject

#Food cons. ⊥ Outdoor humidity | Feed type, Outdoor temp.
testData <- subset(wide_data, select = c(average_food, average_out_hum, feed, average_out_temp))
testData <- na.omit(testData)

dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_out_temp)

test5 <- gcm.test(Y = testData$average_food, X = testData$average_out_hum, Z = Z, alpha = 0.01)
test5

#Food cons. ⊥ Outdoor humidity | Feed type, Temp.
testData <- subset(wide_data, select = c(average_food, average_out_hum, feed, average_Tmax))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_Tmax)
test6 <- gcm.test(Y = testData$average_food, X = testData$average_out_hum, Z = Z, alpha = 0.01)
test6

#Food cons. ⊥ Outdoor humidity | Feed type, Outdoor temp.
testData <- subset(wide_data, select = c(average_food, average_out_hum, feed, average_out_temp))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_out_temp)
test7 <- gcm.test(Y = testData$average_food, X = testData$average_out_hum, Z = Z, alpha = 0.01)
test7

#Food cons. ⊥ Outdoor humidity | Feed type, Water cons.
testData <- subset(wide_data, select = c(average_food, average_out_hum, feed, average_water))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_water)
test8 <- gcm.test(Y = testData$average_food, X = testData$average_out_hum, Z = Z, alpha = 0.01)
test8 

#Food cons. ⊥ Outdoor humidity | Feed type
testData <- subset(wide_data, select = c(average_food, average_out_hum, feed))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix)
test9 <- gcm.test(Y = testData$average_food, X = testData$average_out_hum, Z = Z, alpha = 0.01)
test9

#Food cons. ⊥ Outdoor temp. | Feed type, Temp.
testData <- subset(wide_data, select = c(average_food, average_out_temp, feed, average_Tmax))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_Tmax)
test10 <- gcm.test(Y = testData$average_food, X = testData$average_out_temp, Z = Z, alpha = 0.01)
test10

#Food cons. ⊥ Outdoor temp. | Feed type, Water cons.
testData <- subset(wide_data, select = c(average_food, average_out_temp, feed, average_water))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_water)
test11 <- gcm.test(Y = testData$average_food, X = testData$average_out_temp, Z = Z, alpha = 0.01)
test11

#Food cons. ⊥ Ascites | Bird density, Feed type, Growth, Temp.
testData <- subset(wide_data, select = c(average_food, ascites, bird_density, feed, growth, sqr_growth, average_Tmax))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$bird_density, testData$growth, testData$sqr_growth, testData$average_Tmax)

test12 <- gcm.test(Y = testData$average_food, 
                   X = testData$ascites, 
                   Z = Z, 
                   alpha = 0.01)
test12

#Food cons. ⊥ Ascites | Bird density, Feed type, Growth, Water cons.
testData <- subset(wide_data, select = c(average_food, ascites, bird_density, growth, sqr_growth, average_water))
testData <- na.omit(testData)
test12 <- gcm.test(Y = testData$average_food, 
                   X = testData$ascites, 
                   Z = data.frame(testData$bird_density, testData$growth, testData$sqr_growth, testData$average_water), 
                   alpha = 0.01)
test12

#Food cons. ⊥ Humidity | Outdoor temp., Temp.
testData <- subset(wide_data, select = c(average_food, average_Hmax, average_Hmin, average_out_temp, average_Tmax, average_Tmin))
testData <- na.omit(testData)
Z <- data.frame(testData$average_Tmax, testData$average_Tmin, testData$average_out_temp)
test13 <- gcm.test(Y = testData$average_food, 
                   X = testData$average_Hmax, 
                   Z = Z, 
                   alpha = 0.01)
test13 #Reject

#Food cons. ⊥ Humidity | Outdoor temp., Water cons.
testData <- subset(wide_data, select = c(average_food, average_Hmax, average_Hmin,  average_out_temp, average_water))
testData <- na.omit(testData)
Z <- data.frame(testData$average_water, testData$average_out_temp)
test14 <- gcm.test(Y = testData$average_food, 
                   X = testData$average_Hmax, 
                   Z = Z, 
                   alpha = 0.01)
test14 #Reject

#Food cons. ⊥ Humidity | Feed type, Temp.
testData <- subset(wide_data, select = c(average_food, average_Hmax, average_Hmin, feed, average_Tmin, average_Tmax))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_Tmin, testData$average_Tmax)

test15 <- gcm.test(Y = testData$average_food, 
                   X = testData$average_Hmin, 
                   Z = Z, 
                   alpha = 0.01)
test15 #Reject

#Food cons. ⊥ Humidity | Feed type, Water cons.
testData <- subset(wide_data, select = c(average_food, average_Hmax, average_Hmin, feed, average_water))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_water)

test16 <- gcm.test(Y = testData$average_food, 
                   X = testData$average_Hmax, 
                   Z = Z, 
                   alpha = 0.01)
test16 #Reject

#Food cons. ⊥ Temp. | Outdoor temp., Water cons.
testData <- subset(wide_data, select = c(average_food, average_Tmax, average_Tmin, average_out_temp, average_water))
testData <- na.omit(testData)
Z = data.frame(testData$average_out_temp, testData$average_water)

test17 <- gcm.test(Y = testData$average_food, 
                   X = testData$average_Tmax, 
                   Z = Z, 
                   alpha = 0.01)
test17 

#Food cons. ⊥ Temp. | Feed type, Water cons.
testData <- subset(wide_data, select = c(average_food, average_Tmax, feed, average_water))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_water)

test18 <- gcm.test(Y = testData$average_food, 
                   X = testData$average_Tmax, 
                   Z = Z, 
                   alpha = 0.01)
test18 

#Outdoor humidity ⊥ Water cons. | Temp.
testData <- subset(wide_data, select = c(average_out_hum, average_water, average_Tmax, average_Tmin))
testData <- na.omit(testData)
Z = data.frame(testData$average_Tmax, testData$average_Tmin)

test19 <- gcm.test(Y = testData$average_out_hum, 
                   X = testData$average_water, 
                   Z = Z, 
                   alpha = 0.01)
test19 

#Outdoor humidity ⊥ Water cons. | Outdoor temp.
testData <- subset(wide_data, select = c(average_out_hum, average_water, average_out_temp))
testData <- na.omit(testData)
Z = data.frame(testData$average_out_temp)

test20 <- gcm.test(Y = testData$average_out_hum, 
                   X = testData$average_water, 
                   Z = Z, 
                   alpha = 0.01)
test20 

#Outdoor humidity ⊥ Growth | Feed type, Water cons.
testData <- subset(wide_data, select = c(average_out_hum, growth, feed, average_water))
testData <- na.omit(testData)

dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_water)

test21 <- gcm.test(Y = testData$average_out_hum, 
                   X = testData$growth, 
                   Z = Z, 
                   alpha = 0.01)
test21 #reject 

#Outdoor humidity ⊥ Growth | Feed type, Temp.
testData <- subset(wide_data, select = c(average_out_hum, growth, feed, average_Tmin, average_Tmax))
testData <- na.omit(testData)

dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_Tmin, testData$average_Tmax)

test22 <- gcm.test(Y = testData$average_out_hum, 
                   X = testData$growth, 
                   Z = Z, 
                   alpha = 0.01)
test22 #reject

#Outdoor humidity ⊥ Growth | Feed type, Outdoor temp.
testData <- subset(wide_data, select = c(average_out_hum, growth, feed, average_out_temp))
testData <- na.omit(testData)

dummy_matrix <- model.matrix(~ feed - 1, data = testData)
Z = data.frame(dummy_matrix, testData$average_out_temp)

test23 <- gcm.test(Y = testData$average_out_hum, 
                   X = testData$growth, 
                   Z = Z, 
                   alpha = 0.01)
test23 #reject

#Outdoor humidity ⊥ Humidity | Outdoor temp.
testData <- subset(wide_data, select = c(average_out_hum, average_Hmax, average_out_temp))
testData <- na.omit(testData)
Z = data.frame(testData$average_out_temp)

test24 <- gcm.test(Y = testData$average_out_hum, 
                   X = testData$average_Hmax, 
                   Z = Z, 
                   alpha = 0.01)
test24 #reject

#Outdoor temp. ⊥ Water cons. | Temp.
testData <- subset(wide_data, select = c(average_out_temp, average_water, average_Tmax, average_Tmin))
testData <- na.omit(testData)
Z = data.frame(testData$average_Tmax, testData$average_Tmin)

test25 <- gcm.test(Y = testData$average_out_temp, 
                   X = testData$average_water, 
                   Z = Z, 
                   alpha = 0.01)
test25 

#Outdoor temp. ⊥ Growth | Feed type, Water cons.
testData <- subset(wide_data, select = c(average_out_temp, growth, feed, average_water))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)

Z = data.frame(dummy_matrix, testData$average_water)

test26 <- gcm.test(Y = testData$average_out_temp, 
                   X = testData$growth, 
                   Z = Z, 
                   alpha = 0.01)
test26 #

#Outdoor temp. ⊥ Growth | Feed type, Temp.
testData <- subset(wide_data, select = c(average_out_temp, growth, feed, average_Tmax, average_Tmin))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)

Z = data.frame(dummy_matrix, testData$average_Tmax, testData$average_Tmin)

test27 <- gcm.test(Y = testData$average_out_temp, 
                   X = testData$growth, 
                   Z = Z, 
                   alpha = 0.01)
test27 

#Water cons. ⊥ Ascites | Bird density, Feed type, Growth, Temp.
testData <- subset(wide_data, select = c(average_water, ascites, bird_density, feed, growth, average_Tmax, average_Tmin))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)

Z = data.frame(dummy_matrix, testData$bird_density,  testData$growth, testData$average_Tmax, testData$average_Tmin)

test28 <- gcm.test(Y = testData$average_water, 
                   X = testData$ascites, 
                   Z = Z, 
                   alpha = 0.01)
test28 

#Water cons. ⊥ Humidity | Temp.
testData <- subset(wide_data, select = c(average_water, average_Hmax, average_Tmax, average_Tmin))
testData <- na.omit(testData)

Z = data.frame(testData$average_Tmax, testData$average_Tmin)

test29 <- gcm.test(Y = testData$average_water, 
                   X = testData$average_Hmax, 
                   Z = Z, 
                   alpha = 0.01)
test29 #reject

#Growth ⊥ Humidity | Outdoor temp., Temp.
testData <- subset(wide_data, select = c(average_water, average_Hmax, average_Tmax, average_Tmin))
testData <- na.omit(testData)

Z = data.frame(testData$average_Tmax, testData$average_Tmin)

test29 <- gcm.test(Y = testData$average_water, 
                   X = testData$average_Hmax, 
                   Z = Z, 
                   alpha = 0.01)
test29 

#Growth ⊥ Humidity | Feed type, Temp.
testData <- subset(wide_data, select = c(growth, average_Hmax, feed, average_Tmax, average_Tmin))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)

Z = data.frame(dummy_matrix, testData$average_Tmax, testData$average_Tmin)

test30 <- gcm.test(Y = testData$growth, 
                   X = testData$average_Hmax, 
                   Z = Z, 
                   alpha = 0.01)
test30 

#Growth ⊥ Humidity | Outdoor temp., Water cons.
testData <- subset(wide_data, select = c(growth, average_Hmax, average_out_temp, average_water))
testData <- na.omit(testData)
Z = data.frame(testData$average_out_temp, testData$average_water)

test31 <- gcm.test(Y = testData$growth, 
                   X = testData$average_Hmax, 
                   Z = Z, 
                   alpha = 0.01)
test31 


#Growth ⊥ Humidity | Feed type, Water cons.
testData <- subset(wide_data, select = c(growth, average_Hmax, feed, average_water))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)

Z = data.frame(dummy_matrix, testData$average_water)

test32 <- gcm.test(Y = testData$growth, 
                   X = testData$average_Hmax, 
                   Z = Z, 
                   alpha = 0.01)
test32 


#Growth ⊥ Temp. | Outdoor temp., Water cons.
testData <- subset(wide_data, select = c(growth, average_Tmax, average_out_temp, average_water))
testData <- na.omit(testData)
Z = data.frame(testData$average_out_temp, testData$average_water)

test33 <- gcm.test(Y = testData$growth, 
                   X = testData$average_Tmax, 
                   Z = Z, 
                   alpha = 0.01)
test33 
#Growth ⊥ Temp. | Feed type, Water cons.
testData <- subset(wide_data, select = c(growth, average_Tmax, feed, average_water))
testData <- na.omit(testData)
dummy_matrix <- model.matrix(~ feed - 1, data = testData)

Z = data.frame(dummy_matrix, testData$average_water)

test34 <- gcm.test(Y = testData$growth, 
                   X = testData$average_Tmax, 
                   Z = Z, 
                   alpha = 0.01)
test34 





#Month of Year ⊥ Water cons. | Temp.
#Month of Year ⊥ Water cons. | Outdoor temp.
#Month of Year ⊥ Growth | Feed type, Water cons.
#Month of Year ⊥ Growth | Feed type, Temp.
#Month of Year ⊥ Growth | Feed type, Outdoor temp.
#Month of Year ⊥ Humidity | Outdoor temp.
#Month of Year ⊥ Temp. | Outdoor temp.
#Food cons. ⊥ Month of Year | Feed type, Outdoor temp.
#Food cons. ⊥ Month of Year | Feed type, Temp.
#Food cons. ⊥ Month of Year | Feed type, Water cons.
#Feed type ⊥ Water cons. | Temp.
#Feed type ⊥ Water cons. | Outdoor temp.
#Feed type ⊥ Humidity | Outdoor temp.
#Feed type ⊥ Temp. | Outdoor temp.














