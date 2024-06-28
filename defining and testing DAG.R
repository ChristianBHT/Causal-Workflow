library(dagitty)
library(GeneralisedCovarianceMeasure)
library(dplyr)
rm(list = ls())
setwd("C:/Causal-Workflow/")
load("data/analysis_wide_data.Rda")
dag <- dagitty('dag {
  out_hum [label="Outdoor humidity"]
  out_tem [label="Outdoor temp"]
  month [label="Month of year"]
  geo [latent, label="Geography"]
  feed [label="Feed type"]
  food [label="Food cons"]
  hum [label="Humidity"]
  density [label="Bird density"]
  water [label="Water cons"]
  temp [label="Temp."]
  co2 [latent, label="CO2-level"]
  growth [label="Growth"]
  ascites [label="Ascites"]
  unobs [latent, label="Unknown confounding"]

  growth -> ascites
  out_hum -> ascites
  out_tem -> ascites
  out_tem -> co2
  out_tem -> temp
  out_tem -> hum
  out_tem -> out_hum
  out_tem -> water
  temp -> ascites
  temp -> hum
  temp -> water
  hum -> ascites
  water -> food
  water -> growth
  month -> ascites
  month -> out_hum
  month -> out_tem
  month -> feed
  geo -> out_tem
  geo -> out_hum
  geo -> feed
  feed -> growth
  feed -> ascites
  feed -> food
  co2 -> ascites
  density -> ascites
  density -> growth
  density -> food
  water -> growth
  food -> growth
}')
plot(dag)

# Get the adjustment set for total effect
impliedConditionalIndependencies(dag)
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
## Testing Conditional Independencies using a computational procedure (NullGenerator) if variables are categorical
## or the GCM test.
test_results <- data.frame(CI = NULL,
                           pvalue = NULL)


# Perform the tests
# Define a helper function to add results
add_test_result <- function(statement, test_pvalue) {
  new_row <- data.frame(CI = statement, pvalue = test_pvalue)
  test_results <<- rbind(test_results, new_row)
}
# Initialize a data frame to store the results
test_results <- data.frame(CI = NULL, pvalue = NULL)

# 1. asct _||_ food | dnst, feed, grwt, watr
data <- subset(wide_data, select = c(ascites, average_food, bird_density, feed, growth, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$ascites, Y = data$average_food, Z = data.frame(data$bird_density, as.integer(data$feed), data$growth, data$average_water))
add_test_result("ascites _||_ food | bird density, feed, growth, water", test$p.value)

# 2. asct _||_ food | dnst, feed, grwt, ot_t, temp
data <- subset(wide_data, select = c(ascites, average_food, bird_density, feed, growth, average_out_temp, average_Tmax, average_Tmin))
data <- na.omit(data)
test <- gcm.test(X = data$ascites, Y = data$average_food, Z = data.frame(data$bird_density, as.integer(data$feed), data$growth, data$average_out_temp, data$average_Tmax, data$average_Tmin))
add_test_result("ascites _||_ food | bird density, feed, growth, outdoor temp, temp", test$p.value)

# 3. asct _||_ watr | dnst, feed, grwt, ot_t, temp
data <- subset(wide_data, select = c(ascites, average_water, bird_density, feed, growth, average_out_temp, average_Tmax, average_Tmin))
data <- na.omit(data)
test <- gcm.test(X = data$ascites, Y = data$average_water, Z = data.frame(data$bird_density, as.integer(data$feed), data$growth, data$average_out_temp, data$average_Tmax, data$average_Tmin))
add_test_result("ascites _||_ water | bird density, feed, growth, outdoor temp, temp", test$p.value)

# 4. dnst _||_ feed
data <- subset(wide_data, select = c(bird_density, feed))
data <- na.omit(data)
null <- list()
for (i in 1:1000){
  null[[i]] <- NullGenerator(formula = bird_density ~ feed, data = data, subsample = 0.4, p = 0.80)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
NullDist <- do.call(rbind, null)
test_stat <- TestGenerator(formula = bird_density ~ feed, data = data, p = 0.80)
test <- get_pvalues(objective =  "reg:squarederror", NullDist = NullDist, test1_metric = test_stat[1], test2_metric = test_stat[2])
add_test_result("bird density _||_ feed", test[2])
# 5. dnst _||_ hum
test <- cor.test(x = wide_data$bird_density, y = wide_data$average_Hmax)
add_test_result("bird density _||_ humidity", test$p.value)

# 6. dnst _||_ mnth
data <- subset(wide_data, select = c(bird_density, frequent_month))
data <- na.omit(data)
null <- list()
for (i in 1:1000){
  null[[i]] <- NullGenerator(formula = bird_density ~ frequent_month, data = data, subsample = 0.4, p = 0.80)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
NullDist <- do.call(rbind, null)
test_stat <- TestGenerator(formula = bird_density ~ frequent_month, data = data, p = 0.80)
test <- get_pvalues(objective =  "reg:squarederror", NullDist = NullDist, test1_metric = test_stat[1], test2_metric = test_stat[2])
add_test_result("bird density _||_ month", test[2])

# 7. dnst _||_ ot_h
test <- cor.test(x = wide_data$bird_density, y = wide_data$average_out_hum)
add_test_result("bird density _||_ outdoor humidity", test$p.value)

# 8. dnst _||_ ot_t
test <- cor.test(x = wide_data$bird_density, y = wide_data$average_out_temp)
add_test_result("bird density _||_ outdoor temp", test$p.value)

# 9. dnst _||_ temp
test <- cor.test(x = wide_data$bird_density, y = wide_data$average_Tmax)
add_test_result("bird density _||_ temperature", test$p.value)

# 10. dnst _||_ watr
test <- cor.test(x = wide_data$bird_density, y = wide_data$average_water)
add_test_result("bird density _||_ water", test$p.value)

# 11. feed _||_ hum | ot_t
data <- subset(wide_data, select = c(feed, average_Hmax, average_out_temp))
data <- na.omit(data)
null <- list()
for (i in 1:1000){
  null[[i]] <- NullGenerator(formula = average_Hmax ~ feed + average_out_temp, data = data, subsample = 0.4, p = 0.80)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
NullDist <- do.call(rbind, null)
test_stat <- TestGenerator(formula = average_Hmax ~ feed + average_out_temp, data = data, p = 0.80)
test <- get_pvalues(objective =  "reg:squarederror", NullDist = NullDist, test1_metric = test_stat[1], test2_metric = test_stat[2])
add_test_result("feed _||_ humidity | outdoor temp", test[2])

# 12. feed _||_ temp | ot_t
data <- subset(wide_data, select = c(feed, average_Tmax, average_out_temp))
data <- na.omit(data)
null <- list()
for (i in 1:1000){
  null[[i]] <- NullGenerator(formula = average_Tmax ~ feed + average_out_temp, data = data, subsample = 0.4, p = 0.80)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
NullDist <- do.call(rbind, null)
test_stat <- TestGenerator(formula = average_Tmax ~ feed + average_out_temp, data = data, p = 0.80)
test <- get_pvalues(objective =  "reg:squarederror", NullDist = NullDist, test1_metric = test_stat[1], test2_metric = test_stat[2])
add_test_result("feed _||_ temperature | outdoor temp",  test[2])

# 13. feed _||_ watr | ot_t
data <- subset(wide_data, select = c(feed, average_water, average_out_temp))
data <- na.omit(data)
null <- list()
for (i in 1:1000){
  null[[i]] <- NullGenerator(formula = average_water ~ feed + average_out_temp, data = data, subsample = 0.4, p = 0.80)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
NullDist <- do.call(rbind, null)
test_stat <- TestGenerator(formula = average_water ~ feed + average_out_temp, data = data, p = 0.80)
test <- get_pvalues(objective =  "reg:squarederror", NullDist = NullDist, test1_metric = test_stat[1], test2_metric = test_stat[2])
add_test_result("feed _||_ water | outdoor temp",  test[2])

# 14. food _||_ hum | ot_t, temp
data <- subset(wide_data, select = c(average_food, average_Hmax, average_out_temp, average_Tmax, average_Tmin))
data <- na.omit(data)
test <- gcm.test(X = data$average_food, Y = data$average_Hmax, Z = data.frame(data$average_out_temp, data$average_Tmax, data$average_Tmin))
add_test_result("food _||_ humidity | outdoor temp, temperature", test$p.value)

# 15. food _||_ hum | ot_t, watr
data <- subset(wide_data, select = c(average_food, average_Hmax, average_out_temp, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$average_food, Y = data$average_Hmax, Z = data.frame(data$average_out_temp, data$average_water))
add_test_result("food _||_ humidity | outdoor temp, water", test$p.value)

# 16. food _||_ hum | feed, watr
data <- subset(wide_data, select = c(average_food, average_Hmax, feed, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$average_food, Y = data$average_Hmax, Z = data.frame(as.integer(data$feed), data$average_water))
add_test_result("food _||_ humidity | feed, water", test$p.value)

# 17. food _||_ mnth | feed, ot_t
data <- subset(wide_data, select = c(average_food, frequent_month, feed, average_out_temp))
data <- na.omit(data)
null <- list()
for (i in 1:1000){
  null[[i]] <- NullGenerator(formula = average_food ~ frequent_month + feed + average_out_temp, data = data, subsample = 0.4, p = 0.80)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
NullDist <- do.call(rbind, null)
test_stat <- TestGenerator(formula = average_food ~ frequent_month + feed + average_out_temp, data = data, p = 0.80)
test <- get_pvalues(objective =  "reg:squarederror", NullDist = NullDist, test1_metric = test_stat[1], test2_metric = test_stat[2])
add_test_result("food _||_ month | feed, outdoor temp", test[2])

# 18. food _||_ mnth | feed, watr
data <- subset(wide_data, select = c(average_food, frequent_month, feed, average_water))
data <- na.omit(data)
null <- list()
for (i in 1:1000){
  null[[i]] <- NullGenerator(formula = average_food ~ frequent_month + feed + average_water, data = data, subsample = 0.4, p = 0.80)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
NullDist <- do.call(rbind, null)
test_stat <- TestGenerator(formula = average_food ~ frequent_month + feed + average_water, data = data, p = 0.80)
test <- get_pvalues(objective =  "reg:squarederror", NullDist = NullDist, test1_metric = test_stat[1], test2_metric = test_stat[2])
add_test_result("food _||_ month | feed, water", test[2])

# 19. food _||_ ot_h | feed, ot_t
data <- subset(wide_data, select = c(average_food, average_out_hum, feed, average_out_temp))
data <- na.omit(data)
test <- gcm.test(X = data$average_food, Y = data$average_out_hum, Z = data.frame(as.integer(data$feed), data$average_out_temp))
add_test_result("food _||_ outdoor humidity | feed, outdoor temp", test$p.value)

# 20. food _||_ ot_h | feed, watr
data <- subset(wide_data, select = c(average_food, average_out_hum, feed, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$average_food, Y = data$average_out_hum, Z = data.frame(as.integer(data$feed), data$average_water))
add_test_result("food _||_ outdoor humidity | feed, water", test$p.value)

# 21. food _||_ ot_t | feed, watr
data <- subset(wide_data, select = c(average_food, average_out_temp, feed, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$average_food, Y = data$average_out_temp, Z = data.frame(as.integer(data$feed), data$average_water))
add_test_result("food _||_ outdoor temp | feed, water", test$p.value)

# 22. food _||_ temp | ot_t, watr
data <- subset(wide_data, select = c(average_food, average_Tmax, average_out_temp, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$average_food, Y = data$average_Tmax, Z = data.frame(data$average_out_temp, data$average_water))
add_test_result("food _||_ temperature | outdoor temp, water", test$p.value)

# 23. food _||_ temp | feed, watr
data <- subset(wide_data, select = c(average_food, average_Tmax, feed, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$average_food, Y = data$average_Tmax, Z = data.frame(as.integer(data$feed), data$average_water))
add_test_result("food _||_ temperature | feed, water", test$p.value)

# 24. grwt _||_ hum | ot_t, temp
data <- subset(wide_data, select = c(growth, average_Hmax, average_out_temp, average_Tmax, average_Tmin))
data <- na.omit(data)
test <- gcm.test(X = data$growth, Y = data$average_Hmax, Z = data.frame(data$average_out_temp, data$average_Tmax, data$average_Tmin))
add_test_result("growth _||_ humidity | outdoor temp, temperature", test$p.value)

# 25. grwt _||_ hum | ot_t, watr
data <- subset(wide_data, select = c(growth, average_Hmax, average_out_temp, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$growth, Y = data$average_Hmax, Z = data.frame(data$average_out_temp, data$average_water))
add_test_result("growth _||_ humidity | outdoor temp, water", test$p.value)

# 26. grwt _||_ hum | feed, watr
data <- subset(wide_data, select = c(growth, average_Hmax, feed, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$growth, Y = data$average_Hmax, Z = data.frame(as.integer(data$feed), data$average_water))
add_test_result("growth _||_ humidity | feed, water", test$p.value)

# 27. grwt _||_ mnth | feed, ot_t
data <- subset(wide_data, select = c(growth, frequent_month, feed, average_out_temp))
data <- na.omit(data)
null <- list()
for (i in 1:1000){
  null[[i]] <- NullGenerator(formula = growth ~ frequent_month + feed + average_out_temp, data = data, subsample = 0.4, p = 0.80)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
NullDist <- do.call(rbind, null)
test_stat <- TestGenerator(formula = growth ~ frequent_month + feed + average_out_temp, data = data, p = 0.80)
test <- get_pvalues(objective =  "reg:squarederror", NullDist = NullDist, test1_metric = test_stat[1], test2_metric = test_stat[2])
add_test_result("growth _||_ month | feed, outdoor temp", test[2])

# 28. grwt _||_ mnth | feed, watr
data <- subset(wide_data, select = c(growth, frequent_month, feed, average_water))
data <- na.omit(data)
null <- list()
for (i in 1:1000){
  null[[i]] <- NullGenerator(formula = growth ~ frequent_month + feed + average_water, data = data, subsample = 0.4, p = 0.80)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
NullDist <- do.call(rbind, null)
test_stat <- TestGenerator(formula = growth ~ frequent_month + feed + average_water, data = data, p = 0.80)
test <- get_pvalues(objective =  "reg:squarederror", NullDist = NullDist, test1_metric = test_stat[1], test2_metric = test_stat[2])
add_test_result("growth _||_ month | feed, water", test[2])

# 29. grwt _||_ ot_h | feed, ot_t
data <- subset(wide_data, select = c(growth, average_out_hum, feed, average_out_temp))
data <- na.omit(data)
test <- gcm.test(X = data$growth, Y = data$average_out_hum, Z = data.frame(as.integer(data$feed), data$average_out_temp))
add_test_result("growth _||_ outdoor humidity | feed, outdoor temp", test$p.value)

# 30. grwt _||_ ot_h | feed, watr
data <- subset(wide_data, select = c(growth, average_out_hum, feed, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$growth, Y = data$average_out_hum, Z = data.frame(as.integer(data$feed), data$average_water))
add_test_result("growth _||_ outdoor humidity | feed, water", test$p.value)

# 31. grwt _||_ ot_t | feed, watr
data <- subset(wide_data, select = c(growth, average_out_temp, feed, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$growth, Y = data$average_out_temp, Z = data.frame(as.integer(data$feed), data$average_water))
add_test_result("growth _||_ outdoor temp | feed, water", test$p.value)

# 32. grwt _||_ temp | ot_t, watr
data <- subset(wide_data, select = c(growth, average_Tmax, average_out_temp, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$growth, Y = data$average_Tmax, Z = data.frame(data$average_out_temp, data$average_water))
add_test_result("growth _||_ temperature | outdoor temp, water", test$p.value)

# 33. grwt _||_ temp | feed, watr
data <- subset(wide_data, select = c(growth, average_Tmax, feed, average_water))
data <- na.omit(data)
test <- gcm.test(X = data$growth, Y = data$average_Tmax, Z = data.frame(as.integer(data$feed), data$average_water))
add_test_result("growth _||_ temperature | feed, water", test$p.value)

# 34. hum _||_ mnth | ot_t
data <- subset(wide_data, select = c(average_Hmax, frequent_month, average_out_temp))
data <- na.omit(data)
test <- gcm.test(X = data$average_Hmax, Y = data$frequent_month, Z = data.frame(data$average_out_temp))
add_test_result("humidity _||_ month | outdoor temp", test$p.value)

# 35. hum _||_ ot_h | ot_t
data <- subset(wide_data, select = c(average_Hmax, average_out_hum, average_out_temp))
data <- na.omit(data)
test <- gcm.test(X = data$average_Hmax, Y = data$average_out_hum, Z = data.frame(data$average_out_temp))
add_test_result("humidity _||_ outdoor humidity | outdoor temp", test$p.value)

# 36. hum _||_ watr | ot_t, temp
data <- subset(wide_data, select = c(average_Hmax, average_water, average_out_temp, average_Tmax, average_Tmin))
data <- na.omit(data)
test <- gcm.test(X = data$average_Hmax, Y = data$average_water, Z = data.frame(data$average_out_temp, data$average_Tmax, data$average_Tmin))
add_test_result("humidity _||_ water | outdoor temp, temp", test$p.value)

# 37. mnth _||_ temp | ot_t
data <- subset(wide_data, select = c(frequent_month, average_Tmax, average_out_temp))
data <- na.omit(data)
null <- list()
for (i in 1:1000){
  null[[i]] <- NullGenerator(formula = average_Tmax ~ frequent_month + average_out_temp, data = data, subsample = 0.4, p = 0.80)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
NullDist <- do.call(rbind, null)
test_stat <- TestGenerator(formula =  average_Tmax ~ frequent_month + average_out_temp, data = data, p = 0.80)
test <- get_pvalues(objective =  "reg:squarederror", NullDist = NullDist, test1_metric = test_stat[1], test2_metric = test_stat[2])
add_test_result("month _||_ temperature | outdoor temp", test[2])

# 38. mnth _||_ watr | ot_t
data <- subset(wide_data, select = c(frequent_month, average_water, average_out_temp))
data <- na.omit(data)
null <- list()
for (i in 1:1000){
  null[[i]] <- NullGenerator(formula = average_water ~ frequent_month + average_out_temp, data = data, subsample = 0.4, p = 0.80)
  cat(sprintf("Sample: %d\r", i))
  flush.console()
}
NullDist <- do.call(rbind, null)
test_stat <- TestGenerator(formula =  average_water ~ frequent_month + average_out_temp, data = data, p = 0.80)
test <- get_pvalues(objective =  "reg:squarederror", NullDist = NullDist, test1_metric = test_stat[1], test2_metric = test_stat[2])
add_test_result("month _||_ water | outdoor temp", test[2])

# 39. ot_h _||_ temp | ot_t
data <- subset(wide_data, select = c(average_out_hum, average_Tmax, average_out_temp))
data <- na.omit(data)
test <- gcm.test(X = data$average_out_hum, Y = data$average_Tmax, Z = data.frame(data$average_out_temp))
add_test_result("outdoor humidity _||_ temperature | outdoor temp", test$p.value)

# 40. ot_h _||_ watr | ot_t
data <- subset(wide_data, select = c(average_out_hum, average_water, average_out_temp))
data <- na.omit(data)
test <- gcm.test(X = data$average_out_hum, Y = data$average_water, Z = data.frame(data$average_out_temp))
add_test_result("outdoor humidity _||_ water | outdoor temp", test$p.value)

# Output the test results
print(test_results)


update_dag <- dagitty('dag {
  out_hum [label="Outdoor humidity"]
  out_tem [label="Outdoor temp"]
  month [label="Month of year"]
  geo [latent, label="Geography"]
  feed [exposure, label="Feed type"]
  food [label="Food cons"]
  hum [label="Humidity"]
  density [label="Bird density"]
  water [label="Water cons"]
  temp [label="Temp."]
  co2 [latent, label="CO2-level"]
  growth [label="Growth"]
  ascites [outcome, label="Ascites"]
  
  growth -> ascites
  out_hum -> ascites
  out_tem -> ascites
  out_tem -> co2
  out_tem -> temp
  out_tem -> hum
  out_tem -> out_hum
  out_tem -> water
  temp -> ascites
  temp -> hum
  temp -> water
  hum -> ascites
  water -> food
  water -> growth
  month -> ascites
  month -> out_hum
  month -> out_tem
  month -> feed
  geo -> out_tem
  geo -> out_hum
  geo -> feed
  feed -> growth
  feed -> ascites
  feed -> food
  co2 -> ascites
  density -> ascites
  density -> growth
  density -> food
  water -> growth
  food -> growth
  month -> hum
  out_hum -> growth
  hum -> growth
  hum -> water
}')
adjustmentSets(update_dag)
adjustmentSets(update_dag, effect = "direct")

