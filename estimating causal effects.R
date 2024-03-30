devtools::install_github("junyzhou10/MetaLearners")
rm(list = ls())
library(tidyverse)
setwd("C:/Causal-Workflow/")
load("data/analysis_wide_data.Rda")
wide_data$feed_name <- as.factor(wide_data$feed_name)
wide_data$id_slaughterhouse <- as.factor(wide_data$id_slaughterhouse)
wide_data$LeverandoerNr <- as.factor(wide_data$LeverandoerNr)
wide_data$frequent_month <- as.factor(wide_data$frequent_month)
wide_data$ascites_prev <- 1000*wide_data$aceties/wide_data$n_of_chicken

levels = 5  # Set the number of levels other than other
wide_data$feed_group = fct_lump_n(wide_data$feed_name, n = levels, other_level = "Other")
table(wide_data$feed_group)


MetaLearners()
