library("tidyverse")

source("util.R")
main_model <- readRDS("cache/main_model_ext.rds")

summary(main_model)

rand_pars_confints <-confint(main_model, level = 0.99, parm = "theta_", method="profile") %>%
  cache("cache/main_model_confint.rds", use_cache = TRUE)

print(rand_pars_confints)
