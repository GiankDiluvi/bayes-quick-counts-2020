# Quick counts simulation


### PREAMBLE ####
library(tidyverse)
library(truncnorm)
library(ggplot2)
ggplot2::theme_set(theme_classic)
library(rstan)

source('quick-counts-functions.R')

### DEFINE PARAMETERS
db_names <- c("../data/remesas/REMESAS0100012230.txt")
models <- c('original', 'original_mcmc', 'new')
reps <- 1
save_file <- '../data/results/'

# compile stan models
bayes_model_original <- rstan::stan_model(model_code = model_stan_original)
bayes_model_new <- rstan::stan_model(model_code = model_stan_new)

# individual sims parameters
original_params = list(R = 10000, warmup = 0, verbose = 0, bayes_model = NULL)
original_mcmc_params = list(R = 500, warmup = 250, verbose = 0, bayes_model = bayes_model_original)
new_params = list(R = 250, warmup = 250, verbose = 0, bayes_model = bayes_model_new)

# SIMULATION ####


times <- tibble(model = character(),
                db_time = character(),
                time_s = double())

for(db_name in db_names){
  
  date <- stringr::str_sub(db_name, 30, 33)
  print(paste0('date: ', date))
  
  for(model in models){
    
    print(paste0('model: ', model))
    
    # set parameters
    if(model == 'original') params <- original_params
    if(model == 'original_mcmc') params <- original_mcmc_params
    if(model == 'new') params <- new_params
    
    for(i in reps){
      
      print(paste0('simulation number: ', i))
      
      # init clock
      start_time <- Sys.time()
      
      # fit model to data
      results <- bayes_fit(db_name, R = params$R, warmup = params$warmup, model = model, verbose = params$verbose,
                           bayes_model = params$bayes_model)
      
      # measure time
      time <- difftime(Sys.time(), start_time, units = "secs")[[1]]
      #time <- Sys.time() - start_time
      print(paste0('time transpired: ', round(time / 60, 2), ' minutes'))
      
      times <- times %>% 
        dplyr::bind_rows(tibble(model = model, db_time = date, time_s = as.double(time)))
      
      # save results
      readr::write_csv(results, path = paste0(save_file, model, '_', date, '.csv'))
      
    }
  }
}
