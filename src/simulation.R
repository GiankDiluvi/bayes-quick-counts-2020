# Quick counts simulation

t0 <- Sys.time() # save time
### PREAMBLE ####
library(tidyverse)
library(truncnorm)
library(rstan)

source('quick-counts-functions.R')

### DEFINE PARAMETERS
db_names <- c("../data/remesas/REMESAS0100011900.txt",
              #"../data/remesas/REMESAS0100011905.txt",
              #"../data/remesas/REMESAS0100011910.txt",
              "../data/remesas/REMESAS0100011915.txt",
              #"../data/remesas/REMESAS0100011920.txt",
              #"../data/remesas/REMESAS0100011925.txt",
              # "../data/remesas/REMESAS0100011930.txt",
              # "../data/remesas/REMESAS0100011945.txt",
              # "../data/remesas/REMESAS0100012000.txt",
              # "../data/remesas/REMESAS0100012015.txt",
              # "../data/remesas/REMESAS0100012030.txt",
              # "../data/remesas/REMESAS0100012045.txt",
              # "../data/remesas/REMESAS0100012100.txt",
              # "../data/remesas/REMESAS0100012115.txt",
              # "../data/remesas/REMESAS0100012130.txt",
              # "../data/remesas/REMESAS0100012145.txt",
              # "../data/remesas/REMESAS0100012200.txt",
              # "../data/remesas/REMESAS0100012215.txt",
              # "../data/remesas/REMESAS0100012230.txt"
              )
models <- c('original', 'original_mcmc', 
            'new', 
            'new_exp')
reps <- 1
save_file <- '../data/results/'

# compile stan models
bayes_model_original <- rstan::stan_model(model_code = model_stan_original)
bayes_model_new <- rstan::stan_model(model_code = model_stan_new)
bayes_model_new_exp <- rstan::stan_model(model_code = model_stan_new_exp)

# individual sims parameters
original_params = list(R = 10000, warmup = 0, verbose = 0, bayes_model = NULL)
original_mcmc_params = list(R = 1000, warmup = 250, verbose = 0, bayes_model = bayes_model_original)
new_params = list(R = 500, warmup = 250, verbose = 0, bayes_model = bayes_model_new)
new_exp_params = list(R = 1000, warmup = 250, verbose = 0, bayes_model = bayes_model_new_exp)

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
    if(model == 'new_exp') params <- new_exp_params
    
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


# save time
readr::write_csv(times, path = paste0(save_file, 'times.csv'))

deltat <- difftime(Sys.time(), t0, units = "secs")[[1]]
print(paste0('simulation time: ', deltat))