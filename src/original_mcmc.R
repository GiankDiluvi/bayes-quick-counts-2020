### NEW BAYESIAN MODEL


### PREAMBLE ####
library(tidyverse)
library(truncnorm)
library(ggplot2)
ggplot2::theme_set(theme_classic)
library(rstan)

### FUNCTIONS ####
db_name <- "../data/remesas/REMESAS0100012330.txt"

# Stan model for sampling from the posterior at each stratum
model_stan <- "
  data {
    int<lower=0> c;          // number of polling stations 
    vector[c] rac;
    vector[c] jamk;
    vector[c] amlo;
    vector[c] jhrc;
    vector[c] otros;
    //vector[c] ne;
    vector[c] nl;            // nominal list
  }
  parameters {
    //vector<lower=0, upper=1>[6] theta;
    //vector<lower=0>[6] tau;      // prior scale
    
    vector<lower=0, upper=1>[5] theta;
    vector<lower=0>[5] tau;      // prior scale
  }
  model {
    // priors
    for (j in 1:4){
    tau[j] ~ gamma(0.01, 0.01);
    theta[j] ~ beta(0.1, 0.5195787);
    }
    
    tau[5] ~ gamma(0.01, 0.01);
    theta[5] ~ beta(0.1, 4.9);
    
    //tau[6] ~ gamma(0.01, 0.01);
    //theta[6] ~ beta(0.1, 0.1990431);
    
    
    // model
    for (k in 1:c){
    rac[k] ~ normal(nl[k] * theta[1], sqrt(tau[1] / nl[k]));
    jamk[k] ~ normal(nl[k] * theta[2], sqrt(tau[2] / nl[k]));
    amlo[k] ~ normal(nl[k] * theta[3], sqrt(tau[3] / nl[k]));
    jhrc[k] ~ normal(nl[k] * theta[4], sqrt(tau[4] / nl[k]));
    otros[k] ~ normal(nl[k] * theta[5], sqrt(tau[5] / nl[k]));
    //ne[k] ~ normal(nl[k] * theta[6], sqrt(tau[6] / nl[k]));
    }
  }
  "


model_stan <- "
  data {
    int<lower=0> c;          // number of polling stations 
    vector[c] rac;
    vector[c] jamk;
    vector[c] amlo;
    vector[c] jhrc;
    vector[c] otros;
    //vector[c] ne;
    vector[c] nl;            // nominal list
  }
  parameters {
    //vector<lower=0, upper=1>[6] theta;
    //vector<lower=0>[6] tau;      // prior scale
    
    vector<lower=0, upper=1>[5] theta;
    vector<lower=0>[5] tau;      // prior scale
  }
  model {
    // priors
    for (j in 1:4){
    tau[j] ~ gamma(0.01, 0.01);
    theta[j] ~ beta(0.1, 0.5195787);
    }
    
    tau[5] ~ gamma(0.01, 0.01);
    theta[5] ~ beta(0.1, 4.9);
    
    //tau[6] ~ gamma(0.01, 0.01);
    //theta[6] ~ beta(0.1, 0.1990431);
    
    
    // model
    rac ~ multi_normal_prec(nl * theta[1], diag_matrix(nl / tau[1]));
    jamk ~ multi_normal_prec(nl * theta[2], diag_matrix(nl / tau[2]));
    amlo ~ multi_normal_prec(nl * theta[3], diag_matrix(nl / tau[3]));
    jhrc ~ multi_normal_prec(nl * theta[4], diag_matrix(nl / tau[4]));
    otros ~ multi_normal_prec(nl * theta[5], diag_matrix(nl / tau[5]));
    
  
  }
  "

bayes_model <- rstan::stan_model(model_code = model_stan)











#real rac[c];             // votes for rac
#real jamk[c];            // votes for jamk
#real amlo[c];            // votes for amlo
#real jhrc[c];            // votes for jhrc
#real otros[c];           // votes for otros
#real ne[c];              // votes for ne


bayes_fit_stratum <- function(db, R = 1000, warmup = 250){
  # generate R samples from posterior or prior depending on number of polling stations with information
  # db is a data base with votes from all candidates in a give stratum
  # stratum is used to print out stratum number
  #
  # returns a tibble with R samples from the posterior of theta, or from the prior if c < 2
  
  print(paste0('stratum: ', as.character(unique(db$stratum))))
  
  # calculate stratum size
  c <- nrow(db %>% dplyr::filter(TOTAL != 0))
  
  # if only zero or one polling station, sample from prior
  if(c < 2){
    print('simulating from prior')
    # if only 1 or no rows have info, sample from prior
    out <- tibble(RAC = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  JAMK = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  AMLO = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  JHRC = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  OTROS = rbeta(R, shape1 = 0.1, shape2 = 4.9))
  }else{
    print('simulating from posterior')
    # get only rows with information
    tmp_db <- db %>% 
      dplyr::filter(TOTAL != 0)
    
    votes <- tmp_db %>%
      dplyr::select(RAC, JAMK, AMLO, JHRC, OTROS, NE) %>% 
      as.matrix() %>% 
      unname()
    
    # sample from posterior
    stan_data <- list(
      c = nrow(tmp_db),
      rac = votes[, 1],
      jamk = votes[, 2],
      amlo = votes[, 3],
      jhrc = votes[, 4],
      otros = votes[, 5],
      nl = tmp_db %>% pull(LISTA_NOMINAL)
    )
    
    result <- rstan::sampling(bayes_model, data = stan_data, 
                              chains = 1, iter = R + warmup, warmup = warmup, cores = 8,
                              verbose = FALSE, show_messages = FALSE, refresh=0)
    
    out <- as_tibble(as.data.frame(result)) %>% 
      dplyr::select(`theta[1]`:`theta[5]`) %>% 
      dplyr::rename(RAC = `theta[1]`,
                    JAMK = `theta[2]`,
                    AMLO = `theta[3]`,
                    JHRC = `theta[4]`,
                    OTROS = `theta[5]`)
  }
  #View(out)
  #print('done')
  #print(paste0('amlo mean: ', mean(out$AMLO  / (out$RAC + out$JAMK + out$AMLO + out$JHRC + out$OTROS))))
  return(out)
}


bayes_fit_new <- function(db_name, R = 1000, warmup = 250){
  # generate R samples from posterior using original bayesian model
  # db_name is a string with the name of the data base with current results
  #
  # returns a tibble with R rows, each a sample from lambda vector and voter turnout
  
  # read data base with current results
  db <- read_delim(db_name, delim = "|", skip = 1) %>% 
    dplyr::mutate(OTROS = CNR + NULOS,
                  ID = paste0(as.character(iD_ESTADO), as.character(ID_DISTRITO_FEDERAL), as.character(SECCION),
                              TIPO_CASILLA, as.character(ID_CASILLA), as.character(EXT_CONTIGUA), as.character(TIPO_SECCION))) %>% 
    dplyr::select(ID, RAC, JAMK, AMLO, JHRC, OTROS)
  
  
  # build full data base with current results + 0 in missing spots
  full <- read_csv("../data/4-ConteoRapido18MUESTRA-ELECCION-PRESIDENCIAL.csv",
                   col_types = "cdddcdddddddddcddd") %>% 
    dplyr::mutate(ID = paste0(as.character(iD_ESTADO), as.character(ID_DISTRITO_FEDERAL), as.character(SECCION),
                              TIPO_CASILLA, as.character(ID_CASILLA), as.character(EXT_CONTIGUA), as.character(TIPO_SECCION))) %>% 
    dplyr::left_join(db) %>% 
    dplyr::select(ID, ID_ESTRATO_F, TIPO_CASILLA, LISTA_NOMINAL, RAC, JAMK, AMLO, JHRC, OTROS) %>% 
    dplyr::mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)),
                  LISTA_NOMINAL = ifelse(LISTA_NOMINAL == 0, 750, LISTA_NOMINAL),
                  TOTAL = RAC + JAMK + AMLO + JHRC + OTROS,
                  NE = LISTA_NOMINAL - TOTAL)
  
  
  # modify nominal list for special polling stations
  special <- nrow(full %>% dplyr::filter(TIPO_CASILLA == "S"))
  no_special <- nrow(full) - special
  full <- full %>% 
    dplyr::mutate(LISTA_NOMINAL = ifelse(TIPO_CASILLA == "S", LISTA_NOMINAL, LISTA_NOMINAL - 750 * special / no_special))
  
  
  # read full sample nominal list
  nominal_list <- read_csv("../data/4-ConteoRapido18MUESTRA-ELECCION-PRESIDENCIAL.csv") %>% 
    dplyr::select(ID_ESTRATO_F, LISTA_NOMINAL) %>% 
    dplyr::arrange(ID_ESTRATO_F) %>% 
    dplyr::group_by(ID_ESTRATO_F) %>% 
    dplyr::summarise(LISTA_NOMINAL = sum(LISTA_NOMINAL)) %>% 
    dplyr::mutate(weight = LISTA_NOMINAL / sum(LISTA_NOMINAL))
  
  # get total nominal list size
  N <- sum(nominal_list$LISTA_NOMINAL) # total nominal list
  
  # fit model and compute estimates
  out <- full %>% 
    dplyr::arrange(ID_ESTRATO_F) %>% 
    dplyr::mutate(stratum = ID_ESTRATO_F) %>% 
    dplyr::group_by(ID_ESTRATO_F) %>% 
    dplyr::group_modify(~ bayes_fit_stratum(.x, R = 1000, warmup = 250)) %>% # fit bayesian model to each stratum
    dplyr::mutate(id = 1:n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(nominal_list) %>% 
    dplyr::group_by(id) %>% 
    # now compute national theta estimates
    dplyr::summarise(RAC = weighted.mean(RAC, weight, na.rm = TRUE),
                     JAMK = weighted.mean(JAMK, weight, na.rm = TRUE),
                     AMLO = weighted.mean(AMLO, weight, na.rm = TRUE),
                     JHRC = weighted.mean(JHRC, weight, na.rm = TRUE),
                     OTROS = weighted.mean(OTROS, weight, na.rm = TRUE)) %>% 
    # compute voter turnout and lambdas
    dplyr::mutate(PART = RAC + JAMK + AMLO + JHRC + OTROS,
                  RAC = RAC / PART,
                  JAMK = JAMK / PART,
                  AMLO = AMLO / PART,
                  JHRC = JHRC / PART,
                  OTROS = OTROS / PART) %>% 
    dplyr::select(RAC, JAMK, AMLO, JHRC, OTROS, PART)
  
  return(out)
}


### RUN ####
results <- bayes_fit_new("../data/remesas/REMESAS0100012230.txt", R = 1000, warmup = 250)
results %>%
  dplyr::summarise(across(where(is.numeric), mean))
