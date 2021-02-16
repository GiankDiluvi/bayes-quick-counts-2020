### 2021 Mexico Quick counts functions

### PREAMBLE ####
library(tidyverse)
library(truncnorm)
library(mvtnorm)
library(rstan)


### FUNCTIONS ####



# MAIN FUNCTION
bayes_fit <- function(db_name, R = 1000, warmup = 250, model = 'original', verbose = 0, bayes_model = NULL){
  # generate R samples from posterior
  # db_name is a string with the name of the data base with current results
  # R, warmup are integers indicating final number of samples desired and warmup for mcmc, resp.
  # model is a string with either original, original_mcmc, or new, specifying the model
  # verbose is an integer (either 0, 1, or 2) indicating how much text to print
  # bayes model optionally contains a compiled stan model
  #
  # returns a tibble with R rows, each a sample from lambda vector and voter turnout
  
  # print info
  model_name <- ifelse(model == 'original', 'original model without MCMC',
                       ifelse(model == 'original_mcmc', 'original model with MCMC',
                              'new model'))
  if(verbose > 0) print(paste0('generating samples from ', model_name))
  
  # compile stan model
  if(!is.null(bayes_model)){
    if(verbose > 0) print('compiling stan model')
    if(model == 'original_mcmc') bayes_model <- rstan::stan_model(model_code = model_stan_original)
    if(model == 'new') bayes_model <- rstan::stan_model(model_code = model_stan_new)
  }
  
  if(verbose > 0) print('reading data bases')
  # read data base with current results
  db <- read_delim(db_name, delim = "|", skip = 1, col_types = cols()) %>% 
    dplyr::mutate(OTROS = CNR + NULOS,
                  ID = paste0(as.character(iD_ESTADO), as.character(ID_DISTRITO_FEDERAL), as.character(SECCION),
                              TIPO_CASILLA, as.character(ID_CASILLA), as.character(EXT_CONTIGUA), as.character(TIPO_SECCION))) %>% 
    dplyr::select(ID, RAC, JAMK, AMLO, JHRC, OTROS)
  
  
  # build full data base with current results + 0 in missing spots
  full <- read_csv("../data/4-ConteoRapido18MUESTRA-ELECCION-PRESIDENCIAL.csv",
                   col_types = "cdddcdddddddddcddd") %>% 
    dplyr::mutate(ID = paste0(as.character(iD_ESTADO), as.character(ID_DISTRITO_FEDERAL), as.character(SECCION),
                              TIPO_CASILLA, as.character(ID_CASILLA), as.character(EXT_CONTIGUA), as.character(TIPO_SECCION))) %>% 
    dplyr::left_join(db, by = c("ID" = "ID")) %>% 
    dplyr::select(ID, ID_ESTRATO_F, TIPO_CASILLA, LISTA_NOMINAL, RAC, JAMK, AMLO, JHRC, OTROS) %>% 
    dplyr::mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)),
                  LISTA_NOMINAL = ifelse(LISTA_NOMINAL == 0, 750, LISTA_NOMINAL),
                  TOTAL = RAC + JAMK + AMLO + JHRC + OTROS,
                  NE = LISTA_NOMINAL - TOTAL)
  
  
  # modify nominal list for special polling stations
  if(verbose > 0) print('modifying nominal list')
  special <- nrow(full %>% dplyr::filter(TIPO_CASILLA == "S"))
  no_special <- nrow(full) - special
  full <- full %>% 
    dplyr::mutate(LISTA_NOMINAL = ifelse(TIPO_CASILLA == "S", LISTA_NOMINAL, LISTA_NOMINAL - 750 * special / no_special))
  
  
  # read full sample nominal list
  nominal_list <- read_csv("../data/4-ConteoRapido18MUESTRA-ELECCION-PRESIDENCIAL.csv", col_types = cols()) %>% 
    dplyr::select(ID_ESTRATO_F, LISTA_NOMINAL) %>% 
    dplyr::arrange(ID_ESTRATO_F) %>% 
    dplyr::group_by(ID_ESTRATO_F) %>% 
    dplyr::summarise(LISTA_NOMINAL = sum(LISTA_NOMINAL)) %>% 
    dplyr::mutate(weight = LISTA_NOMINAL / sum(LISTA_NOMINAL))
  
  # get total nominal list size
  N <- sum(nominal_list$LISTA_NOMINAL) # total nominal list
  
  # fit model and compute estimates
  if(verbose > 0) print('init estimations')
  out <- full %>% 
    dplyr::arrange(ID_ESTRATO_F) %>% 
    dplyr::mutate(stratum = ID_ESTRATO_F) %>% 
    dplyr::group_by(ID_ESTRATO_F) %>% 
    dplyr::group_modify(~ bayes_fit_stratum(.x, R = R, warmup = warmup, model = model,
                                            bayes_model = bayes_model, verbose = verbose)) %>% # fit bayesian model to each stratum
    dplyr::mutate(id = 1:n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(nominal_list, by = c("ID_ESTRATO_F" = "ID_ESTRATO_F")) %>% 
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



# SECONDARY FUNCTION
bayes_fit_stratum <- function(db, R = 1000, warmup = 250, model = 'original', bayes_model, verbose = 0){
  # generate R samples from posterior or prior depending on number of polling stations with information
  # db is a data base with votes from all candidates in a give stratum
  # model determines which model to use: original, original with mcmc, or new model
  # bayes_model is a compiled stan model for mcmc sampling
  # verbose controls how many messages are printed out
  #
  # returns a tibble with R samples from the posterior of theta, or from the prior if c < 2
  
  
  if(model == 'original') out <- bayes_fit_stratum_original(db, R, verbose)
  
  if(model == 'original_mcmc') out <- bayes_fit_stratum_original_mcmc(db, bayes_model, R, warmup, verbose)
  
  if(model == 'new') out <- bayes_fit_stratum_new(db, bayes_model, R, warmup, verbose)
  
  if(model == 'new_exp') out <- bayes_fit_stratum_new(db, bayes_model, R, warmup, verbose)
  
  return(out)
  
}



# TERTIARY FUNCTION
rposterior <- function(R, votes, nl){
  # generate R samples from the original posterior in a given stratum
  # votes is a vector with votes for candidate in each polling station of the stratum
  # nl is a vector with nominal lists of each polling station of the stratum
  #
  # returns a vector of size R with samples from posterior
  
  
  c <- length(votes) # no. of polling stations
  N <- sum(nl)       # nominal list in stratum
  
  # generate tau ~ gamma and return theta ~ Normal | tau  
  tau <- rgamma(R, 0.5 * (c - 1), 0.5 * (sum(votes^2 / nl) - (sum(votes))^2 / N))
  return(rtruncnorm(R, a = 0, b = 1, mean = sum(votes) / N, sd =  1 / sqrt(tau * N)))
}


# TERTIARY FUNCTION
bayes_fit_stratum_original <- function(db, R = 10000, verbose = 0){
  # generate R samples from original posterior or modified prior
  # depending on number of polling stations with information
  # db is a data base with votes from all candidates in a give stratum
  # verbose controls how many messages are printed out
  #
  # returns a tibble with R samples from the posterior of theta, or from the prior if c < 2
  
  if(verbose > 0)  print(paste0('stratum: ', as.character(unique(db$stratum))))
  
  # calculate stratum size
  c <- nrow(db %>% dplyr::filter(TOTAL != 0))
  
  # if only zero or one polling station, sample from prior
  if(c < 2){
    if(verbose > 0)  print('simulating from modified prior')
    # if only 1 or no rows have info, sample from prior
    out <- tibble(RAC = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  JAMK = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  AMLO = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  JHRC = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  OTROS = rbeta(R, shape1 = 0.1, shape2 = 4.9),
                  NE = rbeta(R, shape1 = 0.1, shape2 = 0.1*((1/0.3344) - 1)))
  }else{
    if(verbose > 0) print('simulating from original posterior')
    # get only rows with information
    tmp_db <- db %>% 
      dplyr::filter(TOTAL != 0)
    
    # sample from posterior
    out <- tibble(RAC = rposterior(R, tmp_db$RAC, tmp_db$LISTA_NOMINAL),
                  JAMK = rposterior(R, tmp_db$JAMK, tmp_db$LISTA_NOMINAL),
                  AMLO = rposterior(R, tmp_db$AMLO, tmp_db$LISTA_NOMINAL),
                  JHRC = rposterior(R, tmp_db$JHRC, tmp_db$LISTA_NOMINAL),
                  OTROS = rposterior(R, tmp_db$OTROS, tmp_db$LISTA_NOMINAL),
                  NE = rposterior(R, tmp_db$NE, tmp_db$LISTA_NOMINAL))
  }
  #View(out)
  return(out)
}


# TERTIARY FUNCTION
bayes_fit_stratum_original_mcmc <- function(db, bayes_model, R = 1000, warmup = 250, verbose = 0){
  # generate R samples from posterior or prior depending on number of polling stations with information
  # bayes_model is a compiled stan model
  # db is a data base with votes from all candidates in a give stratum
  # verbose controls how many messages are printed out
  #
  # returns a tibble with R samples from the posterior of theta, or from the prior if c < 2
  
  if(verbose > 0)  print(paste0('stratum: ', as.character(unique(db$stratum))))
  
  # calculate stratum size
  c <- nrow(db %>% dplyr::filter(TOTAL != 0))
  
  # if only zero or one polling station, sample from prior
  if(c < 2){
    if(verbose > 0)  print('simulating from prior')
    # if only 1 or no rows have info, sample from prior
    out <- tibble(RAC = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  JAMK = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  AMLO = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  JHRC = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  OTROS = rbeta(R, shape1 = 0.1, shape2 = 4.9))
  }else{
    if(verbose > 0) print('simulating from posterior')
    # get only rows with information
    tmp_db <- db %>% 
      dplyr::filter(TOTAL != 0)
    
    # get votes matrix
    votes <- tmp_db %>%
      dplyr::select(RAC, JAMK, AMLO, JHRC, OTROS, NE) %>% 
      as.matrix() %>% 
      unname()
    
    # build stan data
    stan_data <- list(
      c = nrow(tmp_db),
      rac = votes[, 1],
      jamk = votes[, 2],
      amlo = votes[, 3],
      jhrc = votes[, 4],
      otros = votes[, 5],
      nl = tmp_db %>% pull(LISTA_NOMINAL)
    )
    
    # sample from posterior
    result <- rstan::sampling(bayes_model, data = stan_data, 
                              chains = 1, iter = R + warmup, warmup = warmup, cores = 8,
                              verbose = FALSE, show_messages = FALSE, refresh=ifelse(verbose > 1, 1, 0))
    
    # retrieve samples from proportions
    out <- as_tibble(as.data.frame(result)) %>% 
      dplyr::select(`theta[1]`:`theta[5]`) %>% 
      dplyr::rename(RAC = `theta[1]`,
                    JAMK = `theta[2]`,
                    AMLO = `theta[3]`,
                    JHRC = `theta[4]`,
                    OTROS = `theta[5]`)
  }
  
  return(out)
}

# TERTIARY FUNCTION
bayes_fit_stratum_new <- function(db, bayes_model, R = 1000, warmup = 250, verbose = 0){
  # generate R samples from posterior or prior depending on number of polling stations with information
  # bayes_model is a compiled stan model
  # db is a data base with votes from all candidates in a give stratum
  # verbose controls how many messages are printed out
  #
  # returns a tibble with R samples from the posterior of theta, or from the prior if c < 2
  
  if(verbose > 0) print(paste0('stratum: ', as.character(unique(db$stratum))))
  
  # calculate stratum size
  c <- nrow(db %>% dplyr::filter(TOTAL != 0))
  
  # if only zero or one polling station, sample from prior
  if(c < 2){
    if(verbose > 0) print('simulating from prior')
    # if only 1 or no rows have info, sample from prior
    out <- rprior_new(R)
    
  }else{
    if(verbose > 0) print('simulating from posterior')
    # get only rows with information
    tmp_db <- db %>% 
      dplyr::filter(TOTAL != 0)
    
    # build stan data
    stan_data <- list(
      c = nrow(tmp_db),
      votes = tmp_db %>% dplyr::select(RAC, JAMK, AMLO, JHRC, OTROS, NE) %>% as.matrix() %>% unname(),
      nl = tmp_db %>% pull(LISTA_NOMINAL)
    )
    
    # sample from posterior
    result <- rstan::sampling(bayes_model, data = stan_data, 
                              chains = 1, iter = R + warmup, warmup = warmup, cores = 1, 
                              verbose = FALSE, show_messages = FALSE, refresh = 0)
    
    
    # retrieve samples from proportions
    out <- as_tibble(as.data.frame(result)) %>% 
      dplyr::select(`theta[1]`:`theta[6]`) %>% 
      dplyr::rename(RAC = `theta[1]`,
                    JAMK = `theta[2]`,
                    AMLO = `theta[3]`,
                    JHRC = `theta[4]`,
                    OTROS = `theta[5]`,
                    NE = `theta[6]`)
    
    #print(mean(rowSums(out[, 1:5])))
    
  }
  
  
  return(out)
}


# TERTIARY FUNCTION
rprior_new <- function(R){
  # generate R samples from the new prior distribution
  
  mu1 <- log((0.6456/18)/(1-0.6456))
  mu2 <- log((0.02 / 15)/(1-0.6456))
  y <- mvtnorm::rmvnorm(100000, mean = c(mu1, mu1, mu1, mu1, mu2), sigma = diag(rep(10, 5)))
  
  sp1 <- rowSums(exp(y)) + 1
  theta <- exp(y) / sp1
  theta <- cbind(theta, 1/sp1)
  #print(mean(rowSums(theta[, 1:5])))
  colnames(theta) <- c('RAC', 'JAMK', 'AMLO', 'JHRC', 'OTROS', 'NE')
  
  return(as_tibble(theta))
  
  
}


### STAN MODELS ####


# original model with MCMC
model_stan_original <- "
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


# new model
model_stan_new <- "
  data {
    int<lower=0> c;          // number of polling stations 
    matrix[c, 6] votes;      // votes matrix
    vector[c] nl;            // nominal list
  }
  parameters {
    vector[5] y;
    corr_matrix[6] T;            // prior correlation
    vector<lower=0>[6] tau;      // prior scale
  }
  transformed parameters {
    simplex[6] theta;             // init theta param
    real Z;
    
    Z = 1 + sum(exp(y));         // normalizing constant
    for (j in 1:5){
    theta[j] = exp(y[j]) / Z;    // inv alr
    }
    theta[6] = 1 / Z;            // remaining theta
     
    
  }
  model {
    // settings for prior of theta
    real mu1;
    real mu2;
    vector[5] mu;
    vector[5] diag;  // diagonal of covariance matrix
    matrix[5, 5] sigma; // covariance matrix
    
    // now assign values
    mu1 = log((0.6456/18)/(1-0.6456));
    mu2 = log((0.02 / 15)/(1-0.6456));
    mu = [ mu1, mu1, mu1, mu1, mu2 ]';
    diag = [10, 10, 10, 10, 10]';
    sigma = diag_matrix(diag);
    
    // priors
    y ~ multi_normal(mu, sigma);
    tau ~ cauchy(0, 2.5);
    T ~ lkj_corr(1.0);
    
    
    // model
    for (k in 1:c){
    votes[k, 1:6] ~ multi_normal(nl[k] * theta, (1/sqrt(nl[k])) * quad_form_diag(T, tau));    // each polling station is normal
    }
  }
  "


# new model
model_stan_new_exp <- "
  data {
    int<lower=0> c;          // number of polling stations 
    matrix[c, 6] votes;      // votes matrix
    vector[c] nl;            // nominal list
  }
  parameters {
    vector[5] y;
    vector<lower=0>[6] tau;      // prior scale
  }
  transformed parameters {
    simplex[6] theta;             // init theta param
    real Z;
    
    Z = 1 + sum(exp(y));         // normalizing constant
    for (j in 1:5){
    theta[j] = exp(y[j]) / Z;    // inv alr
    }
    theta[6] = 1 / Z;            // remaining theta
     
    
  }
  model {
    // settings for prior of theta
    vector[5] mu;
    real mu1;
    real mu2;
    vector[5] diag;  // diagonal of covariance matrix
    matrix[5, 5] sigma; // covariance matrix
    matrix[6, 6] T;            // prior correlation
    
    // now assign values
    mu1 = log((0.6456/18)/(1-0.6456));
    mu2 = log((0.02 / 15)/(1-0.6456));
    mu = [ mu1, mu1, mu1, mu1, mu2 ]';
    diag = [10, 10, 10, 10, 10]';
    sigma = diag_matrix(diag);
    T = diag_matrix([1, 1, 1, 1, 1, 1]');
    
    // priors
    y ~ multi_normal(mu, sigma);
    tau ~ cauchy(0, 2.5);
    
    
    // model
    for (k in 1:c){
    votes[k, 1:6] ~ multi_normal(nl[k] * theta, (1/sqrt(nl[k])) * quad_form_diag(T, tau));    // each polling station is normal
    }
  }
  "


### TEST ####
#db_name <- "../data/remesas/REMESAS0100011930.txt"
#bayes_model_original <- rstan::stan_model(model_code = model_stan_original)
bayes_model_new <- rstan::stan_model(model_code = model_stan_new)
bayes_model_new_exp <- rstan::stan_model(model_code = model_stan_new_exp)
results <- bayes_fit(db_name, R = 200, warmup = 250, model = 'new', verbose = 1, bayes_model = bayes_model_new)
results %>%
  dplyr::summarise(across(where(is.numeric), mean))

hist(results$AMLO)
hist(results$PART)
