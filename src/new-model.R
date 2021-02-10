### NEW BAYESIAN MODEL


### PREAMBLE ####
library(tidyverse)
library(truncnorm)
library(ggplot2)
ggplot2::theme_set(theme_classic)
library(rstan)

### FUNCTIONS ####
db_name <- "../data/remesas/REMESAS0100012230.txt"

# Stan model for sampling from the posterior at each stratum
model_stan <- "
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
    vector[6] theta;             // init theta param
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
    real sigma1; // variance of candidates
    real sigmaJ;     // variance of others
    vector[5] diag;  // diagonal of covariance matrix
    matrix[5, 5] sigma; // covariance matrix
    
    // now assign values
    mu = [ -4, -4, -4, -4, -4 ]';
    sigma1 = 2 * (log(0.6456/4) + 4);
    sigmaJ = 2 * (log(0.02) + 4);
    diag = [sigma1, sigma1, sigma1, sigma1, sigmaJ]';
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

bayes_model <- rstan::stan_model(model_code = model_stan)



stan_data <- list(
  c = nrow(out),
  votes = out %>% dplyr::select(RAC, JAMK, AMLO, JHRC, OTROS, NE) %>% as.matrix() %>% unname(),
  nl = out %>% pull(LISTA_NOMINAL)
)

result <- rstan::sampling(bayes_model, data = stan_data, chains = 4, iter = 750, warmup = 500, cores = 4)

out <- as_tibble(as.data.frame(result)) %>% 
  dplyr::select(`theta[1]`:`theta[6]`) %>% 
  dplyr::rename(RAC = `theta[1]`,
                JAMK = `theta[2]`,
                AMLO = `theta[3]`,
                JHRC = `theta[4]`,
                OTROS = `theta[5]`,
                NE = `theta[6]`)





rposterior <- function(db, R = 10000, warmup = 1000, n_chains = 4){
  # generate R samples from the posterior in a given stratum
  # db contains the votes for each presidential candidate and the nominal list
  # R is the number of iterations per chain
  # warmup is the number of warmup iterations
  # n_chains is the number of chains to run
  #
  # returns a RxJ tibble with samples from the posterior of the proportions of votes
  
  
  
  return(out)
}


#real rac[c];             // votes for rac
#real jamk[c];            // votes for jamk
#real amlo[c];            // votes for amlo
#real jhrc[c];            // votes for jhrc
#real otros[c];           // votes for otros
#real ne[c];              // votes for ne


bayes_fit_stratum <- function(db, R = 10000){
  # generate R samples from posterior or prior depending on number of polling stations with information
  # db is a data base with votes from all candidates in a give stratum
  #
  # returns a tibble with R samples from the posterior of theta, or from the prior if c < 2
  
  # calculate stratum size
  c <- nrow(db %>% dplyr::filter(TOTAL != 0))
  
  # if only zero or one polling station, sample from prior
  if(c < 2){
    # if only 1 or no rows have info, sample from prior
    out <- tibble(RAC = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  JAMK = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  AMLO = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  JHRC = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  OTROS = rbeta(R, shape1 = 0.1, shape2 = 4.9),
                  NE = rbeta(R, shape1 = 0.1, shape2 = 0.1*((1/0.3344) - 1)))
  }else{
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


bayes_fit_original <- function(db_name, R = 10000){
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
    dplyr::group_by(ID_ESTRATO_F) %>% 
    dplyr::summarise(LISTA_NOMINAL = sum(LISTA_NOMINAL)) %>% 
    dplyr::mutate(weight = LISTA_NOMINAL / sum(LISTA_NOMINAL))
  
  # get total nominal list size
  N <- sum(nominal_list$LISTA_NOMINAL) # total nominal list
  
  # fit model and compute estimates
  out <- full %>% 
    dplyr::group_by(ID_ESTRATO_F) %>% 
    dplyr::group_modify(~ bayes_fit_stratum(.x, R)) %>% # fit bayesian model to each stratum
    dplyr::mutate(id = 1:n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(nominal_list) %>% 
    dplyr::group_by(id) %>% 
    # now compute national theta estimates
    dplyr::summarise(RAC = weighted.mean(RAC, weight, na.rm = TRUE),
                     JAMK = weighted.mean(JAMK, weight, na.rm = TRUE),
                     AMLO = weighted.mean(AMLO, weight, na.rm = TRUE),
                     JHRC = weighted.mean(JHRC, weight, na.rm = TRUE),
                     OTROS = weighted.mean(OTROS, weight, na.rm = TRUE),
                     NE = weighted.mean(NE, weight, na.rm = TRUE)) %>% 
    # compute voter turnout and lambdas
    dplyr::mutate(PART = RAC + JAMK + AMLO + JHRC + OTROS,
                  TOTAL = PART + NE,
                  RAC = RAC / PART,
                  JAMK = JAMK / PART,
                  AMLO = AMLO / PART,
                  JHRC = JHRC / PART,
                  OTROS = OTROS / PART) %>% 
    dplyr::select(RAC, JAMK, AMLO, JHRC, OTROS, PART)
  
  return(out)
}


### RUN ####
#results <- bayes_fit_original("../data/remesas/REMESAS0100012230.txt")
#results %>%
#  dplyr::summarise(across(where(is.numeric), mean))