### ORIGINAL BAYESIAN MODEL


### PREAMBLE ####
library(tidyverse)
library(truncnorm)
library(ggplot2)
ggplot2::theme_set(theme_classic)

### FUNCTIONS ####

# full sample nominal list
nominal_list <- read_csv("../data/4-ConteoRapido18MUESTRA-ELECCION-PRESIDENCIAL.csv") %>% 
  dplyr::select(ID_ESTRATO_F, LISTA_NOMINAL) %>% 
  dplyr::group_by(ID_ESTRATO_F) %>% 
  dplyr::summarise(LISTA_NOMINAL = sum(LISTA_NOMINAL))




rposterior <- function(R, votes, nl){
  # generate R samples from the posterior in a given stratum
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


bayes_fit_stratum <- function(db, R = 10000){
  # generate R samples from posterior or prior depending on number of polling stations with information
  # db is a data base with votes from all candidates in a give stratum
  #
  # returns a tibble with R samples from the posterior of theta, or from the prior if c < 2
  
  # calculate stratum size
  c <- nrow(db %>% dplyr::filter(LISTA_NOMINAL != NE))
  
  # if only zero or one polling station, sample from prior
  if(c < 2){
    out <- tibble(RAC = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  JAMK = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  AMLO = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  JHRC = rbeta(R, shape1 = 0.1, shape2 = 0.1*((4/0.6456) - 1)),
                  OTROS = rbeta(R, shape1 = 0.1, shape2 = 4.9),
                  NE = rbeta(R, shape1 = 0.1, shape2 = 0.1*((1/0.3344) - 1)))
  }else{
    out <- tibble(RAC = rposterior(R, db$RAC, db$LISTA_NOMINAL),
                  JAMK = rposterior(R, db$JAMK, db$LISTA_NOMINAL),
                  AMLO = rposterior(R, db$AMLO, db$LISTA_NOMINAL),
                  JHRC = rposterior(R, db$JHRC, db$LISTA_NOMINAL),
                  OTROS = rposterior(R, db$OTROS, db$LISTA_NOMINAL),
                  NE = rposterior(R, db$NE, db$LISTA_NOMINAL))
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
  
  out <- full
  View(out)
  
  # get total nominal list size
  N <- sum(nominal_list$LISTA_NOMINAL) # total nominal list
  nominal_list <- nominal_list %>% 
    dplyr::mutate(weight = LISTA_NOMINAL / N)
  
  # fit model and compute estimates
  out <- full %>% 
    dplyr::group_by(ID_ESTRATO_F) %>% 
    dplyr::group_modify(~ bayes_fit_stratum(.x, R)) #%>% # fit bayesian model to each stratum
  View(out)
  out <- out %>% 
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
    #View(out)
  return(out)
}


###
results <- bayes_fit_original("../data/remesas/REMESAS0100012230.txt")
results %>%
  dplyr::summarise(across(where(is.numeric), mean))
