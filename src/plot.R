# Quick counts plotting

# PREAMBLE ####
library(tidyverse)
library(ggplot2)
ggplot2::theme_set(theme_classic())


# SETTINGS ####
dates <- c('1900', '1915', '1930')


# IMPORT AND WRANGLE DATA ####

# import times
times <- readr::read_csv('../data/results/times.csv')

# import votes
db <- tibble(date = character(),
             model = character(),
             candidate = character(),
             vote = double())

for(date in dates){
  # import data bases and add info
  original <- readr::read_csv(paste0('../data/results/original_', date, '.csv')) %>% 
    tidyr::pivot_longer(RAC:PART, names_to = 'candidate', values_to = 'vote') %>% 
    dplyr::mutate(date = date, model = 'original')
  
  original_mcmc <- readr::read_csv(paste0('../data/results/original_mcmc_', date, '.csv')) %>% 
    tidyr::pivot_longer(RAC:PART, names_to = 'candidate', values_to = 'vote') %>% 
    dplyr::mutate(date = date, model = 'original_mcmc')
  
  new <- readr::read_csv(paste0('../data/results/new_', date, '.csv')) %>% 
    tidyr::pivot_longer(RAC:PART, names_to = 'candidate', values_to = 'vote') %>% 
    dplyr::mutate(date = date, model = 'new')
  
  new_exp <- readr::read_csv(paste0('../data/results/new_exp_', date, '.csv')) %>% 
    tidyr::pivot_longer(RAC:PART, names_to = 'candidate', values_to = 'vote') %>% 
    dplyr::mutate(date = date, model = 'new_exp')
  
  # add to db
  db <- db %>% 
    dplyr::bind_rows(original) %>% 
    dplyr::bind_rows(original_mcmc) %>% 
    dplyr::bind_rows(new) %>% 
    dplyr::bind_rows(new_exp)
}


# PLOT ####

colors <- c("original" = "#3F4788FF",
            "original_mcmc" = "#EF7F4FFF",
            "new" = "#56147DFF",
            "new_exp" = "Red")

# distribution of amlo by model and date
db %>% 
  dplyr::filter(candidate == 'AMLO') %>% 
  ggplot() +
  geom_density(aes(x = vote, fill = model),
               alpha = 0.3) +
  facet_wrap(.~date, scales = 'free_y')


# ribbon of amlo by model
db %>% 
  dplyr::filter(candidate == 'AMLO') %>% 
  dplyr::group_by(date, model) %>% 
  dplyr::summarise(lower = quantile(vote, 0.05),
                   mean = mean(vote),
                   upper = quantile(vote, 0.95)) %>% 
  ggplot() +
  geom_ribbon(aes(x = date,
                  ymin = lower,
                  ymax = upper,
                  fill = model,
                  group = model,
                  color = model),
              alpha = 0.3) +
  geom_path(aes(x = date,
                y = mean,
                color = model,
                group = model),
            linetype = 2) +
  #scale_color_manual(values = colors) +
  #scale_fill_manual(values = colors)
  scale_color_viridis_d() +
  scale_fill_viridis_d()



# distribution of voter turnout by model and date
db %>% 
  dplyr::filter(candidate == 'PART') %>% 
  ggplot() +
  geom_density(aes(x = vote, fill = model),
               alpha = 0.3) +
  facet_wrap(.~date, scales = 'free_y')


# ribbon of voter turnout by model
db %>% 
  dplyr::filter(candidate == 'PART') %>% 
  dplyr::group_by(date, model) %>% 
  dplyr::summarise(lower = quantile(vote, 0.05),
                   mean = mean(vote),
                   upper = quantile(vote, 0.95)) %>% 
  ggplot() +
  geom_ribbon(aes(x = date,
                  ymin = lower,
                  ymax = upper,
                  fill = model,
                  group = model,
                  color = model),
              alpha = 0.3) +
  geom_path(aes(x = date,
                y = mean,
                color = model,
                group = model),
            linetype = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)


# evolution of times by model
times %>% 
  ggplot() +
  geom_line(aes(x = db_time, y = time_s, group = model, color = model)) +
  scale_color_manual(values = colors)
