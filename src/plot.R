# Quick counts plotting

# PREAMBLE ####
library(tidyverse)
library(ggplot2)
ggplot2::theme_set(theme_classic())


# SETTINGS ####
dates <- c('1900',
           '1915',
           '1930',
           '1945',
           '2000',
           '2015',
           '2030',
           '2045',
           '2100',
           '2115',
           '2130',
           '2145',
           '2200',
           '2215',
           '2230')


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

colors <- c("original" = viridis::viridis(20, option = "C")[1],
            "original_mcmc" = viridis::viridis(20, option = "D")[14],
            "new" = viridis::viridis(20, option = "D")[19],
            "new_exp" = viridis::viridis(20, option = "B")[15])

# distribution of amlo by model and date
db %>% 
  dplyr::filter(candidate == 'AMLO') %>% 
  ggplot() +
  geom_density(aes(x = vote, fill = model),
               alpha = 0.3) +
  facet_wrap(.~date, scales = 'free_y') +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)


# distribution of amlo by model at 22:30pm
db %>% 
  dplyr::filter(candidate == 'AMLO', date == '2230') %>% 
  ggplot() +
  geom_density(aes(x = vote, fill = model),
               alpha = 0.4) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)


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
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)



# distribution of voter turnout by model and date
db %>% 
  dplyr::filter(candidate == 'PART') %>% 
  ggplot() +
  geom_density(aes(x = vote, fill = model),
               alpha = 0.3) +
  facet_wrap(.~date, scales = 'free_y') +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)



# distribution of voter turnout by model at 22:30pm
db %>% 
  dplyr::filter(candidate == 'PART', date == '2230') %>% 
  ggplot() +
  geom_density(aes(x = vote, fill = model),
               alpha = 0.3)  +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)


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
  dplyr::mutate(minutes= time_s / 60) %>% 
  ggplot() +
  geom_line(aes(x = db_time, y = minutes, group = model, color = model),
            size = 2) +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
  scale_color_manual(values = colors)
