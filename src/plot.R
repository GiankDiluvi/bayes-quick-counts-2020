# Quick counts plotting

# PREAMBLE ####
library(tidyverse)
library(stringr)
library(ggplot2)
ggplot2::theme_set(theme_classic())
library(patchwork)


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


db <- db %>% 
  dplyr::mutate(model = ifelse(model == 'new_exp', 'new_simple',
                               ifelse(model == 'new', 'new_complex', model)))

times <- times %>% 
  dplyr::mutate(model = ifelse(model == 'new_exp', 'new_simple',
                               ifelse(model == 'new', 'new_complex', model)))

# PLOT ####

colors <- c("original" = viridis::viridis(20, option = "C")[1],
            "original_mcmc" = viridis::viridis(20, option = "D")[14],
            "new_complex" = viridis::viridis(20, option = "D")[19],
            "new_simple" = viridis::viridis(20, option = "B")[15])
text_size <- 24
hour_format <- function(time) return(paste0(stringr::str_sub(time, 1, 2), ':', paste0(stringr::str_sub(time, 3, 4))))

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
amlo_dist <- db %>% 
  dplyr::filter(candidate == 'AMLO', date == '2230') %>% 
  ggplot() +
  geom_density(aes(x = vote, fill = model),
               alpha = 0.4) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme(text = element_text(size = text_size),
        legend.position = "top") +
  labs(x = expression(paste(theta[1], ' - AMLO')),
       y = 'Posterior density',
       color = 'Model: ',
       fill = 'Model: ')


# ribbon of amlo by model
amlo_ribbon <- db %>% 
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
              alpha = 0.3, show.legend=FALSE) +
  geom_path(aes(x = date,
                y = mean,
                color = model,
                group = model),
            linetype = 2, show.legend=FALSE) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = hour_format) +
  theme(text = element_text(size = text_size),
        axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "none") +
  labs(x = "Time on election day",
       y = expression(paste(theta[1], ' - AMLO')))


# glue them
combined <- amlo_dist + amlo_ribbon & theme(legend.position = "bottom")
#combined <- amlo_dist + amlo_ribbon 
combined + plot_layout(guides = "collect")
ggsave('../doc/fig/amlo.pdf', width = 16)



amlo_ribbon2 <- db %>% 
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
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = hour_format) +
  theme(text = element_text(size = text_size),
        axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "bottom") +
  labs(x = "Time on election day",
       y = expression(paste(theta[1], ' - AMLO')),
       color = "Model: ",
       fill = "Model: ")
ggsave('../doc/fig/amlo2.pdf', plot = amlo_ribbon2, width = 12)



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
part_dist <- db %>% 
  dplyr::filter(candidate == 'PART', date == '2230') %>% 
  ggplot() +
  geom_density(aes(x = vote, fill = model),
               alpha = 0.3)  +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme(text = element_text(size = text_size),
        legend.position = "top") +
  labs(x = expression(paste('Voter turnout ', rho)),
       y = 'Density',
       color = 'Model: ',
       fill = 'Model: ')


# ribbon of voter turnout by model
part_ribbon <- db %>% 
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
              alpha = 0.3, show.legend=FALSE) +
  geom_path(aes(x = date,
                y = mean,
                color = model,
                group = model),
            linetype = 2, show.legend=FALSE) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme(text = element_text(size = text_size),
        axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "none") +
  scale_x_discrete(labels = hour_format) +
  labs(x = "Time on election day",
       y = expression(paste('Voter turnout ', rho)))


# glue them
combined <- part_dist + part_ribbon  & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")
ggsave('../doc/fig/part.pdf', width = 16)




part_ribbon2 <- db %>% 
  dplyr::filter(candidate == 'PART') %>% 
  dplyr::group_by(date, model) %>% 
  dplyr::summarise(lower = quantile(vote, 0.05),
                   mean = mean(vote),
                   upper = quantile(vote, 0.95)) %>% 
  ggplot() +
  #geom_hline(yintercept = 0.6343, color = "red", linetype = "dashed") +
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
  scale_fill_manual(values = colors) +
  theme(text = element_text(size = text_size),
        axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "bottom") +
  scale_x_discrete(labels = hour_format) +
  labs(x = "Time on election day",
       y = expression(paste('Voter turnout ', rho)),
       color = "Model: ",
       fill = "Model: ")

ggsave('../doc/fig/part2.pdf', plot = part_ribbon2, width = 12)

# evolution of times by model
times_plot <- times %>% 
  dplyr::mutate(minutes= time_s / 60,
                db_time = as.character(db_time)) %>% 
  ggplot() +
  geom_line(aes(x = db_time, y = minutes, group = model, color = model),
            size = 2) +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
  scale_x_discrete(labels = hour_format) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  scale_color_manual(values = colors) +
  labs(x = "Time on election day",
       y = "Minutes to",
       color = 'Model: ') +
  theme(text = element_text(size = text_size),
        axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = 'bottom')

ggsave('../doc/fig/times.pdf', plot = times_plot, width = 12)
