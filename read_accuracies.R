library(tidyverse)
library(ggplot2)
library(quickpsy)

# data is 10 different pictures with 6 different presentation times each = 60 rows

# read data and normalize accuracies per participant, picture and dimension
accuracies <- read.csv('./data/accuracies_raw.csv') %>%
    as_tibble() %>%
    mutate(trialNum = 1:nrow(.),
           id       = 'S001') %>%
    select(8:9, 1:7) %>%
    arrange(id, category, fname, prestime) %>%
    mutate(across(id:prestime,  as.factor)) %>%
    group_by(id, fname) %>% # normalize
    mutate(across(Action:Sensory, ~.x/max(.x), 
                  .names = 'n_{col}')) %>%
    ungroup()

# from RadialFrequencyPattern_b    
# fit without guesses and lapses
fit1 <- accuracies %>% group_by(id, category) %>%
  summarize(N = n(),
            k = sum()/N) %>% # hier across!!
  ungroup() %>%
  quickpsy(., nodd, k, N, grouping = .(vp, waveform), 
           bootstrap = 'none')  
plotAndTest(fit1)


# with guess and lapses; restrict parameter space for fitting
fit2 <- ds %>% group_by(vp, nodd, waveform) %>%
  summarize(N = n(),
            k = sum(isRound == 0)) %>%
  ungroup() %>%
  quickpsy(., nodd, k, N, grouping = .(vp, waveform), 
           lapses = TRUE, guess = TRUE,
           parini = list(c(0, 3), c(0, 4), c(0, .2), c(0, 0.3)),
           bootstrap = 'none')
plotAndTest(fit2)

# normalization per picture, dimension and participant
accuracies <- accuracies %>% 
    group_by(id, fname) %>%
    mutate(across(Action:Sensory, ~.x/max(.x), 
                  .names = 'n_{col}')) %>%
    ungroup() %>%
    mutate(across(id:prestime,  as.factor))


