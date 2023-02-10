library(tidyverse)
library(ggplot2)
library(quickpsy)

# data is 10 different pictures with 6 different presentation times each = 60 rows

counts <- read.csv('./data/counts.csv') %>%
  as_tibble() %>%
  mutate(trialNum = 1:nrow(.),
         id       = 'S001') %>%
  select(trialNum:id, category, fname, prestime, ends_with('_correct')) %>%
  #arrange(id, category, fname, prestime) %>%
  rename(Action = Action_correct,
         Context = Context_correct,
         Object = Object_correct,
         Sensory = Sensory_correct) %>%
  pivot_longer(cols = c(Action, Context, Object, Sensory),
               names_to = 'Target',
               values_to = 'k') %>%
  group_by(id, fname, Target) %>%
  mutate(N_normalized =  max(k),
         N_original   = 5) %>%
  ungroup() %>%
  mutate(across(id:fname,  as.factor)) %>%
  arrange(id, Target, category, fname, prestime)
  
tabC <- counts %>% 
  group_by(id, Target, prestime) %>%
  summarize(k = sum(k),
            N = sum(N_normalized)) %>%
  mutate(correct = k/N) %>%
  ungroup()


# read data and normalize accuracies per participant, picture and dimension
accuracies <- read.csv('./data/accuracies_raw.csv') %>%
    as_tibble() %>%
    mutate(trialNum = 1:nrow(.),
           id       = 'S001') %>%
    select(8:9, 1:7) %>%
    arrange(id, category, fname, prestime) %>%
    mutate(across(id:category,  as.factor)) %>%
    group_by(id, fname) %>% # normalize
    mutate(across(Action:Sensory, ~.x/max(.x), 
                  .names = 'n_{col}')) %>%
    ungroup()

tabA <- accuracies %>% 
  group_by(id, prestime) %>%
  summarize(Action = mean(n_Action),
            Context = mean(n_Context),
            Object = mean(n_Object),
            Sensory = mean(n_Sensory)) %>%
  ungroup()


  
  
# fit without guesses and lapses
fit1 <- counts %>% 
        filter(Target != 'Sensory' & prestime != 500) %>%
        quickpsy(., prestime, k, N_normalized, grouping = .(id, Target), 
           bootstrap = 'none')  

tabF <- fit1$averages %>%
  group_by(id, Target, prestime) %>%
  summarize(correct = mean(prob)) %>%
  ungroup()

  
  
    
#plotAndTest(fit1)


# fit with guess and lapses; restrict parameter space for fitting
fit2 <- counts %>% 
  filter(Target != 'Sensory' & prestime < 500) %>%
  quickpsy(., prestime, k, N_normalized, grouping = .(id, Target), 
           lapses = TRUE, 
           guess = TRUE, xmax = 100,
           parini = list(c(0, 100), c(0, 100),c(0, 1), c(0, 1)),
           bootstrap = 'none')



fit2 <- counts %>% 
        filter(Target == 'Action') %>%
        quickpsy(., prestime, k, N_normalized, grouping = .(id, Target), 
                 lapses = 1-tabC$correct[tabC$Target == 'Action' & tabC$prestime == 500], 
                 guess = TRUE, xmax = 100,
                 parini = list(c(0, 100), c(0, 100),c(0, 1)),
                 bootstrap = 'none')
plotAndTest(fit2)

# normalization per picture, dimension and participant
accuracies <- accuracies %>% 
    group_by(id, fname) %>%
    mutate(across(Action:Sensory, ~.x/max(.x), 
                  .names = 'n_{col}')) %>%
    ungroup() %>%
    mutate(across(id:prestime,  as.factor))


