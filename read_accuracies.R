library(tidyverse)
library(ggplot2)

#counts     <- read.csv('./data/counts.csv') 
# as yet: 10 different pictures with 6 different presentation times each = 60 rows

accuracies <- read.csv('./data/accuracies_raw.csv') %>%
    mutate(trialNum = 1:nrow(.),
           participant  = 'S001') %>%
    arrange(accuracies, category, prestime) %>%
    select(8:9, 1:7) %>%
    as_tibble()


# normalization? per picture, dimension and participant
#Fei-Fei (2007) page 8: The scores were then normalized: The seven scores for a given image (one for each PT) were divided by the highest score achieved for that image (across all PTs). All evaluation scores were therefore between 0 and 1. Due to this within-image normalization, inherent differences in difficulty of perceiving or understanding scenes between different images were eliminated. [PT = presentation time]
accuracies %>% group_by(participant, fname) %>%
               group_split()%>%
               map(~ .$Action / max(.$Action))
               
accuracies %>% group_by(participant, fname) %>%
  group_split()%>%
  map(~ .$Action / max(.$Action)) %>%
  simplify()

https://dcl-prog.stanford.edu/purrr-mutate.html

llist = accuracies %>% group_by(participant, fname) %>%
  group_split()

l %>% mutate(across(Action:Object),
             )
  map(~ .$Action / max(.$Action))

accuracies %>% mutate(across(Action:Object,
                             group_by(participant, fname) %>%
                               group_split()%>%
                               map(~ .x / max(.x))))
             