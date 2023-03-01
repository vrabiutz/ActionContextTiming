library(tidyverse)
#library(ggplot2)
library(quickpsy)

counts <- read.csv('./data/counts.csv') %>%
  as_tibble() %>%
  mutate(trialNum = 1:nrow(.),
         id       = 'S001') %>%
  select(trialNum:id, category, fname, prestime, ends_with('_correct')) %>%
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

# descriptives  
tabC <- counts %>% 
  group_by(id, Target, prestime) %>%
  summarize(k = sum(k),
            N = sum(N_normalized)) %>%
  mutate(correct = k/N) %>%
  ungroup()


# # read data and normalize accuracies per participant, picture and dimension
# accuracies <- read.csv('./data/accuracies_raw.csv') %>%
#     as_tibble() %>%
#     mutate(trialNum = 1:nrow(.),
#            id       = 'S001') %>%
#     select(8:9, 1:7) %>%
#     arrange(id, category, fname, prestime) %>%
#     mutate(across(id:category,  as.factor)) %>%
#     group_by(id, fname) %>% # normalize
#     mutate(across(Action:Sensory, ~.x/max(.x), 
#                   .names = 'n_{col}')) %>%
#     ungroup()

# tabA <- accuracies %>% 
#   group_by(id, prestime) %>%
#   summarize(Action = mean(n_Action),
#             Context = mean(n_Context),
#             Object = mean(n_Object),
#             Sensory = mean(n_Sensory)) %>%
#   ungroup()

# fit without guesses and lapses
# fit1 <- counts %>% 
#         filter(Target != 'Sensory' & prestime != 500) %>%
#         quickpsy(., prestime, k, N_normalized, grouping = .(id, Target), 
#            bootstrap = 'none')  
# 
# tabF <- fit1$averages %>%
#   group_by(id, Target, prestime) %>%
#   summarize(correct = mean(prob)) %>%
#   ungroup()
#plotAndTest(fit1)


# fit M, SD and guesses 
# fix lapse rate to (1 - avgerage accuracy in 500 ms condition).
# fit only data between 33.3 to 100 ms 
fitted2 = counts %>%
    #filter(Target !='Sensory') %>%
    group_by(id, Target) %>%
    mutate(lapse = 1-sum(k[prestime == 500])/sum(N_normalized[prestime == 500])) %>%
    group_split %>%
    map(~quickpsy(., prestime, k, N_normalized, 
                    lapses = unique(.$lapse),  # what if two lapses are equal betw. features?
                    guess = TRUE, xmax = 100,
                    parini = list(c(0, 100), c(0, 100), c(0, 1)),
                    bootstrap = 'none'))

# plotting functions
lapsepnorm <- function(x, m, sd, guess = 0, lapse = 0){
  guess + (1 - guess - lapse) * pnorm(x, m, sd)
}

plotfun = function(plotparameters, plotcolor){
          curve(lapsepnorm(x, plotparameters$par$par[1], # M
                 plotparameters$par$par[2], # SD
                 plotparameters$par$par[3], # guess
                 plotparameters$lapses),     # lapse
                 col = plotcolor, lwd = 2, 
                 xaxt = 'n', add=T)
          prb <- aggregate(prob ~ prestime, FUN=mean, data=plotparameters$averages)
          points(prb$prestime, prb$prob,
                 col=plotcolor, cex=1.5, pch = 19)
}

# actual plotting
plot(NULL, xlim = c(0,180), ylim = c(0,1),
     ylab = 'p (correct)', xlab = 'presentation time (ms)', xaxt = 'n')
plotfun(fitted2[[1]], 'red')
plotfun(fitted2[[2]], 'blue')
plotfun(fitted2[[3]], 'black')
plotfun(fitted2[[4]], 'gold')


# add ticks, labels etc
xvalues <- round(sort(unique(counts$prestime[counts$prestime < 500])))
axis(1, at = xvalues)
legend(10,0.9,legend = c('Action', 'Context', 'Object', 'Sensory'), 
       lty = c('solid', 'solid', 'solid'),
       lwd = c(2,2,2),
       col = c('red', 'blue', 'black', 'gold'), bty='n')
abline(h = 0.5, lty = 'dashed')


