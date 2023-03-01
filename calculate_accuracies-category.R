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
               names_to = 'Feature',
               values_to = 'k') %>%
  group_by(id, fname, Feature, category) %>%
  mutate(N_normalized =  max(k),
         N_original   = 5) %>%
  ungroup() %>%
  mutate(across(id:fname,  as.factor)) %>%
  arrange(id, Feature, category, fname, prestime)

# descriptives  
tabC <- counts %>% 
  group_by(id, Feature, prestime, category) %>%
  summarize(k = sum(k),
            N = sum(N_normalized)) %>%
  mutate(correct = k/N) %>%
  ungroup()

# fit M, SD and guesses 
# fix lapse rate to (1 - avgerage accuracy in 500 ms condition).
# fit only data between 33.3 to 100 ms 
fitted2 = counts %>%
    filter(Feature == 'Action') %>%
    group_by(id, category) %>%
    mutate(lapse = 1-sum(k[prestime == 500])/sum(N_normalized[prestime == 500])) %>%
    group_split %>%
    map(~quickpsy(., prestime, k, N_normalized, 
                    #lapses = unique(.$lapse),  # TODO: what if two lapses are equal betw. features?
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
plotfun(fitted2[[5]], 'pink')


# add ticks, labels etc
xvalues <- round(sort(unique(counts$prestime[counts$prestime < 500])))
axis(1, at = xvalues)
legend(10,0.95,legend = c('cleaning', 'communication', 'food', 'hobby', 'locomotion'), 
       lty = c('solid', 'solid', 'solid', 'solid', 'solid'),
       lwd = c(2,2,2,2,2),
       col = c('red', 'blue', 'black', 'gold', 'pink'), bty='n')
abline(h = 0.5, lty = 'dashed')


