library(tidyverse)
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

#####====== Fit and extract results
# fit M, SD and guesses; use only data between 33.3 to 100 ms 
# fix lapse rate to (1 - avgerage accuracy in 500 ms condition)
fitted2 = counts %>%
    filter(Target !='Sensory') %>%
    group_by(Target) %>% # omitted id here!!!
    mutate(lapse = 1-sum(k[prestime == 500])/sum(N_normalized[prestime == 500])) %>%
    group_split %>%
    map(~quickpsy(., prestime, k, N_normalized, 
                    lapses = unique(.$lapse), 
                    guess = TRUE, xmax = 100,
                    parini = list(c(0, 100), c(0, 100), c(0, 1)),
                    bootstrap = 'nonparametric', B = 1000))

# show deviance and p
fitted2 %>% map_df("deviance")


# write empirical thresholds
empThresh = fitted2 %>% 
  map("thresholds") %>%
  map_dbl("thre")

names(empThresh) = counts %>%
  filter(Target !='Sensory') %>%
  group_by(Target) %>%
  group_keys(.) %>%
  pull("Target")

####=== randomization: generate distribution of differences between conditions 
nruns = 1000
conditions = list(c('Action', 'Context'),
                  c('Action', 'Object'),
                  c('Context', 'Object'))

# allocate vars
permThresh = rep(list(tibble(Targ1 = numeric(nruns)/0,
                        Targ2 = numeric(nruns)/0,
                        Targ1minus2 = numeric(nruns)/0,
                        Targ1Name = char('NULL'),
                        Targ2Name = char('NULL'),
                        empDiff  = numeric(nruns)/0)),length(conditions))
pvals = numeric(length(empThresh))/0

# loop
for (con in 1:length(conditions)){
  for (k in 1:nruns){
  tmp = counts %>% 
    filter(Target == conditions[[con]][1] | Target == conditions[[con]][2]) %>%
    group_by (prestime)  %>%
    mutate(Target = sample(Target)) %>% # permute within prestime
    ungroup(.) %>%
    group_by(Target) %>%
    mutate(lapse = 1-sum(k[prestime == 500])/sum (N_normalized[prestime == 500])) 
  
  firstCondition  = group_keys(tmp)$Target[1]
  secondCondition = group_keys(tmp)$Target[2]
  
  fit = tmp %>%
    group_split %>%
    map(~quickpsy(., prestime, k, N_normalized, 
                  lapses = unique(.$lapse), 
                  guess = TRUE, xmax = 100,
                  parini = list(c(0, 100), c(0, 100), c(0, 1)),
                  bootstrap = 'none'))
  
  permThresh[[con]][k, 1] = fit[[1]]$thresholds$thre
  permThresh[[con]][k, 2] = fit[[2]]$thresholds$thre
  permThresh[[con]][k, 3] = fit[[1]]$thresholds$thre - fit[[2]]$thresholds$thre
  permThresh[[con]][k, 4] = firstCondition
  permThresh[[con]][k, 5] = secondCondition
  permThresh[[con]][k, 6] = empThresh[conditions[[con]][1]] -
                            empThresh[conditions[[con]][2]]
  }
  pvals[con] = sum(permThresh[[con]]$Targ1minus2 > permThresh[[con]]$empDiff[1]) / nruns
}

####==== plot permutation distribution and empirical data
plotPerm <- function(permResults, pvals, comparison){
  plot(ecdf(permResults[[comparison]]$Targ1minus2), 
       main = paste(permResults[[comparison]]$Targ1Name[1], 
                    '-', 
                    permResults[[comparison]]$Targ2Name[1]), 
          xlab = "Difference of detection thresholds")
  points(permResults[[comparison]]$empDiff[1],
         1-pvals[comparison],
         col = 'red')
}
plotPerm(permThresh, pvals, 1)
plotPerm(permThresh, pvals, 2)
plotPerm(permThresh, pvals, 3)


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

# add ticks, labels etc
xvalues <- round(sort(unique(counts$prestime[counts$prestime < 500])))
axis(1, at = xvalues)
legend(10,0.9,legend = c('Action', 'Context', 'Object'), 
       lty = c('solid', 'solid', 'solid'),
       lwd = c(2,2,2),
       col = c('red', 'blue', 'black'), bty='n')
abline(h = 0.5, lty = 'dashed')


