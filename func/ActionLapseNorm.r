# functions for RFPa

lapsepnorm <- function(x, m, sd, guess = 0, lapse = 0){
  guess + (1 - guess - lapse) * pnorm(x, m, sd)
}
# gamma (guess) is left asymptote, lambda (lapse) is right asymptote
# guess + (1 - guess -lambda)) * pnorm(x, m, sd), 
# eg https://journal.r-project.org/archive/2016-1/linares-na.pdf, equation (2)


plotAndTestLapse <- function(fit1){ # plot curves and test for differences in mean
res <-  fit1$par %>% group_by(Target, parn) %>%
  summarize(avg = mean(par)) %>%
  mutate(parn = recode(parn, p1 = 'm', p2 = 'sd', p3 = 'guess', p4 = 'lapse')) %>%
  ungroup() 

if (all(!(res$parn == 'guess'))){
  res = add_case(res, 
                Target = c('Action', 'Action', 'Context', 'Context', 'Object', 'Object'),
                parn = c('guess', 'lapse', 'guess', 'lapse','guess', 'lapse'),
                avg = c(0, 0, 0, 0,0,0))
}

names(res$avg) = res$parn
pars = split(res, res$Target)

curve(lapsepnorm(x, pars$Action$avg['m'],
                    pars$Action$avg['sd'],
                    pars$Action$avg['guess'],
                    pars$Action$avg['lapse']),
      10, 120, ylim = c(0,1),
      ylab = 'p (correct)', xlab = 'presentation time (ms)',
      col = 'red', lwd = 2, xaxt = 'n')

curve(lapsepnorm(x, pars$Context$avg['m'],
                    pars$Context$avg['sd'],
                    pars$Context$avg['guess'],
                    pars$Context$avg['lapse']),
      10, 120, ylim = c(0,1),
      col = 'blue', lwd = 2, add = T)
axis(1, at = sort(unique(fit1$averages$prestime)))
legend(10,0.9,legend = c('Action', 'Context'), 
       lty = c('solid', 'solid'),
       lwd = c(2,2),
       col = c('red', 'blue'), bty='n')

pdats = fit1$averages %>% 
  filter(Target == 'Action') %>%
  group_by(prestime) %>% 
  summarize(m = mean(prob)) %>%
  pull(m)
pdatt = fit1$averages %>% 
  filter(Target == 'Context') %>%
  group_by(prestime) %>% 
  summarize(m = mean(prob)) %>%
  pull(m)
points(sort(unique(fit1$averages$prestime)), pdats, col='red', cex=1.5, pch = 19)
points(sort(unique(fit1$averages$prestime)), pdatt, col='blue', cex=1.5, pch = 19)
tdata = fit1$thresholds %>% split(.$Target) 
#t.test(tdata$sine$thre, tdata$tria$thre, paired = TRUE)
}