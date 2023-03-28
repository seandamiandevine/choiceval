
rm(list=ls())

library(lme4); library(lmerTest)
library(brms); library(bayestestR)

ci = function(x, a=qnorm(.975), na.rm=T) plotrix::std.error(x,na.rm=na.rm)*a

# Load and clean data ---------------------------------------------------------------

dat = read.csv('data/choiceval2.csv')
dat$choice_stim = dplyr::lag(dat$stimulus, 5)
dat = dat[dat$is_data_trial=='true',]

learn  = dat[dat$phase=='learning',]
choice = dat[dat$phase=='choice',]

n_per_sub = table(choice$prol_id)
rmv    = names(n_per_sub[n_per_sub < 190 | n_per_sub > 300])
learn  = learn[!learn$prol_id %in% rmv, ]
choice = choice[!choice$prol_id %in% rmv, ]

choice$block = floor(ifelse(choice$trialinblock==0,choice$trial/54,NA))
choice$block = zoo::na.locf(choice$block)

catch  = choice[choice$deck2 %in% c('Xc', 'Xnc'), ]
choice = choice[!choice$deck2 %in% c('Xc', 'Xnc'), ]

tmp = choice[choice$prol_id==choice$prol_id[10], ]
table(tmp$mutual_info)

table(tmp$deck1, tmp$deck2)

tmp = catch[catch$prol_id == catch$prol_id[1], ]
table(tmp$deck1, tmp$deck2)

length(unique(choice$prol_id))

# Catch behaviour ---------------------------------------------------------

## plot catch decks ----

catch$caught = as.numeric(catch$chosen_deck==catch$deck2)

pdf('figs/catch_per_subject.pdf', width=6, height=6)

pcatch = tapply(catch$caught, list(catch$deck2, catch$subject), mean)
colnames(pcatch) = 1:ncol(pcatch)
rownames(pcatch) = c('Choice', 'No Choice')

barplot(pcatch, beside=T, xlab='Subject', ylab='P(Choose Catch)', main='Catch Trials', 
        legend.text = T, args.legend = list(x='topleft', bty='n', title='Type of Catch Trial', ncol=2), 
        ylim=c(0,1.5), yaxt='n')
axis(2, at=c(0,.5,1), c(0,.5,1))
abline(h=0.5, lty=2)

dev.off()

# Overall catch accuracy
pdf('figs/catch_hist.pdf', width=6, height=6)

hist(pcatch[1,], main='Catch Accuracy', xlab='P(Correct)', col=scales::alpha('grey', .5))
hist(pcatch[2,], col=scales::alpha('black', .5), add=T)
abline(v=2/3, lty=2)
legend('topleft',bty='n', fill = scales::alpha(c('grey','black'), .5), legend=rownames(pcatch))

dev.off()

# pairs of catch
catch$pair = paste0(catch$deck2, '_', catch$deck1)

pdf('figs/catch_pairs.pdf', width=6, height=6)
layout(matrix(1:4, 2, 2, byrow = T))
for(p in sort(unique(catch$pair))) {
  
  deck_names = strsplit(p,'_')[[1]]
  tmp        = catch[catch$pair==p & catch$chosen_deck!='',]
  prob       = c(mean(tmp$chosen_deck==deck_names[1]), mean(tmp$chosen_deck==deck_names[2]))
  cip        = ci(tmp$chosen_deck==deck_names[1])
  
  bp = barplot(prob, xlab='', ylab='P(Choose Deck)', ylim=c(0,1),
               names.arg = deck_names)
  arrows(bp, prob-cip, bp, prob+cip, length=0)
  
}

dev.off()

# pairs of catch barplot
pdf('figs/catch_pairs_barplot.pdf', width=6, height=6)

catch$alt_type   = ifelse(catch$deck1=='A' , 'Choice', 'No Choice')
catch$catch_type = ifelse(catch$deck2=='Xc', 'Choice', 'No Choice')

pcatch  = tapply(catch$caught, list(catch$catch_type, catch$alt_type), mean)
cicatch = tapply(catch$caught, list(catch$catch_type, catch$alt_type), ci)

b = barplot(pcatch, beside=T, ylim=c(0,1.1), 
            ylab='P(Choose Catch Deck)', 
            xlab='Catch Deck Type',
            legend.text = T, 
            args.legend = list(x='topleft', bty='n', title='Alternative Deck Type'))
arrows(b, pcatch-cicatch, b, pcatch+cicatch, length=0)
abline(h=.66, lty=2)

dev.off()

## exclude ----
pcaught = tapply(catch$caught, catch$subject, mean)
rmv     = names(pcaught[pcaught < .66]) # maybe change this
choice  = choice[!choice$subject %in% rmv, ]

# Choice behaviour --------------------------------------------------------

N = length(unique(choice$prol_id))

## Visualize ------

### barplot -----
pdf('figs/pchoose_bar.pdf', width=6, height=6)

pchoice  = tapply(choice$choose_choice, choice$mutual_info, mean)
cichoice = tapply(choice$choose_choice, choice$mutual_info, ci)
ylim = c(min(pchoice-cichoice-.1), max(pchoice+cichoice+.1))

b = barplot(pchoice, xlab='MI', ylab='P(Choose Choice)', ylim=ylim, xpd=F)
arrows(b, pchoice-cichoice, b, pchoice+cichoice, length=0)
abline(h=.5, lty=2)

dev.off()

### barplot (remove first trials) -----
pdf('figs/pchoose_bar_rmv.pdf', width=6, height=6)

prmv = .1
tmp  = choice[choice$trialinblock > prmv*53, ]
pchoice  = tapply(tmp$choose_choice, tmp$mutual_info, mean)
cichoice = tapply(tmp$choose_choice, tmp$mutual_info, ci)
ylim = c(min(pchoice-cichoice-.1), max(pchoice+cichoice+.1))

b = barplot(pchoice, xlab='MI', ylab='P(Choose Choice)', ylim=ylim, xpd=F, 
            main=paste0('First ',prmv*100,'% of Trials\nRemoved'))
arrows(b, pchoice-cichoice, b, pchoice+cichoice, length=0)
abline(h=.5, lty=2)

dev.off()

### time course -----

pdf('figs/pchoose_trialinblock.pdf', width=6, height=6)

nbin = 6
tbin = cut(choice$trialinblock, nbin, labels = F)
blab = floor(seq(1,max(choice$trialinblock)+1,length.out=3))

pchoice  = tapply(choice$choose_choice, list(tbin, choice$mutual_info), mean)
cichoice = tapply(choice$choose_choice, list(tbin, choice$mutual_info), ci)
ylim = c(min(pchoice-cichoice-.2), max(pchoice+cichoice+.2))

matplot(pchoice, type='l', lty=1, lwd=3, col=c('darkred','darkblue'), 
        xlab='Trial', ylab='P(Choose Choice)', main='Trial in Block',
        xaxt='n', ylim=ylim)
axis(1, at=seq(1,nbin,length.out=3), labels=blab)

l = pchoice-cichoice
u = pchoice+cichoice
polygon(c(1:nbin, nbin:1), c(l[,1], rev(u[,1])), col = scales::alpha('darkred', 0.25), lty = 0)
polygon(c(1:nbin, nbin:1), c(l[,2], rev(u[,2])), col = scales::alpha('darkblue', 0.25), lty = 0)
abline(h=.5, lty=2)
legend('topleft', bty='n', lty=1, lwd=3, col=c('darkred','darkblue'), legend=c(0,1), title='MI')

dev.off()

### time course (all) -----

avgchoose = zoo::rollmean(choice$choose_choice, 10, fill=NA)

pchoice  = tapply(avgchoose, list(choice$trial, choice$mutual_info, choice$ctbl), mean)
cichoice = tapply(avgchoose, list(choice$trial, choice$mutual_info, choice$ctbl), ci)
mi = tapply(choice$mutual_info, list(choice$ctbl, choice$trial), mean)
ylim = c(0,1)

# ctbl = 0
pdf('figs/pchoose_trial_ctbl0.pdf', width=6, height=6)

matplot(pchoice[,,1], type='l', lty=1, lwd=3, col=c('darkred','darkblue'), 
        xlab='Trial', ylab='P(Choose Choice)', main='Counterbalance: 0',
        ylim=ylim)
lines(mi[1,], lwd=3, lty=2)
abline(h=.5, lty=2)

legend('bottomright', bty='n', lty=1, lwd=3, col=c('darkred','darkblue'), legend=c(0,1), title='MI')

dev.off()

# ctbl = 1
pdf('figs/pchoose_trial_ctbl1.pdf', width=6, height=6)

matplot(pchoice[,,2], type='l', lty=1, lwd=3, col=c('darkred','darkblue'), 
        xlab='Trial', ylab='P(Choose Choice)', main='Counterbalance: 1',
        ylim=ylim)
lines(mi[2,], lwd=3, lty=2)
abline(h=.5, lty=2)

legend('bottomright', bty='n', lty=1, lwd=3, col=c('darkred','darkblue'), legend=c(0,1), title='MI')

dev.off()


## Model -----

choice$trial_c        = choice$trial - median(choice$trial)
choice$trialinblock_c = choice$trialinblock-median(choice$trialinblock)
choice$mutual_info_c  = choice$mutual_info - .5
choice$ctbl_c         = choice$ctbl - .5

# choice_mod = brm(choose_choice ~ mutual_info_c * trialinblock_c * ctbl_c + 
#                    (trialinblock_c||subject),
#                  data=choice,
#                  family='bernoulli',
#                  seed = 2022,
#                  prior = set_prior("normal(0,1)", class = "b"),
#                  chains = 3, iter = 5000,
#                  sample_prior = T, file='tmp')
# 
# saveRDS(choice_mod, 'out/choice_model_b.rds')

choice_mod = readRDS('out/choice_model_b.rds')

describe_posterior(choice_mod)
h = hypothesis(choice_mod, hypothesis = 'mutual_info_c > 0') 
post = data.frame(choice_mod)
file.remove('tmp.rds')

# mod = glmer(choose_choice ~ mutual_info_c * trialinblock_c + (trialinblock_c||prol_id),
#             family='binomial', data=choice)
# summary(mod)

### visualize model ----

pdf('figs/choice_mod_mi_density.pdf', width=6, height=6)

mi_post = post$b_mutual_info_c
pval = 1-mean(mi_post > 0)
cri  = quantile(mi_post, c(.025, .975))

dens = density(mi_post)
xlim = range(pretty(c(dens$x-.2, dens$x+.1)))
plot(dens, lwd=3, xlab = 'Coefficient', main='mutual_info_c', xlim=xlim)
polygon(dens$x, dens$y, col=scales::alpha('black',.25))
abline(v=0, lty=2)
legend('topleft', bty='n', legend=paste0('P = ',round(pval,2),
                                         '\nCI = [',round(cri[1],2), ', ', round(cri[2],2),']',
                                         '\nBF = ',round(h$hypothesis$Evid.Ratio,2)))

dev.off()

pdf('figs/choice_mod_trial_X_mi_density.pdf', width=6, height=6)

mi_post = post$b_mutual_info_c.trialinblock_c
pval = 1-mean(mi_post > 0)
cri  = quantile(mi_post, c(.025, .975))

dens = density(mi_post)
xlim = range(pretty(c(dens$x-.01, dens$x+.01)))
plot(dens, lwd=3, xlab = 'Coefficient', main='mutual_info_c:trialinblock_c', xlim=xlim)
polygon(dens$x, dens$y, col=scales::alpha('black',.25))
abline(v=0, lty=2)
legend('topleft', bty='n', legend=paste0('P = ',round(pval,2),
                                         '\nCI = [',round(cri[1],2), ', ', round(cri[2],2),']',
                                         '\nBF = ',round(h$hypothesis$Evid.Ratio,2)))

dev.off()

pdf('figs/choice_mod_ctbl_X_mi_density.pdf', width=6, height=6)

mi_post = post$b_mutual_info_c.ctbl_c
pval = mean(mi_post > 0)
cri  = quantile(mi_post, c(.025, .975))

dens = density(mi_post)
xlim = range(pretty(c(dens$x-.01, dens$x+.01)))
plot(dens, lwd=3, xlab = 'Coefficient', main='mutual_info_c:ctbl_c', xlim=xlim)
polygon(dens$x, dens$y, col=scales::alpha('black',.25))
abline(v=0, lty=2)
legend('topleft', bty='n', legend=paste0('P = ',round(pval,2),
                                         '\nCI = [',round(cri[1],2), ', ', round(cri[2],2),']',
                                         '\nBF = ',round(h$hypothesis$Evid.Ratio,2)))

dev.off()

# Confidence ratings ------------------------------------------------------

conf = choice[choice$is_mock=='true', ]
conf$mock_rating = as.numeric(conf$mock_rating)

pdf('figs/hist_of_conf_trials.pdf', width=6, height=6)
hist(conf$trialinblock, main='Trials in which confidence is rated', xlab='Trial num.', xlim=c(0,60))
dev.off()

# points of confidence
pdf('figs/points_of_conf_trials.pdf', width=6, height=6)

mock = ifelse(choice$is_mock=='true', 0, NA)
mi = tapply(choice$mutual_info, choice$trial, mean)
ylim = c(0,1)

plot(mi, type='l', lty=2, lwd=3, xlab='Trial', ylab='Mutual Information')
points(mock, pch=18, cex=2, col='red')

legend('topleft', bty='n', pch=18, pt.cex=2, col='red', legend='Rating Trial')

dev.off()


### barplot -----
pdf('figs/conf_bar.pdf', width=6, height=6)

pchoice  = tapply(conf$mock_rating, list(conf$win_card, conf$mutual_info), mean, na.rm=T)
cichoice = tapply(conf$mock_rating, list(conf$win_card, conf$mutual_info), ci)
rownames(pchoice) = c('Card < 5','Card > 5')
ylim = c(0,100)

b = barplot(pchoice, beside=T, xlab='MI', ylab='Confidence Rating', ylim=ylim, xpd=F, 
            legend.text = T, args.legend = list(x='topleft', bty='n'))
arrows(b, pchoice-cichoice, b, pchoice+cichoice, length=0)

dev.off()

### barplot (by choice) ----

pdf('figs/conf_bar_by_choice.pdf', width=8, height=4)
layout(matrix(1:2,1,2))

pchoice  = tapply(conf$mock_rating, list(conf$win_card, conf$mutual_info, conf$choose_choice), mean, na.rm=T)
cichoice = tapply(conf$mock_rating, list(conf$win_card, conf$mutual_info, conf$choose_choice), ci)
rownames(pchoice) = c('Card < 5','Card > 5')
ylim = c(0,100)

b = barplot(pchoice[,,1], beside=T, xlab='MI', ylab='Confidence Rating', ylim=ylim, xpd=F, 
            legend.text = T, args.legend = list(x='topleft', bty='n'), main='No Choice')
arrows(b, pchoice[,,1]-cichoice[,,1], b, pchoice[,,1]+cichoice[,,1], length=0)

b = barplot(pchoice[,,2], beside=T, xlab='MI', ylab='Confidence Rating', ylim=ylim, xpd=F, main='Choice')
arrows(b, pchoice[,,2]-cichoice[,,2], b, pchoice[,,2]+cichoice[,,2], length=0)

dev.off()

### time course -----

nbin = 6
tbin = cut(conf$trialinblock, nbin, labels = F)
blab = floor(seq(min(conf$trialinblock),max(conf$trialinblock)+1,length.out=3))

pchoice  = tapply(conf$mock_rating, list(tbin, conf$mutual_info, conf$win_card), mean, na.rm=T)
cichoice = tapply(conf$mock_rating, list(tbin, conf$mutual_info, conf$win_card), ci)
ylim = c(min(pchoice-cichoice-10, na.rm=T), max(pchoice+cichoice+10, na.rm=T))

# Lose
pdf('figs/conf_time_lose.pdf', width=6, height=6)

matplot(pchoice[,,1], type='l', lty=1, lwd=3, col=c('darkred','darkblue'), 
        xlab='Trial', ylab='Confidence', main='Card < 5',
        xaxt='n', ylim=ylim)
axis(1, at=seq(1,nbin,length.out=3), labels=blab)

l = pchoice[,,1]-cichoice[,,1]
u = pchoice[,,1]+cichoice[,,1]
polygon(c(1:nbin, nbin:1), c(l[,1], rev(u[,1])), col = scales::alpha('darkred', 0.25), lty = 0)
polygon(c(1:nbin, nbin:1), c(l[,2], rev(u[,2])), col = scales::alpha('darkblue', 0.25), lty = 0)
legend('topleft', bty='n', lty=1, lwd=3, col=c('darkred','darkblue'), legend=c(0,1), title='MI')

dev.off()

# Win
pdf('figs/conf_time_win.pdf', width=6, height=6)

matplot(pchoice[,,2], type='l', lty=1, lwd=3, col=c('darkred','darkblue'), 
        xlab='Trial', ylab='Confidence', main='Win',
        xaxt='n', ylim=ylim)
axis(1, at=seq(1,nbin,length.out=3), labels=blab)

l = pchoice[,,2]-cichoice[,,2]
u = pchoice[,,2]+cichoice[,,2]
polygon(c(1:nbin, nbin:1), c(l[,1], rev(u[,1])), col = scales::alpha('darkred', 0.25), lty = 0)
polygon(c(1:nbin, nbin:1), c(l[,2], rev(u[,2])), col = scales::alpha('darkblue', 0.25), lty = 0)
legend('bottomright', bty='n', lty=1, lwd=3, col=c('darkred','darkblue'), legend=c(0,1), title='MI')

dev.off()

### time course (all) -----

mconf  = tapply(conf$mock_rating, list(conf$trial, conf$mutual_info, conf$win_card), mean)
mi = tapply(conf$mutual_info, conf$trial, mean)
ylim = c(0,125)

# Lose
pdf('figs/conf_trial_lose.pdf', width=6, height=6)

matplot(mconf[,,1], type='l', lty=1, lwd=3, col=c('darkred','darkblue'), 
        xlab='Trial', ylab='Average Confidence', main='Card < 5',
        ylim=ylim, yaxt='n')

axis(2, at=seq(0,100,by=25))
# lines(mi*100, lwd=3, lty=2)
axis(4, at=c(0,100), labels = c(0,1))

legend('topleft', bty='n', lty=1, lwd=3, col=c('darkred','darkblue'), ncol=2, legend=c(0,1), title='MI')

dev.off()


# Win
pdf('figs/conf_trial_win.pdf', width=6, height=6)

matplot(mconf[,,2], type='l', lty=1, lwd=3, col=c('darkred','darkblue'), 
        xlab='Trial', ylab='Average Confidence', main='Card > 5',
        ylim=ylim, yaxt='n')

axis(2, at=seq(0,100,by=25))
# lines(mi*100, lwd=3, lty=2)
axis(4, at=c(0,100), labels = c(0,1))

legend('topleft', bty='n', lty=1, lwd=3, col=c('darkred','darkblue'), ncol=2, legend=c(0,1), title='MI')

dev.off()

## Model -----

conf$trialinblock_c = conf$trialinblock-median(conf$trialinblock)
conf$mutual_info_c  = conf$mutual_info - .5
conf$win_card_c     = as.numeric(conf$win_card) - .5

# conf_mod = brm(mock_rating ~ mutual_info_c * trialinblock_c * win_card_c + (trialinblock_c||subject),
#                data=conf,
#                seed = 2022,
#                chains = 3, iter = 5000,
#                sample_prior = T, file='tmp')
# 
# saveRDS(conf_mod, 'out/conf_model_b.rds')

conf_mod = readRDS('out/conf_model_b.rds')

describe_posterior(conf_mod)
post = data.frame(conf_mod)
file.remove('tmp.rds')


### visualize model ----

pdf('figs/conf_mod_mi_density.pdf', width=6, height=6)

h = hypothesis(conf_mod, hypothesis = 'mutual_info_c > 0') 

mi_post = post$b_mutual_info_c
pval = 1-mean(mi_post > 0)
cri  = quantile(mi_post, c(.025, .975))

dens = density(mi_post)
xlim = range(pretty(c(dens$x-.1, dens$x+.1)))
plot(dens, lwd=3, xlab = 'Coefficient', main='mutual_info_c', xlim=xlim)
polygon(dens$x, dens$y, col=scales::alpha('black',.25))
abline(v=0, lty=2)
legend('topleft', bty='n', legend=paste0('P = ',round(pval,2),
                                         '\nCI = [',round(cri[1],2), ', ', round(cri[2],2),']',
                                         '\nBF = ',round(h$hypothesis$Evid.Ratio,2)))

dev.off()

pdf('figs/conf_mod_mi_X_win_card_density.pdf', width=6, height=6)

h = hypothesis(conf_mod, hypothesis = 'mutual_info_c:win_card_c > 0') 

mi_post = post$b_mutual_info_c.win_card_c
pval = 1-mean(mi_post > 0)
cri  = quantile(mi_post, c(.025, .975))

dens = density(mi_post)
xlim = range(pretty(c(dens$x-.1, dens$x+.1)))
plot(dens, lwd=3, xlab = 'Coefficient', main='mutual_info_c:win_card_c', xlim=xlim)
polygon(dens$x, dens$y, col=scales::alpha('black',.25))
abline(v=0, lty=2)
legend('topleft', bty='n', legend=paste0('P = ',round(pval,2),
                                         '\nCI = [',round(cri[1],2), ', ', round(cri[2],2),']',
                                         '\nBF = ',round(h$hypothesis$Evid.Ratio,2)))

dev.off()


# Sense of Control Ratings ------------------------------------------------

pdf('figs/SoC_bar.pdf', width=6, height=6)

soc   = choice[!duplicated(dat$subject),]
msoc  = c(mean(soc$sense_of_control_B, na.rm=T), mean(soc$sense_of_control_A, na.rm=T))
cisoc = c(ci(soc$sense_of_control_B), ci(soc$sense_of_control_A))

b = barplot(msoc, ylim=c(0,100), names.arg = c(0, 1), xlab='MI', ylab='Sense of Control')
arrows(b, msoc-cisoc, b, msoc+cisoc, length=0)

dev.off()

## Model ----
tmp = reshape2::melt(soc[,c('subject','sense_of_control_A', 'sense_of_control_B')])
tmp$deck_f = as.numeric(tmp$variable=='sense_of_control_A')

# soc_mod = brm(value ~  deck_f,
#               data=tmp,
#               seed = 2022,
#               chains = 3, iter = 5000,
#               sample_prior = T, file='tmp')
# 
# saveRDS(soc_mod, 'out/soc_model_b.rds')

soc_mod = readRDS('out/soc_model_b.rds')

describe_posterior(soc_mod)
h = hypothesis(soc_mod, hypothesis = 'deck_f > 0') 
post = data.frame(soc_mod)
file.remove('tmp.rds')

### visualize model ----

pdf('figs/soc_mod_mi_density.pdf', width=6, height=6)

mi_post = post$b_deck_f
pval = 1-mean(mi_post > 0)
cri  = quantile(mi_post, c(.025, .975))

dens = density(mi_post)
xlim = range(pretty(c(dens$x-.1, dens$x+.1)))
plot(dens, lwd=3, xlab = 'Coefficient', main='sense_of_control', xlim=xlim)
polygon(dens$x, dens$y, col=scales::alpha('black',.25))
abline(v=0, lty=2)
legend('topleft', bty='n', legend=paste0('P = ',round(pval,2),
                                         '\nCI = [',round(cri[1],2), ', ', round(cri[2],2),']',
                                         '\nBF = ',round(h$hypothesis$Evid.Ratio,2)))

dev.off()


# Save data for stan model ------------------------------------------------------------

write.csv(choice, 'models/choice_exp2_clean.csv')



# Look at correlation between VoC and MI effect ---------------------------

voc  = tapply(choice$choose_choice, list(choice$subject, choice$mutual_info), mean)
dvoc = voc[,2] - voc[,1]

plot(rowMeans(voc), dvoc)

