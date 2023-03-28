library(rstan)
rstan_options(auto_write = TRUE)
library(bayestestR)
library(LaplacesDemon)
library(latex2exp) # good for plots

# load data and clean ---------------------------------------------------------------

choice = read.csv('choice_data.csv')

pair_mapping = list('AB'=c('10','11'), 
                    'AC'=c('11','10'), 
                    'AD'=c('10','10'), 
                    'BC'=c('10','01'),
                    'BD'=c('00','01'),
                    'CD'=c('10','00'))

choice$choice_pair = sapply(choice$pair, function(i) pair_mapping[[i]][1])
choice$mi_pair     = sapply(choice$pair, function(i) pair_mapping[[i]][2])

deck_mapping = list('A'=c(1,1), 'B'=c(0,1), 'C'=c(1,0), 'D'=c(0,0))

# create matrix
choice = choice[choice$chosen_deck!='',]
unique(choice$chosen_deck)

N = length(unique(choice$subject))
J = table(choice$subject)

C_L         = matrix(999, N, max(J), byrow=T) 
C_R         = matrix(999, N, max(J), byrow=T) 
M_L         = matrix(999, N, max(J), byrow=T) 
M_R         = matrix(999, N, max(J), byrow=T) 
deck_l      = matrix(999, N, max(J), byrow=T) 
deck_r      = matrix(999, N, max(J), byrow=T) 
choose_L    = matrix(999, N, max(J), byrow=T) 
chosen_deck = matrix(999, N, max(J), byrow=T) 
reward      = matrix(999, N, max(J), byrow=T) 
for(i in 1:N) { 
  tmp = choice[choice$subject==unique(choice$subject)[i],]
  
  C_L[i,1:J[i]]         = as.vector(sapply(tmp$deck_l, function(i) deck_mapping[[i]][1]))
  C_R[i,1:J[i]]         = as.vector(sapply(tmp$deck_r, function(i) deck_mapping[[i]][1]))
  M_L[i,1:J[i]]         = as.vector(sapply(tmp$deck_l, function(i) deck_mapping[[i]][2]))
  M_R[i,1:J[i]]         = as.vector(sapply(tmp$deck_r, function(i) deck_mapping[[i]][2]))
  deck_l[i,1:J[i]]      = as.numeric(as.factor(tmp$deck_l))
  deck_r[i,1:J[i]]      = as.numeric(as.factor(tmp$deck_r))
  choose_L[i,1:J[i]]    = as.numeric(tmp$chosen_deck==tmp$deck_l)
  chosen_deck[i,1:J[i]] = as.numeric(as.factor(tmp$chosen_deck))
  reward[i,1:J[i]]      = tmp$outcome
  
}

stan_dat = list(N=N,J=J,C_L=C_L,C_R=C_R,M_L=M_L,M_R=M_R,
                deck_l=deck_l,deck_r=deck_r,choose_L=choose_L,
                chosen_deck=chosen_deck,reward=reward)

# centred equivalent
stan_dat_c = stan_dat
stan_dat_c$C_L = stan_dat_c$C_L-.5
stan_dat_c$C_R = stan_dat_c$C_R-.5
stan_dat_c$M_L = stan_dat_c$M_L-.5
stan_dat_c$M_R = stan_dat_c$M_R-.5

# Fit, and compare, models ----

## alpha, beta ----
mod_rl_only = stan(file='rl_only.stan', data=stan_dat,
                   chains = 1,
                   iter = 5000, warmup = 1000,
                   sample_file = 'samples/rl_only_samples',
                   diagnostic_file = 'diag/diag_rl_only_diag',
                   seed=2022
)
# mod_rl_only = read_stan_csv('samples/rl_only_samples')

post_rl_only = as.data.frame(mod_rl_only)
ll = post_rl_only[,grepl('log_lik',names(post_rl_only))]
ll = ll[,!is.na(ll[1,])] # remove NAs
post_rl_only = post_rl_only[,!grepl('log_lik',names(post_rl_only))]

WAIC_rl_only    = WAIC(t(ll))
gewekes_rl_only = unlist(sapply(post_rl_only, coda::geweke.diag)[1,])

# save object 
out = list(posterior = post_rl_only, lppd = ll, WAIC = WAIC_rl_only, Gewekes_z = gewekes_rl_only)
saveRDS(out, file='summary/rl_only_summary.rds')


## alpha, beta, phi_c, phi_m ----
mod_choice_only = stan(file='rl_choice_only.stan', data=stan_dat,
                       chains = 1,
                       iter = 5000, warmup = 1000,
                       sample_file = 'samples/choice_only_samples',
                       diagnostic_file = 'diag/diag_choice_only_diag',
                       seed=2022
)
# mod_choice_only = read_stan_csv('samples/choice_only_samples')

post_choice_only = as.data.frame(mod_choice_only)
ll = post_choice_only[,grepl('log_lik',names(post_choice_only))]
ll = ll[,!is.na(ll[1,])] # remove NAs
post_choice_only = post_choice_only[,!grepl('log_lik',names(post_choice_only))]

WAIC_choice_only    = WAIC(t(ll))
gewekes_choice_only = unlist(sapply(post_choice_only, coda::geweke.diag)[1,])

# save object 
out = list(posterior = post_choice_only, lppd = ll, WAIC = WAIC_choice_only, Gewekes_z = gewekes_choice_only)
saveRDS(out, file='summary/choice_only_summary.rds')

## alpha, beta, phi_c, phi_m, phi_cm ----

mod_choice_only_int = stan(file='rl_choice_only_int.stan', data=stan_dat_c,
                           chains = 1,
                           iter = 5000, warmup = 1000,
                           sample_file = 'choice_only_int_samples',
                           diagnostic_file = 'diag_choice_only_int_diag',
                           seed=2022
)
# mod_choice_only_int = read_stan_csv('samples/choice_only_int_samples')

post_choice_only_int = as.data.frame(mod_choice_only_int)
ll = post_choice_only_int[,grepl('log_lik',names(post_choice_only_int))]
ll = ll[,!is.na(ll[1,])] # remove NAs
post_choice_only_int = post_choice_only_int[,!grepl('log_lik',names(post_choice_only_int))]

WAIC_choice_only_int    = WAIC(t(ll))
gewekes_choice_only_int = unlist(sapply(post_choice_only_int, coda::geweke.diag)[1,])

summary(mod_choice_only_int, pars=c('A','BETA','PHI_C','PHI_M','PHI_CM'), use_cache=F)

# save object 
out = list(posterior = post_choice_only_int, lppd = ll, WAIC = WAIC_choice_only_int, Gewekes_z = gewekes_choice_only_int)
saveRDS(out, file='choice_only_int_summary.rds')

## alpha, beta, eta_c, eta_m ----

mod_learn_only = stan(file='rl_learn_only.stan', data=stan_dat,
                      chains = 1,
                      iter = 5000, warmup = 1000,
                      sample_file = 'samples/learn_only_samples',
                      diagnostic_file = 'diag/diag_learn_only_diag',
                      seed=2022
)
# mod_learn_only = read_stan_csv('samples/learn_only_samples')

post_learn_only = as.data.frame(mod_learn_only)
ll = post_learn_only[,grepl('log_lik',names(post_learn_only))]
ll = ll[,!is.na(ll[1,])] # remove NAs
post_learn_only = post_learn_only[,!grepl('log_lik',names(post_learn_only))]

WAIC_learn_only    = WAIC(t(ll))
gewekes_learn_only = unlist(sapply(post_learn_only, coda::geweke.diag)[1,])

# save object 
out = list(posterior = post_learn_only, lppd = ll, WAIC = WAIC_learn_only, Gewekes_z = gewekes_learn_only)
saveRDS(out, file='summary/learn_only_summary.rds')

## alpha, beta, eta_c, eta_m, eta_cm ----

mod_learn_only_int = stan(file='rl_learn_only_int.stan', data=stan_dat_c,
                      chains = 1,
                      iter = 5000, warmup = 1000,
                      sample_file = 'samples/learn_only_int_samples',
                      diagnostic_file = 'diag/diag_learn_only_int_diag',
                      seed=2022
                      )
# mod_learn_only_int = read_stan_csv('samples/learn_only_int_samples')
summary(mod_learn_only_int, pars=c('A','BETA','ETA_C','ETA_M','ETA_CM'), use_cache=F)

post_learn_only_int = as.data.frame(mod_learn_only_int)
ll = post_learn_only_int[,grepl('log_lik',names(post_learn_only_int))]
ll = ll[,!is.na(ll[1,])] # remove NAs
post_learn_only_int = post_learn_only_int[,!grepl('log_lik',names(post_learn_only_int))]

WAIC_learn_only_int    = WAIC(t(ll))
gewekes_learn_only_int = unlist(sapply(post_learn_only_int, coda::geweke.diag)[1,])

# save object 
out = list(posterior = post_learn_only_int, lppd = ll, WAIC = WAIC_learn_only_int, Gewekes_z = gewekes_learn_only_int)
saveRDS(out, file='summary/learn_only_int_summary.rds')


## alpha, beta, phi_c, phi_m, eta_c, eta_m ----

mod_all = stan(file='rl_all.stan', data=stan_dat,
               chains = 1,
               iter = 5000, warmup = 1000,
               sample_file = 'samples/all_samples',
               diagnostic_file = 'diag/diag_all_diag',
               seed=2022
)
# mod_all = read_stan_csv('samples/all_samples')
summary(mod_all, pars=c('A','BETA','PHI_C', 'PHI_M', 'ETA_C', 'ETA_M'), use_cache=F)

post_all = as.data.frame(mod_all)
ll = post_all[,grepl('log_lik',names(post_all))]
ll = ll[,!is.na(ll[1,])] # remove NAs
post_all = post_all[,!grepl('log_lik',names(post_all))]

WAIC_all    = WAIC(t(ll))
gewekes_all = unlist(sapply(post_all, coda::geweke.diag)[1,])

# save object 
out = list(posterior = post_all, lppd = ll, WAIC = WAIC_all, Gewekes_z = gewekes_all)
saveRDS(out, file='summary/all_summary.rds')

## alpha, beta, phi_c, phi_m, phi_cm, eta_c, eta_m, eta_cm ----

mod_all_int = stan(file='rl_all_int.stan', data=stan_dat_c,
               chains = 1,
               iter = 5000, warmup = 1000,
               sample_file = 'samples/all_samples',
               diagnostic_file = 'diag/diag_all_int_diag',
               seed=2022
               )
# mod_all_int = read_stan_csv('samples/all_samples')
summary(mod_all_int, pars=c('A','BETA','PHI_C', 'PHI_M','PHI_CM', 'ETA_C', 'ETA_M', 'ETA_CM'), use_cache=F)

post_all_int = as.data.frame(mod_all_int)
ll = post_all_int[,grepl('log_lik',names(post_all_int))]
ll = ll[,!is.na(ll[1,])] # remove NAs
post_all_int = post_all_int[,!grepl('log_lik',names(post_all_int))]

WAIC_all_int    = WAIC(t(ll))
gewekes_all_int = unlist(sapply(post_all_int, coda::geweke.diag)[1,])

# save object 
out = list(posterior = post_all_int, lppd = ll, WAIC = WAIC_all_int, Gewekes_z = gewekes_all_int)
saveRDS(out, file='summary/all_summary.rds')


# Save image --------------------------------------------------------------

save.image('exp1_with_mods.RData')

# Analyze winning model ----------------------------------------------------

## Plot WAIC values ----

waics = c(WAIC_rl_only$WAIC, 
          WAIC_choice_only$WAIC, WAIC_choice_only_int$WAIC,
          WAIC_learn_only$WAIC, WAIC_learn_only_int$WAIC,
          WAIC_all$WAIC, WAIC_all_int$WAIC)
names(waics) = c('RL Only', 
                 'Choice Only', 'Choice Only (+ Int)',
                 'Learn Only', 'Learn Only (+ Int)',
                 'Choice + Learn', 'Choice + Learn (+ Int)')
waics = sort(waics)
ylim = c(min(waics)-250, max(waics)+250)

pdf('figs/WAIC.pdf', 6, 6)
bp = barplot(waics, ylim=ylim, xpd=F, ylab='WAIC', xaxt='n', yaxt='n')
axis(2, at=ylim, labels = c('Better\nFit', 'Worse\nFit'))
text(bp, min(waics)-275, labels = names(waics), cex = 0.75,srt = 45, adj = 1, xpd = TRUE)
dev.off()

# winning model is choice + learn
mod  = mod_all_int
post = as.data.frame(mod)
post = post[,!grepl('log_lik',names(post))]
pars = c('A','BETA','PHI_C','PHI_M','PHI_CM','ETA_C','ETA_M', 'ETA_CM')
titles = c('A'=TeX("$\\alpha$"), 'BETA'=TeX('$\\beta$'), 
           'PHI_C'=TeX("$\\phi_C$"), 'PHI_M'=TeX("$\\phi_{MI}$"), 'PHI_CM'=TeX("$\\phi_{C:MI}$"), 
           'ETA_C'=TeX("$\\eta_C$"), 'ETA_M'=TeX("$\\eta_{MI}$"), 'ETA_CM'=TeX("$\\eta_{C:MI}$"))

sum_win = summary(mod, pars=pars, use_cache=F)
write.csv(sum_win, 'summary/fixef_summary_winning_mod.csv')

## Visualize individual densities ----
for(p in pars) {
  pdf(paste0('figs/density_',p,'.pdf'), 6, 6)
  dens = density(post[,p])
  plot(dens, lwd=2, xlab='Value', main=titles[p], cex.lab=1.5, cex.axis=1.5, cex.main=2.5)
  polygon(dens$x, dens$y, col=scales::alpha('black', .25))
  dev.off()
}


## Compare Phi_M and Phi_C -----

#### density ---- 
pdf('figs/VoC_vs_VoMI_density.pdf', 6, 6)

dens1 = density(post$PHI_C)
dens2 = density(post$PHI_M)

dens1$y0 = dens1$y/max(dens1$y)
dens2$y0 = dens2$y/max(dens2$y)

xlim = c(min(c(dens1$x, dens2$x))-.1, max(c(dens1$x, dens2$x))+.1)

plot(NULL, xlim=xlim, ylim=c(0,1), ylab='Normalized Density', xlab='Parameter Value')

lines(dens1$x, dens1$y0, col='red', lwd=2)
polygon(dens1$x, dens1$y0, col=scales::alpha('red', 0.25))

lines(dens2$x, dens2$y0, col='blue', lwd=2)
polygon(dens2$x, dens2$y0, col=scales::alpha('blue', 0.25))

legend('topright', bty='n', fill=scales::alpha(c('red','blue'),.25),
       legend = c(expression(phi[C]),
                  expression(phi[MI])))

P = 1-mean(sort(post$PHI_M) > sort(post$PHI_C,decreasing = T))
text(xlim[2]-.5, .5, paste0('P = ', round(P, 2)))

dev.off()


#### barplot ------

pdf('figs/VoC_vs_VoMI_barplot.pdf', 6, 6)

means = sum_win$summary[c('PHI_C','PHI_M'), 'mean']
lb    = sum_win$summary[c('PHI_C','PHI_M'), '2.5%']
ub    = sum_win$summary[c('PHI_C','PHI_M'), '97.5%']
ylim  = c(min(lb-.2), max(ub+.2))

b = barplot(means, ylim=ylim, ylab='MAP Parameter Value', 
            names.arg = c(expression(phi[C]),expression(phi[MI])), border = NA, col=NA, 
            main='Posterior MAP')
points(b, means, pch=18, cex=3.5, col=c('red','blue'))
arrows(b, c(lb[1],lb[2]), b, c(ub[1],ub[2]), length=0, col=c('red','blue'))

dev.off()

#### compare SD ------
pdf('figs/VoC_vs_VoMI_SD_barplot.pdf', 6, 6)

means = sum_win$summary[c('PHI_C','PHI_M'), 'sd']

b = barplot(means, ylim=c(min(means)-.1, max(means)+.1), ylab='Posterior SD', 
            names.arg = c(expression(phi[C]),expression(phi[MI])), border = NA, col=NA, 
            main='Posterior SD')
points(b, means, pch=18, cex=3.5, col=c('red','blue'))

dev.off()

#### Raneff correlations ----

pdf('figs/phi_c_phi_m_raneff_corr.pdf', 6, 6)

phi_c_raneff = colMeans(post[,grepl('phi_c\\[.*?\\]', names(post))]) 
phi_m_raneff = colMeans(post[,grepl('phi_m\\[.*?\\]', names(post))])
xlim = c(min(phi_c_raneff)-.5, max(phi_c_raneff)+.5)
ylim = c(min(phi_m_raneff)-.5, max(phi_m_raneff)+.5)

plot(phi_c_raneff, phi_m_raneff, 
     cex=1, pch=3, col='grey', 
     xlab = expression('Random Effects for '~phi[C]), 
     ylab = expression('Random Effects for '~phi[MI]),
     xlim=xlim, ylim=ylim)
tmp  = lm(phi_c_raneff ~ phi_m_raneff)
abline(tmp, col='red', lwd=3)
rs  = BayesFactor::correlationBF(phi_c_raneff, phi_m_raneff, posterior = T, iter=5000)[,'rho']
P   = 1-mean(rs>0)
legend('bottomright', bty='n', legend=paste0('r = ',round(median(rs), 2), '\nP = ', round(P,2)))

dev.off()


