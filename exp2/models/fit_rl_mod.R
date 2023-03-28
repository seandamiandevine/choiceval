
rm(list=ls())
library(rstan)
library(loo)
source('fx/format_stan.R')
source('fx/corrplot_bayes.R')
options(mc.cores=2)

# Load data ---------------------------------------------------------------

dat = read.csv('choice_exp2_clean.csv')
dat = dat[dat$chosen_deck!='null' & dat$chosen_card!='null' ,
          c('prol_id','mutual_info', 'chosen_deck','choose_choice',
            'win_card', 'is_mock', 'mock_rating', 'outcome', 'trialinblock')]
dat$is_mock = ifelse(dat$is_mock=='null', NA, ifelse(dat$is_mock=='true', 1, 0))
dat$mock_rating = as.numeric(dat$mock_rating)
dat$chosen_deck = as.numeric(dat$chosen_deck=='B') + 1

# matrix of MI prediction errors
  # 1 = win card & reward    | lose card & no reward
  # 0 = win_card & no reward | lose card & reward
mi_pe_mat = diag(1,2)
dat$mi_pe = sapply(1:nrow(dat), function(i) mi_pe_mat[dat$outcome[i]+1, dat$win_card[i]+1])

stan_dat = stan_format(dat)


# Fit intercept model (no Voc or MI) --------------------------------------

# b_mod_0 = stan(data=stan_dat, file = 'stan/rl_mod_0.stan',
#                iter=4000, warmup=1000, chains=2,
#                sample_file = 'stan/samples/rl_mod_0_samples',
#                diagnostic_file = 'stan/diag/rl_mod_0_diag'
#                )
# 
# saveRDS(b_mod_0, 'stan/out/b_mod_0.rds')
# loo_0 = rstan::loo(b_mod_0)
# sum = summary(b_mod_0, pars=c('mu_alpha','mu_beta', 'rho'), use_cache=F)$summary
# sum   = cbind(sum, looic=loo_0$estimates['looic','Estimate'], looic_se = loo_0$estimates['looic','SE'])
# write.csv(sum,'stan/out/b_mod_0_summary_fe.csv')

b_mod_0 = readRDS('stan/out/b_mod_0.rds')
sum_0   = read.csv('stan/out/b_mod_0_summary_fe.csv')


# Fit model with no MI learning -------------------------------------------

# b_mod_no_mi = stan(data=stan_dat, file = 'stan/rl_mod_no_learn.stan',
#                  iter=4000, warmup=1000, chains=2,
#                  sample_file = 'stan/samples/rl_mod_no_learn_samples',
#                  diagnostic_file = 'stan/diag/rl_mod_no_learn_diag'
#                  )

# saveRDS(b_mod_no_mi, 'stan/out/b_mod_no_mi.rds')
# loo_no_mi = rstan::loo(b_mod_no_mi)
# sum = summary(b_mod_no_mi, pars=c('mu_alpha','mu_beta', 'mu_phi_c', 'rho'), use_cache=F)$summary
# sum   = cbind(sum, looic=loo_no_mi$estimates['looic','Estimate'], looic_se = loo_no_mi$estimates['looic','SE'])
# write.csv(sum,'stan/out/b_mod_no_mi_summary_fe.csv')

b_mod_no_mi = readRDS('stan/out/b_mod_no_mi.rds')
sum_no_mi   = read.csv('stan/out/b_mod_no_mi_summary_fe.csv')

# Fit model with MI, but no learning -------------------------------------------

# b_mod_mi_no_learn = stan(data=stan_dat, file = 'stan/rl_mod_mi_no_learn.stan',
#                          iter=4000, warmup=1000, chains=2,
#                          sample_file = 'stan/samples/rl_mod_mi_no_learn_samples',
#                          diagnostic_file = 'stan/diag/rl_mod_mi_no_learn_diag'
#                          )
# 
# saveRDS(b_mod_mi_no_learn, 'stan/out/b_mod_mi_no_learn.rds')
# loo_mi_no_learn = rstan::loo(b_mod_mi_no_learn)
# sum = summary(b_mod_mi_no_learn, pars=c('mu_alpha','mu_beta', 'mu_phi_c', 'mu_phi_mc', 'rho'), use_cache=F)$summary
# sum   = cbind(sum, looic=loo_mi_no_learn$estimates['looic','Estimate'], looic_se = loo_mi_no_learn$estimates['looic','SE'])
# write.csv(sum,'stan/out/b_mod_mi_no_learn_summary_fe.csv')

b_mod_mi_no_learn = readRDS('stan/out/b_mod_mi_no_learn.rds')
sum_mi_no_learn   = read.csv('stan/out/b_mod_mi_no_learn_summary_fe.csv')

# Fit model with MI and learning ----------------------------------------------

# b_mod_mi = stan(data=stan_dat, file = 'stan/rl_mod_learn.stan',
#                    iter=4000, warmup=1000, chains=2,
#                    sample_file = 'stan/samples/rl_mod_learn_samples',
#                    diagnostic_file = 'stan/diag/rl_mod_learn_diag',
#                    init = '0'
#                 )
# 
# saveRDS(b_mod_mi, 'stan/out/b_mod_mi.rds')
# loo_mi = rstan::loo(b_mod_mi)
# sum    = summary(b_mod_mi, pars=c('mu_alpha','mu_beta', 'mu_phi_c', 'mu_eta', 'mu_phi_mc'), use_cache=F)$summary
# sum    = cbind(sum, looic=loo_mi$estimates['looic','Estimate'], looic_se = loo_mi$estimates['looic','SE'])
# write.csv(sum,'stan/out/b_mod_mi_summary_fe.csv')

b_mod_mi = readRDS('stan/out/b_mod_mi.rds')
sum_mi   = read.csv('stan/out/b_mod_mi_summary_fe.csv')


# Visualize fits ----------------------------------------------------------

pdf('../figs/stan/LOO.pdf', 6, 6)

loos = c(sum_0$looic[1], sum_no_mi$looic[1], sum_mi_no_learn$looic[1], sum_mi$looic[1])
ses  = c(sum_0$looic_se[1], sum_no_mi$looic_se[1], sum_mi_no_learn$looic_se[1], sum_mi$looic_se[1])
ylim = range(pretty(c(loos-ses, loos+ses)))

b = barplot(loos, ylim=ylim, xpd=F, ylab=c('PSIS LOO-CV'), 
            names.arg = c('Null',
                          expression(phi[c]~'only'), 
                          expression(phi[C]+phi[M]), 
                          expression(phi[C]+phi[M]+eta)))
arrows(b, loos+ses, b, loos-ses, length=0)

dev.off()

# choose winning model
mod = c(b_mod_0, b_mod_no_mi, b_mod_mi_no_learn, b_mod_mi)[which.min(loos)][[1]]


# Visualize parameters ----------------------------------------------------

norm_dens = function(x) {
  d = density(x)
  d$y0 = d$y/max(d$y)
  d
}
pars = c('mu_alpha','mu_beta', 'mu_phi_c', 'mu_eta', 'mu_phi_mc')
titles = list('mu_alpha' = expression(a), 'mu_beta' = expression(beta), 
              'mu_phi_c' = expression(phi[c]), 'mu_eta' = expression(eta), 
              'mu_phi_mc' = expression(phi[m]))

for(p in pars) {
  
  pdf(paste0('../figs/stan/',p,'_dens.pdf'), 6, 6)
  
  v  = extract(mod, p)[[1]]
  d  = norm_dens(v)
  P  = mean(v > 0)
  P  = ifelse(P > .5, 1-P, P)
  ci = bayestestR::hdi(v)
  xlim = c(min(v)-sd(v), max(v)+sd(v))
  
  plot(d$x, d$y0, xlim=xlim, type='l', ylab='Norm. Density', main=titles[[p]], xlab='')
  polygon(c(d$x[d$x>0], max(d$x)), c(0, d$y0[d$x>0]), col=scales::alpha('darkblue', .5))
  
  legend('topright', bty='n', legend=paste0('P = ', round(P,4), 
                                            '\nCI = [',round(ci$CI_low,2),', ',round(ci$CI_high,2),']'))
  
  dev.off()
  
}


# Visualize correlations --------------------------------------------------


corplot_bayes(mod, 'rho', 'mu', file='../figs/stan/corr.pdf', showdiag = F,
              names = titles, w = 20, h=20, small_text = T)

rho = extract(mod, 'rho')$rho


norm_dens = function(x) {
  d = density(x)
  d$y0 = d$y/max(d$y)
  d
}

pars = c('mu_alpha','mu_beta', 'mu_phi_c', 'mu_eta', 'mu_phi_mc')
titles = list('mu_alpha' = expression(a), 'mu_beta' = expression(beta), 
              'mu_phi_c' = expression(phi[c]), 'mu_eta' = expression(eta), 
              'mu_phi_mc' = expression(phi[m]))

for(p in pars) {
  
  pdf(paste0('../figs/stan/',p,'_dens.pdf'), 6, 6)
  
  v  = extract(mod, p)[[1]]
  d  = norm_dens(v)
  P  = mean(v > 0)
  P  = ifelse(P > .5, 1-P, P)
  ci = bayestestR::hdi(v)
  xlim = c(min(v)-sd(v), max(v)+sd(v))
  
  plot(d$x, d$y0, xlim=xlim, type='l', ylab='Norm. Density', main=titles[[p]], xlab='')
  polygon(c(d$x[d$x>0], max(x)), c(0, d$y0[d$x>0]), col=scales::alpha('darkblue', .5))
  
  legend('topright', bty='n', legend=paste0('P = ', round(P,4), 
                                            '\nCI = [',round(ci$CI_low,2),', ',round(ci$CI_high,2),']'))
  
  dev.off()
  
}

