rm(list=ls())
library(lme4)

beta_coef_test = function(b1, b2, se1, se2, tail=2) {
  # Clogg, C. C., Petkova, E., & Haritou, A. (1995). 
  # Statistical methods for comparing regression coefficients between models.
  # American Journal of Sociology, 100(5), 1261-1293.
  
  num   = b1-b2
  denom = sqrt(se1^2 + se2^2)
  
  z = num/denom
  
  if(tail==1) {
    p = pnorm(abs(z), lower.tail = F)
  } else {
    p = 2*pnorm(abs(z), lower.tail = F)
  }
  
  return(c(z=z,p=p))
  
}

# load data ---------------------------------------------------------------

files = paste0('data/', list.files('data/', pattern = '.csv'))
dat   = do.call(plyr::rbind.fill, lapply(files, read.csv))

dat$is_data_trial[dat$is_data_trial==1] = 'true'
dat  = dat[dat$is_data_trial=='true',]
dat  = dat[!is.na(dat$subject), ]
N    = length(unique(dat$subject))
mage = mean(as.numeric(dat$age), na.rm=T)
sage = sd(as.numeric(dat$age), na.rm = T)
pfem = mean(tolower(dat$gender)=='female', na.rm=T)
  
cat(N, ' subjects, Mean age = ',round(mage,2), 
    ' (SD = ',round(sage,2),'), ',
    pfem*100, '% female',
    '\n', 
    sep = '')

tapply(dat$time_elapsed, dat$subject, max)/60000

se = function(x) sd(x,na.rm=T)/sqrt(length(x))

learning = dat[dat$phase=='learning', ]
choice   = dat[dat$phase=='choice', ]
mock     = dat[dat$phase=='mock', ]

# learning ----------------------------------------------------------------

table(learning$subject, learning$deck1)

# catch -------------------------------------------------------------------

## plot catch decks ----
catch  = choice[choice$deck2 %in% c('Xc', 'Xnc'), ]
choice = choice[!choice$deck2 %in% c('Xc', 'Xnc'), ]

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
layout(matrix(1:8, 2, 4, byrow = T))
for(p in sort(unique(catch$pair))) {
  
  deck_names = strsplit(p,'_')[[1]]
  tmp        = catch[catch$pair==p & catch$chosen_deck!='',]
  prob       = c(mean(tmp$chosen_deck==deck_names[1]), mean(tmp$chosen_deck==deck_names[2]))
  sep        = se(tmp$chosen_deck==deck_names[1])
  
  bp = barplot(prob, xlab='', ylab='P(Choose Deck)', ylim=c(0,1),
               names.arg = deck_names)
  arrows(bp, prob-sep, bp, prob+sep, length=0)
  
}

dev.off()

## exclude ----
pcaught = tapply(catch$caught, catch$subject, mean)
rmv     = names(pcaught[pcaught < .66]) # maybe change this
choice  = choice[!choice$subject %in% rmv, ]

# choice ------------------------------------------------------------------

choice$pair = paste0(choice$deck1, choice$deck2)
table(choice$subject, choice$pair)

## group-level analysis ----

# 1st element: 00: neither deck yields choice, 10: one deck yields choice, but not the other, 11: both decks yield choice
# 2nd element: 00: neither deck yields MI, 10: choice deck yields MI, but not Nc deck, 01, opposite of 10, 11: both decks yield MI
pair_mapping = list('AB'=c('10','11'), 
                    'AC'=c('11','10'), 
                    'AD'=c('10','10'), 
                    'BC'=c('10','01'),
                    'BD'=c('00','01'),
                    'CD'=c('10','00'))

x             = choice[!is.na(choice$deck_rt),c('subject','trial','chosen_deck','pair', 'choose_choice', 'mutual_info')]
x$choice_pair = sapply(x$pair, function(i) pair_mapping[[i]][1])
x$mi_pair     = sapply(x$pair, function(i) pair_mapping[[i]][2])

### choice models ----
x_choice = x[!x$choice_pair %in% c('00','11'),]
x_choice$mi_pair_f = as.factor(x_choice$mi_pair)
contrasts(x_choice$mi_pair_f) = contr.treatment(levels(x_choice$mi_pair_f), 4)
x_choice$trial_c = x_choice$trial - median(x_choice$trial)

m0   = glmer(choose_choice ~ 1 + (1|subject), data=x_choice, family='binomial')
m1   = glmer(choose_choice ~ 0+mi_pair + (1|subject), data=x_choice, family='binomial')
m1_f = glmer(choose_choice ~ mi_pair_f + (1|subject), data=x_choice, family='binomial')
m2_f = glmer(choose_choice ~ mi_pair_f + trial_c + (trial_c|subject), data=x_choice, family='binomial')

anova(m0,m1)
sum  = summary(m1)
sumf = summary(m1_f)
sjPlot::tab_model(m1, transform = NULL, string.pred = 'Comparison',
                  dv.labels = 'P(Choose Choice)',
                  pred.labels = c('Neither Deck Yields MI', 'No Choice Deck yields MI', 
                                                        'Choice Deck yields MI','Both Decks yield MI'),
                  file = 'out/choice_model_coefs.html')

sjPlot::tab_model(m1_f, transform = NULL, string.pred = 'Comparison',
                  dv.labels = 'P(Choose Choice)',
                  pred.labels = c('Both Decks Yield MI [Intercept]', 'Neither Decks Yields MI', 
                                  'No Choice Deck yields MI','Choice Deck yields MI'),
                  file = 'out/choice_model_f_coefs.html')


z_p = beta_coef_test(sum$coefficients['mi_pair11','Estimate'], sum$coefficients['mi_pair11','Std. Error'], 
               sum$coefficients['mi_pair00','Estimate'], sum$coefficients['mi_pair00','Std. Error'],
                tail=1)

write.csv(sum$coefficients,'out/choice_model_coefs.csv')
write.csv(sumf$coefficients,'out/choice_model_f_coefs.csv')

sjPlot::tab_model(m2_f, transform = NULL, string.pred = 'Comparison',
                  dv.labels = 'P(Choose Choice)',
                  pred.labels = c('Both Decks Yield MI [Intercept]', 'Neither Decks Yields MI', 
                                  'No Choice Deck yields MI','Choice Deck yields MI',
                                  'Trial [Centered]'))

### visualize choice proportions ----
pchoose  = tapply(x_choice$choose_choice, x_choice$mi_pair, mean)
sechoose = tapply(x_choice$choose_choice, x_choice$mi_pair, se)
labs = c('Neither Deck Yields MI', 'No Choice Deck yields MI', 
         'Choice Deck yields MI','Both Decks yield MI')

pdf('figs/choice_proportions.pdf', 6, 6)
par(mar=c(7.1, 4.1, 4.1, 2.1))
bp = barplot(pchoose, ylab='P(Choose Choice)', ylim=c(0,1), xaxt='n', main='Preference for Choice')
text(bp, par("usr")[1]-.05, labels = labs, cex = 0.75,srt = 45, adj = 1, xpd = TRUE)
arrows(bp,pchoose-sechoose,bp,pchoose+sechoose,length=0)
abline(h=0.5, lty=2)
dev.off()

## plot pairs ----

pdf('figs/pairs.pdf', width=6, height=6)
layout(matrix(1:6, 2, 3))
for(p in sort(unique(choice$pair))) {
  
  deck_names = strsplit(p,'')[[1]]
  tmp        = choice[choice$pair==p,]
  prob       = c(mean(tmp$chosen_deck==deck_names[1]), mean(tmp$chosen_deck==deck_names[2]))
  sep        = se(tmp$chosen_deck==deck_names[1])
  
  bp = barplot(prob, xlab='', ylab='P(Choose Deck)', ylim=c(0,1),
               names.arg = deck_names)
  arrows(bp, prob-sep, bp, prob+sep, length=0)
  
}

dev.off()

## AB CD ----------------------------------------------------------
tmp = choice[choice$pair %in% c('AB', 'CD'), ]

pdf('figs/AB_vs_CD.pdf', width=6, height=6)

pchoice  = rev(tapply(tmp$choose_choice, tmp$pair, mean, na.rm=T))
sechoice = rev(tapply(tmp$choose_choice, tmp$pair, se))
ylimit   = range(pretty(c(pchoice-sechoice-.1,pchoice+sechoice+.1)))
  
b = barplot(pchoice, names.arg = c(0,1), xlab='Mutual Information', 
            ylab='P(Choose Choice)', main='Pref. for Choice | MI', 
            ylim=ylimit, xpd=F)
arrows(b, pchoice-sechoice, b, pchoice+sechoice, length=0)
abline(h=.5, lty=2)

dev.off()



# MI  ---------------------------------------------------------------------

x_mi = x[!x$mi_pair %in% c('00','11'),]

m0 = glmer(mutual_info ~ 1 + (1|subject), data=x_mi, family='binomial')
m1 = glmer(mutual_info ~ 0+choice_pair + (1|subject), data=x_mi, family='binomial')
anova(m0,m1)
sum = summary(m1)
sjPlot::tab_model(m1, transform = NULL, string.pred = 'Comparison',
                  dv.labels = 'P(Choose MI)',
                  pred.labels = c('Neither deck yields Choice', 'One deck yields Choice', 
                                  'Both Decks yield Choice'),
                  file = 'out/mi_model_coefs.html')

## visualize ---- 
pchoose  = tapply(x_mi$mutual_info, x_mi$choice_pair, mean)[c(1,3)]
sechoose = tapply(x_mi$mutual_info, x_mi$choice_pair, se)[c(1,3)]

labs = c('Neither deck yields Choice', 'Both Decks yield Choice')

pdf('figs/mi_proportions.pdf', 6, 6)

par(mar=c(7.1, 4.1, 4.1, 2.1))
bp = barplot(pchoose, ylab='P(Choose MI)', ylim=c(0,1), xaxt='n', main='Preference for MI')
text(bp, -.1, labels = labs, cex = 0.75,srt = 45, adj = 1, xpd = TRUE)
arrows(bp,pchoose-sechoose,bp,pchoose+sechoose,length=0)
abline(h=0.5, lty=2)

dev.off()

# mock ratings --------------------------------------------------------
mock         = mock[!mock$subject %in% rmv, ]
mi_mapping   = c(A=1, B=1, C=0, D=0)
mock$deck_mi = as.vector(mi_mapping[mock$deck1])
table(mock$deck1, mock$win_card)

## model ----

m0 = lmer(mock_rating ~ 1 + (1|subject), data=mock)
m1 = lmer(mock_rating ~ deck_mi + (1|subject), data=mock)

anova(m0,m1)
sjPlot::tab_model(m1, string.pred = ' ',
                  dv.labels = 'Perceived (P(Outcome|Win)',
                  pred.labels = c('MI = 0 [Intercept]', 'MI = 1'),
                  file = 'out/mock_model_coefs.html')

## visualize ----

pdf('figs/mock_ratings.pdf', width=6, height=6)

mrating  = tapply(mock$mock_rating/100, mock$deck_mi, mean)
serating = tapply(mock$mock_rating/100, mock$deck_mi, se)

b = barplot(mrating,ylab = 'Rating (0-100)', xlab = 'Mutual Information of Deck', main='Perceived P(Outcome | Win)', ylim=c(0,1))
arrows(b, mrating-serating, b, mrating+serating, length=0)
abline(h=0.5, lty=2)

dev.off()

# sense of control --------------------------------------------------------

soc = choice[!duplicated(choice$subject), c('subject',paste0('sense_of_control_', LETTERS[1:4]))]
soc = reshape2::melt(soc, id.vars='subject', variable.name='deck', value.name='rating')
soc$deck = gsub('sense_of_control_','',soc$deck)

## model ----
m0   = lmer(rating ~ 1 + (1|subject),data=soc)
m1   = lmer(rating ~ deck + (1|subject), data=soc)

anova(m0,m1)
sjPlot::tab_model(m1, string.pred = 'Deck',
                  dv.labels = 'Perceived Sense of Control Over Outcome',
                  pred.labels = c('A [Intercept]','B','C','D'),
                  file = 'out/soc_model_coefs.html')

## visualize ----
mcontrol  = tapply(soc$rating, soc$deck, mean)
secontrol = tapply(soc$rating, soc$deck, se)

pdf('figs/soc_ratings.pdf', width=6, height=6)

bp = barplot(mcontrol, xlab='Deck', ylab='Rating (0-100)', ylim=c(0,100), 
             main='Sense of Control')
arrows(bp, mcontrol-secontrol, bp, mcontrol+secontrol, length=0)
abline(h=mean(mcontrol), lty=2, lwd=2, col='red')
legend('topleft', bty='n', lty=2, lwd=2, col='red', legend='Group Average')

dev.off()


# Individual difference ---------------------------------------------------


## P(Choose deck) ~ perceived P(O|W) overall -----

mrating  = tapply(mock$mock_rating/100, list(mock$subject, mock$deck1), mean)
pchoose  = matrix(NA, length(unique(choice$subject)), 4, dimnames=list(unique(choice$subject), LETTERS[1:4]))
for(i in unique(choice$subject)) {
  for (j in LETTERS[1:4]) {
    pchoose[i,j] = mean(choice$chosen_deck[choice$subject==i & grepl(j,choice$pair)]==j)
  }
}

pdf('figs/pchoose_deck_by_perceived_prob.pdf', width=6, height=6)

plot(NULL, xlim=c(0,1), ylim=c(0,1),
     xlab='P(R | O=1)', ylab='P(Choose)', 
     main='Overall Preference',
)
cols = rainbow(4)
for(i in 1:dim(pchoose)[2]) {
  Y = pchoose[,i]; X = mrating[,i]
  mod = lm(Y~X)
  x   = seq(0,1,length.out=100)
  fit = predict(mod, interval = 'confidence', newdata = data.frame(X=x))
  
  points(X, Y, cex=1, pch=3, col=scales::alpha(cols[i],.25), xlab='Perceived Control', ylab='P(Choose Deck)')
  lines(x,fit[,'fit'], lwd=3,col=cols[i])
  polygon(c(rev(x), x), c(rev(fit[ ,'upr']), fit[ ,'lwr']), col = scales::alpha(cols[i], 0.25), border = NA)
  
}

legend('topleft', bty='n', col=cols, lty=1, lwd=2, legend=colnames(pchoose), title='Decks', ncol=2)

dev.off()

### model ----
df = data.frame()
for(i in 1:dim(pchoose)[2]) {
  tmp = data.frame(subject=rownames(pchoose), 
                   pchoose=pchoose[,i],
                   rating=mrating[,i],
                   deck=colnames(pchoose)[i], 
                   row.names = NULL) 
  df = rbind(df, tmp)
  
}

df$deck_f   = as.factor(df$deck)
df$rating_c = df$rating - mean(df$rating)

mod0 = lm(pchoose ~ rating_c , data=df) 
mod1 = lm(pchoose ~ rating_c + rating_c:deck_f , data=df) 
summary(mod1)


## P(Choose deck) ~ sense of control overall -----

mrating  = tapply(soc$rating/100, list(soc$subject, soc$deck), mean)
pchoose  = matrix(NA, length(unique(choice$subject)), 4, dimnames=list(unique(choice$subject), LETTERS[1:4]))
for(i in unique(choice$subject)) {
  for (j in LETTERS[1:4]) {
    pchoose[i,j] = mean(choice$chosen_deck[choice$subject==i & grepl(j,choice$pair)]==j)
  }
}

pdf('figs/pchoose_deck_by_soc.pdf', width=6, height=6)

plot(NULL, xlim=c(0,1), ylim=c(0,1),
     xlab='Sense of Control', ylab='P(Choose)', 
     main='Overall Preference',
)
cols = rainbow(4)
cors = data.frame(deck=rep(NA,dim(pchoose)[2]),r=rep(NA,dim(pchoose)[2]), p=rep(NA,dim(pchoose)[2]))
for(i in 1:dim(pchoose)[2]) {
  Y = pchoose[,i]; X = mrating[,i]
  mod = lm(Y~X)
  x   = seq(0,1,length.out=100)
  fit = predict(mod, interval = 'confidence', newdata = data.frame(X=x))
  
  points(X, Y, cex=1, pch=3, col=scales::alpha(cols[i],.25), xlab='Perceived Control', ylab='P(Choose Deck)')
  lines(x,fit[,'fit'], lwd=3,col=cols[i])
  polygon(c(rev(x), x), c(rev(fit[ ,'upr']), fit[ ,'lwr']), col = scales::alpha(cols[i], 0.25), border = NA)
  
  r = cor.test(X,Y)
  cors[i,] = c(colnames(pchoose)[i], r$estimate, r$p.value)
  
}

legend('bottomright', bty='n', col=cols, lty=1, lwd=2, legend=colnames(pchoose), title='Decks', ncol=2)

dev.off()

### model ----
df = data.frame()
for(i in 1:dim(pchoose)[2]) {
  tmp = data.frame(subject=rownames(pchoose), 
                   pchoose=pchoose[,i],
                   rating=mrating[,i],
                   deck=colnames(pchoose)[i], 
                   row.names = NULL) 
  df = rbind(df, tmp)
  
}

df$rating_c = df$rating - mean(df$rating)
df$deck_f   = as.factor(df$deck)

mod0 = lm(pchoose ~ rating_c , data=df) 
mod1 = lm(pchoose ~ rating_c*deck_f, data=df) 
summary(mod1)

## P(Choose C) ~ Perceived P(R|W)  ----
tmp = choice[grepl('C', choice$pair), ]

pchoose  = tapply(tmp$choose_choice, tmp$subject, mean)
mrating  = tapply(mock$mock_rating/100, list(mock$subject, mock$deck1), mean)[,'C']

pdf('figs/pchooseC_by_perceived_prob.pdf', width=6, height=6)

mod = lm(pchoose ~ mrating)
x   = seq(0,1,length.out=100)
fit = predict(mod, interval = 'confidence', newdata = data.frame(mrating=x))

plot(mrating, pchoose, cex=1, pch=3, col='grey', xlab='P(R|O=1) for Deck C', ylab='P(Choose Deck C)')
lines(x,fit[,'fit'], lwd=3,col='red')
polygon(c(rev(x), x), c(rev(fit[ ,'upr']), fit[ ,'lwr']), col = scales::alpha('red', 0.25), border = NA)

legend('bottomleft', bty='n', legend=paste0('r = ', round(cor(mrating, pchoose), 2), 
                                            '\np = ', round(summary(mod)$coefficients[2,'Pr(>|t|)'],2)))

dev.off()

## P(Choose C) ~ Sense of control  ----
tmp = choice[grepl('C', choice$pair), ]

pchoose  = tapply(tmp$choose_choice, tmp$subject, mean)
mrating  = tapply(soc$rating/100, list(soc$subject, soc$deck), mean)[,'C']

pdf('figs/pchooseC_by_soc.pdf', width=6, height=6)

mod = lm(pchoose ~ mrating)
x   = seq(0,1,length.out=100)
fit = predict(mod, interval = 'confidence', newdata = data.frame(mrating=x))

plot(mrating, pchoose, cex=1, pch=3, col='grey', xlab='Perceived Control for Deck C', ylab='P(Choose Deck C)')
lines(x,fit[,'fit'], lwd=3,col='red')
polygon(c(rev(x), x), c(rev(fit[ ,'upr']), fit[ ,'lwr']), col = scales::alpha('red', 0.25), border = NA)

legend('bottomright', bty='n', legend=paste0('r = ', round(cor(mrating, pchoose), 2), 
                                            '\np = ', round(summary(mod)$coefficients[2,'Pr(>|t|)'],2)))

dev.off()

## P(Choose C | BD) ~ Sense of control  ----

tmp = choice[choice$pair=='BC',]

pchoose  = tapply(tmp$choose_choice, tmp$subject, mean) # choose C
mrating  = tapply(soc$rating/100, list(soc$subject, soc$deck), mean)[,'C']

pdf('figs/pchooseC_BC_by_soc.pdf', width=6, height=6)

mod = lm(pchoose ~ mrating)
x   = seq(0,1,length.out=100)
fit = predict(mod, interval = 'confidence', newdata = data.frame(mrating=x))

plot(mrating, pchoose, cex=1, pch=3, col='grey', xlab='Perceived Control for Deck C', ylab='P(Choose Deck C)', main='B-C Pairs')
lines(x,fit[,'fit'], lwd=3,col='red')
polygon(c(rev(x), x), c(rev(fit[ ,'upr']), fit[ ,'lwr']), col = scales::alpha('red', 0.25), border = NA)

legend('bottomright', bty='n', legend=paste0('r = ', round(cor(mrating, pchoose), 2), 
                                             '\np = ', round(summary(mod)$coefficients[2,'Pr(>|t|)'],2)))

dev.off()

## P(Choose Choice) ~ Sense of Control Median Split ----
choice$soc_split_A = ifelse(choice$sense_of_control_A > median(choice$sense_of_control_A), 1, 0)
choice$soc_split_B = ifelse(choice$sense_of_control_B > median(choice$sense_of_control_B), 1, 0)
choice$soc_split_C = ifelse(choice$sense_of_control_C > median(choice$sense_of_control_C), 1, 0)
choice$soc_split_D = ifelse(choice$sense_of_control_D > median(choice$sense_of_control_D), 1, 0)

x             = choice[!is.na(choice$deck_rt),]
x$choice_pair = sapply(x$pair, function(i) pair_mapping[[i]][1])
x$mi_pair     = sapply(x$pair, function(i) pair_mapping[[i]][2])
x_choice      = x[!x$choice_pair %in% c('00','11'),]

x_choice$choice_soc = NA
choice_list = c('A'=1,'B'=0,'C'=1,'D'=0)

for(i in 1:nrow(x_choice)) {
  if(choice_list[x_choice$deck1[i]] == 1) {
    x_choice$choice_soc[i] = x_choice[i,paste0('soc_split_', x_choice$deck1[i])]
  } else {
    x_choice$choice_soc[i] = x_choice[i,paste0('soc_split_', x_choice$deck2[i])]
  }
}

# visualize
pchoose  = tapply(x_choice$choose_choice, list(x_choice$choice_soc,x_choice$mi_pair), mean)
sechoose = tapply(x_choice$choose_choice, list(x_choice$choice_soc,x_choice$mi_pair), se)
labs = c('Neither Deck Yields MI', 'No Choice Deck yields MI', 
         'Choice Deck yields MI','Both Decks yield MI')

pdf('figs/choice_proportions_by_soc.pdf', 6, 6)
par(mar=c(7.1, 4.1, 4.1, 2.1))
bp = barplot(pchoose, beside=T, ylab='P(Choose Choice)', ylim=c(0,1), xaxt='n',
             main='Preference for Choice', 
             legend.text = T, 
             args.legend = list(x='topleft',bty='n',title='Sense of Control for Choice Deck',
                                legend=c('Low','High'), ncol=2))

text(colMeans(bp), -.05, labels = labs, cex = 0.75,srt = 45, adj = 1, xpd = TRUE)
arrows(bp,pchoose-sechoose,bp,pchoose+sechoose,length=0)
abline(h=0.5, lty=2)
dev.off()

### AB/CD ----
tmp = x_choice[x_choice$pair %in% c('AB', 'CD'), ]
tmp$pair = ifelse(tmp$pair=='AB', 1, 0)

pdf('figs/AB_vs_CD_by_soc.pdf', width=6, height=6)

pchoice  = tapply(tmp$choose_choice, list(tmp$choice_soc, tmp$pair), mean, na.rm=T)
sechoice = tapply(tmp$choose_choice, list(tmp$choice_soc, tmp$pair), se)
ylimit   = range(pretty(c(pchoice-sechoice-.1,pchoice+sechoice+.1)))

b = barplot(pchoice, beside=T, xlab='Mutual Information', 
            ylab='P(Choose Choice)', main='Pref. for Choice | MI', 
            ylim=ylimit, xpd=F, 
            legend.text = T, 
            args.legend = list(x='topleft',bty='n', title='Sense of Control for Choice Deck', 
                               legend=c('Low','High'), ncol=2))
arrows(b, pchoice-sechoice, b, pchoice+sechoice, length=0)
abline(h=.5, lty=2)

dev.off()

x_choice$choice_soc_c = x_choice$choice_soc - mean(x_choice$choice_soc)
x_choice$mi_pair_f    = as.factor(x_choice$mi_pair)
contrasts(x_choice$mi_pair_f) = contr.treatment(levels(x_choice$mi_pair_f), 4)

mod = glmer(choose_choice ~ mi_pair_f*choice_soc_c + (1|subject), data=x_choice, family='binomial')
summary(mod)


## P(Choose MI) ~ Sense of Control ----

x_mi = x[!x$mi_pair %in% c('00','11'),]
mi_list = c('A'=1,'B'=1,'C'=0,'D'=0)
x_mi$mi_soc = NA

for(i in 1:nrow(x_mi)) {
  if(mi_list[x_mi$deck1[i]] == 1) {
    x_mi$mi_soc[i] = x_mi[i,paste0('soc_split_', x_mi$deck1[i])]
  } else {
    x_mi$mi_soc[i] = x_mi[i,paste0('soc_split_', x_mi$deck2[i])]
  }
}

pchoose  = tapply(x_mi$mutual_info,list(x_mi$mi_soc, x_mi$choice_pair), mean)[,c(1,3)]
sechoose = tapply(x_mi$mutual_info,list(x_mi$mi_soc, x_mi$choice_pair), se)[,c(1,3)]
labs = c('Neither deck yields Choice', 'Both Decks yield Choice')

pdf('figs/mi_proportions_by_soc.pdf', 6, 6)

par(mar=c(7.1, 4.1, 4.1, 2.1))

bp = barplot(pchoose, beside=T, ylab='P(Choose MI)', ylim=c(0,1), xaxt='n', main='Preference for MI',
             legend.text = T, 
             args.legend = list(x='topleft',bty='n', title='Sense of Control for MI Deck', 
                                legend=c('Low','High'), ncol=2))
text(colMeans(bp), -.05, labels = labs, cex = 0.75,srt = 45, adj = 1, xpd = TRUE)
arrows(bp,pchoose-sechoose,bp,pchoose+sechoose,length=0)
abline(h=0.5, lty=2)

dev.off()


# Save image --------------------------------------------------------------

save.image('exp1.RData')
write.csv(choice, 'models/choice_data.csv')


# Bayesian Choice Model ---------------------------------------------------
library(brms)
library(bayestestR)

choice$pair = paste0(choice$deck1, choice$deck2)
table(choice$subject, choice$pair)

# 1st element: 00: neither deck yields choice, 10: one deck yields choice, but not the other, 11: both decks yield choice
# 2nd element: 00: neither deck yields MI, 10: choice deck yields MI, but not Nc deck, 01, opposite of 10, 11: both decks yield MI
pair_mapping = list('AB'=c('10','11'), 
                    'AC'=c('11','10'), 
                    'AD'=c('10','10'), 
                    'BC'=c('10','01'),
                    'BD'=c('00','01'),
                    'CD'=c('10','00'))

x             = choice[!is.na(choice$deck_rt),c('subject','trial','chosen_deck','pair', 'choose_choice', 'mutual_info')]
x$choice_pair = sapply(x$pair, function(i) pair_mapping[[i]][1])
x$mi_pair     = sapply(x$pair, function(i) pair_mapping[[i]][2])

## choice models ----
x_choice = x[!x$choice_pair %in% c('00','11'),]
x_choice$mi_pair_f = as.factor(x_choice$mi_pair)
contrasts(x_choice$mi_pair_f) = contr.treatment(levels(x_choice$mi_pair_f), 4)
x_choice$trial_c = x_choice$trial - median(x_choice$trial)

m1_b = brm(choose_choice ~ 0+mi_pair + (1|subject), data=x_choice, 
           family='bernoulli',
           seed = 2022, 
           prior = set_prior("normal(0,1)", class = "b"),
           chains = 3, iter = 5000,
           sample_prior = T, file='tmp')

describe_posterior(m1_b)
h = hypothesis(m1_b, hypothesis = 'mi_pair00 < mi_pair11') # P(mi_pair00 > mi_pair11)/P(mi_pair00 < mi_pair11)

### visualize choice model
post = as.data.frame(m1_b)

dens00 = density(post$b_mi_pair00)
dens11 = density(post$b_mi_pair11)
xlimit = range(pretty(c(dens00$x, dens11$x)))

pdf('choice_mod_post.pdf', 6, 6)

plot(dens00, xlab = 'Log Odds of Choosing Choice', xlim=xlimit, main='Posterior')
polygon(dens00$x, dens00$y, col=scales::alpha("red", 0.25))
lines(dens11)
polygon(dens11$x, dens11$y, col=scales::alpha("blue", 0.25))
legend('topleft', bty='n', fill=scales::alpha(c('red','blue'),0.25), legend=c('0','1'), title='MI')

p = 1-mean(post$b_mi_pair00 < post$b_mi_pair11)
legend('topright',bty='n',legend=paste0('P = ',round(p,2)))

dev.off()