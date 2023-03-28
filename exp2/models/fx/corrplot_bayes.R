
corplot_bayes = function(fit, rho_name, mu_name, file, 
                         idx=NULL, names=NULL, showdiag=F, w=15, h=15, 
                         small_text = F) {
  
  #' Creates a correlation matrix plot from stan output, where posterior densities are plotted 
  #' below the diagonal and MAPs and HDIs above the diagonal.
  #'  
  #'@param fit       stan fit
  #'@param rho_name  name of correlation matrix in stan fit
  #'@param u_name    name of random effects matrix in stan fit
  #'@param file      file name to save output in 
  #'@param names     names for rows and columns of matrix (as vector)
  #'@param idx       which variables to keep in matrix
  #'@param showdiag  whether or not to show diagonal posteriors (1). If T, the posterior will be a histogram. If F, will be the number 1.00.
  #'@param w         width (in inches) of output to file 
  #'@param h         height (in inches) of output to file
  
  # Constants
  cex.main = .125*w
  cex.axis = .125*w
  cex.lab  = .125*w
  font.lab = 2
  lwd      = 3
  text.cex = .2*w
  
  if(small_text) {
    cex.main = .1*w
    cex.axis = .1*w
    cex.lab  = .1*w
    text.cex = .1*w
  }
  
  rho = rstan::extract(fit, pars=rho_name)[[rho_name]] 
  mu  = rstan::extract(fit, pars=mu_name)[[mu_name]]
  
  rho = rho[3001:6000,,]
  mu  = mu[3001:6000,]
  
  if(!is.null(idx)) {
    rho = rho[,idx,idx]
    mu  = mu[,idx]
  }
  
  # Get matrix dims
  n = dim(rho)[2]
  if(is.null(names)) names = paste0('V',1:n)
  
  # Plot
  pdf(file, height=h, width=w)
  layout(matrix(1:n^2, n, n))
  par(mar=c(5.1, 6.1, 4.1, 2.1), mai = c(0.4, 0.9, 0.5, 0.1))
  
  for(i in 1:n) {
    for (j in 1:n) {
      
      ylab = ifelse(i==1, names[j], '')
      main = ifelse(j==1, names[i], '')
      
      
      if(i==j) {
        if(!showdiag) {
          plot(NULL, xlim=c(-1,1), ylim=c(-1,1), xaxt='n', yaxt='n', main=main, xlab='', ylab=ylab, 
               cex.main = cex.main, cex.lab=cex.lab, font.lab=font.lab)
          text(0,0, '1.00', cex=w*.5)
        } else {
          vec  = mu[,i]
          hist(vec, main=main, xlab='', ylab=ylab, cex.axis=cex.axis, 
               cex.main = cex.main, cex.lab=cex.lab, font.lab=font.lab, yaxt='n', xaxt='n')
          q    = quantile(vec, c(0,.5,1))
          nd   = ifelse(all(q<.01), 4, 2)
          near = ifelse(nd==2, .05, .00075)
          axis(1, at=q, labels=round(round(q/near)*near,nd), cex.axis=cex.axis, tick = F)
          box()
          # abline(v=0, lty=2, lwd=.1*h)
          
        }
      }else if(i < j || i==j) {
        # off-diag left and diag if showdiag==T
        vec  = rho[,i,j]
        dens = density(vec)
        
        plot(dens, main=main, xlab='', ylab=ylab, yaxt='n', 
             cex.main = cex.main, cex.axis=cex.axis, cex.lab=cex.lab, font.lab=font.lab,
             xlim=c(-1,1), lwd=lwd, xaxt='n')
        axis(1, at=c(-1,0,1), cex.axis=cex.axis, tick=F)
        polygon(dens$x, dens$y, col=scales::alpha('grey',.4), border = 'black')
        abline(v=0, lty=2, lwd=.1*h)
        
      } else {
        # off-diag right or diag if !showdiag
        vec  = rho[,i,j]
        dens = density(vec)
        
        plot(NULL, xlim=c(-1,1), ylim=c(-1,1), xaxt='n', yaxt='n', main=main, xlab='', ylab=ylab, 
             cex.main = cex.main, cex.lab=cex.lab, font.lab=font.lab)
        med = round(median(vec),2)
        hdi = round(bayestestR::hdi(vec),2)
        stat = paste0('r = ',med,'\n',
                      'CI = ', '[',hdi$CI_low,', ',hdi$CI_high,']'
        )
        text(0,0, stat, cex=text.cex)
        
      }
    }
  }
  dev.off()
  
}


