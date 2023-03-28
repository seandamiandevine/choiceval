

stan_format = function(df, subj_name = 'prol_id') {
  
  df = df[order(df[,subj_name]), ]
  N = length(unique(df[,subj_name]))
  J = table(df[,subj_name])
  cols = colnames(df)
  
  for(c in cols) {
    eval(parse(text=paste0(c, '= matrix(999, N, max(J), byrow=T)')))
  }
  
  for(i in 1:N) {
    tmp = df[df[,subj_name]==unique(df[,subj_name])[i],]
    tmp[is.na(tmp)] = 999 
    for(c in cols) {
      cmd = paste0(c,'[i,1:J[i]] = tmp[,"',c,'"]')
      eval(parse(text=cmd))
    }
  }
  
  stan_list = list(N = N, T = max(J), Tsubj  = unname(J))
  
  for(c in cols) {
    cmd = paste0('stan_list[["',c,'"]] = ', c)
    eval(parse(text=cmd))
  }
  
  
  return(stan_list)
  
} 



