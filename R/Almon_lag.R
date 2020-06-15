Almon_lag = function(polydegree,C,R){
  #Function constructs Almon lag polynomial as in the article by 
  # Matteo Mogliani Bayesian MIDAS Penalized Regressions: Estimation, Selection, and Prediction 2 Jan 2020
  #Enables to put endpoint restricitions on the value and on slope of the lag polynomial default R=c(1,1)
  # Meaning that restricitions jointly constrain the weighting structure to tail off slowly to zero
  
  
  names(R)=c('fC','dfC')
  if (polydegree == 2){
    if (R['fC']==0 && R['dfC']==0){
      #Unrestricted Almon lag polynomial
      Q=matrix(0,polydegree+1,C)
      for (ii in 0:polydegree){
        Q[(ii+1),]=seq(from=0,to=(C-1),by=1)^(ii)
      }
    }
    else if(R['fC']==1 && R['dfC']==0){
      # Almon lag polynomial with tail restriction (fC=0)
      Q=matrix(0,polydegree,C)
      for (ii in 0:polydegree){
        Q[ii,]=seq(from=0,to=(C-1),by=1)^(ii)-(C-1)^(ii)
      }
    }
    else if (R['fC']==0 && R['dfC']==1){
      # Almon lag polynomial with derivative restriction (dfC=0)
      Q=matrix(0,polydegree,C)
      for (ii in 0:polydegree){
        Q[ii,]=(seq(from=0,to=(C-1),by=1)^(ii)-ii*(C-1)*seq(from=0,to=(C-1),by=1))^(ii-1)
      }
    }
    else if (R['fC']==1 && R['dfC']==1){
      #Almon lag polynomial with tail and derivative restrictions (fC=0 and dfC=0)
      Q=matrix(0,polydegree-1,C)
      for (ii in 0:polydegree-1){
        Q[ii,]=seq(from=0,to=(C-1),by=1)^(ii+1)-(ii+1)*(C-1)*seq(from=0,to=(C-1),by=1)+(C-1)^(ii+1)
      }
    }
  }
  else if (polydegree==3){
    if (R['fC']==0 && R['dfC']==0){
      #Unrestricted Almon lag polynomial
      Q=matrix(0,polydegree+1,C)
      for (ii in 0:polydegree){
        Q[(ii+1),]=seq(from=0,to=(C-1),by=1)^(ii)
      }
    }
    else if(R['fC']==1 && R['dfC']==0){
      # Almon lag polynomial with derivative restriction (dfC=0)
      Q=matrix(0,polydegree,C)
      for (ii in 0:polydegree){
        Q[ii,]=(seq(from=0,to=(C-1),by=1))^(ii)-(C-1)^(ii)
      }
    }
    else if (R['fC']==0 && R['dfC']==1){
      # Almon lag polynomial with derivative restriction (dfC=0)
      print('Restrictions on the lag polynomial not allowed')
    }
    else if(R['fC']==1 && R['dfC']==1){
      # Almon lag polynomial with tail and derivative restrictions (fC=0 and dfC=0)
      Q=matrix(0,polydegree-1,C)
      for (ii in 0:polydegree-1){
        Q[ii,]=seq(from=0,to=(C-1),by=1)^(ii+1)-(ii+1)*(C-1)^(ii)*seq(from=0,to=(C-1),by=1)+(ii)*(C-1)^(ii+1)
      }
    }
  }
  return(Q)
}