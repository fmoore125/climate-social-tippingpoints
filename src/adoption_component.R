#adoption of mitigative behaviors, conditional on opinions about climate change, network, costs (PBC), and policy

#pbc given number of adopters in previous time period, initial pbc and parameters of logistic curve
pbcfunc=function(nadopt_t_1,policy_t_1,etcmid=etc_mid,etcsteep=etc_steep,total=etc_total,init_pbc=pbc_0,maxpolpbc=policy_pbcchange_max){
  if(nadopt_t_1==0) pbc_t=init_pbc
  if(nadopt_t_1>0) pbc_t=init_pbc+total/(1+exp(-1*etcsteep*(nadopt_t_1-etcmid)))
  poleffect=ifelse(abs(policy_t_1*0.1)<maxpolpbc,policy_t_1*0.1,ifelse(policy_t_1<0,-1*maxpolpbc,maxpolpbc))
  return(pbc_t+poleffect)
}

#social norm felt by each agent depends on the distribution of opinions and self-similarity of network
fraction_adopters=function(pbc=pbc_t,norm=norm_t,pbcmid=pbc_mid,pbcsteep=pbc_steep,shift=pbc_opinionchange,normstrength=normeffect){
  frac=numeric(length=3)
  for(i in 1:3){
    frac[i]=norm[i]*normstrength+1/(1+exp(-pbcsteep*(pbc-(pbcmid+shift[i]))))
    if(frac[i]<0) frac[i]=0
    if(frac[i]>1) frac[i]=1
  } 
  return(frac)
}

normfunc=function(normfrac){
  #transfer fraction of network adopting into value between -1 and 1, passing through 0 at 0.5
  if(normfrac<0.5) return(-1+4*normfrac-4*normfrac^2)
  if(normfrac>=0.5) return(1-4*normfrac+4*normfrac^2)
}


adopterschange=function(nadopt_t_1,adoptfrac_t_1,policy_t_1,distribution_t,etcmid=etc_mid,etcsteep=etc_steep,total=etc_total,init_pbc=pbc_0,maxpolpbc=policy_pbcchange_max,pbcmid=pbc_mid,pbcsteep=pbc_steep,shift=pbc_opinionchange,normstrength=normeffect,selfsimparam=homophily){
  #nadopt_t_1 - number of adopters in previous time period
  #distribution_t - distribution of opinion in buckets current time period
  pbc_t=pbcfunc(nadopt_t_1,policy_t_1,etcmid,etcsteep,total,init_pbc,maxpolpbc)
  
  norm_t=sapply(adoptfrac_t_1%*%t(networkfunc(distribution_t,selfsimparam)),FUN=normfunc)
  adopt_frac_t=fraction_adopters(pbc_t,norm_t,pbcmid,pbcsteep,shift,normstrength)
  nadopters_t=adopt_frac_t%*%distribution_t
  
  return(list(pbc_t,nadopters_t,adopt_frac_t))
}

