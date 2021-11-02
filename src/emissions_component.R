#emissions depend on bau emissions, adoption of individual actions, and policy
bauchange=function(bau0_t,temperature_t_1,temp_emissions){
  bau_t=bau0_t*(1+(temp_emissions*temperature_t_1))
  return(bau_t)
}

emissionschange=function(bau_t,nadopters_t,policy_t,mitigation,t,effectiveness=adopt_effect,maxm=m_max,rmax=r_max,r0=r_0,lbd=lbd_param,emissions_t_lag,bau_t_lag,bau_outisde_t,lag=lag_param){
  #contemporaneous reduction from policy, depends on policy
  #mitigation_t_1 is a matrix with dimensions of max(t) *t-1 that gives persistent effect of mitigation actions in previous time periods
  #lbd param is a "learning by doing" parameter giving the fraction reduction in cost for a doubling of installed mitigation stock
  
  #caluclate current emissions reduction cap based on m_max, learning by doing (lbd param) and cumulative mitigation
  cummit_t_1=sum(mitigation[t-1,1:(t-1)]) #cumulative policy-induced mitigation in previous time period
  doublings=log2(cummit_t_1/maxm) #number of doublings of total mitigation from initial maximum value
  mmax_t=ifelse(doublings<=1,maxm,maxm*(1+lbd)^doublings)
  
  
  m_t=ifelse(policy_t<=1,0,ifelse(policy_t>=299, mmax_t,mmax_t*(log(policy_t)/log(300)))) #300 is maximum value policy can take
  #lifetime of investments also depends on policy
  r_t=min(r0*(1+policy_t/10),rmax)
  #add effect of current policy
  #mitigation is a t*t matrix of zeroes - fill in columns representing persistent effect of yearly mitigation activity
  futuretime=t:dim(mitigation)[1]-t
  mitigation[,t]=ifelse(rep(m_t==0,dim(mitigation)[1]),rep(0,dim(mitigation)[1]),c(rep(0,t-1),m_t*exp(-futuretime/r_t)))
  
  #emissions reduction from policy is sum of cumulative effect from previous mitigation
  emissions_policy_t=bau_t*max((1-sum(mitigation[t,])),0)
  
  #effect of individual actions is non-cumulative, but additional to policy effect
  emissions_t=emissions_policy_t*(1-nadopters_t*effectiveness)
  
  #outside region follows OECD mitigation pathway, with a lag - if lag is zero, effectively single region - same % reduction in outside region as inside
  total_emissions_t=ifelse(lag==0,emissions_t+bau_outisde_t*(1-(bau_t-emissions_t)/bau_t),emissions_t+bau_outisde_t*(1-(bau_t_lag-emissions_t_lag)/bau_t_lag))
  

  return(list(emissions_t,mitigation,total_emissions_t))
}


