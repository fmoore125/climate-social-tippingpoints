#emissions depend on bau emissions, adoption of individual actions, and policy

emissionschange=function(bau_t,nadopters_t,policy_t,mitigation,t,effectiveness=adopt_effect,maxm=m_max,rmax=r_max,r0=r_0){
  #contemporaneous reduction from policy, depends on policy
  #mitigation_t_1 is a matrix with dimensions of max(t) *t-1 that gives persistent effect of mitigation actions in previous time periods
  m_t=ifelse(policy_t<=1,0,maxm*log(policy_t)/log(300)) #300 is maximum value policy can take
  #lifetime of investments also depends on policy
  r_t=min(r0*(1+policy_t),rmax)
  #add effect of current policy
  #mitigation is a t*t matrix of zeroes - fill in columns representing persistent effect of yearly mitigation activity
  futuretime=t:dim(mitigation)[1]-t
  mitigation[,t]=ifelse(rep(m_t==0,dim(mitigation)[1]),rep(0,dim(mitigation)[1]),c(rep(0,t-1),m_t*exp(-futuretime/r_t)))
  
  #emissions reduction from policy is sum of cumulative effect from previous mitigation
  emissions_policy_t=bau_t*max((1-sum(mitigation[t,])),0)
  
  #effect of individual actions is non-cumulative, but additional to policy effect
  emissions_t=emissions_policy_t*(1-nadopters_t*effectiveness)

  return(list(emissions_t,mitigation))
}


