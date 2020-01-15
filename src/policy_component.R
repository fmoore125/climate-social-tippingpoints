#collective policy measures to lower costs of individual actions and reduce emissions as a function of opinions

policychange=function(distribution_t,policy_t_1,responsiveness=pol_response){
  if(distribution_t[1]>distribution_t[3]){
    #more opposing climate policy than supporting
    if(distribution_t[1]/distribution_t[3]<responsiveness) change=0 #not a big enough majority
    if(distribution_t[1]/distribution_t[3]>responsiveness) change=-1*(1-distribution_t[2])^2*((distribution_t[1]/distribution_t[3])-responsiveness)^(1+(1/responsiveness))
  }
  if(distribution_t[3]>=distribution_t[1]){
    #more supporting climate policy than opposing
    if(distribution_t[3]/distribution_t[1]<responsiveness) change=0 #not a big enough majority
    if(distribution_t[3]/distribution_t[1]>responsiveness) change=(1-distribution_t[2])^2*((distribution_t[3]/distribution_t[1])-responsiveness)^(1+(1/responsiveness))
  }
  return(max(min(policy_t_1+change,300),-300))
}