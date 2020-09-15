#collective policy measures to lower costs of individual actions and reduce emissions as a function of opinions

policychange=function(distribution_t,policy_t_1,pol_t_window,responsiveness=pol_response,feedback=pol_feedback){
  #the policy-feedback effect allows past policy to alter the status-quo bias by creating political constituencies in favor of (or opposed to) the policy change
  response_feedback=sqrt(abs(pol_t_window/300))*feedback
  
  pro_responsiveness=ifelse(pol_t_window>0,max(responsiveness-response_feedback,1),max(responsiveness+response_feedback,1))
  opp_responsiveness=ifelse(pol_t_window>0,max(responsiveness+response_feedback,1),max(responsiveness-response_feedback,1))
  
  if(distribution_t[1]>distribution_t[3]){
    #more opposing climate policy than supporting
    if(distribution_t[1]/distribution_t[3]<opp_responsiveness) change=0 #not a big enough majority
    if(distribution_t[1]/distribution_t[3]>opp_responsiveness) change=-1*(1-distribution_t[2])^2*((distribution_t[1]/distribution_t[3])-opp_responsiveness)^(1+(1/opp_responsiveness))
  }
  if(distribution_t[3]>=distribution_t[1]){
    #more supporting climate policy than opposing
    if(distribution_t[3]/distribution_t[1]<pro_responsiveness) change=0 #not a big enough majority
    if(distribution_t[3]/distribution_t[1]>pro_responsiveness) change=(1-distribution_t[2])^2*((distribution_t[3]/distribution_t[1])-pro_responsiveness)^(1+(1/pro_responsiveness))
  }
  return(max(min(policy_t_1+change,300),-300))
}