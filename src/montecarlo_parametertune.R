source("src/model_parametertune.R")

#subset of variable to vary - those in the opinion and policy components

#opinion component
homophily_param_bounds
forcestrong_tune=c(0,1)
forceweak_tune
evidenceeffect_tune
policyopinionfeedback_param_tune

#policy component
pol_response_tune
pol_feedback_tune

#cognition component
biassedassimilation_tune
shiftingbaselines_tune

nsim=10000

params=matrix(nrow=nsim,ncol=9)
optune=array(dim=c(11,3,nsim))
poltune=matrix(nrow=nsim,ncol=11)

for(i in 1:nsim){
  #draw homophily parameter
  homophily_param_tune=runif(1,min=0.333333334,max=1)
  forcestrong_tune=runif(1,0,1)
  forceweak_tune=runif(1,0,forcestrong_tune)
  evidenceeffect_tune=runif(1,0,1)
  policyopinionfeedback_param_tune=runif(1,0,0.2)
  pol_response_tune=runif(1,1,10)
  pol_feedback_tune=runif(1,pol_response_tune*-1,pol_response_tune)
  biassedassimilation_tune=runif(1,0,1)
  shiftingbaselines_tune=round(runif(1,0,1))
  
  params[i,]=c(homophily_param_tune,forcestrong_tune,forceweak_tune,evidenceeffect_tune,policyopinionfeedback_param_tune,pol_response_tune,pol_feedback_tune,biassedassimilation_tune,shiftingbaselines_tune)
  m=model_tune(homophily_param = homophily_param_tune,forcestrong = forcestrong_tune,forceweak = forceweak_tune,evidenceeffect = evidenceeffect_tune,policyopinionfeedback_param = policyopinionfeedback_param_tune,pol_response = pol_response_tune,pol_feedback = pol_feedback_tune,biassedassimilation = biassedassimilation_tune,shiftingbaselines=shiftingbaselines_tune)
  optune[,,i]=m$distributions;poltune[i,]=m$policy
  
  if(i%%1000==0) print(i)
  
}



