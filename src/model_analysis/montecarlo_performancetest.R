library(ggplot2)
library(plot.matrix)
library(data.table)
library(tidyverse)
library(reshape2)
library(patchwork)
library(forcats)

source("src/model_analysis/model_parametertune.R")

#subset of variable to vary - those in the opinion and policy components

nsim=20000

params=matrix(nrow=nsim,ncol=9)
optune=array(dim=c(8,3,nsim))
poltune=matrix(nrow=nsim,ncol=8)

for(i in 1:nsim){
  #draw homophily parameter
  homophily_param_tune=max(1-rbeta(1,2,12),0.33333334)
  forcestrong_tune=rbeta(1,2,5)
  forceweak_tune=runif(1,0,forcestrong_tune)
  evidenceeffect_tune=runif(1,0,0.5)
  policyopinionfeedback_param_tune=rbeta(1,4,8)*0.01
  pol_response_tune=runif(1,1,30)
  pol_feedback_tune=runif(1,pol_response_tune*-1,pol_response_tune)
  biassedassimilation_tune=runif(1,0,1)
  shiftingbaselines_tune=ifelse(runif(1,0,1)>0.75,0,1)
  
  params[i,]=c(homophily_param_tune,forcestrong_tune,forceweak_tune,evidenceeffect_tune,policyopinionfeedback_param_tune,pol_response_tune,pol_feedback_tune,biassedassimilation_tune,shiftingbaselines_tune)
  m=model_tune(homophily_param = homophily_param_tune,forcestrong = forcestrong_tune,forceweak = forceweak_tune,evidenceeffect = evidenceeffect_tune,policyopinionfeedback_param = policyopinionfeedback_param_tune,pol_response = pol_response_tune,pol_feedback = pol_feedback_tune,biassedassimilation = biassedassimilation_tune,shiftingbaselines=shiftingbaselines_tune)
  optune[,,i]=m$distributions;poltune[i,]=m$policy
  
  if(i%%1000==0) print(i)
  
}

#compare output to initial observation period (2014-2018)
op=read.csv("data/Data for Hindcasting/opinion/pew_final.csv")
op=op[-c(1,5),] #omit initialization year and last two years as the testing dataset
pol=read.csv("data/Data for Hindcasting/policy/worldbank_carbonprices_finalforpewcountries.csv")
pol=pol[2:6,3]

#calculate total error for each simulation
operror=numeric(length=nsim);polerror=numeric(length=nsim)

for(i in 1:nsim){
  operror[i]=sqrt(mean(as.matrix((op[,c(4,3,2)]/100-optune[c(4,5,8),,i])^2)))
  polerror[i]=sqrt(mean((pol-poltune[i,2:6])^2))
  
  if(i%%1000==0) print(i)
}

#get relative performace - standardize across all model runs
operror=(operror-mean(operror))/sd(operror)
polerror=(polerror-mean(polerror))/sd(polerror)
toterror=(operror+polerror)

sampleweight=(-1*toterror)-min(-1*toterror) #convert to strictly positive metric increasing in model performance
sampleweight=sampleweight/sum(sampleweight) #convert to "probability"

params_tot=cbind(params,sampleweight)
colnames(params_tot)=c(as.character(covparamserror$params),"sampleweight")

fwrite(params_tot,file="big_data/MC Runs/parameter_test.csv")

#now run model initializing in 2013 using trained parameter distributions and compare 2019-2020 output to observations
polopparams=fread("big_data/MC Runs/parameter_test.csv")

optune=array(dim=c(8,3,nsim))
poltune=matrix(nrow=nsim,ncol=8)

for(i in 1:nsim){
  polops=as.numeric(polopparams[sample(1:dim(polopparams)[1],size=1,prob=polopparams$sampleweight),1:9])
  homophily_param1=polops[1];forcestrong1=polops[2];forceweak1=polops[3];evidenceeffect1=polops[4];policyopinionfeedback_01=polops[5]
  pol_response1=polops[6];pol_feedback1=polops[7];biassedassimilation1=polops[8];shiftingbaselines1=polops[9]
  
  #uniform sampling of other model parameters -mostly adoption-related
  ced_param1=runif(1,0,0.5)
  policy_pbcchange_max1=runif(1,0,1)
  pbc_01=runif(1,-2,0)
  pbc_steep1=runif(1,1,3)
  opchangeparam=runif(1,0,1);pbc_opinionchange1=c(opchangeparam,0,-1*opchangeparam) #constrain opinion effect on adoption to be symmetric for opposers and supporters
  etc_total1=runif(1,0,2)
  normeffect1=runif(1,0,1)
  adopt_effect1=runif(1,0,0.3)
  lbd_param01=runif(1,0,0.3)
  lag_param01=round(runif(1,0,30))
  
  m=model_tune()
  params[i,]=c(homophily_param1,forcestrong1,forceweak1,evidenceeffect1,policyopinionfeedback_01,pol_response1,pol_feedback1,biassedassimilation1,shiftingbaselines1)
  optune[,,i]=m$distributions;poltune[i,]=m$policy
  
  if(i%%1000==0) print(i)
}


