library(ggplot2)
library(plot.matrix)
library(data.table)
library(tidyverse)
library(reshape2)
library(patchwork)
library(forcats)

source("src/model_analysis/model_parametertune.R")

#subset of variable to vary - those in the opinion and policy components

nsim=40000

params=matrix(nrow=nsim,ncol=9)
optune=array(dim=c(8,3,nsim))
poltune=matrix(nrow=nsim,ncol=8)

for(i in 1:nsim){
  #draw homophily parameter
  homophily_param_tune=max(1-rbeta(1,3,10),0.33333334)
  forcestrong_tune=runif(1,0,1)
  forceweak_tune=runif(1,0,forcestrong_tune)
  evidenceeffect_tune=runif(1,0,0.3)
  policyopinionfeedback_param_tune=runif(1,0,0.005)
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
op=op[-1,] #omit initialization year and last two years as the testing dataset
pol=read.csv("data/Data for Hindcasting/policy/worldbank_carbonprices_finalforpewcountries.csv")
pol=pol[2:8,]

tune_years=2014:2020

optrained=list()
poltrained=list()

for(t in 1:length(tune_years)){
  #drop years from calibration process, then re-run over calibration years to estimate error in predicting dropped year
  #calculate total error for each simulation
  print(t)
  operror=numeric(length=nsim);polerror=numeric(length=nsim)
  
  if(tune_years[t]%in%op$Year){
    opyears=op$Year[-which(op$Year==tune_years[t])]
  } else{
    opyears=op$Year
  }
  
  poltest=pol[-which(pol$Year==tune_years[t]),3]
  
  for(i in 1:nsim){
    operror[i]=(mean(as.matrix((op[which(op$Year%in%opyears),c(4,3,2)]/100-optune[which(2013:2020%in%opyears),,i])^2)))^(1/2)
    polerror[i]=(mean((poltest-poltune[i,-c(1,which(2013:2020==tune_years[t]))])^2))^(1/2)
    
  }
  
  #get relative performace - standardize across all model runs
  operror=(operror-mean(operror))/sd(operror)
  polerror=(polerror-mean(polerror))/sd(polerror)
  toterror=(operror+polerror)/2
  
  sampleweight=(-1*toterror)-min(-1*toterror) #convert to strictly positive metric increasing in model performance
  sampleweight=sampleweight/sum(sampleweight) #convert to "probability"
  
  params_tot=cbind(params,sampleweight)
  colnames(params_tot)=c(as.character(covparamserror$params),"sampleweight")
  
  polopparams=as.data.frame(params_tot)
  
  nsim2=20000
  optune2=array(dim=c(8,3,nsim2))
  poltune2=matrix(nrow=nsim2,ncol=8)
  
  for(i in 1:nsim2){
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
    optune2[,,i]=m$distributions;poltune2[i,]=m$policy
    
  }
  
  optrained[[t]]=optune2;poltrained[[t]]=poltune2
  
}

save(optrained,poltrained,file="big_data/trainingresults.Rdat")

#mean pol error
polerror=numeric(length=length(poltrained))
for(i in 1:length(polerror)) polerror[i]=mean((poltrained[[i]][i+1]-pol$meanprice[i])^2)^(1/2)
mean(polerror)

#mean of opinion - average RMSE of neutral and opposed
operror=numeric(length=dim(op)[1])
inds=which(2014:2020%in%op$Year)
for(i in 1:length(operror)){
  opposeerror=mean((optrained[[inds[i]]][inds[i]+1,1,]-op$Oppose[i]/100)^2)^(1/2)
  neutralerror=mean((optrained[[inds[i]]][inds[i]+1,2,]-op$Neutral[i]/100)^2)^(1/2)
  operror[i]=mean(opposeerror,neutralerror)
}
mean(operror)

