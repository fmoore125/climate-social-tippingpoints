#perceived climate change depends on realized weather and various cognitive biases

#shifting baseline (weights parameter) implemented based on esimtated parameters from Moore et al. 2019

anomaly=function(weather,t,biassed_assimilation=0,shifting_baseline=0,weights=c(0.23,0.20,0.17,0.14,0.11,0.09,0.06),temp0=temp_0[1]){
  if(shifting_baseline==0) anomaly=weather[t]
  if(shifting_baseline==1){
    if(t==2) anomaly=weather[t]-temp0
    if(2<t&t<9) anomaly=weather[t]-weights%*%c(weather[(t-2):1],rep(temp0,(9-t)))
    if(t>=9) anomaly=weather[t]-weights%*%weather[(t-2):(t-8)]
  }
  if(biassed_assimilation==0) evidence=rep(anomaly,3)
  if(biassed_assimilation>0) evidence=ifelse(rep(anomaly<0,3),c(anomaly*(1+biassed_assimilation),anomaly,anomaly*(1-biassed_assimilation)),c(anomaly*(1-biassed_assimilation),anomaly,anomaly*(1+biassed_assimilation)))
  
  return(list(anomaly,evidence))
}