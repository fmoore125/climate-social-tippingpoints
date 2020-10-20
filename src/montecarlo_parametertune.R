library(ggplot2)
library(plot.matrix)

source("src/model_parametertune.R")

#subset of variable to vary - those in the opinion and policy components

nsim=20000

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

#compare output to observations
op=read.csv("data/Data for Hindcasting/opinion/ypcc_sixamericas_final.csv")
pol=read.csv("data/Data for Hindcasting/policy/worldbank_carbonprices.csv")
pol=pol[2:9,6]

#calculate total error for each simulation
operror=numeric(length=nsim);polerror=numeric(length=nsim)

for(i in 1:nsim){
  operror[i]=sqrt(mean(as.matrix((op[,c(4,3,2)]-optune[,,i])^2)))
  polerror[i]=sqrt(mean((pol-poltune[i,1:8])^2))
  
  if(i%%1000==0) print(i)
}

#get relative performace - standardize across all model runs
operror=(operror-mean(operror))/sd(operror)
polerror=(polerror-mean(polerror))/sd(polerror)
toterror=(operror+polerror)/2

cor=apply(params,MARGIN=2,function(x) cor(x,toterror))
covparamserror=data.frame(params=c("Homophily","Strong Force","Weak Force","Evidence","Pol-Opinion","Status-Quo Bias","Pol Int Feedback","Biassed Assimilation","Shifting Baselines"),cor=cor)

a=ggplot(covparamserror,aes(x=params,y=cor))+theme_bw()+geom_bar(stat="identity",fill="darkorchid")+labs(x="",y="Parameter-Error Correlation")+geom_hline(yintercept=0,lwd=2)
x11()
a

sampleweight=(-1*toterror)-min(-1*toterror) #convert to strictly positive metric increasing in model performance
sampleweight=sampleweight/sum(sampleweight) #convert to "probability"

#plot paremter densities before and after tuning
densplots=list()

x11()
par(mfrow=c(3,3))

for(i in 1:dim(params)[2]){
  priordens=density(params[,i])
  postdens=density(params[,i],weights=sampleweight)
  plot(x=priordens$x,y=priordens$y,col="#135678",lwd=2,xlab="",ylab="Density",main=covparamserror$params[i],las=1,type="l",ylim=range(c(priordens$y,postdens$y)),cex=1.5)
  lines(x=postdens$x,y=postdens$y,col="#84c3a0",lwd=2)
  if(i==3) legend("topright",legend=c("Prior","Posterior"),lwd=2,col=c("#135678","#84c3a0"),bty="n",cex=1.5)
}

#calculate covariance of parameters, weighting by error
samp=sample(1:nsim,nsim,replace=TRUE,prob=sampleweight)

postcov=cor(params[samp,])
diag(postcov)=NA
postcov[3,2]=NA;postcov[2,3]=NA #weak and strong forces are mechanically correlated
colnames(postcov)=covparamserror$params;rownames(postcov)=covparamserror$params
postcov[which(upper.tri(postcov))]=NA
x11()
par(mar=c(9,9,1,6))
plot(postcov[-1,-9],axis.col=list(side=1,las=2),axis.row=list(side=2,las=1),xlab="",ylab="",main="",na.col="grey",col=c('#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850'))
