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

params_tot=cbind(params,sampleweight)
colnames(params_tot)=c(as.character(covparamserror$params),"sampleweight")

fwrite(params_tot,file="data/MC Runs/parameter_tune.csv")

#-----Code to fit m_max and r_max given Andersson 2019 study of the effects of carbon tax in Sweden --------------

#Andersson finds a mean Swedish CO2 tax over the 1991 - 2005 period reduced emissions by 12.5% in 2005
#simulate emissions reduction under real policy values given range of m_max and r_max to fit observations

tax=c(rep(30,9),seq(44,109,length.out=6)) #based on descriptions of Swedish tax scheme

nsamp=10000
testgrid=matrix(nrow=nsamp,ncol=2)
for(i in 1:nsamp){
  testgrid[i,1]=runif(1,min=0.01,max=0.1)
  testgrid[i,2]=runif(1,min=5,max=70)
}

#simulate policy effect using emissions module

mitigationcalibration=function(policy,mmax_t,rmax,r0=2){
  mit=matrix(nrow=length(policy),ncol=length(policy))
    for(i in 1:length(policy)){
     m_t=mmax_t*log(policy[i])/log(300) #300 is maximum value policy can take
     #lifetime of investments also depends on policy
     r_t=min(r0*(1+policy[i]/10),rmax)
     #add effect of current policy
     #mitigation is a t*t matrix of zeroes - fill in columns representing persistent effect of yearly mitigation activity
     futuretime=i:length(policy)-i
     mit[,i]=c(rep(0,i-1),m_t*exp(-futuretime/r_t))
   }
   totmit=rowSums(mit)
   return(totmit[length(policy)]*100)
 }

 calib=numeric(length=dim(testgrid)[1])
 for(j in 1:length(calib)){
   calib[j]=mitigationcalibration(tax,testgrid[j,1],testgrid[j,2])
 }
 
testerror=sqrt((calib-12.5)^2)
testerror=(testerror-mean(testerror))/sd(testerror)
sampleweight=(-1*testerror)-min(-1*testerror) #convert to strictly positive metric increasing in model performance
sampleweight=sampleweight/sum(sampleweight) #convert to "probability"

x11()
par(mfrow=c(1,2))
titles=c("Max Annual Mitigation","Max Mitigation Scaling")
xlabs=c("Fraction Emissions","Years")
for(i in 1:dim(testgrid)[2]){
  priordens=density(testgrid[,i])
  postdens=density(testgrid[,i],weights=sampleweight)
  plot(x=priordens$x,y=priordens$y,col="#135678",lwd=2,xlab=xlabs[i],ylab="Density",main=titles[i],las=1,type="l",ylim=range(c(priordens$y,postdens$y)),cex=1.5)
  lines(x=postdens$x,y=postdens$y,col="#84c3a0",lwd=2)
  if(i==1) legend("topright",legend=c("Prior","Posterior"),lwd=2,col=c("#135678","#84c3a0"),bty="n",cex=1.5)
}
testgrid=cbind(testgrid,sampleweight)
colnames(testgrid)=c("m_max","r_max","sampleweight")

fwrite(testgrid,file="data/MC Runs/parameter_tune_mitigation.csv")

#-------------Monte Carlo of full model, with mitigation, policy, and option parameters weighted by tuning-derived probability----------
source("src/model.R")

polopparams=fread("data/MC Runs/parameter_tune.csv")
mitparams=fread("data/MC Runs/parameter_tune_mitigation.csv")

#initial opinion distribution - not varied, but fixed at particular values from Yale Climate Communications Project
frac_opp_01=0.26 #doubtful and dismissive (global warming 6 americas)
frac_neut_01=0.33 #cautious and disengaged (global warming 6 americas)

mc=100000
params=matrix(nrow=mc,ncol=21)
pol=matrix(nrow=mc,ncol=86);ems=matrix(nrow=mc,ncol=86)

for(i in 1:mc){
  #draw mitigation, policy and opinion parameters, weighting by tuned probability
  polops=as.numeric(polopparams[sample(1:dim(polopparams)[1],size=1,prob=polopparams$sampleweight),1:9])
  homophily_param1=polops[1];forcestrong1=polops[2];forceweak1=polops[3];evidenceeffect1=polops[4];policyopinionfeedback_01=polops[5]
  pol_response1=polops[6];pol_feedback1=polops[7];biassedassimilation1=polops[8];shiftingbaselines1=polops[9]
  
  mit=as.numeric(mitparams[sample(1:dim(mitparams)[1],size=1,prob=mitparams$sampleweight),1:2])
  m_max1=mit[1];r_max1=mit[2]
  
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
  
  m=model()
  #save output
  params[i,]=c(polops,mit,ced_param1,policy_pbcchange_max1,pbc_01,pbc_steep1,opchangeparam,etc_total1,normeffect1,adopt_effect1,lbd_param01,lag_param01)
  pol[i,]=m$policy;ems[i,]=m$totalemissions
  
  if(i%%1000==0) print(i)
}
colnames(params)=c(colnames(polopparams)[1:9],colnames(mitparams)[1:2],"ced","policy_pbc","pbc_init","pbc_steep","policy_adoption","etc_total","normeffect","adopt_effect","lbd_param","lag_param")
fwrite(params,file="data/MC Runs/MC Runs_TunedParams/params.csv")
fwrite(pol,file="data/MC Runs/MC Runs_TunedParams/policy.csv")
fwrite(ems,file="data/MC Runs/MC Runs_TunedParams/emissions.csv")

####------kmeans clustering of tuned output---------
params=fread("data/MC Runs/MC Runs_TunedParams/params.csv")
pol=fread("data/MC Runs/MC Runs_TunedParams/policy.csv")
ems=fread("data/MC Runs/MC Runs_TunedParams/emissions.csv")
mc=dim(params)[1]

df=cbind(pol,ems)
df_scaled=scale(df)
#drop zero variance columns
nacols=which(apply(df_scaled,MARGIN=2,function(x) sum(is.na(x)))==mc)
df_scaled=df_scaled[,-nacols]

#visualize ideal number of clusters
nclustertest=2:10
wss=numeric(length=length(nclustertest))
set.seed(1987)
for(i in 1:length(nclustertest)){
  wss[i]=kmeans(df_scaled,nclustertest[i])$tot.withinss
  print(i)
}
x11()
plot(x=nclustertest,y=wss,type="b",xlab="Number of Clusters",ylab="Within Sum of Squares")

#six clusters looks good
nclus=6
set.seed(1987)
test=kmeans(df_scaled,nclus)

#plot outcomes over time for different clusters
ems=as.data.frame(ems)
ems$cluster=test$cluster

clems=ems%>%
  group_by(cluster)%>%
  summarize_all(mean)
colnames(clems)=c("cluster",2015:2100)
clems=melt(clems,id.vars="cluster")
colnames(clems)=c("Cluster","Year","Emissions")
clems$Cluster=as.factor(clems$Cluster)

nruns=data.frame(table(ems$cluster))
colnames(nruns)=c("Cluster","nsims")
nruns$nsims=nruns$nsims/mc*100
clems$Year=as.numeric(as.character(clems$Year))

clems=merge(clems,nruns)

#add names of scenarios and order from most to least common
clems$Cluster=fct_relevel(clems$Cluster, "5","2","4","3","6")
clems$Cluster=fct_recode(clems$Cluster,"Modal Path"="5","Aggresive Action"="2","Technical Challenges"="3","Delayed Recognition"="4","Business as Usual"="6","Victim of Success"="1")

cols=c("#FED789", "#023743", "#72874E", "#476F84", "#A4BED5", "#c42449")
a=ggplot(clems,aes(x=Year,y=Emissions,group=Cluster,col=Cluster,lwd=nsims))+geom_line()+theme_bw()+theme(text=element_text(size=16))
a=a+scale_color_manual(values=cols)+labs(x="",color="Cluster",lwd="Percent of Runs",y="Global Emissions (GtC per year)")+guides(color = guide_legend(override.aes = list(size = 2))) 

pol=as.data.frame(pol)
pol$cluster=test$cluster
clpol=pol%>%
  group_by(cluster)%>%
  summarize_all(mean)
colnames(clpol)=c("cluster",2015:2100)
clpol=melt(clpol,id.vars="cluster")
colnames(clpol)=c("Cluster","Year","Policy")
clpol$Cluster=as.factor(clpol$Cluster)
clpol=merge(clpol,nruns)
clpol$Year=as.numeric(as.character(clpol$Year))

clpol$Cluster=fct_relevel(clpol$Cluster, "5","2","4","3","6")
clpol$Cluster=fct_recode(clpol$Cluster,"Modal Path"="5","Aggresive Action"="2","Technical Challenges"="3","Delayed Recognition"="4","Business as Usual"="6","Victim of Success"="1")


b=ggplot(clpol,aes(x=Year,y=Policy,group=Cluster,col=Cluster,lwd=nsims))+geom_line()+theme_bw()
b=b+scale_color_manual(values=cols)+labs(x="",color="Cluster",lwd="Percent of Runs",y="Climate Policy Stringency")+ theme(legend.position="none",text=element_text(size=16))

x11()
b+a+plot_layout(ncol=2)

#parameter combinations associated with each cluster
params_cluster=scale(params)
params_cluster=data.frame(params_cluster,cluster=test$cluster)
params_cluster=params_cluster%>%
  group_by(cluster)%>%
  summarize_all(mean)

colnames(params_cluster)=c("Cluster",colnames(params_cluster)[2:10],"Max Mit. Rate","Max Mit Time","CED","Policy-PBC","PBC_Init","PBC_Steep","Policy-Adoption","ETC Effect","Social Norm Effect","Adoption Effect","LBD Effect","Lag Time")
#drop weak force as it doesn't add anything interesting over just the strong force
params_cluster=params_cluster[,-which(colnames(params_cluster)=="Weak.Force")]

params_cluster=melt(params_cluster,id.var="Cluster")
params_cluster$Cluster=as.factor(params_cluster$Cluster)

params_cluster$Cluster=fct_relevel(params_cluster$Cluster, "5","2","4","3","6")
params_cluster$Cluster=fct_recode(params_cluster$Cluster,"Modal Path"="5","Aggresive Action"="2","Technical Challenges"="3","Delayed Recognition"="4","Business as Usual"="6","Victim of Success"="1")

#order parameters to group by component
params_cluster$variable=fct_relevel(params_cluster$variable,"Homophily","Strong.Force","Evidence","Pol.Opinion","CED","Policy-PBC","PBC_Init","PBC_Steep","Policy-Adoption","ETC Effect","Social Norm Effect","Status.Quo.Bias","Pol.Int.Feedback","Max Mit. Rate","Max Mit Time","LBD Effect","Lag Time","Adoption Effect","Biassed.Assimilation","Shifting.Baselines")
  
  fct_relevel(sizes, "small", "medium", "large")

d=ggplot(params_cluster,aes(x=variable,y=value,group=Cluster,fill=Cluster))+geom_bar(stat="identity",position="dodge")
d=d+scale_fill_manual(values=cols)+labs(x="",y="Cluster Mean Value",fill="Cluster")+theme_bw()+theme(axis.text.x = element_text(angle = 90))

#run cluster emissions paths through the climate component to generate temperature trajectories

emissionssplit=split(clems,clems$Cluster)
source("src\\climate_component.R")

cltemp=data.frame(Year=2015:2100)

for(i in 1:length(emissionssplit)){
 
  emissions_dat=emissionssplit[[i]]$Emissions[order(emissionssplit[[i]]$Year)]
  
  #initialize ocean and atmosphere and carbon masses
  temperature=matrix(nrow=length(emissions_dat),ncol=2)
  temperature[1,]=temp_0
  
  mass=matrix(nrow=length(emissions_dat),ncol=3)
  mass[1,]=mass_0
  
  for(t in 2:length(emissions_dat)){
    temp3=temperaturechange(temperature[t-1,],mass[t-1,],emissions_dat[t],ex_forcing1[t],psi1_param=psi1,nu_param=nu)
    mass[t,]=temp3[[1]]
    temperature[t,]=temp3[[2]]
    mass[t,]=temp3[[1]]
    temperature[t,]=temp3[[2]]
  } 
  cltemp=cbind(cltemp,temperature[,1])
}
  

