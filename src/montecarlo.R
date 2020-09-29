source("src/model.R")
library(doParallel)
library(foreach)
library(reshape2)
library(ggplot2)
library(viridis)
library(patchwork)
library(randomForest)
library(randomForestExplainer)

#-----Define MC Parameters ---------------

#----opinion component

#network homophily
homophily_param_mc=c(0.35,0.7,0.95)

#policy opinion feedback
policyopinionfeedback_param_mc=c(0,0.03)

#evidence effect
evidenceeffect_mc=c(0,0.4)

#credibility-enhancing display
ced_param_mc=c(0,0.4)

#initial opinion distribution - not varied, but fixed at particular values from Yale Climate Communications Project
frac_opp_mc=0.26 #doubtful and dismissive (global warming 6 americas)
frac_neut_mc=0.33 #cautious and disengaged (global warming 6 americas)



#----policy Component

#status-quo bias
pol_response_mc=c(1,5,9)


#interest group feedback
pol_feedback_mc=c(-8,0,8)


#----adoption component

#norm effectiveness
normeffect_mc=c(0,0.4)

#opinion effect on adoption
pbc_opinionchange_mc=c(0,0.6)


#technical change feedback
etc_total_mc=c(0,2)


#----emissions component

#max annual emissions reduction
m_max_mc=c(0.01,0.03,0.05)

#learning by doing parameter
lbd_param_mc=c(0,0.2)

#effectiveness of adoption
adopt_effect_mc=c(0.1,0.3)

#----cognition component

#shifting vs fixed baseline
shiftingbaselines_mc=c(0,1)

#biased assimilation
biassedassimilation_mc=c(0,0.6)

#-----Run Monte Carlo ----------

#parameter grid
pgrid=expand.grid(homophily_param_mc,policyopinionfeedback_param_mc,evidenceeffect_mc,ced_param_mc,pol_response_mc,pol_feedback_mc,normeffect_mc,pbc_opinionchange_mc,etc_total_mc,m_max_mc,lbd_param_mc,adopt_effect_mc,shiftingbaselines_mc,biassedassimilation_mc)
colnames(pgrid)=c("homophily_param","policyopinionfeedback_param","evidenceeffect","ced_param","pol_response","pol_feedback","normeffect","pbc_opinionchange","etc_total","m_max","lbd_param","adopt_effect","shiftingbaselines","biassedassimilation")

#loop through parameters and run model for each combination

#helper function to parameterize model
helper=function(vec,outputs=c("distributions","adoptersfrac","policy","totalemissions","temp"),initfracs=c(frac_opp_mc,frac_neut_mc)){
  m=model(homophily_param=vec[1],policyopinionfeedback_param=vec[2],evidenceeffect=vec[3],ced_param=vec[4],pol_response=vec[5],pol_feedback=vec[6],normeffect=vec[7],pbc_opinionchange=c(vec[8],0,-1*vec[8]),etc_total=vec[9],m_max=vec[10],lbd_param=vec[11],adopt_effect=vec[12],shiftingbaselines=vec[13],biassedassimilation=vec[14],frac_opp_0=initfracs[1],frac_neut_0=initfracs[2])
  return(m[outputs])
}

cl=makeCluster(20)
clusterExport(cl,c("pgrid","helper","frac_opp_mc","frac_neut_mc","model","forcestrong1","forceweak1","pol_window1","policy_pbcchange_max1","policy_01","adoptfrac_opp_01","adoptfrac_neut_01","adoptfrac_supp_01","pbc_mid1","pbc_steep1","pbc_01","etc_mid1","etc_steep1","networkfunc","forcefunc","emissions","bau1","bau_outside1","lag_param01","r_max1","r_01","ex_forcing1","anomaly","temp_0","mass_0","psi1","nu","forc_param"))
registerDoParallel(cl)

mcmods=foreach(i=1:dim(pgrid)[1])%dopar%{
  helper(as.numeric(pgrid[i,]))
}
save(mcmods,file="data/MC Runs/mcmods.Rdat")


#-------Analyze MC Output------------

load("data/MC Runs/mcmods.Rdat")

#key feedbacks: policyopinion, ced, evidenceeffect, pol_feedback, normeffect, etc_total, lbd_param
#for each key feedback:
#1) find change in distribution of outputs when on vs off
#2) random forest to identify drivers of variance

#two relevant variables - policy and total emissions

keys=c("policyopinionfeedback_param","evidenceeffect","ced_param","pol_feedback","normeffect","etc_total","lbd_param")

cl=makeCluster(20)
clusterExport(cl,c("mcmods","keys","pgrid"))
registerDoParallel(cl)

policy_mc=list();emissions_mc=list();matches_mc=list()

for(i in 2:length(keys)){
  if(i==4) next #skip policy feedback because 3 values instead of 2
  #divide parameter grid into 2 based on presence or absence of feedback
  keyvar=which(colnames(pgrid)==keys[i])
  pgrid_1=which(pgrid[,keyvar]!=0)
  
  clusterExport(cl,c("keyvar","pgrid_1"))
  matches=foreach(j=1:length(pgrid_1),.combine="c")%dopar%{
    tomatch=pgrid[pgrid_1[j],]; tomatch[keyvar]=0
    which(apply(pgrid, 1, function(x) identical(as.numeric(x), as.numeric(tomatch))))
  }
  
  clusterExport(cl,c("matches"))
  diff_policy=foreach(j=1:length(pgrid_1),.combine="rbind")%dopar%{
    mcmods[[pgrid_1[j]]]$policy-mcmods[[matches[j]]]$policy
  }
  diff_emissions=foreach(j=1:length(pgrid_1),.combine="rbind")%dopar%{
    mcmods[[pgrid_1[j]]]$totalemissions-mcmods[[matches[j]]]$totalemissions
  }
  policy_mc[[i]]=diff_policy;emissions_mc[[i]]=diff_emissions;matches_mc[[i]]=matches
  print(i)
}

#slighly different for pol_feedback because can take positive, zero and negative values
i=4
keyvar=which(colnames(pgrid)==keys[i])
pgrid_pos=which(pgrid[,keyvar]>0)
pgrid_neg=which(pgrid[,keyvar]<0)

clusterExport(cl,c("keyvar","pgrid_pos","pgrid_neg"))
matches_pos=foreach(j=1:length(pgrid_pos),.combine="c")%dopar%{
  tomatch=pgrid[pgrid_pos[j],]; tomatch[keyvar]=0
  which(apply(pgrid, 1, function(x) identical(as.numeric(x), as.numeric(tomatch))))
}
matches_neg=foreach(j=1:length(pgrid_neg),.combine="c")%dopar%{
  tomatch=pgrid[pgrid_neg[j],]; tomatch[keyvar]=0
  which(apply(pgrid, 1, function(x) identical(as.numeric(x), as.numeric(tomatch))))
}
clusterExport(cl,c("matches_pos","matches_neg"))
diff_policy_pos=foreach(j=1:length(pgrid_pos),.combine="rbind")%dopar%{
  mcmods[[pgrid_pos[j]]]$policy-mcmods[[matches_pos[j]]]$policy
}
diff_policy_neg=foreach(j=1:length(pgrid_neg),.combine="rbind")%dopar%{
  mcmods[[pgrid_neg[j]]]$policy-mcmods[[matches_neg[j]]]$policy
}
diff_emissions_pos=foreach(j=1:length(pgrid_pos),.combine="rbind")%dopar%{
  mcmods[[pgrid_pos[j]]]$totalemissions-mcmods[[matches_pos[j]]]$totalemissions
}
diff_emissions_neg=foreach(j=1:length(pgrid_neg),.combine="rbind")%dopar%{
  mcmods[[pgrid_neg[j]]]$totalemissions-mcmods[[matches_neg[j]]]$totalemissions
}
policy_mc[[i]][[1]]=diff_policy_pos;policy_mc[[i]][[2]]=diff_policy_neg
emissions_mc[[i]][[1]]=diff_emissions_pos;emissions_mc[[i]][[2]]=diff_emissions_neg
matches_mc[[i]][[1]]=matches_pos;matches_mc[[i]][[2]]=matches_neg

save(policy_mc,emissions_mc,matches_mc,file="data/MC Runs/mc_results.Rdat")

#-----------Graphing Output ---------------
years=2015:2100
relyears_pol=2030
relyears_temp=2100
inds_pol=which(years%in%relyears_pol);inds_ems=which(years%in%relyears_temp)

plots=list()
eplots=list()
titles=c("Policy-Opinion","Evidence Effect","Credibility-Enhancing Feedback","Pol. Int. Feedback","Social Norm Effect","Adoption Cost Feedback","Learning by Doing Feedback")
for(i in 1:length(policy_mc)){
  if(i!=4){
    policydists=policy_mc[[i]][,inds_pol]
    if(length(relyears_pol)>1){
      colnames(policydists)=relyears_pol
      policydists=melt(policydists)
      colnames(policydists)=c("MC","Year","Policy_Effect")
    }
    if(length(relyears_pol)==1){
      policydists=data.frame(MC=1:length(policydists),Year=rep(relyears_pol,length(policydists)),Policy_Effect=policydists)
    }
    policydists$Year=as.factor(policydists$Year)
    policydists$Policy_Effect[which(policydists$Policy_Effect==0)]=NA
    a=ggplot(policydists,aes(x=Policy_Effect,group=Year,fill=Year))+geom_density(alpha=0.6)+theme_bw()
    if(i%in%c(1,5)) a=a+labs(x="Difference in Policy",y="Density (Removing Exact Zeroes)",title=paste0(titles[i],", Policy"))
    if(i!=1&i!=5) a=a+labs(x="Difference in Policy",y="",title=paste0(titles[i],", Policy"))
    if(i!=7) a=a+scale_fill_discrete(guide=FALSE)+scale_color_discrete(guide=FALSE)
    plots[[i]]=a
    
    emissionsdist=emissions_mc[[i]][,inds_ems]
    if(length(relyears_temp)>1){
      colnames(emissionsdist)=relyears
      emissionsdist=melt(emissionsdist)
      colnames(emissionsdist)=c("MC","Year","Emissions_Effect")
    }
    if(length(relyears_temp)==1){
      emissionsdist=data.frame(MC=1:length(emissionsdist),Year=rep(relyears_temp,length(emissionsdist)),Emissions_Effect=emissionsdist)
    }
    
    emissionsdist$Year=as.factor(emissionsdist$Year)
    
    b=ggplot(emissionsdist[-which(emissionsdist$Emissions_Effect==0),],aes(x=Emissions_Effect,group=Year,fill=Year))+geom_density(alpha=0.6)+theme_bw()
    if(i%in%c(1,5)) b=b+labs(x="Difference in Emissions (GtC)",y="Density (Removing Exact Zeroes)",title=paste0(titles[i],", Emissions"))
    if(i!=1&i!=5) b=b+labs(x="Difference in Emissions (GtC)",y="",title=paste0(titles[i],", Emissions"))
    if(i!=7) b=b+scale_fill_discrete(guide=FALSE)+scale_color_discrete(guide=FALSE)
    eplots[[i]]=b
    
  }
  if(i==4){
    plots[[i]]=list();eplots[[i]]=list()
    for(j in 1:2){
      policydists=policy_mc[[i]][[j]][,inds_pol]
      if(length(relyears_pol)>1){
        colnames(policydists)=relyears
        policydists=melt(policydists)
        colnames(policydists)=c("MC","Year","Policy_Effect")
      }
      if(length(relyears_pol)==1){
        policydists=data.frame(MC=1:length(policydists),Year=rep(relyears_pol,length(policydists)),Policy_Effect=policydists)
      }
      policydists$Year=as.factor(policydists$Year)
      a=ggplot(policydists[-which(policydists$Policy_Effect==0),],aes(x=Policy_Effect,group=Year,fill=Year))+geom_density(alpha=0.6)+theme_bw()
      a=a+labs(x="Difference in Policy",y="",title=paste0(titles[i],", Policy"))
      a=a+scale_fill_discrete(guide=FALSE)+scale_color_discrete(guide=FALSE)+annotate("text",x=-480,y=ifelse(j==1,0.02,0.05),label=ifelse(j==1,"Positive","Negative"),size=4)
      plots[[i]][[j]]=a
      
      emissionsdist=emissions_mc[[i]][[j]][,inds_ems]
      if(length(relyears_temp)>1){
        colnames(emissionsdist)=relyears
        emissionsdist=melt(emissionsdist)
        colnames(emissionsdist)=c("MC","Year","Emissions_Effect")
      }
      if(length(relyears_temp)==1){
        emissionsdist=data.frame(MC=1:length(emissionsdist),Year=rep(relyears_temp,length(emissionsdist)),Emissions_Effect=emissionsdist)
      }
      
      emissionsdist$Year=as.factor(emissionsdist$Year)
      
      b=ggplot(emissionsdist[-which(emissionsdist$Emissions_Effect==0),],aes(x=Emissions_Effect,group=Year,fill=Year))+geom_density(alpha=0.6)+theme_bw()
      b=b+labs(x="Difference in Emissions (GtC)",y="",title=paste0(titles[i],", Emissions"))
      b=b+scale_fill_discrete(guide=FALSE)+scale_color_discrete(guide=FALSE)+annotate("text",x=-18,y=ifelse(j==1,2,2),label=ifelse(j==1,"Positive","Negative"),size=4)
      eplots[[i]][[j]]=b
    }
   
  }
  
}

x11()
plots[[1]]+plots[[2]]+plots[[4]][[1]]+plots[[4]][[2]]+plots[[5]]+plots[[3]]+plots[[6]]+plots[[7]]+plot_layout(ncol=4)
x11()
eplots[[1]]+eplots[[2]]+eplots[[4]][[1]]+eplots[[4]][[2]]+eplots[[5]]+eplots[[3]]+eplots[[6]]+eplots[[7]]+plot_layout(ncol=4)

#------Random Forest Analysis of Drivers of Differences ------------
load(file="data/MC Runs/mc_results.Rdat")

emissions_year=2100
policy_year=2030

#merge emissions and policy differences with values of other parameters for random forest
#perform random forest
rf_pol_list=list()
rf_ems_list=list()
rf_pol_plots=list()
rf_ems_plots=list()

for(i in 1:length(keys)){
  inds=which(colnames(pgrid)==keys[i])
  if(i!=4){
    y_pol=policy_mc[[i]][,which(years==policy_year)]
    y_ems=emissions_mc[[i]][,which(years==emissions_year)]
    x=pgrid[matches_mc[[i]],-inds]
    rf_pol=randomForest(x=x,y=y_pol,importance=TRUE, tree=TRUE)
    rf_ems=randomForest(x=x,y=y_ems,importance=TRUE, tree=TRUE)
    
    min_depth_pol=min_depth_distribution(rf_pol)
    min_depth_ems=min_depth_distribution(rf_ems)
    
    a=plot_min_depth_distribution(min_depth_pol, mean_sample = "all_trees", k = 13)
    a=a+labs(x="",title=paste0(titles[i],", ",policy_year," Policy"))
    
    b=plot_min_depth_distribution(min_depth_ems, mean_sample = "all_trees", k = 13)
    b=b+labs(x="",title=paste0(titles[i],", ",emissions_year," Emissions"))
    
    rf_pol_list[[i]]=list(rf_pol,min_depth_pol)
    rf_ems_list[[i]]=list(rf_ems,min_depth_ems)
    
    rf_pol_plots[[i]]=a
    rf_ems_plots[[i]]=b
    
    print(i)
  }
}

for(i in 1:length(rf_pol_plots)){
  if(i==4) next
  
  x11()
  rf_pol_plots[[i]]+rf_ems_plots[[i]]+plot_layout(ncol=2)
}

i=4
inds=which(colnames(pgrid)==keys[i])
rf_pol_list[[i]]=list()
rf_ems_list[[i]]=list()
rf_pol_plots[[i]]=list()
rf_ems_plots[[i]]=list()
for(j in 1:2){
  y_pol=policy_mc[[i]][[j]][,which(years==policy_year)]
  y_ems=emissions_mc[[i]][[j]][,which(years==emissions_year)]
  x=pgrid[matches_mc[[i]][[j]],-inds]
  rf_pol=randomForest(x=x,y=y_pol,importance=TRUE, tree=TRUE)
  rf_ems=randomForest(x=x,y=y_ems,importance=TRUE, tree=TRUE)
  
  min_depth_pol=min_depth_distribution(rf_pol)
  min_depth_ems=min_depth_distribution(rf_ems)
  
  a=plot_min_depth_distribution(min_depth_pol, mean_sample = "all_trees", k = 13)
  a=a+labs(x="",title=paste0(titles[i],ifelse(j==1," (Pos)"," (Neg)"),", ",policy_year," Policy"))
  
  b=plot_min_depth_distribution(min_depth_ems, mean_sample = "all_trees", k = 13)
  b=b+labs(x="",title=paste0(titles[i],ifelse(j==1," (Pos)"," (Neg)"),", ",emissions_year," Emissions"))
  
  rf_pol_list[[i]][[j]]=list(rf_pol,min_depth_pol)
  rf_ems_list[[i]][[j]]=list(rf_ems,min_depth_ems)
  
  rf_pol_plots[[i]][[j]]=a
  rf_ems_plots[[i]][[j]]=b
  print(j)
}

save(rf_pol_plots,rf_ems_plots,rf_pol_list,rf_ems_list,file="data/MC Runs/randomforests.Rdat")


rf_pol_plots[[4]][[1]]+rf_pol_plots[[4]][[2]]+rf_ems_plots[[4]][[1]]+rf_ems_plots[[4]][[2]]+plot_layout(ncol=2)

###------------Determinants of 1.5 and 2 degrees by 2100 ------------------------

cl=makeCluster(6)
clusterExport(cl,c("mcmods"))
registerDoParallel(cl)

templim=foreach(i=1:length(mcmods),.combine="rbind")%dopar%{
  c(ifelse(mcmods[[i]]$temp[86,1]<2,1,0),ifelse(mcmods[[i]]$temp[86,1]<1.5,1,0))
}

stopCluster(cl)

#random forest to predict keeping temp under 1 and 2 degrees

rf_2deg=randomForest(x=pgrid,y=as.factor(templim[,1]),importance=TRUE, tree=TRUE)
rf_15deg=randomForest(x=pgrid,y=as.factor(templim[,2]),importance=TRUE, tree=TRUE)

importance_2deg=measure_importance(rf_2deg)
importance_15deg=measure_importance(rf_15deg)

save(rf_2deg,rf_15deg,importance_2deg,importance_15deg,templim,file="data/MC Runs/templim_randomforests.Rdat")

##important variables
a=plot_multi_way_importance(importance_2deg, size_measure = "no_of_nodes",no_of_labels = 14,main="Important Variables, 2 degrees by 2100 Random Forest Classification")+scale_color_manual(values=c("blue","black"),guide=FALSE)+labs(x="Mean Minimum Depth",y="Times a Root",size="Total Number\nof Nodes")
a

#interactions with important variables
vars2deg <- important_variables(importance_2deg, k = 5, measures = c("mean_min_depth", "no_of_nodes"))
interactions2deg=min_depth_interactions(rf_2deg,vars2deg)
save(interactions2deg,file="data/MC Runs/rfinteractions_2deg.Rdat")
plot_min_depth_interactions(interactions2deg,main="2 degrees by 2100 Classification, 30 Important Interactions")+labs(x="",y="Mean Minimum Depth")

#simple plot of parameter values for runs that do / don't stay under 2 degrees by 2100
dat=data.frame(pgrid,two_deg=templim[,1])
#recode into qualitative values
dat$homophily_param=factor(dat$homophily_param,labels=c("Low","Mid","High"),ordered=T)
dat$policyopinionfeedback_param=factor(dat$policyopinionfeedback_param,labels=c("Off","On"))
dat$evidenceeffect=factor(dat$evidenceeffect,labels=c("Off","On"))
dat$ced_param=factor(dat$ced_param,labels=c("Off","On"))
dat$pol_response=factor(dat$pol_response,labels=c("Off","Mid","High"),ordered=T)
dat$pol_feedback=factor(dat$pol_feedback,labels=c("Negative","Off","Positive"),ordered=T)
dat$normeffect=factor(dat$normeffect,labels=c("Off","On"))
dat$pbc_opinionchange=factor(dat$pbc_opinionchange,labels=c("Off","On"))
dat$etc_total=factor(dat$etc_total,labels=c("Off","On"))
dat$m_max=factor(dat$m_max,labels=c("Low","Mid","High"),ordered=T)
dat$lbd_param=factor(dat$lbd_param,labels=c("Off","On"))
dat$adopt_effect=factor(dat$adopt_effect,labels=c("Low","High"),ordered=T)
dat$shiftingbaselines=factor(dat$shiftingbaselines,labels=c("Off","On"))
dat$biassedassimilation=factor(dat$biassedassimilation,labels=c("Off","On"))

dat=melt(dat,id.vars="two_deg")

levels(dat$variable)=c("Social Network Homophily","Policy-Opinion Feedback","Evidence Effect","Cred-Enhancing Display","Status-Quo Bias","Political Interest Feedback","Social-Norm Effect","Adoption-Opinion Effect","Adoption Cost Feedback","Max Mitigation Rate","Learning-By-Doing Effect","Adoption Efficacy","Shifting Baselines","Biassed Assimilation")

dat_agg=dat%>%
  group_by(variable,value)%>%
  summarize(mean=mean(two_deg),sd=sd(two_deg))

import=dat_agg%>%
  group_by(variable)%>%
  summarize(import=max(mean)-min(mean))%>%
  arrange(desc(import))

import$component=c("Mitigation","Opinion","Cognition","Policy","Opinion","Opinion","Mitigation","Adoption","Adoption","Opinion","Policy","Adoption","Cognition","Mitigation")

cols=data.frame(component=c("Opinion","Adoption","Policy","Mitigation","Cognition"),cols=c("#76a7a2","#7c4a4f","#e1924d","#255542","#e1a497"))


dat_agg$variable=factor(dat_agg$variable,levels=import$variable)

x11()
par(mfrow=c(4,4),mar=c(4,3,4,2))
for(i in 1:length(levels(dat_agg$variable))){
  tempdat=dat_agg%>%filter(variable==levels(dat_agg$variable)[i])
  if(i%in%c(1,6)){
    tempdat$value=factor(tempdat$value,levels=c("Low","Mid","High"))
    tempdat=tempdat%>%arrange(value)
  }
  barplot(tempdat$mean,names.arg = tempdat$value,ylim=c(0,0.55),col=as.character(cols$cols[which(cols$component==import$component[i])]),las=1,main=paste0(i,")  ",levels(dat_agg$variable)[i]))
  abline(h=mean(templim[,1]),col="#a6152a",lwd=2)
}
plot.new()
par(mar=c(0,0,0,0))
legend("center",legend=cols$component[1:3],fill=as.character(cols$cols)[1:3],bty="n",cex=1.5,y.intersp = 2)
plot.new()
par(mar=c(0,0,0,0))
legend("center",legend=cols$component[4:5],fill=as.character(cols$cols)[4:5],bty="n",cex=1.5,y.intersp = 2)

a=ggplot(dat_agg,aes(x=value,y=mean,fill=value))+geom_bar(stat="identity",position="dodge")+theme_bw()+facet_wrap(~variable)
a


