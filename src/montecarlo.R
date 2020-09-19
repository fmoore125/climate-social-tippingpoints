source("src/model.R")
library(doParallel)
library(foreach)

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

