source("src/model.R")
library(doParallel)
library(foreach)

#parameters to vary

#----opinion component

#network homophily
homophily_param_mc=c(0.35,0.7,0.95)

#policy opinion feedback
policyopinionfeedback_param_mc=c(0,0.015,0.03)

#evidence effect
evidenceeffect_mc=c(0,0.2,0.4)

#credibility-enhancing display
ced_param_mc=c(0,0.2,0.4)

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
normeffect_mc=c(0,0.2,0.4)

#opinion effect on adoption_supporters
pbc_opinionchange_mc=c(0,0.3,0.6)


#technical change feedback
etc_total_mc=c(0,1,2)


#----emissions component

#max annual emissions reduction
m_max_mc=c(0.01,0.03,0.05)

#learning by doing parameter
lbd_param_mc=c(0,0.1,0.2)

#effectiveness of adoption
adopt_effect_mc=c(0.1,0.2,0.3)

#----cognition component

#shifting vs fixed baseline
shiftingbaselines_mc=c(0,1)

#biased assimilation
biassedassimilation_mc=c(0,0.3,0.6)

#parameter grid
pgrid=expand.grid(homophily_param_mc,policyopinionfeedback_param_mc,evidenceeffect_mc,ced_param_mc,pol_response_mc,pol_feedback_mc,normeffect_mc,pbc_opinionchange_mc,etc_total_mc,m_max_mc,lbd_param_mc,adopt_effect_mc,shiftingbaselines_mc,biassedassimilation_mc)

#loop through parameters and run model for each combination

#helper function to parameterize model
helper=function(vec,outputs=c("distributions","adoptersfrac","policy","totalemissions","temp"),initfracs=c(frac_opp_mc,frac_neut_mc)){
  m=model(homophily_param=vec[1],policyopinionfeedback_param=vec[2],evidenceeffect=vec[3],ced_param=vec[4],pol_response=vec[5],pol_feedback=vec[6],normeffect=vec[7],pbc_opinionchange=c(vec[8],0,-1*vec[8]),etc_total=vec[9],m_max=vec[10],lbd_param=vec[11],adopt_effect=vec[12],shiftingbaselines=vec[13],biassedassimilation=vec[14],frac_opp_0=initfracs[1],frac_neut_0=initfracs[2])
  return(m[outputs])
}

cl=makeCluster(20)
clusterExport(cl,c("pgrid","helper","frac_opp_mc","frac_neut_mc","model","forcestrong1","forceweak1","pol_window1","policy_pbcchange_max1","policy_01","adoptfrac_opp_01","adoptfrac_neut_01","adoptfrac_supp_01","pbc_mid1","pbc_steep1","pbc_01","etc_mid1","etc_steep1","networkfunc","forcefunc","emissions","bau1","bau_outside1","lag_param01","r_max1","r_01","ex_forcing1","anomaly"))
registerDoParallel(cl)

mcmods=foreach(i=1:dim(pgrid)[1])%dopar%{
  helper(as.numeric(pgrid[i,]))
}

