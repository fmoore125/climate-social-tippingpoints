library(ggplot2)
library(forcats)
#graphs to demonstrate interaction of model tipping points

source("src/model.R")

#1. Individual Behavior

#set some model defaults to demonstrate effect of individual behavior
homophily_param1=0.7
frac_opp_01=0.2
frac_neut_01=0.6
forcestrong1=0.2
pol_feedback1=5
pbc_mid1=-0.5
pbc_steep1=1.5
pbc_opinionchange1=c(0,0,-0.5)
evidenceeffect1=0
policyopinionfeedback_01=0
m_max1=0.025

#loop over ranges of willingness to change behavior and credibility-enhancing display

adoption_param=seq(0,0.7,by=0.05)
cred_param=seq(0,0.5,by=0.05)
lbd_par=c(0,0.2)

params=expand.grid(adoption_param,cred_param,lbd_par)
ems_output=numeric()
for(i in 1:dim(params)[1]){
  m=model(pbc_opinionchange=c(0,0,-1*params[i,1]),ced_param=params[i,2],lbd_param=params[i,3])
  ems_output[i]=m$emissions[86] #extract cumulative emissions for 2020-2100
}

params$emissions=ems_output
colnames(params)=c("individual_adoption","ced_effect","lbd_effect","emissions_2100")
params$lbd_effect=as.factor(params$lbd_effect);params$lbd_effect=fct_recode(params$lbd_effect,"No LBD Feedback"="0","Strong LBD Feedback"="0.2")

a=ggplot(params,aes(x=individual_adoption,y=ced_effect,fill=emissions_2100))+geom_tile()
a=a+facet_wrap(~lbd_effect)+labs(x="Propensity for Individual Action",y="Credibility Enhancing Display Effect",fill="2100\nEmissions\n(GtC)")
a=a+theme_bw()+theme(strip.background =element_rect(fill="white"),legend.title.align = 0.5,legend.box.just = "center",strip.text=element_text(face="bold",size=12),text=element_text(size=14))
x11()
a
