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

adoption_param=seq(0,0.7,by=0.01)
cred_param=seq(0,0.5,by=0.01)
lbd_par=c(0,0.2)

params=expand.grid(adoption_param,cred_param,lbd_par)
ems_output=numeric()
for(i in 1:dim(params)[1]){
  m=model(pbc_opinionchange=c(0,0,-1*params[i,1]),ced_param=params[i,2],lbd_param=params[i,3])
  ems_output[i]=m$totalemissions[86] #extract cumulative emissions for 2020-2100
}

params$emissions=ems_output
colnames(params)=c("individual_adoption","ced_effect","lbd_effect","emissions_2100")
params$lbd_effect=as.factor(params$lbd_effect);params$lbd_effect=fct_recode(params$lbd_effect,"No Endogenous Cost Reductions"="0","Strong Endogenous Cost Reductions"="0.2")

a=ggplot(params,aes(x=individual_adoption,y=ced_effect,fill=emissions_2100))+geom_tile()
a=a+facet_wrap(~lbd_effect)+labs(x="Propensity for Individual Action by Climate Policy Supporters",y="Credibility Enhancing Display Effect",fill="2100\nEmissions\n(GtC)")
a=a+theme_bw()+theme(strip.background =element_rect(fill="white"),legend.title.align = 0.5,legend.box.just = "center",strip.text=element_text(face="bold",size=12),text=element_text(size=14))
a=a+scale_fill_gradient(low="blue",high="tomato3")
x11()
a

#2. Technical Change
homophily_param1=0.75
frac_opp_01=0.2
frac_neut_01=0.50
forcestrong1=0.3
evidenceeffect1=0
policyopinionfeedback_01=0
m_max1=0.025
pol_feedback1=0

lbd_sweep=seq(0,0.25,by=0.005)
fracsupp_sweep=seq(0.15,0.35,by=0.005)
sq_sweep=c(1.25,9)

params=expand.grid(lbd_sweep,fracsupp_sweep,sq_sweep)
ems_output=numeric()
for(i in 1:dim(params)[1]){
  m=model(lbd_param=params[i,1],frac_opp_0=0.5-params[i,2],pol_response=params[i,3])
  ems_output[i]=sum(m$emissions[6:86])
}

params$emissions=ems_output
colnames(params)=c("endogenous_cost","frac_support","status_quo_bias","cumulative_emissions")
params$status_quo_bias=as.factor(params$status_quo_bias);params$status_quo_bias=fct_recode(params$status_quo_bias,"Low Status-Quo Bias"="1.25","High Status Quo Bias"="9")

a=ggplot(params,aes(x=endogenous_cost*100,y=frac_support/0.5,fill=cumulative_emissions))+geom_tile()
a=a+facet_wrap(~status_quo_bias)+labs(x="Endogenous Cost Reductions (% per Doubling)",y="Fraction Climate Policy Supporters",fill="Total Emissions\n2020-2100\n(GtC)")
a=a+theme_bw()+theme(strip.background =element_rect(fill="white"),legend.title.align = 0.5,legend.box.just = "center",strip.text=element_text(face="bold",size=12),text=element_text(size=14))
a=a+scale_fill_gradient(low="yellow",high="turquoise3")
x11()
a

