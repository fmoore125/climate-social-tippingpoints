library(ggplot2)
library(forcats)
#graphs to demonstrate interaction of model tipping points


#1. Individual Behavior

#set some model defaults to demonstrate effect of individual behavior
source("src/model.R")
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
m_max1=0.035
lbd_param=0.2

#loop over ranges of willingness to change behavior and credibility-enhancing display

adoption_param=seq(0,0.7,by=0.01)
cred_param=seq(0,0.5,by=0.01)

params=expand.grid(adoption_param,cred_param)
ems_output=numeric()
for(i in 1:dim(params)[1]){
  m=model(pbc_opinionchange=c(0,0,-1*params[i,1]),ced_param=params[i,2])
  ems_output[i]=m$totalemissions[81] #extract cumulative emissions for 2020-2100
}

params$emissions=ems_output
colnames(params)=c("individual_adoption","ced_effect","emissions_2100")

a=ggplot(params,aes(x=individual_adoption,y=ced_effect,fill=emissions_2100,z=emissions_2100))+geom_tile()
a=a+labs(x="Propensity for Individual Action by Climate Policy Supporters",y="Credibility Enhancing Display Effect",fill="2100\nEmissions\n(GtC)")
a=a+theme_bw()+theme(strip.background =element_rect(fill="white"),legend.title.align = 0.5,text=element_text(size=14))
a=a+scale_fill_gradient(low="blue",high="tomato3")
a=a+geom_contour(breaks=c(2,4,6,8,10,12,14,16,18,20,22),col="black",lwd=0.75)+geom_text_contour(size=6.5,label.placement = label_placement_fraction(),skip=2,rotate=FALSE)
x11()
a

#2. Technical Change
source("src/model.R")
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
  ems_output[i]=sum(m$totalemissions[1:81])
}

params$emissions=ems_output
colnames(params)=c("endogenous_cost","frac_support","status_quo_bias","cumulative_emissions")
params$status_quo_bias=as.factor(params$status_quo_bias);params$status_quo_bias=fct_recode(params$status_quo_bias,"Low Status-Quo Bias"="1.25","High Status Quo Bias"="9")

a=ggplot(params,aes(x=endogenous_cost*100,y=frac_support/0.5,fill=cumulative_emissions,z=cumulative_emissions))+geom_tile()
a=a+facet_wrap(~status_quo_bias)+labs(x="Endogenous Cost Reductions (% per Doubling)",y="Initial Fraction Climate Policy Supporters",fill="Total Emissions\n2020-2100\n(GtC)")
a=a+theme_bw()+theme(strip.background =element_rect(fill="white"),legend.title.align = 0.5,legend.box.just = "center",strip.text=element_text(face="bold",size=12),text=element_text(size=14))
a=a+scale_fill_gradient(low="yellow",high="turquoise3")
a=a+geom_contour(breaks=seq(600,1400,by=50),col="black",lwd=0.75)+geom_text_contour(size=6.5,label.placement = label_placement_fraction(),skip=2,rotate=FALSE)
x11()
a

#3. Perception of Climate Change
source("src/model.R")
frac_opp_01=0.26 #doubtful and dismissive (global warming 6 americas)
frac_neut_01=0.33 #cautious and disengaged (global warming 6 americas)
policyopinionfeedback_01=0
homophily_param=0.95

biassedass_sweep=seq(0,0.9,by=0.05)
perception_sweep=seq(0,0.25,by=0.025 )
baselines=c(0,1)

params=expand.grid(biassedass_sweep,perception_sweep,baselines)

dist_output_pro=numeric()
dist_output_con=numeric()
year=2050
reps=750
for(i in 1:dim(params)[1]){
  print(i)
  temp1=numeric(length=reps);temp2=numeric(length=reps)
  if(params[i,3]==0){
    m=model(biassedassimilation=params[i,1],evidenceeffect=params[i,2],shiftingbaselines=params[i,3])
    dist_output_con[i]=m$distributions[which(m$year==year),1];dist_output_pro[i]=m$distributions[which(m$year==year),3]
    next
  }
  if(params[i,3]>0){ #shifting baselines have a random component so average over many model runs
    for(j in 1:reps){
      m=model(biassedassimilation=params[i,1],evidenceeffect=params[i,2],shiftingbaselines=params[i,3])
      temp1[j]=m$distributions[which(m$year==year),3]
      temp2[j]=m$distributions[which(m$year==year),1]
    }
    dist_output_pro[i]=mean(temp1);dist_output_con[i]=mean(temp2)
  }
}

params$opposed=dist_output_con
colnames(params)=c("biassed_assimilation","evidenceeffect","shiftingbaseline","opposers2050")
params$shiftingbaseline=as.factor(params$shiftingbaseline);params$shiftingbaseline=fct_recode(params$shiftingbaseline,"Shifting Baseline"="1","Fixed Baseline"="0")

a=ggplot(params,aes(x=biassed_assimilation,y=evidenceeffect,fill=opposers2050*100,z=opposers2050*100))+geom_tile()
a=a+facet_wrap(~shiftingbaseline)+labs(x="Biased Assmiliation",y="Effect of Perceived Weather on Opinion",fill="Policy Opposers\n2050 (%)")
a=a+theme_bw()+theme(strip.background =element_rect(fill="white"),legend.title.align = 0.5,legend.box.just = "center",strip.text=element_text(face="bold",size=12),text=element_text(size=14))
a=a+scale_fill_gradient(low="darkorchid",high="palegreen2")
#a=a+geom_contour(breaks=seq(2,8,by=0.5),col="black",lwd=0.75)+geom_text_contour(size=6.5,label.placement = label_placement_fraction(),skip=1,rotate=FALSE)
x11()
a
