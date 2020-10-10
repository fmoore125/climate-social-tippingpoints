#parameters

###-----Opinion Dynamics ----------------------
#starting distribution of opinion
frac_opp_01=0.5 #fraction of population opposing climate policy at t=0
frac_neut_01=0.4 #fraction of population neutral at t=0

#self-similarity parameter - how much more likely are you to preferentially encounter other people in the same group?
#measure of sorting of the population by opinion (homophily?)
#assume same for each group, and that other contacts are equally split between other groups
#number between 1/3 (no homophily) and 1 (no connection between different groups)
homophily_param1=0.8

#effect of contact on opinion - i.e. effect on opinion, conditional on contact
#assume symmetric to avoid parameter explosion
forcestrong1=0.2 #effect of someone with strong opinions on neutral opinions
forceweak1=0.1 #effect of someone with neutral opinions on strong opinions

#credibility-enhancing display - this parameter controls a feedback from adoption to opinion
#if it is greater than zero, the persuasive force of people who support climate policy increases with the fraction that are adopters
#the parameter gives the largest possible change in persuasive force (starting at forcestrong)- i.e the change in persuasive force if all supporters are adopters
ced_param1=0

#this parameter scales the evidence from weather from the congition component to changes in the probability of transition
#a value of 0 means people are only influenced by social norm dynamics, not by experience of weather
#a value of 0.1 means a perceived anomaly of 1 degree will increase the probability of transition toward a more climate-believing state by 0.1
evidenceeffect1=0.1

#this parameter governs the feedback from policy to opinion e.g. via the institutional signalling of norms. 
#a value of 0 turns this feebdack off so that opinion does not depend on policy change
#a value of 0.01 means a policy change of 5 last period increases the probability of supporting climate policy by 0.05
policyopinionfeedback_01=0.01


###--------Responsiveness of Policy to Opinion Distribution-----------------------

#policy responsiveness -> how many more people in favor of policy change do you need before you get change?
#measures how much inertia there is in the political system
#values should be >=1 
#a value of 1 implies fully responsive, no intertia, values much larger in 1 imply inertia 
pol_response1=1.5

#there is a feedback within the policy component that allows policy over time to create political constituencies in favor of more policy
#the magnitude of this effect depends on the average policy value over some previous time period (window) and a feedback strength parameter
#negative values for the policy feedback are possible - these capture the opposite effect where policy change motivates political constituencies in opposition to that change

pol_window1=10 #number of years of prior policy that determine strength of current policy feedback
pol_feedback1=3 #maximum change (in absolute terms) in pol responsiveness parameter (compare to base value, pol_response). 


#this parameter gives the extent to which policy can raise or lower PBC 
#change is linear in policy up to this absolute amount
#reference values to consider would be inital pbc (pbc_0) and adoption midpoint (pbc_mid) below
policy_pbcchange_max1=0.5


#initial policy - zero=> no effect, negative numbers => support for fossil fues, positive number => support for mitigation
policy_01=0



###------------Adoption of Mitigative Behaviors-------------------------
#initial fraction of adoptors in each opinion bucket
adoptfrac_opp_01=0
adoptfrac_neut_01=0
adoptfrac_supp_01=0

#fraction of adopters as a function of PBC is a logistic uptake curve, defined by two parameters - midpoint and steepness
pbc_mid1=0
pbc_steep1=2

#opinion effect on adoption - to what extent does opinion on climate change affect probability of adopting mitigative behaviors?
#parameterized as a shift in the midpoint of the adoption-pbc curve to the left => negative numbers mean adoption more likely conditional on pbc
#defined relative to neutral, so middle value should be 0
pbc_opinionchange1=c(0.2,0,-0.5) #effect of climate chnage opinion on probability of adoption for opposition, neutral, support

#initial pbc
pbc_01=-1.5

#pbc change with adoption - endogenous technical change / network effects lowers costs of adoption
#another logistic curve describes change in pbc with number of adopters
#three parameters - total change in pbc allowable, midpoint (betwen 0 and 1 - higher numbers imply more people needed before significant cost reduction effects), steepness
etc_mid1=0.5 #etc = endogenous technical change
etc_total1=2 #reference point should be initial pbc, pbc_0 - this parameter determines how much this moves with full adoption
etc_steep1=2

#norm sensitivity - how much effect does having your network be adopters of non-adopters have on your probability of adoption?
#shifts adoption-pbc curve up and down, assume symmetric for adoption and non-adoption
#value between 0 and 1 (probably less than 0.5)
normeffect1=0.1



#functions for multiple components

#calculate social network, conditional on population and self-similarity
networkfunc=function(distribution=c(frac_opp_0,frac_neut_0,frac_supp_0),selfsimparam=homophily){
  return(matrix(c(distribution*selfsimparam[[1]]/c(distribution%*%selfsimparam[[1]]),distribution*selfsimparam[[2]]/c(distribution%*%selfsimparam[[2]]),distribution*selfsimparam[[3]]/c(distribution%*%selfsimparam[[3]])),byrow=TRUE,nrow=3))
}

#return opinion social force matrix given force from neutral, opposers, and supporters
forcefunc=function(force_opp=forcestrong1,force_neut=forceweak1,force_supp=forcestrong1){
  return(matrix(c(0,force_neut,force_supp,force_opp,0,force_supp,force_opp,force_neut,0),byrow=TRUE,nrow=3))
}


####----------Emissions Component --------------------------

#Emissions depend on bau emissions, adoption of mitigative behaviours, and policy

emissions=read.csv("data/emissions_ssp3_rcp7.csv")
bau1=emissions[,3]/1000*12/(12+16+16) #conversion factor from MtCO2 per year to GtC per year
bau_outside1=emissions[,4]/1000*12/(12+16+16)

#more policy increases the contemporaneous effect on emissions and the duration of the effect
#two parameters describe how the contemporaneous effect changes with policy
#m_max has a value less than one and describes the maximum fraction of emissions that could be cut instantly by policy
#m assympototes to this value
m_max1=0.08

lag_param01=10 #number of years lag from mitigation pathway in OECD to rest of world - set to zero to treat as single region

#this parameter captures learning by doing (lbd) for policy-induced mitigation
#it gives the fractional decrease in cost for each doubling of installed mitigation capacity, relative to the initial maximum value (i.e. m_max1)
#a value of 0 implies no learning by doing, a value of 0.1 implies a 10% reduction in cost for each doubling.
#values from a review by Rubin et al in Energy Policy suggest a range of 0 - 0.3
lbd_param01=0.1

#policy also increases the duration of mitigation measures, thorugh increasingly large investments to long-lived capital
#r is the scaling time of the mitigation investment, policy increases this linearly up to a maximum
#r_max is a value in years describing the maximum scaling time - should be similar to turnover of capital stock - can take values>1
r_max1=30
#r0 is the initial duration of investments at very low levels of policy - should be greater than 1
#at t=r_0, 36% of original remains
r_01=2

#effectiveness of individual action at reducing emissions of adopters compared to non-adopters
#value 0 to 1 - zero implies no effect, 1 implies no ghg emissions from adopters
adopt_effect1=0.1

####----------Climate Component --------------------------
ex_forcing1=emissions[,5]

####----------Cognition Component -------------------------
#This parameter weights evidence provided by the weather depending on opinions about climate change
#a value of 0 means the same weather anomaly will be interpreted by all three opinion groups the same way
#values >0 mean that cold anomalies will be downweighted by climate chnage supporters and overweighted by skeptics by that %
#and vice versa for hot anoamlies - e.g. value of 0.1 means over/underwieghting will be 10%
biassedassimilation1=0

#This parameter can take a value of 0 or 1. 0 means that everyone's baseline is pre-industrial temperatures
#a value of 1 means baselines shift according to pareameters estimated in Moore et al 2019
shiftingbaselines1=0

#-----Code to fit m_max and r_max given Andersson 2019 study of the effects of carbon tax in Sweden --------------

# #Andersson finds a mean Swedish CO2 tax over the 1991 - 2005 period reduced emissions by 12.5% in 2005
# #simulate emissions reduction under real policy values given range of m_max and r_max to fit observations
# 
# tax=c(rep(30,9),seq(44,109,length.out=6)) #based on descriptions of Swedish tax scheme
# 
# mmax_test=seq(0.01,0.1,length.out=50)
# rmax_test=seq(5,70,length.out = 50)
# testgrid=expand.grid(mmax_test,rmax_test)
# 
# #simulate policy effect using emissions module
# 
# mitigationcalibration=function(policy,mmax_t,rmax,r0=2){
#   mit=matrix(nrow=length(policy),ncol=length(policy))
#   for(i in 1:length(policy)){
#     m_t=mmax_t*log(policy[i])/log(300) #300 is maximum value policy can take
#     #lifetime of investments also depends on policy
#     r_t=min(r0*(1+policy[i]/10),rmax)
#     #add effect of current policy
#     #mitigation is a t*t matrix of zeroes - fill in columns representing persistent effect of yearly mitigation activity
#     futuretime=i:length(policy)-i
#     mit[,i]=c(rep(0,i-1),m_t*exp(-futuretime/r_t))
#   }
#   totmit=rowSums(mit)
#   return(totmit[length(policy)]*100)
# }
# 
# calib=numeric(length=dim(testgrid)[1])
# for(j in 1:length(calib)){
#   calib[j]=mitigationcalibration(tax,testgrid[j,1],testgrid[j,2])
# }
# 
# #parameter combinations that perform well:
# good=testgrid[which(abs(calib-12.5)<1),] #runs with less than 1pp difference with Anderssson estimate of 12.5% reduction in 2005
# #note m_max is tightly clustered around 8%
# #provides no contraints on rmax though (likely as too short a time period)

