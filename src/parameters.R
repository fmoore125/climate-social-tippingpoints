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
#diagnoals should be zero - meeting someone with same opinion as you doens't change opinion
#assume symmetric to avoid parameter explosion
forcestrong1=0.2 #effect of someone with strong opinions on neutral opinions
forceweak1=0.1 #effect of someone with neutral opinions on strong opinions

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
etc_steep1=4

#norm sensitivity - how much effect does having your network be adopters of non-adopters have on your probability of adoption?
#shifts adoption-pbc curve up and down, assume symmetric for adoption and non-adoption
#value between 0 and 1 (probably less than 0.5)
normeffect1=0.1



#functions for multiple components

#calculate social network, conditional on population and self-similarity
networkfunc=function(distribution=c(frac_opp_0,frac_neut_0,frac_supp_0),selfsimparam=homophily){
  return(matrix(c(distribution*selfsimparam[[1]]/c(distribution%*%selfsimparam[[1]]),distribution*selfsimparam[[2]]/c(distribution%*%selfsimparam[[2]]),distribution*selfsimparam[[3]]/c(distribution%*%selfsimparam[[3]])),byrow=TRUE,nrow=3))
}

####----------Emissions Component --------------------------

#Emissions depend on bau emissions, adoption of mitigative behaviours, and policy

emissions=read.csv("data/emissions_ssp3_rcp7.csv")
bau1=emissions[,2]/1000*12/(12+16+16) #conversion factor from MtCO2 per year to GtC per year

#more policy increases the contemporaneous effect on emissions and the duration of the effect
#two parameters describe how the contemporaneous effect changes with policy
#m_max has a value less than one and describes the maximum fraction of emissions that could be cut instantly by policy
#m assympototes to this value
m_max1=0.05

#policy also increases the duration of mitigation measures, thorugh increasingly large investments to long-lived capital
#r is the scaling time of the mitigation investment, policy increases this linearly up to a maximum
#r_max is a value in years describing the maximum scaling time - should be similar to turnover of capital stock - can take values>1
r_max1=15
#r0 is the initial duration of investments at very low levels of policy - should be greater than 1
#at t=r_0, 36% of original remains
r_01=2

#effectiveness of individual action at reducing emissions of adopters compared to non-adopters
#value 0 to 1 - zero implies no effect, 1 implies no ghg emissions from adopters
adopt_effect1=0.1

####----------Climate Component --------------------------
ex_forcing1=emissions[,3]

####----------Cognition Component -------------------------
#This parameter weights evidence provided by the weather depending on opinions about climate change
#a value of 0 means the same weather anomaly will be interpreted by all three opinion groups the same way
#values >0 mean that cold anomalies will be downweighted by climate chnage supporters and overweighted by skeptics by that %
#and vice versa for hot anoamlies - e.g. value of 0.1 means over/underwieghting will be 10%
biassedassimilation1=0

#This parameter can take a value of 0 or 1. 0 means that everyone's baseline is pre-industrial temperatures
#a value of 1 means baselines shift according to pareameters estimated in Moore et al 2019
shiftingbaselines1=0
