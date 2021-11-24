#DICE 2007 climate model, annualized using parameters in Cai and Lontzek 2019
#takes CO2 emissions and gives global temperatures
#initial values are based on 2020 values - calculated by running the model from 2005 - 2020 using observed emissions 2005-2019

source("src/utility_scripts/initialize climate model.R")

mass_0=init_mass[16,] #initial mass of carbon in atmosphere, upper ocean, lower ocean (GtC) in 2020, from initializing climate model in 2005
temp_0=init_temperature[16,] #warming of atomsphere and lower ocean in 2020 relative to 1900 - 1.08 degrees air, 0.13 degrees ocean
psi1=0.037
nu=3.8

#transition matrix for cabon cycle - parameters from Cai, Judd and Lontzek
phi_carbon=matrix(c(1-0.0189288,0.0097213,0,0.0189288,1-0.0147213,0.0003119,0,0.005,1-0.0003119),byrow=TRUE,nrow=3)

#raditive forcing equation given mass in atmosphere and exogenous forcing
#note m_at_0 is preindustrial atmospheric carbon concentration
forcing=function(m_at_t,ex_forcing_t,m_at_0=596.4,nu_param=nu){
  forcing_t=nu_param*log2(m_at_t/m_at_0)+ex_forcing_t
  return(forcing_t)
}

#temperature transition matrix for temperature in atmosphere and lower ocean
#parameters from DICE2016 annualized using formula in Cai, Judd and Lontzek
phi_temp=matrix(c(1-(0.01+0.047),0.01,0.0048,(1-0.0048)),byrow=TRUE,nrow=2)

temperaturechange=function(temp_t_1,mass_t_1,emissions_t,ex_forcing_t,bau_tot_t,psi1_param=psi1,nu_param=nu,mass_0_param=mass_0,
                           phi_carbon_param=phi_carbon,phi_temp_param=phi_temp){
  
  #scale exogenous forcing based on % emissions reduction - assume max 50% effectivness for non-co2 gases
  red=(bau_tot_t-emissions_t)/(bau_tot_t)
  red=red*0.46
  
  ex_forcing_new=ex_forcing_t*(1-red)
  
  mass_t=phi_carbon_param%*%mass_t_1+c(emissions_t,0,0)
  forcing_t=forcing(mass_t[1],ex_forcing_new,m_at_0=596.4,nu_param=nu)
  temp_t=phi_temp%*%temp_t_1+c(psi1_param*forcing_t,0)
  return(list(mass_t,temp_t))
}