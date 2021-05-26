#DICE 2010 climate model, annualized using parameters in Cai, Judd and Lontzek 2012
#takes CO2 emissions and gives global temperatures
#initial values are updated based on DICE2016 for a 2015 start year

mass_0=c(851,460,1740) #initial mass of carbon in atmosphere, upper ocean, lower ocean (GtC)
temp_0=c(0.85,0.068) #warming of atomsphere and lower ocean in 2015 - update to more accurate number? 0.85 is probably too low
psi1=0.022
nu=3.8
forc_param=3.8

#transition matrix for cabon cycle - parameters from Cai, Judd and Lontzek
phi_carbon=matrix(c(1-0.0189288,0.0097213,0,0.018922,1-0.0147213,0.0003119,0,0.005,1-0.0003119),byrow=TRUE,nrow=3)

#raditive forcing equation given mass in atmosphere and exogenous forcing
forcing=function(m_at_t,ex_forcing_t,m_at_0=mass_0[1],nu_param=nu){
  forcing_t=nu_param*log2(m_at_t/m_at_0)+ex_forcing_t
  return(forcing_t)
}

#temperature transition matrix for temperature in atmosphere and lower ocean
#parameters from DICE2016 annualized using formula in Cai, Judd and Lontzek
phi_temp=matrix(c(1-0.01005,0.0088,0.0025,1-0.0025),byrow=TRUE,nrow=2)

temperaturechange=function(temp_t_1,mass_t_1,emissions_t,ex_forcing_t,psi1_param=psi1,nu_param=nu,mass_0_param=mass_0,
                           phi_carbon_param=phi_carbon,phi_temp_param=phi_temp){
  mass_t=phi_carbon_param%*%mass_t_1+c(emissions_t,0,0)
  forcing_t=forcing(mass_t[1],ex_forcing_t,m_at_0=mass_0[1],nu_param=nu)
  temp_t=phi_temp%*%temp_t_1+c(psi1_param*forcing_t,0)
  return(list(mass_t,temp_t))
}