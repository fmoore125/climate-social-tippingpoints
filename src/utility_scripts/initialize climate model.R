# get carbon mass and temperature values to initialize climate model in 2020
# given initial values for 2005 and global emissions 2005-2020

mass_0=c(808.9,1255,18365) #initial (2005) mass of carbon in atmosphere, upper ocean, lower ocean (GtC)
temp_0=c(0.7307,0.0068) #warming above 1900 in 2005
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
phi_temp=matrix(c(1-(0.010+0.047),0.0048,0.010,(1-0.0048)),byrow=TRUE,nrow=2)

temperaturechange=function(temp_t_1,mass_t_1,emissions_t,ex_forcing_t,psi1_param=psi1,nu_param=nu,mass_0_param=mass_0,
                           phi_carbon_param=phi_carbon,phi_temp_param=phi_temp){
  mass_t=phi_carbon_param%*%mass_t_1+c(emissions_t,0,0)
  forcing_t=forcing(mass_t[1],ex_forcing_t,m_at_0=596.4,nu_param=nu)
  temp_t=phi_temp%*%temp_t_1+c(psi1_param*forcing_t,0)
  return(list(mass_t,temp_t))
}

#get exogenous forcing and emissions for 2005-2020
ems=read.csv("data/emissions_forcing_climateinitialization.csv")

emissions=ems[,5]
ex_forcing=ems[,2]

#run climate model
years=2005:2020

init_temperature=matrix(nrow=length(years),ncol=2)
init_temperature[1,]=temp_0

init_mass=matrix(nrow=length(years),ncol=3)
init_mass[1,]=mass_0

for(t in 2:length(years)){
  temp3=temperaturechange(temperature[t-1,],mass[t-1,],emissions[t],ex_forcing[t],psi1_param=psi1,nu_param=nu)
  init_mass[t,]=temp3[[1]]
  init_temperature[t,]=temp3[[2]]
}

#initialize main climate model using 2020 values for carbon mass and temperature 
