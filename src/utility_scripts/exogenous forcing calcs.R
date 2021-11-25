#use rcp data emissions to constrain how non-co2 forcing / emissions scales with co2 emission changes
library(tidyverse)
library(ggplot2)

emis=read.csv("big_data/cmip6_emissions.csv")
emis=emis%>%
  filter(REGION=="World")%>%
  filter(VARIABLE%in%c("CMIP6 Emissions|CH4","CMIP6 Emissions|CO2","CMIP6 Emissions|N2O"))%>%
  filter(SCENARIO%in%c("SSP1-26","SSP2-45","SSP3-70 (Baseline)","SSP4-34","SSP4-60"))%>%
  pivot_longer(cols=X2015:X2100)

baseline=emis%>%
  filter(SCENARIO=="SSP3-70 (Baseline)")%>%
  select(VARIABLE,name,value)

colnames(baseline)[3]="value_baseline"

emis=merge(emis,baseline)

emis=emis%>%
  filter(SCENARIO%in%c("SSP1-26","SSP2-45","SSP4-34","SSP4-60"))%>%
  mutate(frac_red=(value_baseline-value)/value_baseline)

co2=emis%>%
  filter(VARIABLE=="CMIP6 Emissions|CO2")%>%
  select(name,SCENARIO,frac_red)
colnames(co2)[3]="frac_redco2"

emis=emis%>%
  filter(VARIABLE%in%c("CMIP6 Emissions|CH4","CMIP6 Emissions|N2O"))

emis=merge(emis,co2)

emis=emis%>%
  filter(frac_red>0,frac_redco2>0)%>% 
  mutate(ratio_emis_red=frac_red/frac_redco2)

emis$name=substr(emis$name,2,5)

a=ggplot(emis%>%filter(name>2020),aes(x=frac_redco2,y=frac_red))
a=a+geom_point(aes(col=VARIABLE,pch=SCENARIO))+theme_bw()+geom_smooth(method="lm",se=FALSE,col="black")
mod=lm(emis$frac_red~emis$frac_redco2)
a=a+annotate("text",label="y=0.04+0.49x",x=0.3,y=0.7)+labs(x="Fraction CO2 Reduced (from RCP7)",y="Fraction Other Gas Reduced (from RCP7)")
