#process pew opinion data for calibration exercise
library(tidyverse)

ops=read.csv("data/Data for Hindcasting/opinion/pew_climatechangethreat.csv")

#drop countries without data from 2013
ops=ops%>%
  group_by(Country)%>%
  filter(Year>=2013)%>%
  filter(n()==5)

#combine with population data to get population-weighted estimates

pop=read.csv("data/Data for Hindcasting/un_population.csv")
pop=pop[,1:3]

ops=merge(ops,pop)

#aggregate up to the year level for three opinion groups
#assume non-responses are "neutral"

ops_processed=ops%>%
  rename(Support=Major.Threat,Oppose=Not.a.Threat,Pop=Pop..thousands.)%>%
  mutate(Neutral=Minor.Threat+DK.Refused)%>%
  select(Year,Support,Neutral,Oppose,Pop)%>%
  group_by(Year)%>%
  summarize_at(vars(Support:Oppose),~round(weighted.mean(.x,Pop)))

#fix a couple of weird values coming from rounding - each row has to sum to 100
ops_processed[3,2]=ops_processed[3,2]-1
ops_processed[5,2]=ops_processed[5,2]+1

write.csv(ops_processed,file="data/Data for Hindcasting/opinion/pew_final.csv")

co2=read.csv("C:/Users/fmoore/Documents/GitHub/co2emissions.csv")

co2_processed=co2%>%
  filter(Code%in%c("OWID_WRL","USA","CAN"))%>%
  filter(Year%in%c(2013,2016:2018,2020))

global=co2_processed$Annual.CO2.emissions..zero.filled.[9:12]

co2_processed=co2_processed%>%
  filter(Code%in%c("CAN","USA"))

co2_processed$global=c(global,global)
co2_processed$fraction=co2_processed$Annual.CO2.emissions..zero.filled./co2_processed$global

#merge in emissions data to carbon pricing data to get weighted average of carbon prices in opinion regions by year
prices=read.csv("data/Data for Hindcasting/policy/worldbank_carbonprices2.csv")

prices=merge(prices,co2,by.x=c("Country",'Year'),by.y=c("Code","Year"),all.x=T,all.y=F)

prices_processed=prices%>%
  select(-Entity)%>%
  rename(Emissions=Annual.CO2.emissions..zero.filled.)%>%
  group_by(Country)%>%
  fill(Emissions)

meanprices=prices_processed%>%
  group_by(Year)%>%
  summarize(meanprice=sum(Price*Emissions)/sum(Emissions))

write.csv(meanprices,"data/Data for Hindcasting/policy/worldbank_carbonprices_finalforpewcountries.csv")
