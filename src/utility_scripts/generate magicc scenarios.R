#use template to generate magicc scenarios for 5 emissions clusters

template=read.csv("data/magicc_template.csv")
scenarios=read.csv("data/mitigationrates.csv")

emissionssplit=split(scenarios,scenarios$Cluster)

years=as.numeric(substr(colnames(template)[6:30],2,5))
relyears=which(emissionssplit[[1]]$Year%in%years)


co2rows=grep("CO2",template$variable)

for(i in 1:length(emissionssplit)){
 name=emissionssplit[[i]]$Cluster[1]
 
 mitrate=emissionssplit[[i]]$mit_rate[relyears]
 
 #mitigation rates apply directly to CO2
 temp=template
 
 for(j in 1:dim(temp)[2]){
   if(j%in%co2rows) temp[j,22:30]=temp[j,22:30]*(1-mitrate)
   else temp[j,22:30]=temp[j,22:30]*(1-(mitrate*0.49))
 }
 
 write.csv(temp,file=paste0("data/MAGICC Scenarios/",name,".csv"))
}
