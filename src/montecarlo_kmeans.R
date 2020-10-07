load(file="data/MC Runs/mcmods.Rdat")
library(reshape2)
library(tidyverse)
library(patchwork)

#test number - practice kmeans on smaller dataset
tn=2000
years=2015:2100

#assemble data frame for k-means clustering
for(i in 1:tn){
  temp=c(mcmods[[i]]$distributions[,1],mcmods[[i]]$distributions[,2],melt(mcmods[[i]]$adoptersfrac)[,3],mcmods[[i]]$policy,mcmods[[i]]$totalemissions)
  if(i==1){
    df=data.frame(t(temp))
    colnames(df)=c(paste0("Oppose",years),paste0("Neutral",years),paste0("Oppose_Adopters",years),paste0("Neutral_Adopters",years),paste0("Support_Adopters",years),paste0("Policy",years),paste0("Emissions",years))
  }
  if(i>1) df=rbind(df,temp)
}

df_scaled=scale(df)
#drop zero variance columns
nacols=which(apply(df_scaled,MARGIN=2,function(x) sum(is.na(x)))==tn)
df_scaled=df_scaled[,-nacols]

nclus=5
test=kmeans(df_scaled,nclus)

#plot outcomes over time for different clusters
df$cluster=test$cluster

clems=df%>%
  select(cluster,contains("Emissions"))%>%
  group_by(cluster)%>%
  summarize_all(mean)

clems=melt(clems,id.vars="cluster")
clems$variable=rep(years,each=nclus)
colnames(clems)=c("cluster","year","Emissions")
clems$cluster=as.factor(clems$cluster)

a=ggplot(clems,aes(x=year,y=Emissions,group=cluster,col=cluster))+geom_line(lwd=2)+theme_bw()

clpol=df%>%
  select(cluster,contains("Policy"))%>%
  group_by(cluster)%>%
  summarize_all(mean)

clpol=melt(clpol,id.vars="cluster")
clpol$variable=rep(years,each=nclus)
colnames(clpol)=c("cluster","year","Policy")
clpol$cluster=as.factor(clpol$cluster)

b=ggplot(clpol,aes(x=year,y=Policy,group=cluster,col=cluster))+geom_line(lwd=2)+theme_bw()

x11()
a+b+plot_layout(ncol=2)

clops=df%>%
  select(cluster,paste0("Oppose",years),paste0("Neutral",years))%>%
  group_by(cluster)%>%
  summarize_all(mean)

clops=melt(clops,id.vars="cluster")
group=numeric(length=dim(clops)[2])
group[grep("Oppose",clops$variable)]="Oppose";group[grep("Neutral",clops$variable)]="Neutral"
clops$group=group
clops$variable=rep(rep(years,each=nclus),2)
clops=dcast(clops,cluster+variable~group,value.var="value")
colnames(clops)=c("cluster","year","Neutral","Oppose")
clops$Support=1-(clops$Neutral+clops$Oppose)
clops=melt(clops,id.vars=c("cluster","year"))
colnames(clops)[3:4]=c("OpinionGroup","Fraction")
clops$OpinionGroup=factor(clops$OpinionGroup,levels=c("Support","Neutral","Oppose"))

c=ggplot(clops,aes(x=year,y=Fraction,group=OpinionGroup,fill=OpinionGroup))+geom_area()+theme_bw()+facet_wrap(~cluster)
