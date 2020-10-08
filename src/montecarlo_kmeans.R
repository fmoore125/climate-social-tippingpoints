load(file="data/MC Runs/mcmods.Rdat")
library(reshape2)
library(tidyverse)
library(patchwork)
library(data.table)
library(factoextra)

#test number - practice kmeans on smaller dataset
tn=length(mcmods)
years=2015:2100

#assemble data frame for k-means clustering
for(i in 1:tn){
  temp=c(mcmods[[i]]$distributions[,1],mcmods[[i]]$distributions[,2],reshape2::melt(mcmods[[i]]$adoptersfrac)[,3],mcmods[[i]]$policy,mcmods[[i]]$totalemissions)
  if(i==1){
    df=data.frame(t(temp))
    colnames(df)=c(paste0("Oppose",years),paste0("Neutral",years),paste0("Oppose_Adopters",years),paste0("Neutral_Adopters",years),paste0("Support_Adopters",years),paste0("Policy",years),paste0("Emissions",years))
  }
  if(i>1) df=rbind(df,temp)
  if(i%%1000==0) print(i)
}

fwrite(df,file="data/MC Runs/outputdataframe.csv")

df_scaled=scale(df)
#drop zero variance columns
nacols=which(apply(df_scaled,MARGIN=2,function(x) sum(is.na(x)))==tn)
df_scaled=df_scaled[,-nacols]

#visualize ideal number of clusters
nclustertest=2:10
wss=numeric(length=length(nclustertest))
for(i in 1:length(nclustertest)){
  wss[i]=kmeans(df_scaled,nclustertest[i])$tot.withinss
  print(i)
}
x11()
plot(x=nclustertest,y=wss,type="b",xlab="Number of Clusters",ylab="Within Sum of Squares")

nclus=5
set.seed(1987)
test=kmeans(df_scaled,nclus)

#plot outcomes over time for different clusters
df$cluster=test$cluster

clems=df%>%
  select(cluster,contains("Emissions"))%>%
  group_by(cluster)%>%
  summarize_all(mean)

clems=reshape2::melt(clems,id.vars="cluster")
clems$variable=rep(years,each=nclus)
colnames(clems)=c("cluster","year","Emissions")
clems$cluster=as.factor(clems$cluster)

cols=c("#469BEC", "#C9FAFF", "#F1E3B6", "#C4878C", "#6D882B")
a=ggplot(clems,aes(x=year,y=Emissions,group=cluster,col=cluster))+geom_line(lwd=2)+theme_bw()
a=a+scale_color_manual(values=cols)+labs(x="Year",color="Cluster")



clpol=df%>%
  select(cluster,contains("Policy"))%>%
  group_by(cluster)%>%
  summarize_all(mean)

clpol=reshape2::melt(clpol,id.vars="cluster")
clpol$variable=rep(years,each=nclus)
colnames(clpol)=c("cluster","year","Policy")
clpol$cluster=as.factor(clpol$cluster)

b=ggplot(clpol,aes(x=year,y=Policy,group=cluster,col=cluster))+geom_line(lwd=2)+theme_bw()
b=b+scale_color_manual(values=cols,guide="none")+labs(x="Year")

x11()
b+a+plot_layout(ncol=2)

clops=df%>%
  select(cluster,paste0("Oppose",years),paste0("Neutral",years))%>%
  group_by(cluster)%>%
  summarize_all(mean)

clops=reshape2::melt(clops,id.vars="cluster")
group=numeric(length=dim(clops)[2])
group[grep("Oppose",clops$variable)]="Oppose";group[grep("Neutral",clops$variable)]="Neutral"
clops$group=group
clops$variable=rep(rep(years,each=nclus),2)
clops=reshape2::dcast(clops,cluster+variable~group,value.var="value")
colnames(clops)=c("cluster","year","Neutral","Oppose")
clops$Support=1-(clops$Neutral+clops$Oppose)
clops=reshape2::melt(clops,id.vars=c("cluster","year"))

clad=df%>%
  select(cluster,contains("Adopt"))%>%
  group_by(cluster)%>%
  summarize_all(mean)

clad=reshape2::melt(clad,id.vars="cluster")
group=numeric(length=dim(clad)[2])
group[grep("Oppose",clad$variable)]="Oppose";group[grep("Neutral",clad$variable)]="Neutral";group[grep("Support",clad$variable)]="Support"
clad$group=group
clad$variable=rep(rep(years,each=nclus),3)

clopsad=merge(clops,clad,by.x=c("cluster","year","variable"),by.y=c("cluster","variable","group"))
clopsad$FractionAdopt=clopsad$value.x*clopsad$value.y;clopsad$FractionNonAdopt=clopsad$value.x*(1-clopsad$value.y)
clopsad_adopt=reshape2::dcast(clopsad[,-c(4:5)],cluster+year~variable,value.var="FractionAdopt")
clopsad_nonadopt=reshape2::dcast(clopsad[,-c(4:5)],cluster+year~variable,value.var="FractionNonAdopt")
colnames(clopsad_adopt)=c("cluster","year","Neutral_Adopt","Oppose_Adopt","Support_Adopt")
colnames(clopsad_nonadopt)=c("cluster","year","Neutral_NonAdopt","Oppose_NonAdopt","Support_NonAdopt")
clopsad=cbind(clopsad_adopt,clopsad_nonadopt[,c(3:5)])
clopsad=reshape2::melt(clopsad,id.vars=c("cluster","year"))
clopsad$variable=factor(clopsad$variable,levels=c("Oppose_Adopt","Oppose_NonAdopt","Neutral_Adopt","Neutral_NonAdopt","Support_Adopt","Support_NonAdopt"))

cols2=c("#0f334a","#82b5ee","#1d4846","#add0b0","#361432","#985d93")

clusters=paste("Cluster",1:nclus)
clus_labeller=function(variable, value) return(clusters[value])

c=ggplot(clopsad,aes(x=year,y=value,group=variable,fill=variable))+geom_area()+theme_bw()+facet_wrap(~cluster,labeller = clus_labeller)
c=c+scale_fill_manual(values=cols2)+labs(x="Year",y="Fraction of Population",fill="Opinion Group")
c=c+theme(strip.background = element_rect(fill="White"))
x11()
c
