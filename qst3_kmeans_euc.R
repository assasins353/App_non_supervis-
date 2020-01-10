library(chron)
library("ggplot2")
library("lubridate")
library(NbClust)
library(factoextra)
library(mclust)
library(FactoMineR)
library('Rmixmod')
library(cluster)
library("dbscan")
setwd("m2/app_non_sup")
m=as.matrix(read.table("mat_qst3_new",sep = ","))

#6 ont vote pour 10 cluster 
#10 ont vote pour 2
# 3 vote pour 3 cluster et 1 pour 4 cluster

fviz_nbclust(m, kmeans, method='wss')
#2 cluster avec la methode des coudes

res.NbClust.kmeans = NbClust(m,method = 'kmeans',min.nc=2,max.nc = 10,index = 'all')
unique(res.NbClust.kmeans$Best.partition)
#12 proposed cluster 3 5 proposed 2 4 proposed 8
res=res.NbClust.kmeans$Best.partition


c=0

for(i in 1:length(res.NbClust.kmeans$Best.partition))
{
  if(res.NbClust.kmeans$Best.partition[i]==1)
  {
    c=c+1
  }
}
print(c)
par(mfrow=c(1,2))
for(j in 1:3)
{
  d=0
  couleur=5
  for(i in 1:length(res))
  {
    if(res[i]==j)
    {
      if(d==0)
      {
        ts.plot(m[i,],col=couleur)
        d=1
        couleur=couleur+5
      }
      else
      {
        lines(m[i,],col=couleur)
        couleur=couleur+5
      }
    }
  }
}
x=0
for(i in 1:(28))
{
  x=mean(res.NbClust.kmeans$Best.partition[((i-1)*11+1):(i*11)])
  #print(mean(res.NbClust.kmeans$Best.partition[((i-1)*11+1):(i*11)]))
  if(x!=1 && x!=2 && x!=3)
  {
    cat("le parking numero ",i," est dans plusieurs cluster \n")
    
  }
  else
  {
    cat("le parking numero ",i," est dans un seul cluster ",res.NbClust.kmeans$Best.partition[((i-1)*11+1)],"\n")
    
  }
}
#parking 2 4 10 11 14 20 23 ont des mesures dans plus d'un cluster
#pour le parking 2 c'est 2 mesures
#parking 4 ==> 4
#parking 10 ==> 2
#parking 11 ==> 4 (dans les 3 cluster 2 2 )
#parking 14 ==> 4
#parking 20 ==> 4
#parking 23 ==> 1




res.NbClust.single = NbClust(m,distance = 'euclidean', method = 'single',min.nc=min,max.nc = max,index = 'alllong')
#cluster 2 

res.NbClust.average = NbClust(m,distance = 'euclidean', method = 'average',min.nc=min,max.nc = max,index = 'alllong')
#cluster 3

res.NbClust.wardD2 = NbClust(m,distance = 'euclidean', method = 'ward.D2',min.nc=min,max.nc = max,index = 'alllong')
#cluster 3

res.NbClust.wardD = NbClust(m,distance = 'euclidean', method = 'ward.D',min.nc=min,max.nc = max,index = 'alllong')
#cluster 3

res.NbClust.complete = NbClust(m,distance = 'euclidean', method = 'complete',min.nc=min,max.nc = max,index = 'alllong')
#cluster 2



#interpretation des cluster 
m[res.NbClust.kmeans$Best.partition[i]==2,]
for(z in 1:3)
{
  somme_gen=0
  nbr=0
  for(i in 1:dim(m)[1])
  {
    
    if(res.NbClust.kmeans$Best.partition[i]==z)
    {somme=0
    nbr=nbr+1
    cmp=0
    for(j in (4*18+1):(6*18))
    {
      somme=somme+m[i,j]
      cmp=cmp+1
    }
    #print(cmp)
    somme=somme/(18*2)
    somme_gen=somme_gen+somme
    #print(somme)
    }
  }
  cat("le cluster numero ",z," posséde ",somme_gen/nbr," le week end \n")
 
}

for(z in 1:3)
{
  somme_gen=0
  nbr=0
  for(i in 1:dim(m)[1])
  {
    
    if(res.NbClust.kmeans$Best.partition[i]==z)
    {somme=0
    nbr=nbr+1
    for(j in (1):(7*18))
    {
      if(j<(4*18+1) || j>(6*18))
      {
        somme=somme+m[i,j]
      }
      
    }
    somme=somme/(18*5)
    somme_gen=somme_gen+somme
    #print(somme)
    }
  }
  cat("le cluster numero ",z," posséde ",somme_gen/nbr," les jours de travail \n")
  
}
res.NbClust.kmeans$Best.partitio
for(z in 1:3)
{
  somme_gen=0
  nbr=0
  for(i in 1:dim(m)[1])
  {
    
    if(res.NbClust.kmeans$Best.partition[i]==z)
    {somme=0
    nbr=nbr+1
    for(j in (1):(7*18))
    {
      
      somme=somme+m[i,j]
      
      
    }
    somme=somme/(18*7)
    somme_gen=somme_gen+somme
    #print(somme)
    }
  }
  cat("le cluster numero ",z," posséde ",somme_gen/nbr," sur toute la semaine \n")
  
}
res.NbClust.kmeans$Best.partition[12]
ts.plot(m[11*5+12,])
m=as.matrix(read.table("mat_qst3_new",sep = ","))
