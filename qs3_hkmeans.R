library(TSdist)
library(TSclust)
library(cluster)
library(factoextra)
library(FactoMineR)
library(NbClust)
setwd("m2/app_non_sup")
m=as.matrix(read.table("mat_qst3_new",sep = ","))

#kmeans avec distance dtw
met="ward.D2"
algo="MacQueen"
#average macqueen 
#ward et macqueen donne un le mm qu'un kmeans normal
reshkmeans=hkmeans(m, 3, hc.metric = "Minkowski", hc.method =met,
                   iter.max = 30, km.algorithm = algo)
#plot les 3 clusters
res=reshkmeans$cluster
length(res)
11*20
reshkmeans$cluster[reshkmeans$cluster==3]
par(mfrow=c(1,1))
m[120,]
for(j in 1:3)
{
  d=0
  couleur=5
  for(i in 1:length(res))
  {
    if(res[i]==j)
    {
      if(j==3)
      {
        print(i)
      }
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
#write.table(kmeans.dtw, file ="qst4_kmedoid_dtw", sep = ",")

x=0
for(i in 1:(28))
{
  x=mean(reshkmeans$cluster[((i-1)*11+1):(i*11)])
  #print(mean(res.NbClust.kmeans$Best.partition[((i-1)*11+1):(i*11)]))
  if(x!=1 && x!=2 && x!=3)
  {
    cat("parking numero ",i," dans plusieur cluster ")
    print("")
  }
  else
  {
    cat("parking numero ",i," dansun suel cluster ",reshkmeans$cluster[((i-1)*11+1)])
    print("")
  }
}
#parking 2 4 10 11 14 20 23  ont des mesures dans plus d'un cluster
#pour le parking 2 c'est 2 mesures
#parking 4 ==> 4
#parking 10 ==> 2
#parking 11 ==> 4 (dans les 3 cluster 2 2 )
#parking 14 ==> 4
#parking 20 ==> 4
#parking 23 ==> 1





#interpretation des cluster 
for(z in 1:3)
{
  somme_gen=0
  nbr=0
  for(i in 1:dim(m)[1])
  {
    
    if(reshkmeans$cluster[i]==z)
    {somme=0
    nbr=nbr+1
    for(j in (4*18+1):(6*18))
    {
      somme=somme+m[i,j]
    }
    somme=somme/(18*2)
    somme_gen=somme_gen+somme
    #print(somme)
    }
  }
  cat("le cluster numero ",z," posséde ",somme_gen/nbr," le week end")
  print("")
}

for(z in 1:3)
{
  somme_gen=0
  nbr=0
  for(i in 1:dim(m)[1])
  {
    
    if(reshkmeans$cluster[i]==z)
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
  cat("le cluster numero ",z," posséde ",somme_gen/nbr," les jours de travail")
  print("")
}

for(z in 1:3)
{
  somme_gen=0
  nbr=0
  for(i in 1:dim(m)[1])
  {
    
    if(reshkmeans$cluster[i]==z)
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
  cat("le cluster numero ",z," posséde ",somme_gen/nbr," sur toute la semaine")
  print("")
}
ts.plot(m[218,])
m[218,]
11*19
