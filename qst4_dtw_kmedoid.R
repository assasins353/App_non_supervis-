library(TSdist)
library(TSclust)
library(cluster)
library(factoextra)
library(FactoMineR)
library(NbClust)
setwd("m2/app_non_sup")
m=as.matrix(read.table("semhouli_qst4",sep = ","))

#kmeans avec distance dtw
kmeans.dtw = KMedoids(m, k=3, "dtw")
#plot les 3 clusters
kmeans.dtw
#write.table(kmeans.dtw, file ="qst4_kmedoid_dtw", sep = ",")

for(j in 1:3)
{
  d=0
  couleur=5
  for(i in 1:length(kmeans.dtw))
  {
    if(kmeans.dtw[i]==j)
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






for(z in 1:3)
{
  somme_gen=0
  nbr=0
  for(i in 1:dim(m)[1])
  {
    
    if(kmeans.dtw[i]==z)
    {somme=0
    nbr=nbr+1
    j=5
    cmp=0
    while(j <78)
    {
      somme=somme+m[i,j]
      somme=somme+m[i,j+1]
      cmp=cmp+2
      j=j+7
      
      #print(somme)
      
      
    }
    #print(cmp)
    somme=somme/(2*11)
    somme_gen=somme_gen+somme
    
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
    
    if(kmeans.dtw[i]==z)
    {somme=0
    nbr=nbr+1
    j=0
    while(j <71)
    {
      
      somme=somme+m[i,j+1]
      somme=somme+m[i,j+2]
      somme=somme+m[i,j+3]
      somme=somme+m[i,j+4]
      somme=somme+m[i,j+7]
      #print(j)
      
      j=j+7
      
      
    }
    somme=somme/(5*11)
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
    
    if(kmeans.dtw[i]==z)
    {somme=0
    nbr=nbr+1
    
    #print("ok")
    for(j in (1):(7*11))
    {
      
      somme=somme+m[i,j]
      #print(m[i,j])
      
      
    }
    somme=somme/(7*11)
    somme_gen=somme_gen+somme
    }
  }
  cat("le cluster numero ",z," posséde ",somme_gen/nbr," sur toute la semaine")
  print("")
}

kmeans.dtw
7*18
dim(m)