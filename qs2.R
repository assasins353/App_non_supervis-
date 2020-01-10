setwd("m2/app_non_sup")
df=read.table("backup2.csv",sep=",")
df=df[2:dim(df)[1],][c("V3","V4","V5","V6")]
liste_park=read.table("dataset_name_park.csv",sep=",")
liste_park=liste_park[-1,]
# remarque code servant a transformer le ddataframe de type factor en dataframe de type numeric/character
df2 <- data.frame(sapply(df[,2:3], function(x) { if(is.factor(x)) {
  as.numeric(as.character(x))
} else {
  x
}
}))
df3 <- data.frame("V2"=sapply(df[,2], function(x) { if(is.factor(x)) {
  as.character(x)
} 
}))
#normalisation des données 
m <- matrix(0, ncol = 1, nrow = dim(df)[1])
d=data.frame(m)
for(i in 1:dim(df)[1])
{
  d[i,]=df2[i,]["V5"]/df2[i,]["V4"]
}
data_norme=data.frame(df,d)

#calcul le nbr de fois ou chaque parking a atteint sa capacité maximal

liste_cap_max=c()
for(i in 1:28)
{
  nbr=0
  for(j in 1:(77*18))
  {
    if(df2[(i-1)*77*18+j,]["V4"]==df2[(i-1)*77*18+j,]["V5"])
    {
      nbr=nbr+1
    }
  }
  
  liste_cap_max=c(liste_cap_max,nbr)
}

liste_cap_max
sum(liste_cap_max)

#moyenne de selon les jour 

cmp=0
nbr_park=2
j=1+18*77*(nbr_park-1)
k=nbr_park*18*77

liste_moy_jour=c()
moy=0
for(i in j:k)
{
 
  
  moy=moy+as.double(data_norme[i,]["m"])
  
  cmp=cmp+1
  if(cmp==18)
  { 
    moy=moy/18
    liste_moy_jour=c(liste_moy_jour,moy)
    moy=0
    cmp=0
  }
}

summary(liste_moy_jour)




#calcule des taux d'occupation moyen pour l'ensemble des parking
liste_moy_per=c()
for(x in 1:28)
{
  cmp=0
  nbr_park=x
  j=1+18*77*(nbr_park-1)
  k=nbr_park*18*77
  
  
  moy=0
  for(i in j:k)
  {
    
    
    moy=moy+as.double(data_norme[i,]["m"])
    
    cmp=cmp+1
    if(cmp==(18*77))
    { 
      moy=moy/(18*77)
      liste_moy_per=c(liste_moy_per,moy)
      moy=0
      cmp=0
    }
  }
  
  
}
summary(liste_moy_per)
liste_moy_per

#verifiant les stat par semaine et par jour pour la parking numero 4 (le plus occupé  )
cmp=0
nbr_park=4
j=1+18*77*(nbr_park-1)
k=nbr_park*18*77
liste_moy_sem=c()
moy=0
for(i in j:k)
{
  
  
  moy=moy+as.double(data_norme[i,]["m"])
  
  cmp=cmp+1
  if(cmp==(18*7))
  { 
    moy=moy/(18*7)
    liste_moy_sem=c(liste_moy_sem,moy)
    moy=0
    cmp=0
  }
}

summary(liste_moy_sem)
liste_moy_sem
k=4
df[77*18*(k-1)+1,]

#verifiant les stat par semaine et par jour pour la parking numero 19 (le moins occupé  )
cmp=0
nbr_park=19
j=1+18*77*(nbr_park-1)
k=nbr_park*18*77
liste_moy_sem=c()
moy=0
for(i in j:k)
{
  
  
  moy=moy+as.double(data_norme[i,]["m"])
  
  cmp=cmp+1
  if(cmp==(18*7))
  { 
    moy=moy/(18*7)
    liste_moy_sem=c(liste_moy_sem,moy)
    moy=0
    cmp=0
  }
}

summary(liste_moy_sem)
liste_moy_sem

#affichage des capacity de chaque parking
library("varhandle") 
liste_capacity=c()
for(k in 1:28)
{
  liste_capacity=c(liste_capacity,as.integer(df2[77*18*(k-1)+1,]["V4"]))
  x=unfactor(df[77*18*(k-1)+1,]["V3"])
  x=as.character(x)
  cat("le parking ",x)
  cat(" posséde", as.integer(df2[77*18*(k-1)+1,]["V4"]),"palces disponnible ")
  cat(" avec un taux d'occupation moyen de ",as.double(liste_moy_per[k]))
  print("")
}
summary(liste_capacity)


#verifiant les stat par semaine et par jour pour la parking numero 23 (le plus de place disponnible  )
cmp=0
nbr_park=23
j=1+18*77*(nbr_park-1)
k=nbr_park*18*77
liste_moy_sem=c()
moy=0
for(i in j:k)
{
  
  
  moy=moy+as.double(data_norme[i,]["m"])
  
  cmp=cmp+1
  if(cmp==(18*7))
  { 
    moy=moy/(18*7)
    liste_moy_sem=c(liste_moy_sem,moy)
    moy=0
    cmp=0
  }
}

summary(liste_moy_sem)
liste_moy_sem


#verifiant les stat par semaine et par jour pour la parking numero 9 (le moins de place disponnible  )
cmp=0
nbr_park=9
j=1+18*77*(nbr_park-1)
k=nbr_park*18*77
liste_moy_sem=c()
moy=0
for(i in j:k)
{
  
  
  moy=moy+as.double(data_norme[i,]["m"])
  
  cmp=cmp+1
  if(cmp==(18*7))
  { 
    moy=moy/(18*7)
    liste_moy_sem=c(liste_moy_sem,moy)
    moy=0
    cmp=0
  }
}

summary(liste_moy_sem)
liste_moy_sem
