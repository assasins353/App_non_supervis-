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
df=read.table("backup3.csv",sep=",")
df=df[,c(-1)]
df=df[c(-1),]
#calcule du taux d'occupency

df2 <- data.frame(sapply(df[,2:3], function(x) { if(is.factor(x)) {
  as.numeric(as.character(x))
} else {
  x
}
}))

#normalizaion des données
m <- matrix( ncol = (77*18), nrow = 28)
d=data.frame(m)
for(i in 10505:dim(df)[1])
{

  d[i,]=df2[i,]["V4"]/df2[i,]["V3"]
}
i
data_norme=data.frame(df,d)

m <- matrix(0L,ncol = (77*18), nrow = 28)

l=1
c=1
i=1
cmp=0
for(i in 1:dim(df)[1])
{
print(i)
  cmp=cmp+1
  #print(cmp)
  if(cmp<(77*18))
  {
    #print(cmp)
    m[l,c]=as.double(data_norme[i,]["m"])
    c=c+1
  }
  if(cmp==(77*18))
  {
    #print(cmp)
    m[l,c]=as.double(data_norme[i,]["m"])
    c=1
    l=l+1
    cmp=0
  }
  #print(i)
}
m[3,1]
n[3,1]

moy=0
i=77+50
for(j in (18*(i-1)+1):(18*i))
{
  #print(j)
  moy=moy+as.double(data_norme[j,]["m"])
 #print(j)
}
moy=moy/28
moy
m[2,50]
n[2,50]
#creation de la matrice de serie temporelle hebdomadaire
m <- matrix(0L,ncol = 7*18, nrow = 28*11)
j=1
k=1

for(i in 1:dim(df)[1])
{
  m[j,][k]=as.double(data_norme[i,]["m"])

  k=k+1
  if(k>(7*18))
  {
    j=j+1
    k=1
  }
}
dim(m)
#write.table(m, file ="mat_qst3_new", sep = ",")
#utilisation de methode k-means et cah classique 
min=2
max=10
res.NbClust.kmeans = NbClust(m,method = 'kmeans',min.nc=min,max.nc = max,index = 'all')
#cluster 3
par(mfrow=c(1,1))
clusplot(m, res.NbClust.kmeans$Best.partition, color=TRUE,
         labels=3, lines=0,col.p =3, )


res.NbClust.single = NbClust(m,distance = 'euclidean', method = 'single',min.nc=min,max.nc = max,index = 'all')
res.NbClust.average = NbClust(m,distance = 'euclidean', method = 'average',min.nc=min,max.nc = max,index = 'all')
res.NbClust.wardD2 = NbClust(m,distance = 'euclidean', method = 'ward.D2',min.nc=min,max.nc = max,index = 'all')
res.NbClust.wardD = NbClust(m,distance = 'euclidean', method = 'ward.D',min.nc=min,max.nc = max,index = 'all')
res.NbClust.complete = NbClusté(m,distance = 'euclidean', method = 'complete',min.nc=min,max.nc = max,index = 'all')

#silhouette

silhouette_score <- function(k){
  km <- kmeans(mat, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(m))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

#algorithme de method hybrid cah k-means
mat <- matrix(0L,ncol = 7*18, nrow = 28*11)
for(i in 1:(7*18))
{
  mat[i,]=ts(m[i,])
}
#test
n=as.matrix(read.table("matrix_qst3",sep = ","))
res.hk.hat=hkmeans(m, 3, hc.metric = "euclidean", hc.method ="ward.D2",
                   iter.max = 30, km.algorithm = "MacQueen")
clusplot(m, res.hk.hat$cluster, color=TRUE,
         labels=3, lines=0,col.p =res.hk.hat$cluster)
res.hk.hat$cluster
m[1,2]
dim(m)
6*18
somme_gen=0
nbr=0
for(i in 1:dim(m)[1])
{
 
  if(res.hk.hat$cluster[i]==1)
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
print(somme_gen/nbr)

plot(ts(res.hk.hat$centers[3,]))
res.hk.hat$cluster
res=dbscan(mat, eps = 0.4, minPts = 4,borderPoints = TRUE)
res$cluster
clusplot(mat, res$cluster, color=TRUE,
         labels=4, lines=0,col.p =res$cluster )
dbscan::kNNdistplot(mat,k=5)
abline(h=0.43)
abline(h=1.63)
#tes


res.hk.hat=hkmeans(m, 3, hc.metric = "euclidean", hc.method = "ward.D2",
        iter.max = 10, km.algorithm = "Hartigan-Wong")
clusplot(m, res.hk.hat$cluster, color=TRUE,
         labels=3, lines=0,col.p =res.hk.hat$cluster, )

res.hk.hat=hkmeans(m, 3, hc.metric = "euclidean", hc.method = "ward.D2",
                   iter.max = 10, km.algorithm = "Forgy")
clusplot(m, res.hk.hat$cluster, color=TRUE,
         labels=3, lines=0,col.p =res.hk.hat$cluster, )

res.hk.hat=hkmeans(m, 3, hc.metric = "euclidean", hc.method = "ward.D2",
                   iter.max = 10, km.algorithm = "Lloyd")
clusplot(m, res.hk.hat$cluster, color=TRUE,
         labels=3, lines=0,col.p =res.hk.hat$cluster, )

res.hk.hat=hkmeans(m, 3, hc.metric = "euclidean", hc.method ="ward.D2",
                   iter.max = 30, km.algorithm = "MacQueen")
clusplot(m, res.hk.hat$cluster, color=TRUE,
         labels=3, lines=0,col.p =res.hk.hat$cluster, )


par(mfrow=c(1,1))

res.hk.hat=hkmeans(m, 3, hc.metric = "euclidean", hc.method ="ward.D2",
                   iter.max = 30, km.algorithm = "MacQueen")
clusplot(m, res.hk.hat$cluster, color=TRUE,
         labels=3, lines=0,col.p =res.hk.hat$cluster, )


res.hk.hat=hkmeans(m, 3, hc.metric = "euclidean", hc.method ="complete",
                   iter.max = 10, km.algorithm = "MacQueen")
clusplot(m, res.hk.hat$cluster, color=TRUE,
         labels=3, lines=0,col.p =res.hk.hat$cluster, )

res.hk.hat=hkmeans(m, 3, hc.metric = "euclidean", hc.method ="average",
                   iter.max = 10, km.algorithm = "MacQueen")
clusplot(m, res.hk.hat$cluster, color=TRUE,
         labels=3, lines=0,col.p =res.hk.hat$cluster, )

res.hk.hat=hkmeans(m, 3, hc.metric = "euclidean", hc.method ="single",
                   iter.max = 10, km.algorithm = "MacQueen")
clusplot(m, res.hk.hat$cluster, color=TRUE,
         labels=3, lines=0,col.p =res.hk.hat$cluster, )
?kmeans

#write.table(m, file ="matrix_qst3", sep = ",")
m=as.matrix(read.table("matrix_qst3",sep = ","))

#preparation de la matrice de la question 4
mat <- matrix(0L,ncol = 77, nrow = 28)

for(k in 1:28)
{

  for(i in 1:77)
  {
  sum=0
    for(z in 1:18)
    {
      y=as.double(data_norme[(z+18*(i-1)+77*(k-1)),]["m"])
      sum=sum+y
    }
 
  
  
  mat[k,][i]=sum/18
 
  }
}
write.table(mat, file ="mat_qst4_new", sep = ",")
dim(mat) 
n=as.matrix(read.table("matrix_qst4",sep = ","))
dim(mat)
write.table(m, file ="qst4_full", sep = ",")


min =2
max=10

res.NbClust.kmeans = NbClust(m,method = 'kmeans',min.nc=min,max.nc = max,index = 'all')
res.NbClust.single = NbClust(m,distance = 'euclidean', method = 'single',min.nc=min,max.nc = max,index = 'all')
res.NbClust.average = NbClust(m,distance = 'euclidean', method = 'average',min.nc=min,max.nc = max,index = 'all')
res.NbClust.wardD2 = NbClust(m,distance = 'euclidean', method = 'ward.D2',min.nc=min,max.nc = max,index = 'all')
res.NbClust.wardD = NbClust(m,distance = 'euclidean', method = 'ward.D',min.nc=min,max.nc = max,index = 'all')
res.NbClust.complete = NbClust(m,distance = 'euclidean', method = 'complete',min.nc=min,max.nc = max,index = 'all')

df.pca=PCA(m,ncp=2)
plot.PCA(df.pca,axes=c(1,2))

clsut=Mclust(as.matrix(df.pca$ind$contrib))
summary(clsut)
plot(clsut,what = "classification")
cls

res.NbClust.kmeans = NbClust(m,method = 'kmeans',min.nc=min,max.nc = max,index = 'all')
res.NbClust.single = NbClust(m,distance = 'euclidean', method = 'single',min.nc=min,max.nc = max,index = 'all')
res.NbClust.average = NbClust(m,distance = 'euclidean', method = 'average',min.nc=min,max.nc = max,index = 'all')
res.NbClust.wardD2 = NbClust(m,distance = 'euclidean', method = 'ward.D2',min.nc=min,max.nc = max,index = 'all')
res.NbClust.wardD = NbClust(m,distance = 'euclidean', method = 'ward.D',min.nc=min,max.nc = max,index = 'all')
res.NbClust.complete = NbClust(m,distance = 'euclidean', method = 'complete',min.nc=min,max.nc = max,index = 'all')

?NbClust