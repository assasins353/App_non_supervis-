
library(class)
library(MASS)
library(kohonen)

setwd("m2/app_non_sup")
coolBlueHotRed = function(n, alpha = 1){rainbow(n, end=4/6, alpha=alpha)[n:1]}
#m = scale(m, center = TRUE, scale = TRUE)
m=as.matrix(read.table("matrix_qst4",sep = ","))
som_grid <- somgrid(xdim =5 , ydim=5, topo="hexagonal",neighbourhood.fct="gaussian")
som_model = som(m, som_grid,rlen=100)
?somgrid
plot(som_model, type="changes")
plot(som_model, type="count", main="Node Counts",palette.name=coolBlueHotRed)
#carte  uniforme avec buuble qye avec gausssian 

plot(som_model, type="dist.neighbours", main = "SOM neighbour distances",palette.name=coolBlueHotRed)
plot(som_model, type="quality",palette.name=coolBlueHotRed)
plot(som_model, type="codes",palette.name=coolBlueHotRed)

par(mfrow=c(1,1))
for (i in 5:5){
  plot(som_model, type="property", palette.name=coolBlueHotRed,property=som_model$codes[[1]][,i], main=colnames(som_model$codes[[1]])[i])
  }
som_model$codes[[1]][,1]
mydata <- som_model$codes[[1]]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)


# plot these results:


kn=kmeans(som_model$codes[[1]],2,nstart=50)
plot(som_model, type="codes",palette.name=coolBlueHotRed)
add.cluster.boundaries(som_model, kn$cluster)


plot(som_model , type="mapping", labels =kn$cluster,pchs=2,bgcol=kn$cluster)     
#somgrid(xdim = 8, ydim = 6, topo = c("rectangular", "hexagonal"),neighbourhood.fct = c("bubble", "gaussian"), toroidal = FALSE)
