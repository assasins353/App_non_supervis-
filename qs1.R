dataframe=read.csv("dataframe_sans_occupency.csv",sep=",",stringsAsFactors = FALSE,col.names = c("V1","V2","V3","V4","V5"))
setwd('m2/app_non_sup')
df2 <- data.frame(sapply(df[,2:3], function(x) { if(is.factor(x)) {
  as.numeric(as.character(x))
} else {
  x
}
}))
df=read.table("dataset.csv",sep=",")
df[5,]["V1"]=="BHMBCCMKT01"
save
df[2,]["V1"]
as.integer(df2[3,]["V2"])
if(save==as.integer(df2[3,]["V2"]))
{
  save=as.integer(df[i,]["V1"])
  print(cmp)
}
save=as.integer(df2[2,]["V2"])
cmp=0
for(i in 2:dim(df)[1])
{
  if(save!=as.integer(df2[i,]["V2"]))
  {
    save=as.integer(df2[i,]["V2"])
    print(cmp)
    cmp=0
  }
  cmp=cmp+1
}
1920/4
df[34407,]
i=344407
print(df[(i+6-1),]["V3"])
print(df[(i+5-1),]["V3"])

35718-34407
77*18
i=34407
cmp1=0
cmp2=0
cmp3=0
while(i <35719)
{
  cmp3=cmp3+2
  if(as.integer(df[(i+6-1),]["V3"])>480)
  {
    cmp1=cmp1+1
  }
  if(as.integer(df[(i+6-1),]["V3"])>1000)
  {
    cmp2=cmp2+1
  }
  print(df[(i+6-1),]["V3"])
  print(df[(i+5-1),]["V3"])
  i=i+7
}
print(cmp1)
print(cmp2)
print(cmp3)
if(as.integer(df[(34407+6-1),]["V3"])>1000)
{
  cmp2=cmp2+1
}
df[34407,]["V1"]

df=read.table("dataset_clean_1.csv",sep=",")
df=df[-1,]
nbr_park=20
j=1+18*77*(nbr_park-1)
k=nbr_park*18*77

df[j:k,][c("V2","V3")]


#replace row with the same date and time fro exemple if we have :
# row 300: 22/10/2016 08:00:00
# row 301: 22/10/2016 08:00:00
# row 302: 22/10/2016 09:00:00
#we will replace row 301 with 22/10/2016 08:30:00
library(chron)
library("lubridate")
cmp=0
for (i in 1:(dim(df)[1]-1))
{
  x=lapply(df[i,]['V4'], as.character)
  x=toString((x))
  x1=lapply(df[i+1,]['V4'], as.character)
  x1=toString((x1))
  z1=ymd_hms(x1)
  if(x1==x)
  {
    cmp=cmp+1
    z1=z1+30*60
    z1=toString(z1)
    df[i+1,]["V4"]=z1
  }
}
print(cmp)

#replace row with data and time at 07:30:00 with the value 08:00:00
cmp=0
for (i in 1:dim(df)[1])
{
  print(i)
  x=lapply(df[i,]['V4'], as.character)
  x=toString((x))
  z=sapply(strsplit(x," "), `[`, 2)
  y=sapply(strsplit(x," "), `[`, 1)
  if(z=="07:30:00")
  {
    cmp=cmp+1
    df[i,]["V4"]=paste(y,"08:00:00")
  }
}

write.csv(df, file = "dataset_arrange.csv",row.names=FALSE)
df[15782,]["V4"]="2016-11-20 15:30:00"
#fill our new dataset with the same value as the old dataset (missing value will be at 0)
j=1
for (i in 1:dim(dataframe)[1])
{
  x=lapply(df[j,]['V4'], as.character)
  x=toString((x))
  y=lapply(df[j,]['V1'], as.character)
  y=toString((y))
 #print(y)
  if(dataframe[i,]["V5"]==x && y==dataframe[i,]["V2"])
  {

    save=lapply(df[j,]["V3"], as.character)
    save=as.integer(save)
    dataframe[i,]["V4"]=save
    j=j+1

  }
  print(j)
}

i
df[17733,]
#Important nombre de mesures manquantes 38808-32636
dim(dataframe)[1]
nbr_park=20
j=1+18*77*(nbr_park-1)
k=nbr_park*18*77

df[17734,]["V4"]
dataframe[j:k+100,][c("V3","V4","V2","V5")]

nbr_park=20
j=1+18*77*(nbr_park-1)
k=nbr_park*18*77

dataframe[(j+18*18):(j+18*19),][c("V2","V5","V4")]
save=lapply(df[23901,]["V3"], as.character)
save=as.integer(save)
dataframe[26700,]
df[23942,]

dataframe[j,]["V4"]
df[2,]["V2"]

nbr=0
liste=c()
for(j in 1:dim(dataframe)[1])
{
  if(dataframe[j,]["V3"]==dataframe[j,]["V4"])
  {
    nbr=nbr+1
    liste=c(liste,j)
  }
}
nbr
liste[1]
df[1392,]
dataframe[1386+18*4+13,]
dataframe[1386+18*4+13,]["V3"]==dataframe[1386+18*4+13,]["V4"]
77*18
x=lapply(df[3,]['V4'], as.character)
x=toString((x))
save=lapply(df[3,]["V3"], as.character)
save=as.integer(save)
save
dataframe[3,]["V4"]=save
dataframe[3,]["V5"]==x
dataframe[3,]["V5"]
x

#replace missing values; if we have a misssing value in a day we repalce it with
#the mean of the last and the next value; if we have a weekday missing we will
#fill the 18 missing values (from 08:00:00 to 16:30:00) with the values of 
#the 7th before last day
for(z in 1:28)
{
  d=1386*z
  i=1+(z-1)*1386
  while(i<d)
  {
    
    if(dataframe[i,]["V4"]==0)
    {
      if(dataframe[i+1,]["V4"]!=0)
      {
        if(i>1)
        {
          dataframe[i,]["V4"]=(as.numeric(dataframe[(i+1),]["V4"])+as.numeric(dataframe[(i-1),]["V4"]))%/%2
          i=i+1
        }
        else
        {
          dataframe[i,]["V4"]=as.numeric(dataframe[(i+1),]["V4"])
          i=i+1
        }
      }
      else
      {
        k=i
        i=i+2
        cmp=2
        while(dataframe[i,]["V4"]==0)
        {
          i=i+1
          cmp=cmp+1
          if(cmp==18)
          {
            for(y in 1:18)
            {
              dataframe[k,]["V4"]=dataframe[(k-18*7),]["V4"]
              k=k+1
            }
            cmp=0
          }
        }
        if(cmp<18)
        { 
          for(h in 1:cmp)
          {
            dataframe[k+h-1,]["V4"]=(as.numeric(dataframe[(k+cmp),]["V4"])+as.numeric(dataframe[(k-1),]["V4"]))%/%2
          }
          
        }
      }
    }
    else
    {
      i=i+1
    }
  }
}
#we put the last ajjustment

#we check if there is not any missing value
liste=c()
c=0
for(i in 1:dim(dataframe)[1])
{
  x=lapply(dataframe[i,]['V5'], as.character)
  x=toString((x))
  z=sapply(strsplit(x," "), `[`, 2)
  if(as.numeric(dataframe[i,]["V4"])==0 && z!="08:00:00" && z!="16:30:00")
  {
   
    c=c+1
  }
  if(as.numeric(dataframe[i,]["V4"])>as.numeric(dataframe[i,]["V3"]))
  {
    dataframe[i,]["V4"]=as.numeric(dataframe[i,]["V3"])
    liste=c(liste,i)
    c=c+1
  }
}
c
write.csv(dataframe, file = "backup2.csv")
dataframe[2,]["V4"]


library(ggplot2)
theme_set(theme_bw())
setwd("m2/app_non_sup/")
d <- read.csv(file="backup1.csv", header=TRUE, sep=",")
d[1,]
ggplot(data=d) +  geom_line(aes(time,y), color="#6666CC") +  ylab("nuclear reponse")
tser <- ts(d["V4"], freq = 8)
tser
plot(tser)
