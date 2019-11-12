library(quantreg)
library(tidyverse)
library(readxl)
library(data.table)
library(magrittr)
library(midasr)
library(zoo)
library(ggplot2)
library(forecast)
library(data.table)
library(FactoMineR)
library(tseries)

#68/3/1-10/3/1 forecast 97/3.1-10/3.1
yq<-read.csv("C:/Users/lenovo/Desktop/MF_VAR_2013/Data/YQ.csv")[17:185,]
rsq<-yq[117:169,]
#quarterly:gdpgrowth rate(grr)  loginvfix
#monthly:unrate	hours	cpi	indpro	pce	fedfund	tbond	sp500
grr<-read.csv("C:/Users/lenovo/Desktop/A191RL1Q225SBEA.csv")[85:253,]
rsgrr<-grr[117:169,]
invfix<-read.csv("C:/Users/lenovo/Desktop/MF_VAR_2013/Data/YQ.csv")[17:185,3]
loginv<-log(invfix)
rsloginv<-loginv[117:169]
ym<-read.csv("C:/Users/lenovo/Desktop/MF_VAR_2013/Data/YM.csv")[49:555,c(1,2,3,4,5,6,7,8,9)]
gov<-log(yq$GCEC1)
rsym<-ym[349:507,]
dim(rsym)
rsgrr[1,]
ym$pce<-log(ym$pce)
pce<-ym$pce
unrate<-ym$unrate
hours<-log(ym$hours)
cpi<-log(ym$cpi)
indpro<-log(ym$indpro)
ff<-ym$fedfund
tbond<-ym$tbond
sp500<-log(ym$sp500)

c1<-data.frame(seq(3,507,3))
c2<-data.frame(c(1:507))
dataq1<-data.frame(c1,grr,gov,loginv)
datam1<-data.frame(c2,pce,cpi,tbond,sp500,unrate,ff,hours,indpro)
library(reshape)
testdataq1<-rename(dataq1,c(seq.3..507..3.='sasdate'))
testdatam1<-rename(datam1,c(c.1.507.='sasdate'))
datam3<-merge(testdatam1,testdataq1,by='sasdate',all=TRUE)

X1<-datam3[1:507,-c(1,10)]
X12<-datam3[2:507,-c(1,10)]
X13<-datam3[3:507,-c(1,10)]

library(mfbvar)
#change n_lags from seq(3,24,3) can get the forecast horizon from 1:8
prior_objunrate1 <- set_prior(Y = X1, freq = c(rep("m",8),rep("q",3)),
                              n_lags =24, n_burnin = 100, n_reps = 2000,n_fcst=159)
prior_objunrate2 <- set_prior(Y = X12, freq = c(rep("m",8),rep("q",3)),
                              n_lags =24, n_burnin = 100, n_reps = 2000,n_fcst=159)
prior_objunrate3 <- set_prior(Y = X13, freq = c(rep("m",8),rep("q",3)),
                              n_lags =24, n_burnin = 100, n_reps = 2000,n_fcst=159)

mod_minn81<- estimate_mfbvar(prior_objunrate1, prior_type = "minn")
mod_minn82<- estimate_mfbvar(prior_objunrate2, prior_type = "minn")
mod_minn83<- estimate_mfbvar(prior_objunrate3, prior_type = "minn")
cunratepre1<-mod_minn81$Z_fcst[,1:6,]
cunratepre2<-mod_minn82$Z_fcst[,1:6,]
cunratepre3<-mod_minn83$Z_fcst[,1:6,]
cunratepre11<-mod_minn81$Z_fcst[,7:11,]

cunratepre12<-mod_minn82$Z_fcst[,7:11,]
cunratepre13<-mod_minn83$Z_fcst[,7:11,]

d11<-array(0,dim=c(54,6,2000))
d12<-array(0,dim=c(54,6,2000))
d13<-array(0,dim=c(54,6,2000))
d111<-array(0,dim=c(54,5,2000))
d122<-array(0,dim=c(54,5,2000))
d133<-array(0,dim=c(54,5,2000))

for(k in 1:6){
  for (j in 1:2000){
    for(i in 1:162){
      if(i%%3==0){d11[i/3,k,j]=mean(cunratepre1[i-2,k,j],cunratepre1[i-1,k,j],cunratepre1[i,k,j])}
    }
  }
}
for(k in 1:6){
  for (j in 1:2000){
    for(i in 1:162){
      if(i%%3==0){d12[i/3,k,j]=mean(cunratepre2[i-2,k,j],cunratepre2[i-1,k,j],cunratepre2[i,k,j])}
    }
  }
}
for(k in 1:6){
  for (j in 1:2000){
    for(i in 1:162){
      if(i%%3==0){d13[i/3,k,j]=mean(cunratepre3[i-2,k,j],cunratepre3[i-1,k,j],cunratepre3[i,k,j])}
    }
  }
}
for(k in 1:5){
  for (j in 1:2000){
    for(i in 1:162){
      if(i%%3==0){d111[i/3,k,j]=mean(cunratepre11[i-2,k,j],cunratepre11[i-1,k,j],cunratepre11[i,k,j])}
    }
  }
}
for(k in 1:5){
  for (j in 1:2000){
    for(i in 1:162){
      if(i%%3==0){d122[i/3,k,j]=mean(cunratepre12[i-2,k,j],cunratepre12[i-1,k,j],cunratepre12[i,k,j])}
    }
  }
}
for(k in 1:5){
  for (j in 1:2000){
    for(i in 1:162){
      if(i%%3==0){d133[i/3,k,j]=mean(cunratepre13[i-2,k,j],cunratepre13[i-1,k,j],cunratepre13[i,k,j])}
    }
  }
}

cc1<-data.frame(matrix(0,2000,6))
cc2<-data.frame(matrix(0,2000,6))
cc3<-data.frame(matrix(0,2000,6))
cc11<-data.frame(matrix(0,2000,5))
cc22<-data.frame(matrix(0,2000,5))
cc33<-data.frame(matrix(0,2000,5))


for(i in 1:2000){
  for(j in 1:6)
  cc1[i,j]<-sqrt(mean((d11[2:54,j,i]-X1[117:169,j,i])^2))
}
for(i in 1:2000){
  for(j in 1:6)
    cc2[i,j]<-sqrt(mean((d12[2:54,j,i]-X1[117:169,j,i])^2))
}
for(i in 1:2000){
  for(j in 1:6)
    cc3[i,j]<-sqrt(mean((d13[2:54,j,i]-X1[117:169,j,i])^2))
}
for(i in 1:2000){
  for(j in 1:5)
    cc11[i,j]<-sqrt(mean((d111[2:54,j,i]-X1[117:169,j,i])^2))
}
for(i in 1:2000){
  for(j in 1:5)
    cc22[i,j]<-sqrt(mean((d122[2:54,j,i]-X1[117:169,j,i])^2))
}
for(i in 1:2000){
  for(j in 1:5)
    cc33[i,j]<-sqrt(mean((d133[2:54,j,i]-X1[117:169,j,i])^2))
}

result8<-data.frame(c(apply(cc1,2,min),apply(cc11,2,min)),c(apply(cc2,2,min),apply(cc22,2,min)),c(apply(cc3,2,min),apply(cc33,2,min)))

colnames(result8)<-c("month0","month1","month2")
rmse8<-data.frame(t(result8))
colnames(rmse8)<-names(X1)
#library(readr)
write_csv(rmse7,"rmsemfvar3.csv",append=TRUE)


