library(tidyverse)
library(midasr)
View(data1)
#68/3/1-10/3/1 forecast 97/3.1-10/3.1
yq<-read.csv("C:/Users/lenovo/Desktop/MF_VAR_2013/Data/YQ.csv")[17:185,]
rsq<-yq[117:169,]
#quarterly:gdpgrowth rate(grr)  loginvfix
#monthly:unrate	hours	cpi	indpro	pce	fedfund	tbond	sp500
grr<-read.csv("C:/Users/lenovo/Desktop/A191RL1Q225SBEA.csv")[85:253,2]
rsgrr<-grr[117:169]
invfix<-read.csv("C:/Users/lenovo/Desktop/MF_VAR_2013/Data/YQ.csv")[17:185,3]
loginv<-log(invfix)
rsloginv<-loginv[117:169]
ym<-read.csv("C:/Users/lenovo/Desktop/MF_VAR_2013/Data/YM.csv")[49:555,c(1,2,3,4,5,6,7,8,9)]

rsym<-ym[349:507,]
dim(rsym)
rsgrr[1,]
ym$pce<-log(ym$pce)
pce<-ym$pce
gov<-log(yq$GCEC1)
unrate<-ym$unrate
hours<-ym$hours
cpi<-log(ym$cpi)
indpro<-ym$indpro
ff<-ym$fedfund
tbond<-ym$tbond
sp500<-ym$sp500
trend <- c(1:169)
#1step
almon_model2.1_pca= midas_r(grr[1:168] ~ mls(pce[2:505], 1:3, m = 3,nealmon) +mls(unrate[2:505], 1:3, m=3,nealmon)+mls(hours[2:505],1:3,m=3,nealmon)+mls(cpi[2:505],1:3,m=3,nealmon)+mls(indpro[2:505],1:3,m=3,nealmon)+mls(ff[2:505],1:3,m=3,nealmon)+mls(tbond[2:505],1:3,m=3,nealmon)+mls(sp500[2:505],1:3,m=3,nealmon)+mls(loginv[1:168],1:3,m=1,nealmon)+mls(gov[1:168],1:3,m=1,nealmon),
                            start = list(pce=c(0,0,0),unrate=c(1,-0.5,-0.2),hours=c(1,-0.5,-0.2),cpi=c(0,0,-1),indpro=c(1,-0.5,-0.2),ff=c(1,-0.5,-0.2),tbond=c(1,-0.5,-0.2),sp500=c(1,-0.5,-0.2),loginv=c(1,-0.5,-0.2),gov=c(1,-0.5,-0.2)))
almon_model2.0_pca= midas_u(grr ~ mls(pce, 1:3, m = 3,nealmon) +mls(unrate, 1:3, m=3,nealmon)+mls(hours,1:3,m=3,nealmon)+mls(cpi,1:3,m=3,nealmon)+mls(indpro,1:3,m=3,nealmon)+mls(ff,1:3,m=3,nealmon)+mls(tbond,1:3,m=3,nealmon)+mls(sp500,1:3,m=3,nealmon)+mls(loginv,1:3,m=1,nealmon)+mls(gov,1:3,m=1,nealmon))
almon_model2.1_pca= midas_u(grr[1:168] ~ mls(pce[2:505], 1:3, m = 3,nealmon) +mls(unrate[2:505], 1:3, m=3,nealmon)+mls(hours[2:505],1:3,m=3,nealmon)+mls(cpi[2:505],1:3,m=3,nealmon)+mls(indpro[2:505],1:3,m=3,nealmon)+mls(ff[2:505],1:3,m=3,nealmon)+mls(tbond[2:505],1:3,m=3,nealmon)+mls(sp500[2:505],1:3,m=3,nealmon)+mls(loginv[1:168],1:3,m=1,nealmon)+mls(gov[1:168],1:3,m=1,nealmon))
almon_model2.2_pca= midas_u(grr[1:168] ~ mls(pce[3:506], 1:3, m = 3,nealmon) +mls(unrate[3:506], 1:3, m=3,nealmon)+mls(hours[3:506],1:3,m=3,nealmon)+mls(cpi[3:506],1:3,m=3,nealmon)+mls(indpro[3:506],1:3,m=3,nealmon)+mls(ff[3:506],1:3,m=3,nealmon)+mls(tbond[3:506],1:3,m=3,nealmon)+mls(sp500[3:506],1:3,m=3,nealmon)+mls(loginv[1:168],1:3,m=1,nealmon)+mls(gov[1:168],1:3,m=1,nealmon))
m2.0_pca= midas_u(grr ~ mls(pce, 2:4, m = 3,nealmon) +mls(unrate, 2:4, m=3,nealmon)+mls(hours,2:4,m=3,nealmon)+mls(cpi,2:4,m=3,nealmon)+mls(indpro,2:4,m=3,nealmon)+mls(ff,2:4,m=3,nealmon)+mls(tbond,2:4,m=3,nealmon)+mls(sp500,2:4,m=3,nealmon)+mls(loginv,2:4,m=1,nealmon)+mls(gov,2:4,m=1,nealmon))
m2.1_pca= midas_u(grr[1:168] ~ mls(pce[2:505], 2:4, m = 3,nealmon) +mls(unrate[2:505], 2:4, m=3,nealmon)+mls(hours[2:505],2:4,m=3,nealmon)+mls(cpi[2:505],2:4,m=3,nealmon)+mls(indpro[2:505],2:4,m=3,nealmon)+mls(ff[2:505],2:4,m=3,nealmon)+mls(tbond[2:505],2:4,m=3,nealmon)+mls(sp500[2:505],2:4,m=3,nealmon)+mls(loginv[1:168],2:4,m=1,nealmon)+mls(gov[1:168],2:4,m=1,nealmon))
m2.2_pca= midas_u(grr[1:168] ~ mls(pce[3:506], 2:4, m = 3,nealmon) +mls(unrate[3:506], 2:4, m=3,nealmon)+mls(hours[3:506],2:4,m=3,nealmon)+mls(cpi[3:506],2:4,m=3,nealmon)+mls(indpro[3:506],2:4,m=3,nealmon)+mls(ff[3:506],2:4,m=3,nealmon)+mls(tbond[3:506],2:4,m=3,nealmon)+mls(sp500[3:506],2:4,m=3,nealmon)+mls(loginv[1:168],2:4,m=1,nealmon)+mls(gov[1:168],2:4,m=1,nealmon))
m3.0_pca= midas_u(grr ~ mls(pce, 3:5, m = 3,nealmon) +mls(unrate, 3:5, m=3,nealmon)+mls(hours,3:5,m=3,nealmon)+mls(cpi,3:5,m=3,nealmon)+mls(indpro,3:5,m=3,nealmon)+mls(ff,3:5,m=3,nealmon)+mls(tbond,3:5,m=3,nealmon)+mls(sp500,3:5,m=3,nealmon)+mls(loginv,3:5,m=1,nealmon)+mls(gov,3:5,m=1,nealmon))
m3.1_pca= midas_u(grr[1:168] ~ mls(pce[2:505], 3:5, m = 3,nealmon) +mls(unrate[2:505], 3:5, m=3,nealmon)+mls(hours[2:505],3:5,m=3,nealmon)+mls(cpi[2:505],3:5,m=3,nealmon)+mls(indpro[2:505],3:5,m=3,nealmon)+mls(ff[2:505],3:5,m=3,nealmon)+mls(tbond[2:505],3:5,m=3,nealmon)+mls(sp500[2:505],3:5,m=3,nealmon)+mls(loginv[1:168],3:5,m=1,nealmon)+mls(gov[1:168],3:5,m=1,nealmon))
m3.2_pca= midas_u(grr[1:168] ~ mls(pce[3:506], 3:5, m = 3,nealmon) +mls(unrate[3:506], 3:5, m=3,nealmon)+mls(hours[3:506],3:5,m=3,nealmon)+mls(cpi[3:506],3:5,m=3,nealmon)+mls(indpro[3:506],3:5,m=3,nealmon)+mls(ff[3:506],3:5,m=3,nealmon)+mls(tbond[3:506],3:5,m=3,nealmon)+mls(sp500[3:506],3:5,m=3,nealmon)+mls(loginv[1:168],3:5,m=1,nealmon)+mls(gov[1:168],3:5,m=1,nealmon))
m4.0_pca= midas_u(grr ~ mls(pce, 4:6, m = 3,nealmon) +mls(unrate, 4:6, m=3,nealmon)+mls(hours,4:6,m=3,nealmon)+mls(cpi,4:6,m=3,nealmon)+mls(indpro,4:6,m=3,nealmon)+mls(ff,4:6,m=3,nealmon)+mls(tbond,4:6,m=3,nealmon)+mls(sp500,4:6,m=3,nealmon)+mls(loginv,4:6,m=1,nealmon)+mls(gov,4:6,m=1,nealmon))
m4.1_pca= midas_u(grr[1:168] ~ mls(pce[2:505], 4:6, m = 3,nealmon) +mls(unrate[2:505], 4:6, m=3,nealmon)+mls(hours[2:505],4:6,m=3,nealmon)+mls(cpi[2:505],4:6,m=3,nealmon)+mls(indpro[2:505],4:6,m=3,nealmon)+mls(ff[2:505],4:6,m=3,nealmon)+mls(tbond[2:505],4:6,m=3,nealmon)+mls(sp500[2:505],4:6,m=3,nealmon)+mls(loginv[1:168],4:6,m=1,nealmon)+mls(gov[1:168],4:6,m=1,nealmon))
m4.2_pca= midas_u(grr[1:168] ~ mls(pce[3:506], 4:6, m = 3,nealmon) +mls(unrate[3:506], 4:6, m=3,nealmon)+mls(hours[3:506],4:6,m=3,nealmon)+mls(cpi[3:506],4:6,m=3,nealmon)+mls(indpro[3:506],4:6,m=3,nealmon)+mls(ff[3:506],4:6,m=3,nealmon)+mls(tbond[3:506],4:6,m=3,nealmon)+mls(sp500[3:506],4:6,m=3,nealmon)+mls(loginv[1:168],4:6,m=1,nealmon)+mls(gov[1:168],4:6,m=1,nealmon))

m5.0_pca= midas_u(grr ~ mls(pce, 5:7, m = 3,nealmon) +mls(unrate, 5:7, m=3,nealmon)+mls(hours,5:7,m=3,nealmon)+mls(cpi,5:7,m=3,nealmon)+mls(indpro,5:7,m=3,nealmon)+mls(ff,5:7,m=3,nealmon)+mls(tbond,5:7,m=3,nealmon)+mls(sp500,5:7,m=3,nealmon)+mls(loginv,5:7,m=1,nealmon)+mls(gov,5:7,m=1,nealmon))
m5.1_pca= midas_u(grr[1:168] ~ mls(pce[2:505], 5:7, m = 3,nealmon) +mls(unrate[2:505], 5:7, m=3,nealmon)+mls(hours[2:505],5:7,m=3,nealmon)+mls(cpi[2:505],5:7,m=3,nealmon)+mls(indpro[2:505],5:7,m=3,nealmon)+mls(ff[2:505],5:7,m=3,nealmon)+mls(tbond[2:505],5:7,m=3,nealmon)+mls(sp500[2:505],5:7,m=3,nealmon)+mls(loginv[1:168],5:7,m=1,nealmon)+mls(gov[1:168],5:7,m=1,nealmon))
m5.2_pca= midas_u(grr[1:168] ~ mls(pce[3:506], 5:7, m = 3,nealmon) +mls(unrate[3:506], 5:7, m=3,nealmon)+mls(hours[3:506],5:7,m=3,nealmon)+mls(cpi[3:506],5:7,m=3,nealmon)+mls(indpro[3:506],5:7,m=3,nealmon)+mls(ff[3:506],5:7,m=3,nealmon)+mls(tbond[3:506],5:7,m=3,nealmon)+mls(sp500[3:506],5:7,m=3,nealmon)+mls(loginv[1:168],5:7,m=1,nealmon)+mls(gov[1:168],5:7,m=1,nealmon))

m6.0_pca= midas_u(grr ~ mls(pce, 6:8, m = 3,nealmon) +mls(unrate, 6:8, m=3,nealmon)+mls(hours,6:8,m=3,nealmon)+mls(cpi,6:8,m=3,nealmon)+mls(indpro,6:8,m=3,nealmon)+mls(ff,6:8,m=3,nealmon)+mls(tbond,6:8,m=3,nealmon)+mls(sp500,6:8,m=3,nealmon)+mls(loginv,6:8,m=1,nealmon)+mls(gov,6:8,m=1,nealmon))
m6.1_pca= midas_u(grr[1:168] ~ mls(pce[2:505], 6:8, m = 3,nealmon) +mls(unrate[2:505], 6:8, m=3,nealmon)+mls(hours[2:505],6:8,m=3,nealmon)+mls(cpi[2:505],6:8,m=3,nealmon)+mls(indpro[2:505],6:8,m=3,nealmon)+mls(ff[2:505],6:8,m=3,nealmon)+mls(tbond[2:505],6:8,m=3,nealmon)+mls(sp500[2:505],6:8,m=3,nealmon)+mls(loginv[1:168],6:8,m=1,nealmon)+mls(gov[1:168],6:8,m=1,nealmon))
m6.2_pca= midas_u(grr[1:168] ~ mls(pce[3:506], 6:8, m = 3,nealmon) +mls(unrate[3:506],6:8, m=3,nealmon)+mls(hours[3:506],6:8,m=3,nealmon)+mls(cpi[3:506],6:8,m=3,nealmon)+mls(indpro[3:506],6:8,m=3,nealmon)+mls(ff[3:506],6:8,m=3,nealmon)+mls(tbond[3:506],6:8,m=3,nealmon)+mls(sp500[3:506],6:8,m=3,nealmon)+mls(loginv[1:168],6:8,m=1,nealmon)+mls(gov[1:168],6:8,m=1,nealmon))

m7.0_pca= midas_u(grr ~ mls(pce,7:9, m = 3,nealmon) +mls(unrate, 7:9, m=3,nealmon)+mls(hours,7:9,m=3,nealmon)+mls(cpi,7:9,m=3,nealmon)+mls(indpro,7:9,m=3,nealmon)+mls(ff,7:9,m=3,nealmon)+mls(tbond,7:9,m=3,nealmon)+mls(sp500,7:9,m=3,nealmon)+mls(loginv,7:9,m=1,nealmon)+mls(gov,7:9,m=1,nealmon))
m7.1_pca= midas_u(grr[1:168] ~ mls(pce[2:505],7:9, m = 3,nealmon) +mls(unrate[2:505],7:9, m=3,nealmon)+mls(hours[2:505],7:9,m=3,nealmon)+mls(cpi[2:505],7:9,m=3,nealmon)+mls(indpro[2:505],7:9,m=3,nealmon)+mls(ff[2:505],7:9,m=3,nealmon)+mls(tbond[2:505],7:9,m=3,nealmon)+mls(sp500[2:505],7:9,m=3,nealmon)+mls(loginv[1:168],7:9,m=1,nealmon)+mls(gov[1:168],7:9,m=1,nealmon))
m7.2_pca= midas_u(grr[1:168] ~ mls(pce[3:506],7:9, m = 3,nealmon) +mls(unrate[3:506], 7:9, m=3,nealmon)+mls(hours[3:506],7:9,m=3,nealmon)+mls(cpi[3:506],7:9,m=3,nealmon)+mls(indpro[3:506],7:9,m=3,nealmon)+mls(ff[3:506],7:9,m=3,nealmon)+mls(tbond[3:506],7:9,m=3,nealmon)+mls(sp500[3:506],7:9,m=3,nealmon)+mls(loginv[1:168],7:9,m=1,nealmon)+mls(gov[1:168],7:9,m=1,nealmon))

m8.0_pca= midas_u(grr ~ mls(pce,8:10, m = 3,nealmon) +mls(unrate, 8:10, m=3,nealmon)+mls(hours,8:10,m=3,nealmon)+mls(cpi,8:10,m=3,nealmon)+mls(indpro,8:10,m=3,nealmon)+mls(ff,8:10,m=3,nealmon)+mls(tbond,8:10,m=3,nealmon)+mls(sp500,8:10,m=3,nealmon)+mls(loginv,8:10,m=1,nealmon)+mls(gov,8:10,m=1,nealmon))
m8.1_pca= midas_u(grr[1:168] ~ mls(pce[2:505],8:10, m = 3,nealmon) +mls(unrate[2:505], 8:10, m=3,nealmon)+mls(hours[2:505],8:10,m=3,nealmon)+mls(cpi[2:505],8:10,m=3,nealmon)+mls(indpro[2:505],8:10,m=3,nealmon)+mls(ff[2:505],8:10,m=3,nealmon)+mls(tbond[2:505],8:10,m=3,nealmon)+mls(sp500[2:505],8:10,m=3,nealmon)+mls(loginv[1:168],8:10,m=1,nealmon)+mls(gov[1:168],8:10,m=1,nealmon))
m8.2_pca= midas_u(grr[1:168] ~ mls(pce[3:506],8:10, m = 3,nealmon) +mls(unrate[3:506], 8:10, m=3,nealmon)+mls(hours[3:506],8:10,m=3,nealmon)+mls(cpi[3:506],8:10,m=3,nealmon)+mls(indpro[3:506],8:10,m=3,nealmon)+mls(ff[3:506],8:10,m=3,nealmon)+mls(tbond[3:506],8:10,m=3,nealmon)+mls(sp500[3:506],8:10,m=3,nealmon)+mls(loginv[1:168],8:10,m=1,nealmon)+mls(gov[1:168],8:10,m=1,nealmon))
#forecast horizon
s=117
e=169
almon_model2.1_pca_forecast<-forecast(almon_model2.0_pca,list(gov=gov[(s+1):e],unrate=unrate[(s*3+1):(e*3)],cpi=cpi[(s*3+1):(e*3)],
                                                              indpro=indpro[(s*3+1):(e*3)],loginv=loginv[(s+1):e],hours=hours[(s*3+1):(e*3)],ff=ff[(s*3+1):(e*3)],tbond=tbond[(s*3+1):(e*3)],sp500=sp500[(s*3+1):(e*3)],
                                                               pce=pce[(s*3+1):(e*3)]),method = 'static')


gdp_forecast1<-almon_model2.1_pca_forecast$mean
summary(almon_model2.1_pca_forecast)
grrrmse<-data.frame(matrix(0,3,8))

grrrmse[1,1]<-sqrt(mean((almon_model2.0_pca$fitted.values[114:166]-grr[117:169])^2))
grrrmse[2,1]<-sqrt(mean((almon_model2.1_pca$fitted.values[113:165]-grr[116:168])^2))
grrrmse[3,1]<-sqrt(mean((almon_model2.2_pca$fitted.values[113:165]-grr[116:168])^2))
grrrmse[1,2]<-sqrt(mean((m2.0_pca$fitted.values[113:165]-grr[117:169])^2))
grrrmse[2,2]<-sqrt(mean((m2.1_pca$fitted.values[112:164]-grr[116:168])^2))
grrrmse[3,2]<-sqrt(mean((m2.2_pca$fitted.values[112:164]-grr[116:168])^2))
grrrmse[1,3]<-sqrt(mean((m3.0_pca$fitted.values[112:164]-grr[117:169])^2))
grrrmse[2,3]<-sqrt(mean((m3.1_pca$fitted.values[111:163]-grr[116:168])^2))
grrrmse[3,3]<-sqrt(mean((m3.2_pca$fitted.values[111:163]-grr[116:168])^2))
grrrmse[1,4]<-sqrt(mean((m4.0_pca$fitted.values[111:163]-grr[117:169])^2))
grrrmse[2,4]<-sqrt(mean((m4.1_pca$fitted.values[110:162]-grr[116:168])^2))
grrrmse[3,4]<-sqrt(mean((m4.2_pca$fitted.values[110:162]-grr[116:168])^2))
grrrmse[1,5]<-sqrt(mean((m5.0_pca$fitted.values[110:162]-grr[117:169])^2))
grrrmse[2,5]<-sqrt(mean((m5.1_pca$fitted.values[109:161]-grr[116:168])^2))
grrrmse[3,5]<-sqrt(mean((m5.2_pca$fitted.values[109:161]-grr[116:168])^2))
grrrmse[1,6]<-sqrt(mean((m6.0_pca$fitted.values[109:161]-grr[117:169])^2))
grrrmse[2,6]<-sqrt(mean((m6.1_pca$fitted.values[108:160]-grr[116:168])^2))
grrrmse[3,6]<-sqrt(mean((m6.2_pca$fitted.values[108:160]-grr[116:168])^2))
grrrmse[1,7]<-sqrt(mean((m7.0_pca$fitted.values[108:160]-grr[117:169])^2))
grrrmse[2,7]<-sqrt(mean((m7.1_pca$fitted.values[107:159]-grr[116:168])^2))
grrrmse[3,7]<-sqrt(mean((m7.2_pca$fitted.values[107:159]-grr[116:168])^2))

grrrmse[1,8]<-sqrt(mean((m8.0_pca$fitted.values[107:159]-grr[117:169])^2))
grrrmse[2,8]<-sqrt(mean((m8.1_pca$fitted.values[106:158]-grr[116:168])^2))
grrrmse[3,8]<-sqrt(mean((m8.2_pca$fitted.values[106:158]-grr[116:168])^2))
c1<-grrrmse[1,]
datagrr<-data.frame(t(grrrmse))
colnames(datagrr)<-c("month0","month1","month2")

library(data.table)
datagrr1<-gather(datagrr,period,rmse)
unrate1<-rep(0,169)
for(i in 1:507){
  if(i%%3==0){unrate1[i/3]=mean(unrate[i-2],unrate[i-1],unrate[i])}
}
unrate1
##unrate
u1.0_pca= midas_u(unrate ~ mls(pce, 1:3, m = 1,nealmon) +mls(grr, 1:3, m=1/3,nealmon)+mls(hours,1:3,m=1,nealmon)+mls(cpi,1:3,m=1,nealmon)+mls(indpro,1:3,m=1,nealmon)+mls(ff,1:3,m=1,nealmon)+mls(tbond,1:3,m=1,nealmon)+mls(sp500,1:3,m=1,nealmon)+mls(loginv,1:3,m=1/3,nealmon)+mls(gov,1:3,m=1/3,nealmon))
u1.1_pca= midas_u(unrate[2:505] ~ mls(pce[2:505], 1:3, m = 1,nealmon) +mls(grr[1:168], 1:3, m=1/3,nealmon)+mls(hours[2:505],1:3,m=1,nealmon)+mls(cpi[2:505],1:3,m=1,nealmon)+mls(indpro[2:505],1:3,m=1,nealmon)+mls(ff[2:505],1:3,m=1,nealmon)+mls(tbond[2:505],1:3,m=1,nealmon)+mls(sp500[2:505],1:3,m=1,nealmon)+mls(loginv[1:168],1:3,m=1/3,nealmon)+mls(gov[1:168],1:3,m=1/3,nealmon))
u1.2_pca= midas_u(unrate[3:506] ~ mls(pce[3:506], 1:3, m = 1,nealmon) +mls(grr[1:168], 1:3, m=1/3,nealmon)+mls(hours[3:506],1:3,m=1,nealmon)+mls(cpi[3:506],1:3,m=1,nealmon)+mls(indpro[3:506],1:3,m=1,nealmon)+mls(ff[3:506],1:3,m=1,nealmon)+mls(tbond[3:506],1:3,m=1,nealmon)+mls(sp500[3:506],1:3,m=1,nealmon)+mls(loginv[1:168],1:3,m=1/3,nealmon)+mls(gov[1:168],1:3,m=1/3,nealmon))
u2.0_pca= midas_u(unrate~ mls(pce, 2:4, m =1,nealmon) +mls(grr, 2:4, m=1/3,nealmon)+mls(hours,2:4,m=1,nealmon)+mls(cpi,2:4,m=1,nealmon)+mls(indpro,2:4,m=1,nealmon)+mls(ff,2:4,m=1,nealmon)+mls(tbond,2:4,m=1,nealmon)+mls(sp500,2:4,m=1,nealmon)+mls(loginv,2:4,m=1/3,nealmon)+mls(gov,2:4,m=1/3,nealmon))
u2.1_pca= midas_u(unrate[2:505] ~ mls(pce[2:505], 2:4, m = 1,nealmon) +mls(grr[1:168], 2:4, m=1/3,nealmon)+mls(hours[2:505],2:4,m=1,nealmon)+mls(cpi[2:505],2:4,m=1,nealmon)+mls(indpro[2:505],2:4,m=1,nealmon)+mls(ff[2:505],2:4,m=1,nealmon)+mls(tbond[2:505],2:4,m=1,nealmon)+mls(sp500[2:505],2:4,m=1,nealmon)+mls(loginv[1:168],2:4,m=1/3,nealmon)+mls(gov[1:168],2:4,m=1/3,nealmon))
u2.2_pca= midas_u(unrate[3:506] ~ mls(pce[3:506], 2:4, m = 1,nealmon) +mls(grr[1:168], 2:4, m=1/3,nealmon)+mls(hours[3:506],2:4,m=1,nealmon)+mls(cpi[3:506],2:4,m=1,nealmon)+mls(indpro[3:506],2:4,m=1,nealmon)+mls(ff[3:506],2:4,m=1,nealmon)+mls(tbond[3:506],2:4,m=1,nealmon)+mls(sp500[3:506],2:4,m=1,nealmon)+mls(loginv[1:168],2:4,m=1/3,nealmon)+mls(gov[1:168],2:4,m=1/3,nealmon))
u3.0_pca= midas_u(unrate~ mls(pce, 3:5, m = 1,nealmon) +mls(grr, 3:5, m=1/3,nealmon)+mls(hours,3:5,m=1,nealmon)+mls(cpi,3:5,m=1,nealmon)+mls(indpro,3:5,m=1,nealmon)+mls(ff,3:5,m=1,nealmon)+mls(tbond,3:5,m=1,nealmon)+mls(sp500,3:5,m=1,nealmon)+mls(loginv,3:5,m=1/3,nealmon)+mls(gov,3:5,m=1/3,nealmon))
u3.1_pca= midas_u(unrate[2:505] ~ mls(pce[2:505], 3:5, m = 1,nealmon) +mls(grr[1:168], 3:5, m=1/3,nealmon)+mls(hours[2:505],3:5,m=1,nealmon)+mls(cpi[2:505],3:5,m=1,nealmon)+mls(indpro[2:505],3:5,m=1,nealmon)+mls(ff[2:505],3:5,m=1,nealmon)+mls(tbond[2:505],3:5,m=1,nealmon)+mls(sp500[2:505],3:5,m=1,nealmon)+mls(loginv[1:168],3:5,m=1/3,nealmon)+mls(gov[1:168],3:5,m=1/3,nealmon))
u3.2_pca= midas_u(unrate[3:506] ~ mls(pce[3:506], 3:5, m = 1,nealmon) +mls(grr[1:168], 3:5, m=1/3,nealmon)+mls(hours[3:506],3:5,m=1,nealmon)+mls(cpi[3:506],3:5,m=1,nealmon)+mls(indpro[3:506],3:5,m=1,nealmon)+mls(ff[3:506],3:5,m=1,nealmon)+mls(tbond[3:506],3:5,m=1,nealmon)+mls(sp500[3:506],3:5,m=1,nealmon)+mls(loginv[1:168],3:5,m=1/3,nealmon)+mls(gov[1:168],3:5,m=1/3,nealmon))
u4.0_pca= midas_u(unrate~ mls(pce, 4:6, m = 1,nealmon) +mls(grr, 4:6, m=1/3,nealmon)+mls(hours,4:6,m=1,nealmon)+mls(cpi,4:6,m=1,nealmon)+mls(indpro,4:6,m=1,nealmon)+mls(ff,4:6,m=1,nealmon)+mls(tbond,4:6,m=1,nealmon)+mls(sp500,4:6,m=1,nealmon)+mls(loginv,4:6,m=1/3,nealmon)+mls(gov,4:6,m=1/3,nealmon))
u4.1_pca= midas_u(unrate[2:505] ~ mls(pce[2:505], 4:6, m =1,nealmon) +mls(grr[1:168], 4:6, m=1/3,nealmon)+mls(hours[2:505],4:6,m=1,nealmon)+mls(cpi[2:505],4:6,m=1,nealmon)+mls(indpro[2:505],4:6,m=1,nealmon)+mls(ff[2:505],4:6,m=1,nealmon)+mls(tbond[2:505],4:6,m=1,nealmon)+mls(sp500[2:505],4:6,m=1,nealmon)+mls(loginv[1:168],4:6,m=1/3,nealmon)+mls(gov[1:168],4:6,m=1/3,nealmon))
u4.2_pca= midas_u(unrate[3:506]~ mls(pce[3:506], 4:6, m =1,nealmon) +mls(grr[1:168], 4:6, m=1/3,nealmon)+mls(hours[3:506],4:6,m=1,nealmon)+mls(cpi[3:506],4:6,m=1,nealmon)+mls(indpro[3:506],4:6,m=1,nealmon)+mls(ff[3:506],4:6,m=1,nealmon)+mls(tbond[3:506],4:6,m=1,nealmon)+mls(sp500[3:506],4:6,m=1,nealmon)+mls(loginv[1:168],4:6,m=1/3,nealmon)+mls(gov[1:168],4:6,m=1/3,nealmon))
u5.0_pca= midas_u(unrate~ mls(pce, 5:7, m =1,nealmon) +mls(grr, 5:7, m=1/3,nealmon)+mls(hours,5:7,m=1,nealmon)+mls(cpi,5:7,m=1,nealmon)+mls(indpro,5:7,m=1,nealmon)+mls(ff,5:7,m=1,nealmon)+mls(tbond,5:7,m=1,nealmon)+mls(sp500,5:7,m=1,nealmon)+mls(loginv,5:7,m=1/3,nealmon)+mls(gov,5:7,m=1/3,nealmon))
u5.1_pca= midas_u(unrate[2:505] ~ mls(pce[2:505], 5:7, m =1,nealmon) +mls(grr[1:168], 5:7, m=1/3,nealmon)+mls(hours[2:505],5:7,m=1,nealmon)+mls(cpi[2:505],5:7,m=1,nealmon)+mls(indpro[2:505],5:7,m=1,nealmon)+mls(ff[2:505],5:7,m=1,nealmon)+mls(tbond[2:505],5:7,m=1,nealmon)+mls(sp500[2:505],5:7,m=1,nealmon)+mls(loginv[1:168],5:7,m=1/3,nealmon)+mls(gov[1:168],5:7,m=1/3,nealmon))
u5.2_pca= midas_u(unrate[3:506] ~ mls(pce[3:506], 5:7, m =1,nealmon) +mls(grr[1:168], 5:7, m=1/3,nealmon)+mls(hours[3:506],5:7,m=1,nealmon)+mls(cpi[3:506],5:7,m=1,nealmon)+mls(indpro[3:506],5:7,m=1,nealmon)+mls(ff[3:506],5:7,m=1,nealmon)+mls(tbond[3:506],5:7,m=1,nealmon)+mls(sp500[3:506],5:7,m=1,nealmon)+mls(loginv[1:168],5:7,m=1/3,nealmon)+mls(gov[1:168],5:7,m=1/3,nealmon))

u6.0_pca= midas_u(unrate~ mls(pce, 6:8, m = 1,nealmon) +mls(grr, 6:8, m=1/3,nealmon)+mls(hours,6:8,m=1,nealmon)+mls(cpi,6:8,m=1,nealmon)+mls(indpro,6:8,m=1,nealmon)+mls(ff,6:8,m=1,nealmon)+mls(tbond,6:8,m=1,nealmon)+mls(sp500,6:8,m=1,nealmon)+mls(loginv,6:8,m=1/3,nealmon)+mls(gov,6:8,m=1/3,nealmon))
u6.1_pca= midas_u(unrate[2:505] ~ mls(pce[2:505], 6:8, m = 1,nealmon) +mls(grr[1:168], 6:8, m=1/3,nealmon)+mls(hours[2:505],6:8,m=1,nealmon)+mls(cpi[2:505],6:8,m=1,nealmon)+mls(indpro[2:505],6:8,m=1,nealmon)+mls(ff[2:505],6:8,m=1,nealmon)+mls(tbond[2:505],6:8,m=1,nealmon)+mls(sp500[2:505],6:8,m=1,nealmon)+mls(loginv[1:168],6:8,m=1/3,nealmon)+mls(gov[1:168],6:8,m=1/3,nealmon))
u6.2_pca= midas_u(unrate[3:506] ~ mls(pce[3:506], 6:8, m = 1,nealmon) +mls(grr[1:168],6:8, m=1/3,nealmon)+mls(hours[3:506],6:8,m=1,nealmon)+mls(cpi[3:506],6:8,m=1,nealmon)+mls(indpro[3:506],6:8,m=1,nealmon)+mls(ff[3:506],6:8,m=1,nealmon)+mls(tbond[3:506],6:8,m=1,nealmon)+mls(sp500[3:506],6:8,m=1,nealmon)+mls(loginv[1:168],6:8,m=1/3,nealmon)+mls(gov[1:168],6:8,m=1/3,nealmon))

u7.0_pca= midas_u(unrate~ mls(pce,7:9, m = 1,nealmon) +mls(grr, 7:9, m=1/3,nealmon)+mls(hours,7:9,m=1,nealmon)+mls(cpi,7:9,m=1,nealmon)+mls(indpro,7:9,m=1,nealmon)+mls(ff,7:9,m=1,nealmon)+mls(tbond,7:9,m=1,nealmon)+mls(sp500,7:9,m=1,nealmon)+mls(loginv,7:9,m=1/3,nealmon)+mls(gov,7:9,m=1/3,nealmon))
u7.1_pca= midas_u(unrate[2:505] ~ mls(pce[2:505],7:9, m = 1,nealmon) +mls(grr[1:168],7:9, m=1/3,nealmon)+mls(hours[2:505],7:9,m=1,nealmon)+mls(cpi[2:505],7:9,m=1,nealmon)+mls(indpro[2:505],7:9,m=1,nealmon)+mls(ff[2:505],7:9,m=1,nealmon)+mls(tbond[2:505],7:9,m=1,nealmon)+mls(sp500[2:505],7:9,m=1,nealmon)+mls(loginv[1:168],7:9,m=1/3,nealmon)+mls(gov[1:168],7:9,m=1/3,nealmon))
u7.2_pca= midas_u(unrate[3:506]~ mls(pce[3:506],7:9, m = 1,nealmon) +mls(grr[1:168], 7:9, m=1/3,nealmon)+mls(hours[3:506],7:9,m=1,nealmon)+mls(cpi[3:506],7:9,m=1,nealmon)+mls(indpro[3:506],7:9,m=1,nealmon)+mls(ff[3:506],7:9,m=1,nealmon)+mls(tbond[3:506],7:9,m=1,nealmon)+mls(sp500[3:506],7:9,m=1,nealmon)+mls(loginv[1:168],7:9,m=1/3,nealmon)+mls(gov[1:168],7:9,m=1/3,nealmon))

u8.0_pca= midas_u(unrate~ mls(pce,8:10, m = 1,nealmon) +mls(grr, 8:10, m=1/3,nealmon)+mls(hours,8:10,m=1,nealmon)+mls(cpi,8:10,m=1,nealmon)+mls(indpro,8:10,m=1,nealmon)+mls(ff,8:10,m=1,nealmon)+mls(tbond,8:10,m=1,nealmon)+mls(sp500,8:10,m=1,nealmon)+mls(loginv,8:10,m=1/3,nealmon)+mls(gov,8:10,m=1/3,nealmon))
u8.1_pca= midas_u(unrate[2:505] ~ mls(pce[2:505],8:10, m = 1,nealmon) +mls(grr[1:168], 8:10, m=1/3,nealmon)+mls(hours[2:505],8:10,m=1,nealmon)+mls(cpi[2:505],8:10,m=1,nealmon)+mls(indpro[2:505],8:10,m=1,nealmon)+mls(ff[2:505],8:10,m=1,nealmon)+mls(tbond[2:505],8:10,m=1,nealmon)+mls(sp500[2:505],8:10,m=1,nealmon)+mls(loginv[1:168],8:10,m=1/3,nealmon)+mls(gov[1:168],8:10,m=1/3,nealmon))
u8.2_pca= midas_u(unrate[3:506] ~ mls(pce[3:506],8:10, m = 1,nealmon) +mls(grr[1:168], 8:10, m=1/3,nealmon)+mls(hours[3:506],8:10,m=1,nealmon)+mls(cpi[3:506],8:10,m=1,nealmon)+mls(indpro[3:506],8:10,m=1,nealmon)+mls(ff[3:506],8:10,m=1,nealmon)+mls(tbond[3:506],8:10,m=1,nealmon)+mls(sp500[3:506],8:10,m=1,nealmon)+mls(loginv[1:168],8:10,m=1/3,nealmon)+mls(gov[1:168],8:10,m=1/3,nealmon))


#almon_model2.1_pca= midas_r(grr ~ mls(pce, 1:3, m = 3,nealmon),start=list(pce=rep(0,3)))
#forecast horizon
168*3
s=117
e=169

almon_model2.1_pca_forecast<-forecast(almon_model2.0_pca,list(gov=gov[(s+1):e],unrate=unrate[(s*3+1):(e*3)],cpi=cpi[(s*3+1):(e*3)],
                                                              indpro=indpro[(s*3+1):(e*3)],loginv=loginv[(s+1):e],hours=hours[(s*3+1):(e*3)],ff=ff[(s*3+1):(e*3)],tbond=tbond[(s*3+1):(e*3)],sp500=sp500[(s*3+1):(e*3)],
                                                              pce=pce[(s*3+1):(e*3)]),method = 'static')

unratermse<-data.frame(matrix(0,3,8))
length(u8.2_pca$fitted.values)

unratermse[1,1]<-sqrt(mean((u1.0_pca$fitted.values[340:498]-unrate[349:507])^2))
unratermse[2,1]<-sqrt(mean((u1.1_pca$fitted.values[337:495]-unrate[348:506])^2))
unratermse[3,1]<-sqrt(mean((u1.2_pca$fitted.values[337:495]-unrate[348:506])^2))
unratermse[1,2]<-sqrt(mean((u2.0_pca$fitted.values[337:495]-unrate[349:507])^2))
unratermse[2,2]<-sqrt(mean((u2.1_pca$fitted.values[334:492]-unrate[348:506])^2))
unratermse[3,2]<-sqrt(mean((u2.2_pca$fitted.values[334:492]-unrate[348:506])^2))
unratermse[1,3]<-sqrt(mean((u3.0_pca$fitted.values[334:492]-unrate[349:507])^2))
unratermse[2,3]<-sqrt(mean((u3.1_pca$fitted.values[331:489]-unrate[348:506])^2))
unratermse[3,3]<-sqrt(mean((u3.2_pca$fitted.values[331:489]-unrate[348:506])^2))
unratermse[1,4]<-sqrt(mean((u4.0_pca$fitted.values[331:489]-unrate[349:507])^2))
unratermse[2,4]<-sqrt(mean((u4.1_pca$fitted.values[328:486]-unrate[348:506])^2))
unratermse[3,4]<-sqrt(mean((u4.2_pca$fitted.values[328:486]-unrate[348:506])^2))
unratermse[1,5]<-sqrt(mean((u5.0_pca$fitted.values[328:486]-unrate[349:507])^2))
unratermse[2,5]<-sqrt(mean((u5.1_pca$fitted.values[325:483]-unrate[348:506])^2))
unratermse[3,5]<-sqrt(mean((u5.2_pca$fitted.values[325:483]-unrate[348:506])^2))
unratermse[1,6]<-sqrt(mean((u6.0_pca$fitted.values[325:483]-unrate[349:507])^2))
unratermse[2,6]<-sqrt(mean((u6.1_pca$fitted.values[322:480]-unrate[348:506])^2))
unratermse[3,6]<-sqrt(mean((u6.2_pca$fitted.values[322:480]-unrate[348:506])^2))
unratermse[1,7]<-sqrt(mean((u7.0_pca$fitted.values[322:480]-unrate[349:507])^2))
unratermse[2,7]<-sqrt(mean((u7.1_pca$fitted.values[319:477]-unrate[348:506])^2))
unratermse[3,7]<-sqrt(mean((u7.2_pca$fitted.values[319:477]-unrate[348:506])^2))
unratermse[1,8]<-sqrt(mean((u8.0_pca$fitted.values[319:477]-unrate[349:507])^2))
unratermse[2,8]<-sqrt(mean((u8.1_pca$fitted.values[316:474]-unrate[348:506])^2))
unratermse[3,8]<-sqrt(mean((u8.2_pca$fitted.values[316:474]-unrate[348:506])^2))
grrrmse[,8]
unratermse
##cpi
c1.0_pca= midas_u(cpi ~ mls(pce, 1:3, m = 1,nealmon) +mls(grr, 1:3, m=1/3,nealmon)+mls(hours,1:3,m=1,nealmon)+mls(unrate,1:3,m=1,nealmon)+mls(indpro,1:3,m=1,nealmon)+mls(ff,1:3,m=1,nealmon)+mls(tbond,1:3,m=1,nealmon)+mls(sp500,1:3,m=1,nealmon)+mls(loginv,1:3,m=1/3,nealmon)+mls(gov,1:3,m=1/3,nealmon))
c1.1_pca= midas_u(cpi[2:505] ~ mls(pce[2:505], 1:3, m = 1,nealmon) +mls(grr[1:168], 1:3, m=1/3,nealmon)+mls(hours[2:505],1:3,m=1,nealmon)+mls(unrate[2:505],1:3,m=1,nealmon)+mls(indpro[2:505],1:3,m=1,nealmon)+mls(ff[2:505],1:3,m=1,nealmon)+mls(tbond[2:505],1:3,m=1,nealmon)+mls(sp500[2:505],1:3,m=1,nealmon)+mls(loginv[1:168],1:3,m=1/3,nealmon)+mls(gov[1:168],1:3,m=1/3,nealmon))
c1.2_pca= midas_u(cpi[3:506] ~ mls(pce[3:506], 1:3, m = 1,nealmon) +mls(grr[1:168], 1:3, m=1/3,nealmon)+mls(hours[3:506],1:3,m=1,nealmon)+mls(unrate[3:506],1:3,m=1,nealmon)+mls(indpro[3:506],1:3,m=1,nealmon)+mls(ff[3:506],1:3,m=1,nealmon)+mls(tbond[3:506],1:3,m=1,nealmon)+mls(sp500[3:506],1:3,m=1,nealmon)+mls(loginv[1:168],1:3,m=1/3,nealmon)+mls(gov[1:168],1:3,m=1/3,nealmon))
c2.0_pca= midas_u(cpi~ mls(pce, 2:4, m =1,nealmon) +mls(grr, 2:4, m=1/3,nealmon)+mls(hours,2:4,m=1,nealmon)+mls(unrate,2:4,m=1,nealmon)+mls(indpro,2:4,m=1,nealmon)+mls(ff,2:4,m=1,nealmon)+mls(tbond,2:4,m=1,nealmon)+mls(sp500,2:4,m=1,nealmon)+mls(loginv,2:4,m=1/3,nealmon)+mls(gov,2:4,m=1/3,nealmon))
c2.1_pca= midas_u(cpi[2:505] ~ mls(pce[2:505], 2:4, m = 1,nealmon) +mls(grr[1:168], 2:4, m=1/3,nealmon)+mls(hours[2:505],2:4,m=1,nealmon)+mls(unrate[2:505],2:4,m=1,nealmon)+mls(indpro[2:505],2:4,m=1,nealmon)+mls(ff[2:505],2:4,m=1,nealmon)+mls(tbond[2:505],2:4,m=1,nealmon)+mls(sp500[2:505],2:4,m=1,nealmon)+mls(loginv[1:168],2:4,m=1/3,nealmon)+mls(gov[1:168],2:4,m=1/3,nealmon))
c2.2_pca= midas_u(cpi[3:506] ~ mls(pce[3:506], 2:4, m = 1,nealmon) +mls(grr[1:168], 2:4, m=1/3,nealmon)+mls(hours[3:506],2:4,m=1,nealmon)+mls(unrate[3:506],2:4,m=1,nealmon)+mls(indpro[3:506],2:4,m=1,nealmon)+mls(ff[3:506],2:4,m=1,nealmon)+mls(tbond[3:506],2:4,m=1,nealmon)+mls(sp500[3:506],2:4,m=1,nealmon)+mls(loginv[1:168],2:4,m=1/3,nealmon)+mls(gov[1:168],2:4,m=1/3,nealmon))
c3.0_pca= midas_u(cpi~ mls(pce, 3:5, m = 1,nealmon) +mls(grr, 3:5, m=1/3,nealmon)+mls(hours,3:5,m=1,nealmon)+mls(unrate,3:5,m=1,nealmon)+mls(indpro,3:5,m=1,nealmon)+mls(ff,3:5,m=1,nealmon)+mls(tbond,3:5,m=1,nealmon)+mls(sp500,3:5,m=1,nealmon)+mls(loginv,3:5,m=1/3,nealmon)+mls(gov,3:5,m=1/3,nealmon))
c3.1_pca= midas_u(cpi[2:505] ~ mls(pce[2:505], 3:5, m = 1,nealmon) +mls(grr[1:168], 3:5, m=1/3,nealmon)+mls(hours[2:505],3:5,m=1,nealmon)+mls(unrate[2:505],3:5,m=1,nealmon)+mls(indpro[2:505],3:5,m=1,nealmon)+mls(ff[2:505],3:5,m=1,nealmon)+mls(tbond[2:505],3:5,m=1,nealmon)+mls(sp500[2:505],3:5,m=1,nealmon)+mls(loginv[1:168],3:5,m=1/3,nealmon)+mls(gov[1:168],3:5,m=1/3,nealmon))
c3.2_pca= midas_u(cpi[3:506] ~ mls(pce[3:506], 3:5, m = 1,nealmon) +mls(grr[1:168], 3:5, m=1/3,nealmon)+mls(hours[3:506],3:5,m=1,nealmon)+mls(unrate[3:506],3:5,m=1,nealmon)+mls(indpro[3:506],3:5,m=1,nealmon)+mls(ff[3:506],3:5,m=1,nealmon)+mls(tbond[3:506],3:5,m=1,nealmon)+mls(sp500[3:506],3:5,m=1,nealmon)+mls(loginv[1:168],3:5,m=1/3,nealmon)+mls(gov[1:168],3:5,m=1/3,nealmon))
c4.0_pca= midas_u(cpi~ mls(pce, 4:6, m = 1,nealmon) +mls(grr, 4:6, m=1/3,nealmon)+mls(hours,4:6,m=1,nealmon)+mls(unrate,4:6,m=1,nealmon)+mls(indpro,4:6,m=1,nealmon)+mls(ff,4:6,m=1,nealmon)+mls(tbond,4:6,m=1,nealmon)+mls(sp500,4:6,m=1,nealmon)+mls(loginv,4:6,m=1/3,nealmon)+mls(gov,4:6,m=1/3,nealmon))
c4.1_pca= midas_u(cpi[2:505] ~ mls(pce[2:505], 4:6, m =1,nealmon) +mls(grr[1:168], 4:6, m=1/3,nealmon)+mls(hours[2:505],4:6,m=1,nealmon)+mls(unrate[2:505],4:6,m=1,nealmon)+mls(indpro[2:505],4:6,m=1,nealmon)+mls(ff[2:505],4:6,m=1,nealmon)+mls(tbond[2:505],4:6,m=1,nealmon)+mls(sp500[2:505],4:6,m=1,nealmon)+mls(loginv[1:168],4:6,m=1/3,nealmon)+mls(gov[1:168],4:6,m=1/3,nealmon))
c4.2_pca= midas_u(cpi[3:506]~ mls(pce[3:506], 4:6, m =1,nealmon) +mls(grr[1:168], 4:6, m=1/3,nealmon)+mls(hours[3:506],4:6,m=1,nealmon)+mls(unrate[3:506],4:6,m=1,nealmon)+mls(indpro[3:506],4:6,m=1,nealmon)+mls(ff[3:506],4:6,m=1,nealmon)+mls(tbond[3:506],4:6,m=1,nealmon)+mls(sp500[3:506],4:6,m=1,nealmon)+mls(loginv[1:168],4:6,m=1/3,nealmon)+mls(gov[1:168],4:6,m=1/3,nealmon))
c5.0_pca= midas_u(cpi~ mls(pce, 5:7, m =1,nealmon) +mls(grr, 5:7, m=1/3,nealmon)+mls(hours,5:7,m=1,nealmon)+mls(unrate,5:7,m=1,nealmon)+mls(indpro,5:7,m=1,nealmon)+mls(ff,5:7,m=1,nealmon)+mls(tbond,5:7,m=1,nealmon)+mls(sp500,5:7,m=1,nealmon)+mls(loginv,5:7,m=1/3,nealmon)+mls(gov,5:7,m=1/3,nealmon))
c5.1_pca= midas_u(cpi[2:505] ~ mls(pce[2:505], 5:7, m =1,nealmon) +mls(grr[1:168], 5:7, m=1/3,nealmon)+mls(hours[2:505],5:7,m=1,nealmon)+mls(unrate[2:505],5:7,m=1,nealmon)+mls(indpro[2:505],5:7,m=1,nealmon)+mls(ff[2:505],5:7,m=1,nealmon)+mls(tbond[2:505],5:7,m=1,nealmon)+mls(sp500[2:505],5:7,m=1,nealmon)+mls(loginv[1:168],5:7,m=1/3,nealmon)+mls(gov[1:168],5:7,m=1/3,nealmon))
c5.2_pca= midas_u(cpi[3:506] ~ mls(pce[3:506], 5:7, m =1,nealmon) +mls(grr[1:168], 5:7, m=1/3,nealmon)+mls(hours[3:506],5:7,m=1,nealmon)+mls(unrate[3:506],5:7,m=1,nealmon)+mls(indpro[3:506],5:7,m=1,nealmon)+mls(ff[3:506],5:7,m=1,nealmon)+mls(tbond[3:506],5:7,m=1,nealmon)+mls(sp500[3:506],5:7,m=1,nealmon)+mls(loginv[1:168],5:7,m=1/3,nealmon)+mls(gov[1:168],5:7,m=1/3,nealmon))
c6.0_pca= midas_u(cpi~ mls(pce, 6:8, m = 1,nealmon) +mls(grr, 6:8, m=1/3,nealmon)+mls(hours,6:8,m=1,nealmon)+mls(unrate,6:8,m=1,nealmon)+mls(indpro,6:8,m=1,nealmon)+mls(ff,6:8,m=1,nealmon)+mls(tbond,6:8,m=1,nealmon)+mls(sp500,6:8,m=1,nealmon)+mls(loginv,6:8,m=1/3,nealmon)+mls(gov,6:8,m=1/3,nealmon))
c6.1_pca= midas_u(cpi[2:505] ~ mls(pce[2:505], 6:8, m = 1,nealmon) +mls(grr[1:168], 6:8, m=1/3,nealmon)+mls(hours[2:505],6:8,m=1,nealmon)+mls(unrate[2:505],6:8,m=1,nealmon)+mls(indpro[2:505],6:8,m=1,nealmon)+mls(ff[2:505],6:8,m=1,nealmon)+mls(tbond[2:505],6:8,m=1,nealmon)+mls(sp500[2:505],6:8,m=1,nealmon)+mls(loginv[1:168],6:8,m=1/3,nealmon)+mls(gov[1:168],6:8,m=1/3,nealmon))
c6.2_pca= midas_u(cpi[3:506] ~ mls(pce[3:506], 6:8, m = 1,nealmon) +mls(grr[1:168],6:8, m=1/3,nealmon)+mls(hours[3:506],6:8,m=1,nealmon)+mls(unrate[3:506],6:8,m=1,nealmon)+mls(indpro[3:506],6:8,m=1,nealmon)+mls(ff[3:506],6:8,m=1,nealmon)+mls(tbond[3:506],6:8,m=1,nealmon)+mls(sp500[3:506],6:8,m=1,nealmon)+mls(loginv[1:168],6:8,m=1/3,nealmon)+mls(gov[1:168],6:8,m=1/3,nealmon))
c7.0_pca= midas_u(cpi~ mls(pce,7:9, m = 1,nealmon) +mls(grr, 7:9, m=1/3,nealmon)+mls(hours,7:9,m=1,nealmon)+mls(unrate,7:9,m=1,nealmon)+mls(indpro,7:9,m=1,nealmon)+mls(ff,7:9,m=1,nealmon)+mls(tbond,7:9,m=1,nealmon)+mls(sp500,7:9,m=1,nealmon)+mls(loginv,7:9,m=1/3,nealmon)+mls(gov,7:9,m=1/3,nealmon))
c7.1_pca= midas_u(cpi[2:505] ~ mls(pce[2:505],7:9, m = 1,nealmon) +mls(grr[1:168],7:9, m=1/3,nealmon)+mls(hours[2:505],7:9,m=1,nealmon)+mls(unrate[2:505],7:9,m=1,nealmon)+mls(indpro[2:505],7:9,m=1,nealmon)+mls(ff[2:505],7:9,m=1,nealmon)+mls(tbond[2:505],7:9,m=1,nealmon)+mls(sp500[2:505],7:9,m=1,nealmon)+mls(loginv[1:168],7:9,m=1/3,nealmon)+mls(gov[1:168],7:9,m=1/3,nealmon))
c7.2_pca= midas_u(cpi[3:506]~ mls(pce[3:506],7:9, m = 1,nealmon) +mls(grr[1:168], 7:9, m=1/3,nealmon)+mls(hours[3:506],7:9,m=1,nealmon)+mls(unrate[3:506],7:9,m=1,nealmon)+mls(indpro[3:506],7:9,m=1,nealmon)+mls(ff[3:506],7:9,m=1,nealmon)+mls(tbond[3:506],7:9,m=1,nealmon)+mls(sp500[3:506],7:9,m=1,nealmon)+mls(loginv[1:168],7:9,m=1/3,nealmon)+mls(gov[1:168],7:9,m=1/3,nealmon))

c8.0_pca= midas_u(cpi~ mls(pce,8:10, m = 1,nealmon) +mls(grr, 8:10, m=1/3,nealmon)+mls(hours,8:10,m=1,nealmon)+mls(unrate,8:10,m=1,nealmon)+mls(indpro,8:10,m=1,nealmon)+mls(ff,8:10,m=1,nealmon)+mls(tbond,8:10,m=1,nealmon)+mls(sp500,8:10,m=1,nealmon)+mls(loginv,8:10,m=1/3,nealmon)+mls(gov,8:10,m=1/3,nealmon))
c8.1_pca= midas_u(cpi[2:505] ~ mls(pce[2:505],8:10, m = 1,nealmon) +mls(grr[1:168], 8:10, m=1/3,nealmon)+mls(hours[2:505],8:10,m=1,nealmon)+mls(unrate[2:505],8:10,m=1,nealmon)+mls(indpro[2:505],8:10,m=1,nealmon)+mls(ff[2:505],8:10,m=1,nealmon)+mls(tbond[2:505],8:10,m=1,nealmon)+mls(sp500[2:505],8:10,m=1,nealmon)+mls(loginv[1:168],8:10,m=1/3,nealmon)+mls(gov[1:168],8:10,m=1/3,nealmon))
c8.2_pca= midas_u(cpi[3:506] ~ mls(pce[3:506],8:10, m = 1,nealmon) +mls(grr[1:168], 8:10, m=1/3,nealmon)+mls(hours[3:506],8:10,m=1,nealmon)+mls(unrate[3:506],8:10,m=1,nealmon)+mls(indpro[3:506],8:10,m=1,nealmon)+mls(ff[3:506],8:10,m=1,nealmon)+mls(tbond[3:506],8:10,m=1,nealmon)+mls(sp500[3:506],8:10,m=1,nealmon)+mls(loginv[1:168],8:10,m=1/3,nealmon)+mls(gov[1:168],8:10,m=1/3,nealmon))

cpirmse<-data.frame(matrix(0,3,8))
length(u8.2_pca$fitted.values)

cpirmse[1,1]<-sqrt(mean((c1.0_pca$fitted.values[340:498]-cpi[349:507])^2))
cpirmse[2,1]<-sqrt(mean((c1.1_pca$fitted.values[337:495]-cpi[348:506])^2))
cpirmse[3,1]<-sqrt(mean((c1.2_pca$fitted.values[337:495]-cpi[348:506])^2))
cpirmse[1,2]<-sqrt(mean((c2.0_pca$fitted.values[337:495]-cpi[349:507])^2))
cpirmse[2,2]<-sqrt(mean((c2.1_pca$fitted.values[334:492]-cpi[348:506])^2))
cpirmse[3,2]<-sqrt(mean((c2.2_pca$fitted.values[334:492]-cpi[348:506])^2))
cpirmse[1,3]<-sqrt(mean((c3.0_pca$fitted.values[334:492]-cpi[349:507])^2))
cpirmse[2,3]<-sqrt(mean((c3.1_pca$fitted.values[331:489]-cpi[348:506])^2))
cpirmse[3,3]<-sqrt(mean((c3.2_pca$fitted.values[331:489]-cpi[348:506])^2))
cpirmse[1,4]<-sqrt(mean((c4.0_pca$fitted.values[331:489]-cpi[349:507])^2))
cpirmse[2,4]<-sqrt(mean((c4.1_pca$fitted.values[328:486]-cpi[348:506])^2))
cpirmse[3,4]<-sqrt(mean((c4.2_pca$fitted.values[328:486]-cpi[348:506])^2))
cpirmse[1,5]<-sqrt(mean((c5.0_pca$fitted.values[328:486]-cpi[349:507])^2))
cpirmse[2,5]<-sqrt(mean((c5.1_pca$fitted.values[325:483]-cpi[348:506])^2))
cpirmse[3,5]<-sqrt(mean((c5.2_pca$fitted.values[325:483]-cpi[348:506])^2))
cpirmse[1,6]<-sqrt(mean((c6.0_pca$fitted.values[325:483]-cpi[349:507])^2))
cpirmse[2,6]<-sqrt(mean((c6.1_pca$fitted.values[322:480]-cpi[348:506])^2))
cpirmse[3,6]<-sqrt(mean((c6.2_pca$fitted.values[322:480]-cpi[348:506])^2))
cpirmse[1,7]<-sqrt(mean((c7.0_pca$fitted.values[322:480]-cpi[349:507])^2))
cpirmse[2,7]<-sqrt(mean((c7.1_pca$fitted.values[319:477]-cpi[348:506])^2))
cpirmse[3,7]<-sqrt(mean((c7.2_pca$fitted.values[319:477]-cpi[348:506])^2))
cpirmse[1,8]<-sqrt(mean((c8.0_pca$fitted.values[319:477]-cpi[349:507])^2))
cpirmse[2,8]<-sqrt(mean((c8.1_pca$fitted.values[316:474]-cpi[348:506])^2))
cpirmse[3,8]<-sqrt(mean((c8.2_pca$fitted.values[316:474]-cpi[348:506])^2))
cpirmse

##ff 
f1.0_pca= midas_u(ff ~ mls(pce, 1:3, m = 1,nealmon) +mls(grr, 1:3, m=1/3,nealmon)+mls(hours,1:3,m=1,nealmon)+mls(unrate,1:3,m=1,nealmon)+mls(indpro,1:3,m=1,nealmon)+mls(cpi,1:3,m=1,nealmon)+mls(tbond,1:3,m=1,nealmon)+mls(sp500,1:3,m=1,nealmon)+mls(loginv,1:3,m=1/3,nealmon)+mls(gov,1:3,m=1/3,nealmon))
f1.1_pca= midas_u(ff[2:505] ~ mls(pce[2:505], 1:3, m = 1,nealmon) +mls(grr[1:168], 1:3, m=1/3,nealmon)+mls(hours[2:505],1:3,m=1,nealmon)+mls(unrate[2:505],1:3,m=1,nealmon)+mls(indpro[2:505],1:3,m=1,nealmon)+mls(cpi[2:505],1:3,m=1,nealmon)+mls(tbond[2:505],1:3,m=1,nealmon)+mls(sp500[2:505],1:3,m=1,nealmon)+mls(loginv[1:168],1:3,m=1/3,nealmon)+mls(gov[1:168],1:3,m=1/3,nealmon))
f1.2_pca= midas_u(ff[3:506] ~ mls(pce[3:506], 1:3, m = 1,nealmon) +mls(grr[1:168], 1:3, m=1/3,nealmon)+mls(hours[3:506],1:3,m=1,nealmon)+mls(unrate[3:506],1:3,m=1,nealmon)+mls(indpro[3:506],1:3,m=1,nealmon)+mls(cpi[3:506],1:3,m=1,nealmon)+mls(tbond[3:506],1:3,m=1,nealmon)+mls(sp500[3:506],1:3,m=1,nealmon)+mls(loginv[1:168],1:3,m=1/3,nealmon)+mls(gov[1:168],1:3,m=1/3,nealmon))
f2.0_pca= midas_u(ff~ mls(pce, 2:4, m =1,nealmon) +mls(grr, 2:4, m=1/3,nealmon)+mls(hours,2:4,m=1,nealmon)+mls(unrate,2:4,m=1,nealmon)+mls(indpro,2:4,m=1,nealmon)+mls(cpi,2:4,m=1,nealmon)+mls(tbond,2:4,m=1,nealmon)+mls(sp500,2:4,m=1,nealmon)+mls(loginv,2:4,m=1/3,nealmon)+mls(gov,2:4,m=1/3,nealmon))
f2.1_pca= midas_u(ff[2:505] ~ mls(pce[2:505], 2:4, m = 1,nealmon) +mls(grr[1:168], 2:4, m=1/3,nealmon)+mls(hours[2:505],2:4,m=1,nealmon)+mls(unrate[2:505],2:4,m=1,nealmon)+mls(indpro[2:505],2:4,m=1,nealmon)+mls(cpi[2:505],2:4,m=1,nealmon)+mls(tbond[2:505],2:4,m=1,nealmon)+mls(sp500[2:505],2:4,m=1,nealmon)+mls(loginv[1:168],2:4,m=1/3,nealmon)+mls(gov[1:168],2:4,m=1/3,nealmon))
f2.2_pca= midas_u(ff[3:506] ~ mls(pce[3:506], 2:4, m = 1,nealmon) +mls(grr[1:168], 2:4, m=1/3,nealmon)+mls(hours[3:506],2:4,m=1,nealmon)+mls(unrate[3:506],2:4,m=1,nealmon)+mls(indpro[3:506],2:4,m=1,nealmon)+mls(cpi[3:506],2:4,m=1,nealmon)+mls(tbond[3:506],2:4,m=1,nealmon)+mls(sp500[3:506],2:4,m=1,nealmon)+mls(loginv[1:168],2:4,m=1/3,nealmon)+mls(gov[1:168],2:4,m=1/3,nealmon))
f3.0_pca= midas_u(ff~ mls(pce, 3:5, m = 1,nealmon) +mls(grr, 3:5, m=1/3,nealmon)+mls(hours,3:5,m=1,nealmon)+mls(unrate,3:5,m=1,nealmon)+mls(indpro,3:5,m=1,nealmon)+mls(cpi,3:5,m=1,nealmon)+mls(tbond,3:5,m=1,nealmon)+mls(sp500,3:5,m=1,nealmon)+mls(loginv,3:5,m=1/3,nealmon)+mls(gov,3:5,m=1/3,nealmon))
f3.1_pca= midas_u(ff[2:505] ~ mls(pce[2:505], 3:5, m = 1,nealmon) +mls(grr[1:168], 3:5, m=1/3,nealmon)+mls(hours[2:505],3:5,m=1,nealmon)+mls(unrate[2:505],3:5,m=1,nealmon)+mls(indpro[2:505],3:5,m=1,nealmon)+mls(cpi[2:505],3:5,m=1,nealmon)+mls(tbond[2:505],3:5,m=1,nealmon)+mls(sp500[2:505],3:5,m=1,nealmon)+mls(loginv[1:168],3:5,m=1/3,nealmon)+mls(gov[1:168],3:5,m=1/3,nealmon))
f3.2_pca= midas_u(ff[3:506] ~ mls(pce[3:506], 3:5, m = 1,nealmon) +mls(grr[1:168], 3:5, m=1/3,nealmon)+mls(hours[3:506],3:5,m=1,nealmon)+mls(unrate[3:506],3:5,m=1,nealmon)+mls(indpro[3:506],3:5,m=1,nealmon)+mls(cpi[3:506],3:5,m=1,nealmon)+mls(tbond[3:506],3:5,m=1,nealmon)+mls(sp500[3:506],3:5,m=1,nealmon)+mls(loginv[1:168],3:5,m=1/3,nealmon)+mls(gov[1:168],3:5,m=1/3,nealmon))
f4.0_pca= midas_u(ff~ mls(pce, 4:6, m = 1,nealmon) +mls(grr, 4:6, m=1/3,nealmon)+mls(hours,4:6,m=1,nealmon)+mls(unrate,4:6,m=1,nealmon)+mls(indpro,4:6,m=1,nealmon)+mls(cpi,4:6,m=1,nealmon)+mls(tbond,4:6,m=1,nealmon)+mls(sp500,4:6,m=1,nealmon)+mls(loginv,4:6,m=1/3,nealmon)+mls(gov,4:6,m=1/3,nealmon))
f4.1_pca= midas_u(ff[2:505] ~ mls(pce[2:505], 4:6, m =1,nealmon) +mls(grr[1:168], 4:6, m=1/3,nealmon)+mls(hours[2:505],4:6,m=1,nealmon)+mls(unrate[2:505],4:6,m=1,nealmon)+mls(indpro[2:505],4:6,m=1,nealmon)+mls(cpi[2:505],4:6,m=1,nealmon)+mls(tbond[2:505],4:6,m=1,nealmon)+mls(sp500[2:505],4:6,m=1,nealmon)+mls(loginv[1:168],4:6,m=1/3,nealmon)+mls(gov[1:168],4:6,m=1/3,nealmon))
f4.2_pca= midas_u(ff[3:506]~ mls(pce[3:506], 4:6, m =1,nealmon) +mls(grr[1:168], 4:6, m=1/3,nealmon)+mls(hours[3:506],4:6,m=1,nealmon)+mls(unrate[3:506],4:6,m=1,nealmon)+mls(indpro[3:506],4:6,m=1,nealmon)+mls(cpi[3:506],4:6,m=1,nealmon)+mls(tbond[3:506],4:6,m=1,nealmon)+mls(sp500[3:506],4:6,m=1,nealmon)+mls(loginv[1:168],4:6,m=1/3,nealmon)+mls(gov[1:168],4:6,m=1/3,nealmon))
f5.0_pca= midas_u(ff~ mls(pce, 5:7, m =1,nealmon) +mls(grr, 5:7, m=1/3,nealmon)+mls(hours,5:7,m=1,nealmon)+mls(unrate,5:7,m=1,nealmon)+mls(indpro,5:7,m=1,nealmon)+mls(cpi,5:7,m=1,nealmon)+mls(tbond,5:7,m=1,nealmon)+mls(sp500,5:7,m=1,nealmon)+mls(loginv,5:7,m=1/3,nealmon)+mls(gov,5:7,m=1/3,nealmon))
f5.1_pca= midas_u(ff[2:505] ~ mls(pce[2:505], 5:7, m =1,nealmon) +mls(grr[1:168], 5:7, m=1/3,nealmon)+mls(hours[2:505],5:7,m=1,nealmon)+mls(unrate[2:505],5:7,m=1,nealmon)+mls(indpro[2:505],5:7,m=1,nealmon)+mls(cpi[2:505],5:7,m=1,nealmon)+mls(tbond[2:505],5:7,m=1,nealmon)+mls(sp500[2:505],5:7,m=1,nealmon)+mls(loginv[1:168],5:7,m=1/3,nealmon)+mls(gov[1:168],5:7,m=1/3,nealmon))
f5.2_pca= midas_u(ff[3:506] ~ mls(pce[3:506], 5:7, m =1,nealmon) +mls(grr[1:168], 5:7, m=1/3,nealmon)+mls(hours[3:506],5:7,m=1,nealmon)+mls(unrate[3:506],5:7,m=1,nealmon)+mls(indpro[3:506],5:7,m=1,nealmon)+mls(cpi[3:506],5:7,m=1,nealmon)+mls(tbond[3:506],5:7,m=1,nealmon)+mls(sp500[3:506],5:7,m=1,nealmon)+mls(loginv[1:168],5:7,m=1/3,nealmon)+mls(gov[1:168],5:7,m=1/3,nealmon))
f6.0_pca= midas_u(ff~ mls(pce, 6:8, m = 1,nealmon) +mls(grr, 6:8, m=1/3,nealmon)+mls(hours,6:8,m=1,nealmon)+mls(unrate,6:8,m=1,nealmon)+mls(indpro,6:8,m=1,nealmon)+mls(cpi,6:8,m=1,nealmon)+mls(tbond,6:8,m=1,nealmon)+mls(sp500,6:8,m=1,nealmon)+mls(loginv,6:8,m=1/3,nealmon)+mls(gov,6:8,m=1/3,nealmon))
f6.1_pca= midas_u(ff[2:505] ~ mls(pce[2:505], 6:8, m = 1,nealmon) +mls(grr[1:168], 6:8, m=1/3,nealmon)+mls(hours[2:505],6:8,m=1,nealmon)+mls(unrate[2:505],6:8,m=1,nealmon)+mls(indpro[2:505],6:8,m=1,nealmon)+mls(cpi[2:505],6:8,m=1,nealmon)+mls(tbond[2:505],6:8,m=1,nealmon)+mls(sp500[2:505],6:8,m=1,nealmon)+mls(loginv[1:168],6:8,m=1/3,nealmon)+mls(gov[1:168],6:8,m=1/3,nealmon))
f6.2_pca= midas_u(ff[3:506] ~ mls(pce[3:506], 6:8, m = 1,nealmon) +mls(grr[1:168],6:8, m=1/3,nealmon)+mls(hours[3:506],6:8,m=1,nealmon)+mls(unrate[3:506],6:8,m=1,nealmon)+mls(indpro[3:506],6:8,m=1,nealmon)+mls(cpi[3:506],6:8,m=1,nealmon)+mls(tbond[3:506],6:8,m=1,nealmon)+mls(sp500[3:506],6:8,m=1,nealmon)+mls(loginv[1:168],6:8,m=1/3,nealmon)+mls(gov[1:168],6:8,m=1/3,nealmon))
f7.0_pca= midas_u(ff~ mls(pce,7:9, m = 1,nealmon) +mls(grr, 7:9, m=1/3,nealmon)+mls(hours,7:9,m=1,nealmon)+mls(unrate,7:9,m=1,nealmon)+mls(indpro,7:9,m=1,nealmon)+mls(cpi,7:9,m=1,nealmon)+mls(tbond,7:9,m=1,nealmon)+mls(sp500,7:9,m=1,nealmon)+mls(loginv,7:9,m=1/3,nealmon)+mls(gov,7:9,m=1/3,nealmon))
f7.1_pca= midas_u(ff[2:505] ~ mls(pce[2:505],7:9, m = 1,nealmon) +mls(grr[1:168],7:9, m=1/3,nealmon)+mls(hours[2:505],7:9,m=1,nealmon)+mls(unrate[2:505],7:9,m=1,nealmon)+mls(indpro[2:505],7:9,m=1,nealmon)+mls(cpi[2:505],7:9,m=1,nealmon)+mls(tbond[2:505],7:9,m=1,nealmon)+mls(sp500[2:505],7:9,m=1,nealmon)+mls(loginv[1:168],7:9,m=1/3,nealmon)+mls(gov[1:168],7:9,m=1/3,nealmon))
f7.2_pca= midas_u(ff[3:506]~ mls(pce[3:506],7:9, m = 1,nealmon) +mls(grr[1:168], 7:9, m=1/3,nealmon)+mls(hours[3:506],7:9,m=1,nealmon)+mls(unrate[3:506],7:9,m=1,nealmon)+mls(indpro[3:506],7:9,m=1,nealmon)+mls(cpi[3:506],7:9,m=1,nealmon)+mls(tbond[3:506],7:9,m=1,nealmon)+mls(sp500[3:506],7:9,m=1,nealmon)+mls(loginv[1:168],7:9,m=1/3,nealmon)+mls(gov[1:168],7:9,m=1/3,nealmon))

f8.0_pca= midas_u(ff~ mls(pce,8:10, m = 1,nealmon) +mls(grr, 8:10, m=1/3,nealmon)+mls(hours,8:10,m=1,nealmon)+mls(unrate,8:10,m=1,nealmon)+mls(indpro,8:10,m=1,nealmon)+mls(cpi,8:10,m=1,nealmon)+mls(tbond,8:10,m=1,nealmon)+mls(sp500,8:10,m=1,nealmon)+mls(loginv,8:10,m=1/3,nealmon)+mls(gov,8:10,m=1/3,nealmon))
f8.1_pca= midas_u(ff[2:505] ~ mls(pce[2:505],8:10, m = 1,nealmon) +mls(grr[1:168], 8:10, m=1/3,nealmon)+mls(hours[2:505],8:10,m=1,nealmon)+mls(unrate[2:505],8:10,m=1,nealmon)+mls(indpro[2:505],8:10,m=1,nealmon)+mls(cpi[2:505],8:10,m=1,nealmon)+mls(tbond[2:505],8:10,m=1,nealmon)+mls(sp500[2:505],8:10,m=1,nealmon)+mls(loginv[1:168],8:10,m=1/3,nealmon)+mls(gov[1:168],8:10,m=1/3,nealmon))
f8.2_pca= midas_u(ff[3:506] ~ mls(pce[3:506],8:10, m = 1,nealmon) +mls(grr[1:168], 8:10, m=1/3,nealmon)+mls(hours[3:506],8:10,m=1,nealmon)+mls(unrate[3:506],8:10,m=1,nealmon)+mls(indpro[3:506],8:10,m=1,nealmon)+mls(cpi[3:506],8:10,m=1,nealmon)+mls(tbond[3:506],8:10,m=1,nealmon)+mls(sp500[3:506],8:10,m=1,nealmon)+mls(loginv[1:168],8:10,m=1/3,nealmon)+mls(gov[1:168],8:10,m=1/3,nealmon))

ffrmse<-data.frame(matrix(0,3,8))
length(u8.2_pca$fitted.values)

ffrmse[1,1]<-sqrt(mean((f1.0_pca$fitted.values[340:498]-ff[349:507])^2))
ffrmse[2,1]<-sqrt(mean((f1.1_pca$fitted.values[337:495]-ff[348:506])^2))
ffrmse[3,1]<-sqrt(mean((f1.2_pca$fitted.values[337:495]-ff[348:506])^2))
ffrmse[1,2]<-sqrt(mean((f2.0_pca$fitted.values[337:495]-ff[349:507])^2))
ffrmse[2,2]<-sqrt(mean((f2.1_pca$fitted.values[334:492]-ff[348:506])^2))
ffrmse[3,2]<-sqrt(mean((f2.2_pca$fitted.values[334:492]-ff[348:506])^2))
ffrmse[1,3]<-sqrt(mean((f3.0_pca$fitted.values[334:492]-ff[349:507])^2))
ffrmse[2,3]<-sqrt(mean((f3.1_pca$fitted.values[331:489]-ff[348:506])^2))
ffrmse[3,3]<-sqrt(mean((f3.2_pca$fitted.values[331:489]-ff[348:506])^2))
ffrmse[1,4]<-sqrt(mean((f4.0_pca$fitted.values[331:489]-ff[349:507])^2))
ffrmse[2,4]<-sqrt(mean((f4.1_pca$fitted.values[328:486]-ff[348:506])^2))
ffrmse[3,4]<-sqrt(mean((f4.2_pca$fitted.values[328:486]-ff[348:506])^2))
ffrmse[1,5]<-sqrt(mean((f5.0_pca$fitted.values[328:486]-ff[349:507])^2))
ffrmse[2,5]<-sqrt(mean((f5.1_pca$fitted.values[325:483]-ff[348:506])^2))
ffrmse[3,5]<-sqrt(mean((f5.2_pca$fitted.values[325:483]-ff[348:506])^2))
ffrmse[1,6]<-sqrt(mean((f6.0_pca$fitted.values[325:483]-ff[349:507])^2))
ffrmse[2,6]<-sqrt(mean((f6.1_pca$fitted.values[322:480]-ff[348:506])^2))
ffrmse[3,6]<-sqrt(mean((f6.2_pca$fitted.values[322:480]-ff[348:506])^2))
ffrmse[1,7]<-sqrt(mean((f7.0_pca$fitted.values[322:480]-ff[349:507])^2))
ffrmse[2,7]<-sqrt(mean((f7.1_pca$fitted.values[319:477]-ff[348:506])^2))
ffrmse[3,7]<-sqrt(mean((f7.2_pca$fitted.values[319:477]-ff[348:506])^2))
ffrmse[1,8]<-sqrt(mean((f8.0_pca$fitted.values[319:477]-ff[349:507])^2))
ffrmse[2,8]<-sqrt(mean((f8.1_pca$fitted.values[316:474]-ff[348:506])^2))
ffrmse[3,8]<-sqrt(mean((f8.2_pca$fitted.values[316:474]-ff[348:506])^2))
ffrmse

