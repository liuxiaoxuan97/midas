library(midasr)
datam<-read.csv("C:/Users/lenovo/Desktop/currentm.csv")
datam[0:4,]
dataq<-read.csv("C:/Users/lenovo/Desktop/currentq.csv")
varm<-names(datam)
varq<-names(dataq)
intersect(varq,varm)
length(names(datam))
length(names(dataq))
length(intersect(varm,varq))
library(tidyverse)
data1<-datam

mls(datam[,1],0:7,6)[0:3,]
fn.x <- nealmon(p=c(1,-0.5),d=8)
#先删掉有缺失值的列 所有的
na_flag <- apply(is.na(dataq), 2, sum)
na_flag
dataq2 <- dataq[,which(na_flag == 0)]
na_flag <- apply(is.na(datam), 2, sum)
datam2 <- datam[,which(na_flag == 0)]

varm<-names(datam2)
varq<-names(dataq2)
intername<-intersect(varq,varm)
dataq22<-dataq2[,-which(names(dataq2)%in%intername[2:89])]
#model1 预测失业率
n<-length(dataq2)
#monthly variable
x<-rnorm(4*22)
class(x)
x1<-as.numeric(datam2[1:720,c('INDPRO')])
is.numeric(x1)
is.numeric(y1)
0:4
class(x1)
#quarterly variable
library(tidyverse)
x2<-as.numeric(dataq2[,c('IPFINAL')])
class(x2)
x3<-list(datam2%>%select(-UNRATE))
colnames(dataq2)

y1<-as.numeric(dataq2[,c('UNRATE')])
y2<-as.numeric(dataq2[,c('FEDFUNDS')])
y2<-as.numeric(datam2[,c('UNRATE')])
trend<-c(1:240)
dim(datam2)[2]
passtr(3)
for (i in 1:dim(datam2)[2]) eval(parse(text=paste(paste('a',i,sep=''), '= datam2[,',i,']$value')))
a1
c<-()
for(i in 1:117){
  assign(paste('a',i,sep=''),fmls(datam2[,i][1:720],3,3,nealmon))
  assign(paste('b',i,sep=''),datam2[,i][661:720])
}
for(i in 1:117){
  c[i]<-paste('+a',i,sep='')
}
fmls(x2[1:720],3,3,nealmon)+fmls(x1[1:720],3,3,nealmon)
## Exponential Almon polynomial constraint-consistent coefficients
fn.x1 <- nealmon(p=c(1,-0.5,-0.1),d=3)
fn.x2<- nealmon(p=c(2,0.5,-0.1),d=3)
/////////////////////
## Simulated low-frequency series (e.g. yearly)
trend<-c(1:720)
mod4 <- midas_u(y1[1:240] ~ fmls(pca_m1[1:720],3,3,nealmon)+fmls(pca_m2[1:720],3,3,nealmon)+fmls(pca_m3[1:720],3,3,nealmon)+fmls(pca_m4[1:720],3,3,nealmon)+fmls(pca_m5[1:720],3,3,nealmon))
mod5 <- midas_u(y1[1:240] ~ fmls(pca_m1[1:720],3,3,nealmon)+fmls(pca_m2[1:720],3,3,nealmon)+fmls(pca_m3[1:720],3,3,nealmon)+fmls(pca_m4[1:720],3,3,nealmon)+fmls(pca_m5[1:720],3,3,nealmon)+fmls(pca_q1[1:240],1,1,nealmon)+fmls(pca_q2[1:240],1,1,nealmon)+fmls(pca_q3[1:240],1,1,nealmon))

mod6 <- midas_u(y1[1:240] ~ fmls(pca_m1[1:720],3,3,nealmon)+fmls(pca_m2[1:720],3,3,nealmon)+fmls(pca_m3[1:720],3,3,nealmon)+fmls(pca_q1[1:240],1,1,nealmon)+fmls(pca_q2[1:240],1,1,nealmon)+fmls(pca_q3[1:240],1,1,nealmon))
pca_m1 <- pca_m[,1]
pca_m2 <- pca_m[,2]
pca_m3 <- pca_m[,3]

for(i in 1:117){
  list1<-list(assign(paste))
}
mod4$fitted.values[221:239]
mod1 <- midas_u(y1[1:240] ~ fmls(x1[1:720],3,3,nealmon)+fmls(x2[1:240],1,1,nealmon))
mod1$fitted.values[221:240]
mod1_forecast$fitted[221:240]
y ~ trend + fmls(x, 23, 12, nealmon), start = list(x = rep(0, 3))
mod1$residuals
mod1_forecast <-forecast(mod1, list(x1=x1[661:720],x2=x2[221:240]))
mod1_forecast
#预测季度的换下一个月
mod2 <- midas_u(y1[1:239] ~ fmls(x1[2:718],3,3,nealmon)+fmls(x2[1:239],1,1,nealmon))
mod2_forecast <-forecast(mod2, list(x1=x1[662:718],x2=x2[221:239]))
mod3 <- midas_u(y1[1:239] ~ fmls(x1[3:719],3,3,nealmon)+fmls(x2[1:239],1,1,nealmon))
mod3_forecast <-forecast(mod3, list(x1=x1[663:718],x2=x2[221:239]))
y1step2<-mod2_forecast$fitted
y1step2
y1step3<-mod3_forecast$fitted
length(y1step3)
plot(y1step2[221:239])
plot(y1[222:239],xlab='forecast horizon',ylab='UNRATE one_step_prediction')
lines(y1step2[221:238],col='red')
lines(y1step[221:238],col='green')
lines(y1step3[221:238],col='blue')
lines(mod4$fitted.values[221:240],col='blue',pch=15,lty=2)
lines(mod5$fitted.values[221:240],col='red',pch=17,lty=4)
lines(mod6$fitted.values[221:240],col='green',pch=20,lty=6)

legend("topright",title="MIDAS", c("PCA_M5","PCA_M5+PCA_Q3","PCA_M3+PCA_Q3"),lty=c(2,4,6),pch=c(15, 17,20),col=c("blue","red","green"))


sqrt(sum((y1step[221:239]-y1[222:240])^2)/19)
sqrt(sum((y1step2[221:238]-y1[222:239])^2)/18)
sqrt(sum((y1step3[221:238]-y1[222:239])^2)/18)
sqrt(sum((mod4$fitted.values[221:239]-y1[222:240])^2)/19)
sqrt(sum((mod5$fitted.values[221:239]-y1[222:240])^2)/19)
sqrt(sum((mod6$fitted.values[221:239]-y1[222:240])^2)/19)

##Calculate average forecasts
avgf <- average_forecast(list(mod1,mod2),
                         data=list(y=y,x=x,z=z,trend=trend),
                         insample=1:200,outsample=201:250,
                         type="fixed",
                         measures=c("MSE","MAPE","MASE"),
                         fweights=c("EW","BICW","MSFE","DMSFE"))
#PCA jiangwei
library(zoo)
getPCA <- function( timeseries_vector, number0fPC = 3, showgraph = FALSE){
  # head(daily_series)
  pca = PCA(timeseries_vector, graph = showgraph)
  # pca$eig
  # head(pca$ind$coord)
  result = pca$ind$coord[,1:number0fPC]
  return(result)
}
pca_m = getPCA(datam2[,-c(1,25)], showgraph = TRUE, number0fPC = 5)
pca_q = getPCA(dataq2[,-c(1,60)], showgraph = TRUE, number0fPC = 3)
pca_q1 <- pca_q[,1]
pca_q2 <- pca_q[,2]
pca_q3 <- pca_q[,3]

pca_m1 <- pca_m[,1]
pca_m2 <- pca_m[,2]
pca_m3 <- pca_m[,3]
pca_m4 <- pca_m[,4]
pca_m5 <- pca_m[,5]

length(mod1_forecast$mean)
length(mod1_forecast$fitted)
sqrt(sum((y1step[221:239]-y1[222:240])^2)/19)
y1step<-mod1_forecast$fitted
length(y1step)
plot(y1step[221:239])
plot(y1[222:240])
lines(y1step[221:239])
mod1$coefficients
mod2 <- midas_r(y ~ trend + mls(x, 4:20, 4, nealmon) + mls(z, 12:25, 12, nealmon),
                start=list(x=c(10,1,-0.1),z=c(2,-0.1)))
##Calculate average forecasts
avgf <- average_forecast(list(mod1,mod2),
                         data=list(y=y,x=x,z=z,trend=trend),
                         insample=1:200,outsample=201:250,
                         type="fixed",
                         measures=c("MSE","MAPE","MASE"),
                         fweights=c("EW","BICW","MSFE","DMSFE"))
n<-250
## Linear trend and higher-frequency explanatory variables (e.g. quarterly and monthly)
trend<-c(1:n)
x<-rnorm(4*n)
z<-rnorm(12*n)
## Exponential Almon polynomial constraint-consistent coefficients
fn.x <- nealmon(p=c(1,-0.5),d=8)
fn.z <- nealmon(p=c(2,0.5,-0.1),d=17)
## Simulated low-frequency series (e.g. yearly)
y<-2+0.1*trend+mls(x,0:7,4)%*%fn.x+mls(z,0:16,12)%*%fn.z+rnorm(n)
dim(y)
length(mls(z, 12:22, 12, nealmon))
mod1 <- midas_r(y ~ trend + mls(x, 4:14, 4, nealmon) + mls(z, 12:22, 12, nealmon),
                start=list(x=c(10,1,-0.1),z=c(2,-0.1)))

