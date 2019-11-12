library(readxl)
df<-read_excel("F:/北大/遥感大数据/主要城市年度数据绍兴.xls.xlsx",2)
library(ggplot2)
df<-as.data.frame(df)
head(df)
plot(df)
time<-as.factor(df$时间)
z<-df[,2:6]
z<-as.data.frame(z)
head(z)
class(z)
matplot(time,z)
tm<-df[,2]
class(tm)
sm<-df[,3]

oy<-df[,4]

ty<-df[,5]

time<-df[,1]

thy<-df[,6]


plot(tm~time,type="l",lty=1)+xlim(200,4000)
points(sm~time,col="red",lty=2)
lines(oy~time,col="steelblue",lty=7)
points(ty~time,col="pink3",lty=4)
lines(thy~time,col="darkgreen",lty=5)
lines(fy~time,col="grey4",lty=6)
title("YIELD(m)",lwd=3)
legend("topleft",cex=.6,c("tm","sm","oy","ty","thy","fy"),col=c("black","red","blue","yellow","green","grey"),lty=1:6)

library(readxl)
library(ggplot2)
gdpdata13<-read_excel("C:/Users/lenovo/Desktop/gdp.xlsx",1)
gdpdata13<-as.data.frame(gdpdata13)
names(gdpdata13)
head(gdpdata13)
gdpdata13$percent13<-(gdpdata13$gdp-gdpdata13$公报gdp)/gdpdata13$公报gdp
gdpdata13$percent131<-(gdpdata13$第一产业-gdpdata13$公报1)/gdpdata13$公报1
gdpdata13$percent132<-(gdpdata13$第二产业-gdpdata13$公报2)/gdpdata13$公报2
gdpdata13$percent133<-(gdpdata13$第三产业-gdpdata13$公报3)/gdpdata13$公报3
plot(gdpdata13$percent13)
gdpdata130<-subset(gdpdata13,gdpdata13$percent13==0)
length(gdpdata130)
gdpdata130
gdpdata080<-subset(gdpdata08,gdpdata08$percent08==0)
length(gdpdata080)
gdpdata080





par(mfrow=c(2,2))
hist(gdpdata13$percent13,binwidth=0.2,breaks=50,main="13年普查后gdp变化比分布图",xlab="gdp变化比",ylab="频数")
hist(gdpdata13$percent131,binwidth=0.2,breaks=50,main="13年普查后第一产业增加值变化分布图",xlab="第一产业增加值变化比",ylab="频数")
hist(gdpdata13$percent132,binwidth=0.2,breaks=50,main="13年普查后第二产业增加值变化分布图",xlab="第二产业增加值变化比",ylab="频数")
hist(gdpdata13$percent133,binwidth=0.2,breaks=50,main="13年普查后第三产业增加值变化分布图",xlab="第三产业增加值变化比",ylab="频数")

ggplot(gdpdata13,aes(x=gdpdata13$percent133))+geom_density(alpha=0.3)
#ggplot(gdpdata13,aes(x=gdpdata13$percent13))+geom_histogram()
summary(gdpdata13$percent13)
df13<-data.frame(num=gdpdata13$percent13,group="gdp比例13")
df131<-data.frame(num=gdpdata13$percent131,group="第一产业比例13")
df132<-data.frame(num=gdpdata13$percent132,group="第二产业比例13")
df133<-data.frame(num=gdpdata13$percent133,group="第三产业比例13")
data13<-data.frame(rbind(df13,df131,df132,df133))
head(data13)
ggplot(data13,aes(x=num,fill=factor(group)))+geom_histogram(position="identity",alpha=0.5)

gdpdata08<-read_excel("C:/Users/lenovo/Desktop/gdp.xlsx",2)
gdpdata08<-as.data.frame(gdpdata08)

gdpdata08$percent08<-(gdpdata08$gdp-gdpdata08$公报gdp)/gdpdata08$公报gdp
gdpdata08$percent081<-(gdpdata08$第一产业-gdpdata08$公报1)/gdpdata08$公报1
gdpdata08$percent082<-(gdpdata08$第二产业-gdpdata08$公报2)/gdpdata08$公报2
gdpdata08$percent083<-(gdpdata08$第三产业-gdpdata08$公报3)/gdpdata08$公报3

gdpdata030<-subset(gdpdata03,gdpdata03$percent03==0)
length(gdpdata080)
gdpdata030



hist(gdpdata08$percent08,binwidth=0.2,breaks=50,main="08gdp")

ggplot(gdpdata08,aes(x=gdpdata08$percent083))+geom_density(alpha=0.3)
#ggplot(gdpdata13,aes(x=gdpdata13$percent13))+geom_histogram()
summary(gdpdata08$percent08)
df08<-data.frame(num=gdpdata08$percent08,group="gdp比例08")
df081<-data.frame(num=gdpdata08$percent081,group="第一产业比例08")
df082<-data.frame(num=gdpdata08$percent082,group="第二产业比例08")
df083<-data.frame(num=gdpdata08$percent083,group="第三产业比例08")
data08<-data.frame(rbind(df08,df081,df082,df083))
head(data13)
ggplot(data08,aes(x=num,fill=factor(group)))+geom_histogram(position="identity",alpha=0.5)
hist(gdpdata08$percent08,binwidth=0.2,breaks=50,main="08年普查后gdp变化比分布图",xlab="gdp变化比",ylab="频数")
hist(gdpdata08$percent081,binwidth=0.2,breaks=50,main="08年普查后第一产业增加值变化分布图",xlab="第一产业增加值变化比",ylab="频数")
hist(gdpdata08$percent082,binwidth=0.2,breaks=50,main="08年普查后第二产业增加值变化分布图",xlab="第二产业增加值变化比",ylab="频数")
hist(gdpdata08$percent083,binwidth=0.2,breaks=50,main="08年普查后第三产业增加值变化分布图",xlab="第三产业增加值变化比",ylab="频数")

##03nian 
gdpdata03<-read_excel("C:/Users/lenovo/Desktop/gdp.xlsx",3)
gdpdata03<-as.data.frame(gdpdata03)

gdpdata03$percent03<-(gdpdata03$gdp-gdpdata03$公报gdp)/gdpdata03$公报gdp
gdpdata03$percent031<-(gdpdata03$第一产业-gdpdata03$公报1)/gdpdata03$公报1
gdpdata03$percent032<-(gdpdata03$第二产业-gdpdata03$公报2)/gdpdata03$公报2
gdpdata03$percent033<-(gdpdata03$第三产业-gdpdata03$公报3)/gdpdata03$公报3


df03<-data.frame(num=gdpdata03$percent03,group="gdp比例04")
df031<-data.frame(num=gdpdata03$percent031,group="第一产业比例04")
df032<-data.frame(num=gdpdata03$percent032,group="第二产业比例04")
df033<-data.frame(num=gdpdata03$percent033,group="第三产业比例04")
data03<-data.frame(rbind(df03,df031,df032,df033))
head(data13)
ggplot(data03,aes(x=num,fill=factor(group)))+geom_histogram(position="identity",alpha=0.5)
hist(gdpdata03$percent03,binwidth=0.2,breaks=50,main="04年普查后gdp变化比分布图",xlab="gdp变化比",ylab="频数")
hist(gdpdata03$percent031,binwidth=0.2,breaks=50,main="04年普查后第一产业增加值变化分布图",xlab="第一产业增加值变化比",ylab="频数")
hist(gdpdata03$percent032,binwidth=0.2,breaks=50,main="04年普查后第二产业增加值变化分布图",xlab="第二产业增加值变化比",ylab="频数")
hist(gdpdata03$percent033,binwidth=0.2,breaks=50,main="04年普查后第三产业增加值变化分布图",xlab="第三产业增加值变化比",ylab="频数")


##3年的gdp 123增加值比较
gdp13<-data.frame(rbind(df13,df08,df03))
ggplot(data03,aes(x=data03$num,group=factor(data03$group)))+geom_density()
ggplot(gdpdata13,aes(x=gdpdata13$percent13))+geom_density()
ggplot(gdp13,aes(x=num,fill=factor(group)))+geom_histogram(position="identity",alpha=0.5)+ggtitle("3次普查后gdp变化比对比图")+scale_fill_hue(name="年份",labels=c("13年","08年","04年"))+labs(x="变化比",y="频数")
gdp1<-data.frame(rbind(df131,df081,df031))
ggplot(gdp1,aes(x=gdp13$num,group=factor(gdp13$group)))+geom_density()
ggplot(gdp1,aes(x=num,fill=factor(group)))+geom_histogram(position="identity",alpha=0.5)+ggtitle("3次普查后第一产业增加值变化比对比图")+scale_fill_hue(name="年份",labels=c("13年","08年","03年"))+labs(x="变化比",y="频数")

gdp2<-data.frame(rbind(df132,df082,df032))
ggplot(gdp13,aes(x=gdp13$num,group=factor(gdp13$group)))+geom_density()
ggplot(gdp2,aes(x=num,fill=factor(group)))+geom_histogram(position="identity",alpha=0.5)+ggtitle("3次普查后第二产增加值变化比对比图")+scale_fill_hue(name="年份",labels=c("13年","08年","03年"))+labs(x="变化比",y="频数")

gdp3<-data.frame(rbind(df133,df083,df033))
ggplot(gdp13,aes(x=gdp13$num,group=factor(gdp13$group)))+geom_density()
ggplot(gdp3,aes(x=num,fill=factor(group)))+geom_histogram(position="identity",alpha=0.5)+ggtitle("3次普查后第三产业增加值变化比对比图")+scale_fill_hue(name="年份",labels=c("13年","08年","03年"))+labs(x="变化比",y="频数")
detach(gdp13)
zhongshan<-read_excel("F:/北大/遥感大数据/主要城市年度数据中山.xlsx",2)
zhongshan<-as.data.frame(zhongshan)
install.packages("reshape")
detach(zhongshan)
#zhongshan<-zhongshan[,-5:-6]
names(zhongshan)
test_data_long<-melt(zhongshan,id="date")
head(test_data_long)
ggplot(data=test_data_long, aes(x=date, y=value, colour=variable)) + geom_line(stat="identity")
library(reshape2)
ggplot(test_data_long)+geom_point(aes(x=date,y=value,colour=variable),size=4)+ggtitle("中山")
##分城市
beijing<-read_excel("F:/北大/遥感大数据/主要城市年度数据沈阳.xls",2)
sz<-as.data.frame(beijing)
sz<-melt(sz,id="date")
p1<-ggplot(sz,aes(x=date,y=value,colour=variable))+geom_line()+geom_point(shape=20,size=2)
p1<-p1+scale_colour_hue("指标",labels=c("公报gdp","13年普查后gdp","08年普查后gdp","04年普查后gdp"))
p1+labs(x="年份",y="值")+ggtitle("沈阳公报与3次普查之后gdp对比情况")+geom_vline(xintercept=c(2004,2008,2013))+annotate("text", x=c(2004,2008,2013), y=10, label=c("2004","2008","2013"))



bj1<-read_excel("F:/北大/遥感大数据/主要城市年度数据北京.xls",3)
bj1<-as.data.frame(bj1)
bjj1<-melt(bj1,id="date")
p1<-ggplot(bjj1,aes(x=date,y=value,colour=variable))+geom_line()+geom_point(size=1)
p1<-p1+scale_colour_hue("指标",labels=c("公报第一产业值","13年普查第一产业","08年普查第一产业","04年普查第一产业"))
p1+labs(x="年份",y="值")+ggtitle("北京第一产业增加值公报与3次普查之后变化情况")+geom_vline(xintercept=c(2004,2008,2013))+annotate("text", x=c(2004,2008,2013), y=0, label=c("2004","2008","2013"))


bj2<-read_excel("F:/北大/遥感大数据/主要城市年度数据沈阳.xls",4)
bj2<-as.data.frame(bj2)
test_data_long1<-melt(bj2,id="date")
p<-ggplot(test_data_long1,aes(x=date,y=value,colour=variable))+geom_line()+geom_point(size=1.5)
p<-p+scale_colour_hue("指标",labels=c("公报第二产业值","13年普查第二产业","08年普查第二产业","04年普查第二产业"))
p+labs(x="年份",y="值")+ggtitle("沈阳第二产业增加值公报与3次普查之后变化情况")+geom_vline(xintercept=c(2004,2008,2013))+annotate("text", x=c(2004,2008,2013), y=10, label=c("2004","2008","2013"))


bj<-read_excel("F:/北大/遥感大数据/主要城市年度数据北京.xls",5)
bj<-as.data.frame(bj)

bjj<-melt(bj,id="date")
head(bjj)
p<-ggplot(bjj,aes(x=date,y=value,colour=variable))+geom_line()+geom_point(size=2)
p<-p+scale_colour_hue("指标",labels=c("公报第三产业值","13年普查第三产业","08年普查第三产业","04年普查第三产业"))
p+labs(x="年份",y="值")+ggtitle("北京第三产业增加值公报与3次普查之后变化情况")+geom_vline(xintercept=c(2004,2008,2013))+annotate("text", x=c(2004,2008,2013), y=10, label=c("2004","2008","2013"))




shangh<-read.csv("C:/Users/lenovo/Desktop/上海3.csv")

names(tianjin)
shangh<-melt(shangh,id="date")
p<-ggplot(shangh,aes(x=date,y=value,colour=variable))+geom_line()+geom_point(size=2)
p<-p+scale_colour_hue("指标",labels=c("公报第三产业值","13年普查第三产业","08年普查第三产业","03年普查第三产业"))
p+labs(x="年份",y="值")+ggtitle("上海第三产业增加值公报与3次普查之后变化情况")




shanghai<-read.csv("C:/Users/lenovo/Desktop/天津1.csv")
test_data_long3<-melt(shanghai,id="date")
head(test_data_long)
p<-ggplot(test_data_long3,aes(x=date,y=value,colour=variable))+geom_line()+geom_point(shape=20,size=2)
p<-p+scale_colour_hue("指标",labels=c("公报第一产业值","13年普查第一产业","08年普查第一产业","04年普查第一产业"))
p+labs(x="年份",y="值")+ggtitle("天津第一产业增加值公报与3次普查之后变化情况")+geom_vline(xintercept=c(2004,2008,2013))+annotate("text", x=c(2004,2008,2013), y=10, label=c("2004","2008","2013"))


(aes(x=date,y=value,colour=variable),size=1,shape=20)

class(gdp13$group)
install.packages("sm")
library(sm)
attach(gdp13)
sm.density.compare(gdp13$num,gdp13$group,linetype=gdp13$group)
title(main="130803gdp比较")
colfill=c(2:1+length(levels(gdp13$group)))
legend(locator(1),levels(gdp13$group),fill=colfill)

summary(gdp13)
gdp13$num<-as.factor(gdp13$num)
head(gdp13)
library(readxl)
dfgdp<-read_excel("C:/Users/lenovo/Desktop/gdp.xlsx",1)
dfgdp$城市1<-paste0(dfgdp$城市,"市")

write.csv(dfgdp,"13.csv")
