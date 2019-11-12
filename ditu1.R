library(maptools)
library(maps)
library(mapdata)
library(maptools)
x <- readShapePoly("F:/北大/CN-shi-A.shp")
length(x)
names(x)


replicate()

# 測试数据
# plot(x,col=gray(924:0/924));

# 定义地图颜色函数
getColor <- function(mapdata,provname,provcol,othercol) 
{ f=function(x,y) ifelse(x %in% y,which(y==x),0); 
colIndex=sapply(iconv(x@data$NAME,"GBK","UTF-8"),f,provname); 
col=c(othercol,provcol)[colIndex+1]; 
return(col); 
}

library(raster)
library(rgdal)
library(tidyverse)
library(mapproj)
library(jpeg)
library(grid)
library(maptools)
library(sp)
#读入外部数据
library(readxl)
# 拼接图片
library(jpeg)
library(png)
library(grid)
library(magick)
library(rgeos)

# 数据分组 取色
library(RColorBrewer)
library(classInt)

library(maptools)
axx<-rgdal::readOGR("E:/map/chinaadm/CHN/CHN_adm1.shp")
View(axx@data)
x<-axx
#plot(x,col=gray(924:0/924));#using col instead of fg!
getColor=function(mapdata,provname,provcol,othercol)
{ 
  f=function(x,y) ifelse(x %in% y,which(y==x),0); 
  colIndex=sapply(sapply(mapdata$NAME,f,provname)); 
  fg=c(othercol,provcol)[colIndex+1]; 
  return(fg)
} 
provname=c("北京市","天津市","上海市","重庆市")
provcol=c("red","green","yellow","purple")
plot(x,fg=getColor(x,provname,provcol,"white"))

provname=c("北京市","天津市","河北省","山西省","内蒙古自治区",
           "辽宁省","吉林省","黑龙江省","上海市","江苏省",
           "浙江省","安徽省","福建省","江西省","山东省",
           "河南省","湖北省","湖南省","广东省",
           "广西壮族自治区","海南省","重庆市","四川省","贵州省",
           "云南省","西藏自治区","陕西省","甘肃省","青海省",
           "宁夏回族自治区","新疆维吾尔自治区","台湾省",
           "香港特别行政区")
pop=c(1633,1115,6943,3393,2405,4298,2730,3824,1858,7625,
      5060,6118,3581,4368,9367,9360,5699,6355,9449,
      4768,845,2816,8127,3762,4514,284,3748,2617,
      552,610,2095,2296,693)
provcol=rgb(red=1-pop/max(pop)/1,green=1-pop/max(pop)/1,blue=1/1.5)
plot(x)
plot(x,fg=getColor(x,provname,provcol,"white"),xlab="",ylab="")



#读入分县市地图，省一级的地图比较容易做
setwd("E:map/chinaadm/")
mland<-rgdal::readOGR("CHN/CHN_adm1.shp")
#mland <- raster::aggregate(mland,by="ID_2")
tw<-rgdal::readOGR("TWN/TWN_adm0.shp")
tw <- raster::aggregate(tw)
mc<-rgdal::readOGR("MAC/MAC_adm0.shp")
mc <- raster::aggregate(mc)
hk<-rgdal::readOGR("HKG/HKG_adm0.shp")
hk <- raster::aggregate(hk)

#分省及全国地图
guo<-rgdal::readOGR("BASIC/国家.shp")
lguo<-rgdal::readOGR("BASIC/中国线.shp")
l9<-rgdal::readOGR("BASIC/九段线.shp")
sheng<-rgdal::readOGR("BASIC/中国政区.shp")
nh<-rgdal::readOGR("BASIC/南海诸岛及其它岛屿.shp")
sh<-rgdal::readOGR("BASIC/省会城市.shp")

#合并地理单元数量
china_map<- bind(mland, tw, mc,hk)

#可以写出合并后的结果，中文字符编码问题，以后解决
#如果解决好，以后不用读入多个地图
#writeOGR(china_map, "BASIC/","counties", driver="ESRI Shapefile",encoding="UTF-8",overwrite_layer=TRUE)
#china_map2<-rgdal::readOGR("BASIC/counties.shp")

# 现成的南海地图
nhimg <- image_read("BASIC/南海诸岛.png")
#保留想保留下来的ID信息
#china_map <- raster::aggregate(china_map,by=c("ID_2","NL_NAME_2"))

#读入外部统计数据  
mydata<- read_excel("C:/Users/lenovo/Desktop/gdp_test_industrial.xls", 
                     sheet = "04比较")
temp<-read_excel("C:/Users/lenovo/Desktop/新建 Microsoft Excel 工作表.xlsx", sheet = "Sheet1")
mydata1<-mydata[,1:4]
names(mydata1)
names(mydata1)<-c("NAME_2","gdp","gdp1","gdpcha")
names(temp) <- c("id","NAME_1","NAME_2","gdp")
library(plyr)
gdpdata<-join(temp,mydata1,by="NAME_2",type="full")
View(gdpdata)
# 地图统计单元数据中ID_2,NL_NAME_2
x <- china_map@data
# ID加上行id,这个行id-与地理元素单元list的顺序一致
xs <- data.frame(id=row.names(x),x)
#香港、澳门、台湾没有ID_2,"NL_NAME_2"的信息还需要完善（留到以后）
xs[,"ID_2"] <- xs[,"id"]
View(xs)

dim(mydata1)
write.csv(xs,"C:/Users/lenovo/Desktop/sheng.csv")

# 清理地理统计单元ID变量中不合适的数据
xs$NL_NAME_2 <- as.character(xs$NL_NAME_2)
xs$NL_NAME_2[xs$NL_NAME_2 =="北京|北京"] <-"北京市"
xs$NL_NAME_2[xs$NL_NAME_2 =="重慶|重庆"] <-"重庆市"
xs$NL_NAME_2[xs$NL_NAME_2 =="常德市|常德市"] <-"常德市"
xs$NL_NAME_2[xs$NL_NAME_2 =="长沙市|長沙市"] <-"长沙市"
xs$NL_NAME_2[xs$NL_NAME_2 =="郴州市|郴州市"] <-"郴州市"
xs$NL_NAME_2[xs$NL_NAME_2 =="衡阳市|衡陽市"] <-"衡阳市"
xs$NL_NAME_2[xs$NL_NAME_2 =="怀化市|懷化市"] <-"怀化市"
xs$NL_NAME_2[xs$NL_NAME_2 =="娄底市|婁底市"] <-"娄底市"
xs$NL_NAME_2[xs$NL_NAME_2 =="邵阳市|邵陽市"] <-"邵阳市"
xs$NL_NAME_2[xs$NL_NAME_2 =="湘潭市|湘潭市"] <-"湘潭市"
xs$NL_NAME_2[xs$NL_NAME_2 =="湘西土家族苗族自治州|湘西土家族苗族自治州"] <-"湘西土家族苗族自治州"     
xs$NL_NAME_2[xs$NL_NAME_2 =="益阳市|益陽市"] <-"益阳市"      
xs$NL_NAME_2[xs$NL_NAME_2 =="永州市|永州市"] <-"永州市"
xs$NL_NAME_2[xs$NL_NAME_2 =="岳阳市|岳陽市"] <-"岳阳市"
xs$NL_NAME_2[xs$NL_NAME_2 =="张家界市|張家界市"] <-"张家界市"
xs$NL_NAME_2[xs$NL_NAME_2 =="株洲市|株洲市"] <-"株洲市"
xs$NL_NAME_2[xs$NL_NAME_2 =="滨州"] <-"滨州市"
xs$NL_NAME_2[xs$NL_NAME_2 =="上海|上海"] <-"上海市"
xs$NL_NAME_2[xs$NL_NAME_2 =="运城县"] <-"运城市"
xs$NL_NAME_2[xs$NL_NAME_2 =="天津|天津"] <-"天津市"
xs$NL_NAME_2[xs$NL_NAME_2 =="巴音郭愣蒙古自治州"] <-"巴音郭楞蒙古自治州"
xs$NL_NAME_2[345] <-"台湾"
xs$NL_NAME_2[346] <-"澳门"
xs$NL_NAME_2[345] <-"香港"

#地理统计单元数据和外部统计数据合并
# by NL_NAME_2
data<-plyr::join(xs,gdpdata,type="full",by="NAME_1")
#data<-data[-c(176,182,192,251,351,173),]
#data<-data[-c(15,22,80,117,355,356,357,358),]
data1<-data
dim(data1)
View(data1)
names(data1)
#离散颜色标度分割：
qa <- quantile(na.omit(data1$gdpcha),c(0,0.4,0.6,0.8,1.0))
qa[1]
qa<-c(-1,-0.0000009,0.0009,0.04,1)
data1$fic_qq<-cut(data1$gdpcha,qa,labels= c("qa[1]-qa[2]","qa[2]-qa[3]","qa[3]-qa[4]","qa[4]-qa[5]"),include.lowest = TRUE)#labels= c("qa[1]-qa[2]","qa[2]-qa[3]","qa[3]-qa[4]","qa[4]-qa[5]"),

table(data1$fic_qq)
View(data1)
data1<-data1[1:31,]
dim(data1)
# 地图元素单元数据，有list 顺序信息，即前面的id信息
map_data <- fortify(china_map)
# by id，将地理统计单元的ID信息合并到地理元素单元中
map_data <- plyr::join(map_data, data1, type ="full")
dim(map_data)
names(map_data)
# 制成地图，为拼接外部来源图片做准备
p <- ggplot()+
  geom_polygon(data=map_data,aes(long,lat,group=group,fill=fic_qq),colour=NA,size=0.25) + #地区
  geom_polygon(data=sheng,aes(long,lat,group=group,fill=NA),colour="black",size=0.25) + # 省界
  scale_fill_brewer(palette="RdYlGn")+
  coord_map() +  
  theme(
    legend.position=c(0.1, 0.20),
    legend.text = element_text(size=rel(2)),
    legend.title = element_text(size=rel(2)),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background=element_rect(I(0),linetype=0)
    #plot.title = element_text(family="STKaiti",size=32,
    #           hjust=0.5,  colour = "red",face = "bold")
  )
#+scale_fill_discrete(name="Experimental\nCondition",
                      #breaks=qa,labels=c("Control", "Treatment 1", "Treatment 2","A","B"))
#  theme_nothing() 
# 画底图(大陆),保存 比较慢,8分钟
# ggsave(p,filename = "中国地图.png",
#                width = 5, height = 4, dpi = 300)
#  
#最好将地图输出到文件中而不是使用ploty打印在窗口——很慢
plot2 <- function(theplot, name, ...) {
  name <- paste0(name, ".png")
  png(filename=name,width = 960, height = 960, units = "px", pointsize = 12)
  print(theplot)
  dev.off()
} #plotting function
plot2(p, name="地图sheng2")


bias<-read.csv("C:/Users/lenovo/Desktop/bias.csv",encoding = "utf-8")
View(bias)
