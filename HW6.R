data<-read.csv("POLIO_Incidence.csv")
if(!require(ggplot2)) {
  install.packages(ggplot2, dep=TRUE)
  library(ggplot2)
}
if(!require(reshape2)) {
  install.packages(reshape2, dep=TRUE)
  library(reshape2)
}


data.m <- melt(data, id.vars = c("YEAR", "WEEK"))
data.m[data.m$value=="-",]$value<-NA
data.m$value<-as.numeric(data.m$value)
data.sumYear<-aggregate(value~YEAR+variable, data=data.m, FUN=sum, na.rm=F)
ggplot(data.sumYear,aes(YEAR,variable))+#x軸為year，y軸為variable
  geom_tile(aes(fill=value),colour="white")+#將區塊進行著色
  scale_fill_gradient(low="white",high="blue")#白色部分是數值低的，高的為藍色