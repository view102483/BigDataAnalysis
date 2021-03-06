# NBA 2014-2015球季 各隊分析
if (!require('SportsAnalytics')){
  install.packages("SportsAnalytics")
  library(SportsAnalytics)
}
if (!require('knitr')){
  install.packages("knitr")
  library(knitr)
}
NBA1415<-fetch_NBAPlayerStatistics("14-15")

## 各隊最辛苦的球員
MaxMinutes<-aggregate(TotalMinutesPlayed~Team,NBA1415,max)
NBA1415MaxMinutes<-merge(NBA1415,MaxMinutes)
output<-NBA1415MaxMinutes[order(NBA1415MaxMinutes$TotalMinutesPlayed,decreasing = T),c("Team","Name","TotalMinutesPlayed")]
kable(output,digits=2)
## 各隊得分王
MaxPoint<-aggregate(TotalPoints~Team,NBA1415,max)
NBA1415MaxPoint<-merge(NBA1415,MaxPoint)
output<-NBA1415MaxPoint[order(NBA1415MaxPoint$TotalPoints,decreasing = T),c("Team","Name","TotalPoints")]
kable(output,digits = 2)
## 各隊最有效率的球員
NBA1415$Efficiency<-NBA1415$TotalPoints/NBA1415$TotalMinutesPlayed 
MaxEfficiency<-aggregate(Efficiency~Team,NBA1415,max)
NBA1415MaxEfficiency<-merge(NBA1415,MaxEfficiency)
output<-NBA1415MaxEfficiency[order(NBA1415MaxEfficiency$Efficiency,decreasing = T),c("Team","Name","Efficiency")]
kable(output,digits = 2)
## 各隊三分球出手最準的球員
NBA1415$ThreesRate<-NBA1415$ThreesMade/NBA1415$ThreesAttempted 
MaxThreesRate<-aggregate(ThreesRate~Team,NBA1415,max)
NBA1415MaxThreesRate<-merge(NBA1415,MaxThreesRate)
output<-NBA1415MaxThreesRate[order(NBA1415MaxThreesRate$ThreesRate,decreasing = T),c("Team","Name","ThreesRate")]
kable(output,digits = 2)

