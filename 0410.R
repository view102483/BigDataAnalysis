if (!require('Rfacebook')) {
  install.packages('Rfacebook')
  library('Rfacebook')
}

if (!require('knitr')) {
  install.packages('knitr')
  library('knitr')
}

target <- 'tsaiingwen'
token <- '1588328308124163|Zmf9XuZ3Am5ZWkxJpKfyrVlkrgY'
firstDate <- '2016-01-01'

totalPage <- NULL
lastDate <- Sys.Date()
dateVectorStr <- as.character(seq(as.Date(firstDate),lastDate,by="5 days"))
for(i in 1 : (length(dateVectorStr)-1) ){
  tempPage <- getPage(target, token,
                    since = dateVectorStr[i], until = dateVectorStr[i + 1])
  totalPage <- rbind(totalPage,tempPage)
}
nrow(totalPage)

#totalPage$datetime <- as.POSIXct(totalPage$created_time,
#                                 format = "%Y-%m-%dT%H:%M:%S+0000",
#                                 tz = "GMT")

totalPage$dateTPE <- format(as.POSIXct(totalPage$created_time,
                                       format = "%Y-%m-%dT%H:%M:%S+0000",
                                       tz = "GMT"),
                            "%Y-%m-%d",
                            tz = "Asia/Taipei")
#totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
postCount<-aggregate(id~dateTPE, totalPage, length)
kable(head(postCount[order(postCount$id, decreasing = T),]))

likeCount<-aggregate(likes_count~dateTPE, totalPage, mean)
kable(head(likeCount[order(likeCount$likes_count, decreasing = T),]))

commentCount<-aggregate(comments_count~dateTPE, totalPage, mean)
kable(head(commentCount[order(commentCount$comments_count, decreasing = T),]))

shareCount<-aggregate(shares_count~dateTPE, totalPage, mean)
kable(head(shareCount[order(shareCount$shares_count, decreasing = T),]))


totalComments<-NULL
postId<-totalPage$id
for(i in 1 : (length(postId)-1)) {
  temp<-getPost(postId[i], token, n.comments = 100)
  totalComments<-rbind(totalComments, temp$comments)
}
postCommentsCount<-aggregate(id~from_name, totalComments, length)
postCommentsCount$id<-(postCommentsCount$id/length(postId))
kable(head(postCommentsCount[order(postCommentsCount$id, decreasing = T),]))