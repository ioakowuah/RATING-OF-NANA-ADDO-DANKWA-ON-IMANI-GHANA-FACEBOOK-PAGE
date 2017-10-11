#############
########Facebook popularity with Imani
############

##############
######Packages Required
#############
library("Rfacebook")
library("stringr")
library("reshape2")
library("ggplot2")
library("scales")
###########
#Download 1000 posts from facebook Imani Page
###########
Imani<-getPage("***********",token,n=1000)

#########
####Plot of trend of popularity in terms of likes, comments and shares
########
format.facebook.date<-function(datestring){
  date<-as.POSIXct(datestring,format="%Y-%m-%dT%H:%M:%S+0000",tz="GMT")
}

aggregate.metric<-function(metric){
  m<-aggregate(Imani[[paste0(metric,"_count")]],list(month=Imani$month),mean)
  m$month<-as.Date(paste0(m$month,"-15"))
  m$metric<-metric
  return(m)
}

Imani$datetime<-format.facebook.date(Imani$created_time)
Imani$month<-format(Imani$datetime,"%Y-%m")
df.List<-lapply(c("likes","comments","shares"),aggregate.metric)
df<-do.call(rbind,df.List)
df

ggplot(df,aes(x=month,y=x,group=metric))+geom_line(aes(color=metric))+scale_x_date(date_breaks = "years",labels = date_format("%Y"))+theme_bw()+theme(axis.title.x = element_blank())+ylab("Average count per post")+ggtitle("IMANI'S FACEBOOK PAGE POPULARITY")


########
####Knowing popular or maximum likes,shares,comments
#######
Imani[which.max(Imani$likes_count),]
Imani[which.max(Imani$comments_count),]
Imani[which.max(Imani$shares_count),]

##############
#Download post of maximum comments
##############
ImaniCO<-getPage("*************",token,since ='2017/04/13',until = '2017/04/14')

############
####Numbering of posts on the single day
###########
ImaniCO$order<-1:nrow(ImaniCO) 
ImaniCOo<-ImaniCO[1,] ##Continue with proper row selected
###########

##########
###Function to download Comments on post of maximum comments
##########
download.maybe<-function(i,refetch=FALSE,path=","){
  post<-getPost(post = ImaniCO$id[i],comments = TRUE,likes = TRUE, token=token)
  post1<-as.data.frame(melt(post))
}
#########
#Apply function to download comment
#########
files<-data.frame(melt(lapply(ImaniCO$order,download.maybe)))

#########
###Subsetting Only  Related words (Excellent, Very Good, Good, Satisfactory, Poor, Fail)
########
comM<-subset(files,message!="NA" & from_name!="Imani Center for Policy & Education"& from_name!="Prince Alec Douglas Gaisie")
attach(comM)
Only<-subset(comM,comM$message=="A" | comM$message=="B" | comM$message=="C" | comM$message=="D" | comM$message=="E"| comM$message=="F"| comM$message=="Excellent" | comM$message=="Very Good"| comM$message=="Good" | comM$message=="Satisfactory" | comM$message=="Poor" | comM$message=="Fail")
Only$message[Only$message=="A"]<-"Excellent"
Only$message[Only$message=="B"]<-"Very_Good"
Only$message[Only$message=="Very Good"]<-"Very_Good"
Only$message[Only$message=="C"]<-"Good"
Only$message[Only$message=="D"]<-"Satisfactory"
Only$message[Only$message=="E"]<-"Poor"
Only$message[Only$message=="F"]<-"Fail"

###########
###Tabulating rankings (Excellent, Very Good, Good, Satisfactory, Poor, Fail)
##########
table(Only$message)
dim(Only)

ggplot(Only, aes(Only$message))+ geom_bar(aes(fill = Only$message)) + scale_fill_brewer(palette = "Blues")+ theme_classic()+xlab("Rating")+ggtitle("Rating of President Nana Addo's First Quarter Governance")+guides(fill=guide_legend(title = "Rating"))
