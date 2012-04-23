library(XML)
library(ggplot2)
##### World Cup 2011


dat <- NULL
for (i in c(433558:433576,433578:433606)){
url <- paste("http://www.espncricinfo.com/icc_cricket_worldcup2011/engine/current/match/",i,".html", sep = "")

tables <- readHTMLTable(url) 
bd1 <- tables$inningsBat1
res1 <- bd1[seq(3,25,by=2),-1]
res1$inings <- 1
bd2 <- tables$inningsBat2
res2 <- bd2[seq(3,25,by=2),-1]
res2$inings <- 2
dat <- rbind(dat,cbind(rbind(res1,res2),match=i))
}
dat$V4 <- as.numeric(as.character(dat$V4))
names(dat)<-c("Player", "method_get_out", "Runs", "Mins", "Balls_faced", "Fours", "Sixes", "Strike_Rate", "innings", "Match")
dd <- ddply(dat,.(V2),summarize,total = sum(V4),average_score = mean(V4), bat = length(V4))
dd$player <- factor(dd$V2,levels=dd$V2[order(dd$average_score)])
qplot(player,average_score,geom="bar",data=subset(dd,!is.na(V2) & V2!='Total'))+coord_flip()

##1975
#http://www.espncricinfo.com/ci/engine/match/65035.html
#http://www.espncricinfo.com/ci/engine/match/65049.html
dat<-NULL
for (i in c(65050:65063,65091:65125,65127:65169,65171:65272,65274:65286)){
url <- paste("http://www.espncricinfo.com/ci/engine/match/",i,".html", sep = "")

tables <- readHTMLTable(url) 
bd1 <- tables$inningsBat1
res1 <- bd1[seq(3,25,by=2),-1]
res1$inings <- 1
bd2 <- tables$inningsBat2
res2 <- bd2[seq(3,25,by=2),-1]
res2$inings <- 2
dat <- rbind(dat,cbind(rbind(res1,res2),match=i))	
}
names(dat)<-c("Player", "method_get_out", "Runs", "Mins", "Balls_faced", "Fours", "Sixes", "Strike_Rate", "innings", "Match")
dat$Fours <- as.numeric(as.character(dat$Fours))
##1979
http://www.espncricinfo.com/ci/engine/match/65050.html
http://www.espncricinfo.com/ci/engine/match/65063.html
##1983
http://www.espncricinfo.com/wc1983/engine/match/65064.html
http://www.espncricinfo.com/wc1983/engine/match/65090.html
##1987
http://www.espncricinfo.com/ci/engine/match/65091.html
http://www.espncricinfo.com/ci/engine/match/65117.html
##1992
http://www.espncricinfo.com/ci/engine/match/65118.html
http://www.espncricinfo.com/ci/engine/match/65156.html
##1996
http://www.espncricinfo.com/ci/engine/match/65157.html
http://www.espncricinfo.com/ci/engine/match/65192.html
##1999
http://www.espncricinfo.com/ci/engine/match/65193.html
http://www.espncricinfo.com/ci/engine/match/65234.html
##2003
http://www.espncricinfo.com/ci/engine/match/65235.html
http://www.espncricinfo.com/ci/engine/match/65286.html
##2007
http://www.espncricinfo.com/wc2007/engine/match/247457.html
http://www.espncricinfo.com/wc2007/engine/match/247507.html




###T20 World Cup

#url<-"http://www.espncricinfo.com/twenty20wc/engine/match/287853.html"

#url<-"http://www.espncricinfo.com/twenty20wc/engine/match/287879.html"

#"http://www.espncricinfo.com/wt202009/engine/match/355991.html"

#"http://www.espncricinfo.com/wt202009/engine/match/356017.html"

#"http://www.espncricinfo.com/world-twenty20-2010/engine/match/412678.html"

#"http://www.espncricinfo.com/world-twenty20-2010/engine/match/412703.html"

##Reading the data

dat1 <- NULL
bowling1 <- NULL
for (i in c(287853:287858,287860:287879)){
url <- paste("http://www.espncricinfo.com/twenty20wc/engine/match/",i,".html", sep = "")
tables <- readHTMLTable(url) 
bd1 <- tables$inningsBat1
res1 <- bd1[seq(3,25,by=2),-1]
res1$innings <- 1
res1$Team <- substring(tables$inningsBat1[,2][2],1,3)
res1$Opp <- substring(tables$inningsBat2[,2][2],1,3)
bowl1 <- tables$inningsBowl1
ball1 <- bowl1[seq(3,25,by=2),-1]
ball1$innings <- 1
bd2 <- tables$inningsBat2
res2 <- bd2[seq(3,25,by=2),-1]
res2$innings <- 2
res2$Team<-substring(tables$inningsBat2[,2][2],1,3)
res2$Opp<-substring(tables$inningsBat1[,2][2],1,3)
bowl2 <- tables$inningsBowl2
ball2 <- bowl2[seq(3,25,by=2),-1]
ball2$innings <- 2
dat1 <- rbind(dat1,cbind(rbind(res1,res2),match=i,year=2007))
bowling1 <- rbind(bowling1,cbind(rbind(ball1,ball2),match=i,year=2007))
}

dat2 <- NULL
bowling2<-NULL
for (i in c(355991:356017)){
url <- paste("http://www.espncricinfo.com/wt202009/engine/match/",i,".html", sep = "")
tables <- readHTMLTable(url) 
bd1 <- tables$inningsBat1
res1 <- bd1[seq(3,25,by=2),-1]
res1$innings <- 1
res1$Team <- substring(tables$inningsBat1[,2][2],1,3)
res1$Opp <- substring(tables$inningsBat2[,2][2],1,3)
bowl1 <- tables$inningsBowl1
ball1 <- bowl1[seq(3,25,by=2),-1]
ball1$innings <- 1
bd2 <- tables$inningsBat2
res2 <- bd2[seq(3,25,by=2),-1]
res2$innings <- 2
res2$Team<-substring(tables$inningsBat2[,2][2],1,3)
res2$Opp<-substring(tables$inningsBat1[,2][2],1,3)
bowl2 <- tables$inningsBowl2
ball2 <- bowl2[seq(3,25,by=2),-1]
ball2$innings <- 2
dat2 <- rbind(dat2,cbind(rbind(res1,res2),match=i,year=2009))
bowling2 <- rbind(bowling2,cbind(rbind(ball1,ball2),match=i,year=2009))
}

dat3 <- NULL
bowling3<-NULL
for (i in c(412678:412703)){
url <- paste("http://www.espncricinfo.com/world-twenty20-2010/engine/match/",i,".html", sep = "")
tables <- readHTMLTable(url) 
bd1 <- tables$inningsBat1
res1 <- bd1[seq(3,25,by=2),-1]
res1$innings <- 1
res1$Team <- substring(tables$inningsBat1[,2][2],1,3)
res1$Opp <- substring(tables$inningsBat2[,2][2],1,3)
bowl1 <- tables$inningsBowl1
ball1 <- bowl1[seq(3,25,by=2),-1]
ball1$innings <- 1
bd2 <- tables$inningsBat2
res2 <- bd2[seq(3,25,by=2),-1]
res2$innings <- 2
res2$Team<-substring(tables$inningsBat2[,2][2],1,3)
res2$Opp<-substring(tables$inningsBat1[,2][2],1,3)
bowl2 <- tables$inningsBowl2
ball2 <- bowl2[seq(3,25,by=2),-1]
ball2$innings <- 2
dat3 <- rbind(dat3,cbind(rbind(res1,res2),match=i,year=2010))
bowling3 <- rbind(bowling3,cbind(rbind(ball1,ball2),match=i,year=2010))
}
#head(dat1)

##Batting Data

##Binding the data together 
dat<-rbind(dat1,dat2,dat3)

dat1_s1_1<-subset(dat,match==287853)
dat1_s1_2<-subset(dat,match==287862)
dat1_s1_3<-subset(dat,match==412680)
dat1_s1_4<-subset(dat,match==412685)
dat1_s1<-rbind(dat1_s1_1,dat1_s1_2,dat1_s1_3,dat1_s1_4)
dat1_s2<-subset(dat,match!=287853 & match!=287862 & match!=412680 & match!=412685)
names(dat1_s1)<-c("Batsman","Kind_of_Dismissal","Runs","Balls","Fours","Sixes","Strike_Rate","Mins","innings","Team","Opp","match","Year")
names(dat1_s2)<-c("Batsman","Kind_of_Dismissal","Runs","Mins","Balls","Fours","Sixes","Strike_Rate","innings","Team","Opp","match","Year")
dat_f<-rbind(dat1_s1,dat1_s2)

dim(dat_f)
head(dat_f)

##Number of matches played in the 3 world cups

length(unique(dat_f$match))
length(unique(dat_f$Team))
length(unique(dat_f$Year))

##Changing the variables into numeric 

dat_f$Runs <- as.numeric(as.character(dat_f$Runs))
dat_f$Balls<- as.numeric(as.character(dat_f$Balls))
dat_f$Mins<-as.numeric(as.character(dat_f$Mins))


##Cleaning the data by keeping only Batsman scores

dat_clean<-subset(dat_f, Batsman!='Total' & Batsman!='Extras' )

dat_clean$Fours<-as.numeric(as.character(dat_clean$Fours))
dat_clean$Sixes<-as.numeric(as.character(dat_clean$Sixes))
dat_clean$Strike_Rate<-as.numeric(as.character(dat_clean$Strike_Rate))

##Number of matches played by each team

match_team<-ddply(dat_clean,.(Team),summarize,match_played=length(unique(match)))
match_year<-ddply(dat_clean,.(Year),summarize,match_played=length(unique(match)))
team_year<-ddply(dat_clean,.(Year,Team),summarize,match_played=length(unique(match)))
cast(team_year, Team ~ Year)

##Distribution of the variables

qplot(Runs, data=subset(dat_clean, !is.na(Runs)))
qplot(Fours,geom="bar",data=dat_clean)
qplot(Sixes, geom="bar",data=dat_clean)

##Scatterplots

qplot(Balls,Runs, data=subset(dat_clean, !is.na(Runs) & !is.na(Balls)), ylim=c(0,125), xlim=c(0,80), alpha=I(0.6)) + geom_abline(slope=1, colour="red") + geom_smooth(method="lm")

qplot(Mins,Balls, data=subset(dat_clean, !is.na(Mins) & !is.na(Balls)), ylim=c(0,110), xlim=c(0,110), alpha=I(0.6)) + geom_abline(slope=1, colour="red")


qplot(Balls, Strike_Rate, data=subset(dat_clean, !is.na(Balls) & !is.na(Strike_Rate)))

qplot(Balls, Strike_Rate, data=subset(dat_clean, !is.na(Balls) & !is.na(Strike_Rate)),  size=Sixes, alpha=I(0.6), colour=Fours) + scale_colour_gradient(low="blue", high="red")

qplot(Fours, Strike_Rate, data=dat_clean,  size=Sixes, alpha=I(0.4))

qplot(Sixes, Strike_Rate, data=dat_clean, size=Fours, alpha=I(0.4))

##Fitting a LS regression line

fit1<-lm(Strike_Rate~Fours + Sixes + Balls, data=dat_clean)
summary(fit1)

##Cleaning the data by removing Total

dat_e<-subset(dat_f, Batsman!='Total')
head(dat_e)
dim(dat_e)
dat_e$Runs <- as.numeric(as.character(dat_e$Runs))
dat_e<-subset(dat_e, !is.na(Runs))
dd_tot<-ddply(dat_e,.(match, innings),summarize, total=sum(Runs))

##Average Score by each innings

dd_sum<-ddply(dd_tot, .(innings), summarize, average_score=mean(total), max_score=max(total), min_score=min(total))

##Average Score in each innings by each team

dd_team<-ddply(dat_e,.(match,innings,Team),summarize, total=sum(Runs))
dd_ave_team<-ddply(dd_team, .(innings,Team), summarize, average_score=round(mean(total),0))
cast(dd_ave_team, Team ~ innings)

dd_max_team<-ddply(dd_team, .(innings,Team), summarize, max_score=max(total))
cast(dd_max_team, Team ~ innings)

dd_min_team<-ddply(dd_team, .(innings,Team), summarize, min_score=min(total))
cast(dd_min_team, Team ~ innings)

##Number of wins by batting or bowling

dd_win<-ddply(dd_tot, .(match), summarize, w1=total[innings==2]>total[innings==1])
dd_win$innings<-1
dd_win$innings[dd_win$w1=="TRUE"]<-2
dd_innings<-ddply(dd_win,.(innings),summarize,win=length(w1))

##Win by each team (CHECK)

dd_team<-ddply(dat_e,.(match,innings,Team),summarize, total=sum(Runs))
dd_win_team<-ddply(dd_team, .(match), summarize, w1=total[innings==2]>total[innings==1])
dd_win_team$innings<-1
dd_win_team$innings[dd_win$w1=="TRUE"]<-2

dd_win_team_inn<-merge(dd_team,dd_win_team,by=c("match", "innings"))
dd_win_team_inn_1<-ddply(dd_win_team_inn,.(innings,Team),summarize,win=length(w1))
cast(dd_win_team_inn_1, Team ~ innings)


####Practice


dd_tot_team<-ddply(dat_e,.(match, innings, Team),summarize, total=sum(Runs))

dd_win<-ddply(dd_tot_team, .(match), summarize, 
     response=sum(total[innings==2]<total[innings==1]), 
     team = as.factor(Team[innings==1]),
     venue = as.factor(c("Sou", "Eng", "Wes")[floor(match[innings==1]/100000) - 1]))
dd_win$innings<-1
dd_win$innings[dd_win$w1=="TRUE"]<-2
dd_innings<-ddply(dd_win,.(innings),summarize,win=length(w1))

mod<-glm(response ~ team + venue + team*venue, family=binomial, data=dd_win)


dd_win<-ddply(dd_tot_team, .(match), summarize, 
     team=as.factor(Team[which.max(total)]), 
     venue = as.factor(c("Sou", "Eng", "Wes")[floor(match[innings==1]/100000) - 1]))

prodplot(dd_win,  ~ team + venue) + aes(fill=team) 

####Bowling

bowl<-rbind(bowling1,bowling2, bowling3)

bowl<-bowl[ ,c(1:6,9:10)]
names(bowl)<-c("Bowler","Overs","Maidens","Runs_conceded","Wickets","Econ_rate","innings","match")
head(bowl,20)
dim(bowl)

bowl_f<-subset(bowl, !is.na(Overs))
dim(bowl_f)
is.numeric(bowl_f$Overs)

bowl_f$Overs <- as.numeric(as.character(bowl_f$Overs))
bowl_f$Maidens<- as.numeric(as.character(bowl_f$Maidens))
bowl_f$Runs_conceded<-as.numeric(as.character(bowl_f$Runs_conceded))
bowl_f$Wickets<-as.numeric(as.character(bowl_f$Wickets))
bowl_f$Econ_rate<-as.numeric(as.character(bowl_f$Econ_rate))

dd_bowl<-ddply(bowl_f,.(Bowler),summarize, total_wickets=sum(Wickets), Bowl_Ave=sum(Runs_conceded)/sum(Wickets), Total_Overs=sum(Overs),matches_played=length(match),wickets_per_match=round(sum(Wickets)/length(match),2))
best_bowler<-dd_bowl[order(dd_bowl$total_wickets,decreasing=T),]
best_bowler_20<-best_bowler[1:20,]
best_bowler_20

