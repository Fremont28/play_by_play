#import play by play data 
data_pbp<-read.csv(file.choose(),header=TRUE)
head(data_pbp)
levels(data_pbp$event_type)
library(ggplot2)
#convert x,y shot coords
data_pbp$converted_x<-as.numeric(as.character(data_pbp$converted_x))
data_pbp$converted_y<-as.numeric(as.character(data_pbp$converted_y))
player<-subset(data_pbp,(result=="made"|result=="missed") & player=="Enes Kanter")
player_davis<-player[grep("Kanter",player$description),]
ggplot(player_davis,aes(x=converted_x,y=converted_y,color=result))+geom_point()+
  xlab("x-coordinates")+ylab("y-coordinates")+ggtitle("Damian Lillard Shot Chart 2014-15")+
  theme(plot.title = element_text(hjust = 0.5))
  
#early in the shot clock looks?
#convert player length to numeric 
data_pbp$play_length<-as.numeric(data_pbp$play_length)
data_pbp$play_length<-data_pbp$play_length-1
#shots time<=5 seconds 
library(dplyr)
player1<-subset(player_davis,play_length<6)
ggplot(player1,aes(x=converted_x,y=converted_y,color=result))+geom_point()
summary(player1)
#shots 6 to 10 seconds (he gets less shots between 6 to 10 seconds)
player2<-subset(player_davis,play_length>=6 & play_length<11)
ggplot(player2,aes(x=converted_x,y=converted_y,color=result))+geom_point()+
  xlab("x-coordinates")+
  ylab("y-coordinates")+ggtitle("Enes Kanter Shot Attempts 6-10 seconds")+
  theme(plot.title = element_text(hjust = 0.5))
summary(player2)
#shots 11-15 seconds (shots spike)
player3<-subset(player_davis,play_length>=11 & play_length<16)
ggplot(player3,aes(x=converted_x,y=converted_y,color=result))+geom_point()+
  xlab("x-coordinates")+
  ylab("y-coordinates")+ggtitle("Enes Kanter Shot Attempts 11-15 seconds")+
  theme(plot.title = element_text(hjust = 0.5))
summary(player3)
#shots 
player4<-subset(player_davis,play_length>=16 & play_length<20)
ggplot(player4,aes(x=converted_x,y=converted_y,color=result))+geom_point()+xlab("x-coordinates")+
  ylab("y-coordinates")+ggtitle("Anthony Davis Shot Attempts 11-15 seconds")
summary(player4)
#davis shotclock?
player_davis1<-subset(player_davis,result=="missed"|result=="made")
play_length_davis<-subset(player_davis1,play_length>2)
ggplot(play_length_davis, aes(play_length)) +
  geom_histogram(bins=200)+xlab("Play Length (seconds)")
#histogram play_length
#play length=1 (mostly rebounds)
play_length<-subset(data_pbp,play_length>2)
ggplot(play_length, aes(play_length)) +
  geom_histogram(bins=200)

###1/18/18*******
#probability of making a shot?
head(data_pbp)
names(data_pbp)
levels(data_pbp$event_type)
shots<-subset(data_pbp,event_type=="shot",select=c("event_type","play_length","assist","steal","shot_distance","shot_distance",
                                                   "converted_x","converted_y","result"))
#create play id 
data_pbp$pbp_id <- seq.int(nrow(data_pbp))
#make,miss binary variable
data_pbp$miss_make<-ifelse(data_pbp$result=="made",1,0)

#1. what happens after a rebound?
#classify event 0,1 (miss,make)
follow_up$miss_make<-ifelse(follow_up$V8=="made",1,0)
#percent of shots made?
reb_shot<-subset(follow_up,V1=="shot"|V1=="miss") #8,100
reb_make<-subset(reb_shot,reb_shot$miss_make==1) #3750 
reb_shoot_pct<-3570/8100
reb_shoot_pct #~44.1% 

##insert player name
player_reb<-subset(follow_up,(V1=="shot"|V1=="miss") & V3=="Russell Westbrook")
reb_make_player<-subset(player_reb,player_reb$miss_make==1) 
dim(player_reb)
dim(reb_make_player)
#chris paul= 70.5% 
#j. holiday=37.5%
#t. parker= 88.9%
#g. dragic 80% 

###1/19/17****************
#create play id 
data_pbp$pbp_id <- seq.int(nrow(data_pbp))
#make,miss binary variable
data_pbp$miss_make<-ifelse(data_pbp$result=="made",1,0)
#does not contain free throw?
data_pbp1<-data_pbp$event_type[!grepl("free throw", data_pbp$event_type) & data_pbp$pbp_id>0]

#lagging
library(dplyr)
lag_play<-mutate(data_pbp,D=lag(data_pbp$event_type))
#relavant event sequences
levels(lag_play$event_type) #rebound,shot,miss, turnover,foul, violation, jump ball
#subset
lag_play1<-subset(lag_play,D=="rebound" | 
                    D =="foul" | D=="miss"| D=="shot" |D=="timeout"|D=="turnover"|
                    D=="violation" | D=="jump ball")
dim(lag_play1)

#subset for shots only
shots_sub<-subset(lag_play1,event_type=="shot" | event_type=="miss")
dim(shots_sub) #109306
levels(shots_sub$D)
write.csv(shots_sub,file="nba_pbp.csv")

shot_pct<-shots_sub%>% 
  group_by(miss_make) %>%
  summarise(n=n())
shot_pct

###clippers vs. kings shot patterns?
#lac-three-point heavy
#sac-paint heavy
#sacramento shots
sac_to_shots<-subset(shots_sub,shots_sub$D=="turnover" & shots_sub$team=="SAC")
dim(sac_to_shots)
ggplot(sac_to_shots,aes(x=converted_x,y=converted_y,color=result))+geom_point()
mean(sac_to_shots$shot_distance) #10.03 ft 

##opponent which player did sac "burn" the most?
sac_burn<-subset(shots_sub,team=="SAC" & miss_make==1)
dim(sac_burn)
count_opponent<-sac_burn%>% 
  group_by(opponent) %>%
  summarise(n=n())
count_opponent

#los angeles clippers shots
lac_to_shots<-subset(shots_sub,shots_sub$D=="turnover" & shots_sub$team=="LAC")
dim(lac_to_shots)
levels(lac_to_shots$D)
mean(lac_to_shots$shot_distance) #12.49 ft
ggplot(lac_to_shots,aes(x=converted_x,y=converted_y,color=result))+geom_point()


###after a steal?
levels(shots_sub$type)
steal_makes<-subset(shots_sub,event="steal")


## do teams shoot better after miss,make?******* struggling here 

#1.FG% (after rebounds)
make_shots<-subset(shots_sub,shots_sub$D=="timeout")
ggplot(make_shots,aes(x=converted_x,y=converted_y,color=result))+geom_point()
str(make_shots)
#shoot_pct?
count_miss_make<-make_shots%>% 
  group_by(miss_make) %>% 
  summarise(n=n())
count_miss_make 

##offense and defense cure????
make_shots1<-mutate(shots_sub,prev_shot=lag(shots_sub$miss_make))
dim(make_shots1)
#a. previous shot made, next shot?
prev_shot_made<-subset(make_shots1,make_shots1$prev_shot==1)
#shoot_pct
count_make_make<-prev_shot_made%>% 
  group_by(miss_make) %>% 
  summarise(n=n())
count_make_make #44.10 fg%

#b. previous shot missed,next shot?
prev_shot_miss<-subset(make_shots1,make_shots1$prev_shot==0)
#shoot_pct
count_miss_make1<-prev_shot_miss%>% 
  group_by(miss_make) %>% 
  summarise(n=n())
count_miss_make1 #46.1% 


#fg%= 45.86% rebounds
#fg%= 51.12% turnover
#fg%= 41.78% timeout
#fg%=45.22% field goal average 













