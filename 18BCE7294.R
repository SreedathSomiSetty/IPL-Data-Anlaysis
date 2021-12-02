#installing the required packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gapminder")
install.packages("lubridate")
install.packages("tidyverse")


#using the required packages
library(ggplot2)
library(dplyr)
library(gapminder)
library(lubridate)
library(tidyverse)


#importing the datasets of Indian Primier League
data_project_1<-read.csv("C:/Users/SREEDATH/Desktop/CSE-4027/DAproject/IPL Ball-by-Ball 2008-2020.csv")
data_project_2<-read.csv("C:/Users/SREEDATH/Desktop/CSE-4027/DAproject/IPL Matches 2008-2020.csv")


#load the dataset
data_project_1
data_project_2


#checking the characteristics of the datasets
class(data_project_1)
class(data_project_2)


#checking the dimensions of the datasets(number of rows and columns)
dim(data_project_1)
dim(data_project_2)


#summary and structures of the datasets
summary(data_project_1)
summary(data_project_2)
str(data_project_1)
str(data_project_2)


#head of the datasets
head(data_project_1)
head(data_project_2)


#cleaning the datasets by removing NAs from the dataframes
na.omit(data_project_1)
na.omit(data_project_2)

#highest number of toss wins by any team
data_project_2 %>%  group_by(toss_winner) %>%  summarise(No_Of_wins = n()) %>% filter(No_Of_wins == max(No_Of_wins))


#highest number of games as  umpire
data_project_2 %>%  group_by(umpire1) %>%  summarise(No_Of_times = n()) %>% filter(No_Of_times == max(No_Of_times))


#Table on number of matches held in cities
data_project_2 %>% group_by(city)  %>% summarise(No_of_times = n()) 


#Which decision is taken more often after winning toss
data_project_2 %>%  group_by(toss_decision) %>%  summarise(No_Of_times = n()) %>% filter(No_Of_times == max(No_Of_times))


#highest number of wins by any team
data_project_2 %>%  group_by(winner) %>%  summarise(No_Of_wins = n()) %>% filter(No_Of_wins == max(No_Of_wins))


#highest number of "player of the match" award for any player
data_project_2 %>% group_by(player_of_match) %>% summarise(No_of_Man_of_the_match = n()) %>% filter(No_of_Man_of_the_match == max(No_of_Man_of_the_match))


#barplot depicting the top 15 players with the most number of "player of the match" awards
data_project_2 %>% group_by(player_of_match) %>% summarise(No_of_Man_of_the_match = n()) %>% top_n(15) %>% ggplot(aes(x = reorder(player_of_match,No_of_Man_of_the_match),y = No_of_Man_of_the_match)) + geom_bar(stat = "identity",fill = "blue" )+coord_flip() + scale_x_discrete("players") + scale_y_continuous("No of times Man Of the Match")


#Graph of total wins by all teams
data_project_2 %>% group_by(winner) %>% summarise(wins = n()) %>% ggplot(aes(winner, wins, fill = winner)) + geom_bar(stat = "identity") + coord_flip() + scale_y_continuous("Total matches won")



#which team in dominating the game in any certain pitch(city)
data_project_2 %>% filter(result != 'no result') %>% group_by(winner,city) %>% summarise(win = n()) %>% arrange(desc(win))



#list of bowlers and their wickets taken in descending order
data_project_1 %>%group_by(bowler) %>% summarise(is_wicket = sum(is_wicket)) %>% arrange(desc(is_wicket))


#bar-plot of top 15 bowlers with highest number of wickets
data_project_1 %>%group_by(bowler) %>% summarise(is_wicket = sum(is_wicket)) %>% arrange(desc(is_wicket))%>% top_n(15) %>% ggplot(aes(x= reorder(bowler,is_wicket),y = is_wicket )) + geom_bar(stat = "identity",fill = "purple" )+coord_flip() + scale_x_discrete("players") + scale_y_continuous("Total wickets")



#list of batsmen and their total runs in descending order
data_project_1 %>% group_by(batsman) %>% summarise(batsman_runs = sum(batsman_runs)) %>% arrange(desc(batsman_runs))


#barplot of top 15 batsman with highest number of runs
data_project_1 %>%group_by(batsman) %>% summarise(batsman_runs = sum(batsman_runs)) %>% arrange(desc(batsman_runs)) %>% top_n(15) %>% ggplot(aes(x= reorder(batsman,batsman_runs),y = batsman_runs )) + geom_bar(stat = "identity",fill = "orange" )+coord_flip() + scale_x_discrete("players") + scale_y_continuous("Total runs")





#which over gets more number of wickets
data_project_1 %>% group_by(over) %>% summarise(is_wicket = sum(is_wicket)) %>% arrange(desc(is_wicket)) %>% top_n(20) 

#bar-plot of number of wickets taken in respective overs
data_project_1 %>% group_by(over) %>% summarise(is_wicket = sum(is_wicket))%>% ggplot(aes(over, is_wicket, fill =over)) + geom_bar(stat = "identity") + coord_flip() + scale_y_continuous("wickets")

#which over gets the highest number of runs
data_project_1 %>% group_by(over) %>% summarise(total_runs = sum(total_runs)) %>% arrange(desc(total_runs)) %>% top_n(20) 

#bar-plot of number of runs scored in respective overs
data_project_1 %>% group_by(over) %>% summarise(total_runs = sum(total_runs)) %>% ggplot(aes(over, total_runs, fill =over)) + geom_bar(stat = "identity") + coord_flip() + scale_y_continuous("runs scored")



#scatter plot on win margin of teams through out the years 

team_margin<-ggplot(data_project_2, aes(x=date , y = result_margin , col = winner)) + geom_point()
team_margin


#total runs scored in ipl from 2008 to 2020
total_runs_ipl<-sum(data_project_1$total_runs)

#table of all wickets taken in ipl and all types of wickets taken
wickets_ipl<-filter(data_project_1, is_wicket == 1)
wickets_ipl
total_wickets_ipl<-sum(data_project_1$is_wicket==1)
dismissal_bowled<-filter(data_project_1,dismissal_kind == "bowled")
da<-nrow(dismissal_bowled)
dismissal_caught<-filter(data_project_1,dismissal_kind == "caught")
db<-nrow(dismissal_caught)
dismissal_caughtnbowled<-filter(data_project_1,dismissal_kind == "caught and bowled")
dc<-nrow(dismissal_caughtnbowled)
dismissal_hit_wicket<-filter(data_project_1,dismissal_kind == "hit wicket")
dd<-nrow(dismissal_hit_wicket)
dismissal_lbw<-filter(data_project_1,dismissal_kind == "lbw")
de<-nrow(dismissal_lbw)
dismissal_obstructing_the_field<-filter(data_project_1,dismissal_kind == "obstructing the field")
df<-nrow(dismissal_obstructing_the_field)
dismissal_retired_hurt<-filter(data_project_1,dismissal_kind == "retired hurt")
dg<-nrow(dismissal_retired_hurt)
dismissal_run_out<-filter(data_project_1,dismissal_kind == "run out")
dh<-nrow(dismissal_run_out)
dismissal_stumped<-filter(data_project_1,dismissal_kind == "stumped")
di<-nrow(dismissal_stumped)

#===============================================================================================================================
#whole stat of top team
top_team<-data_project_2 %>%  group_by(winner) %>%  summarise(No_Of_wins = n()) %>% filter(No_Of_wins == max(No_Of_wins))

total_runs_ipl <- sum(data_project_1$total_runs)
total_runs_ipl



install.packages("naniar")
library(naniar)



ipl_boxplot<-ggplot(data = data_project_1)+geom_boxplot(mapping = aes(x = factor(total_runs),y = total_runs_ipl,factor(total_runs)))+labs(title = " run distribution ",x="forms",y="Number of runs")

#-----------------------------------------

team_mi<-filter(data_project_2,winner == "Mumbai Indians")
team_mi_wins<-nrow(team_mi)
team_mi_wins
team_mi
format(as.Date(team_mi$date, format="%d/%m/%Y"),"%Y")
team_mi
team_mi_plot<-team_mi(date,)



aes(x=date , y = result_margin , col = winner)

team_margin<-ggplot(data_project_2, aes(x=date , y = result_margin , col = winner)) + geom_point()
team_margin
#================================
time <- as.numeric(rep(seq(1,7),each=7))  # x Axis
value <- runif(49, 10, 100)               # y Axis
group <- rep(LETTERS[1:7],times=7)        # group, one shape per group
data <- data.frame(date, value, winner)

# stacked area chart
ggplot(data, aes(x=time, y=value, fill=group)) + 
  geom_area()
#------------------------------------------------------
team_mi %>% ggplot( aes(x=year, y=winner, group=name, color=name)) +geom_line() +
  scale_color_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum()
#----------------------------------------------------------------------------
virat_kohli<-filter(data_project_1,batsman == "V Kohli")
virat_kohli
virat_kohli_total<-c(sum(virat_kohli$total_runs))
virat_kohli_total
#ggplot(virat_kohli, aes(x=date , y = result_margin , col = winner)) + geom_point()
ggplot(virat_kohli, aes(x=as.factor(total_runs), y=over)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("total_runs")

ggplot(data_project_1, aes(x=as.factor(total_runs), y=over)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("total_runs")+ylab("over number")



#total runs scored in ipl from 2008 to 2020
total_runs_ipl<-sum(data_project_1$total_runs)

#table of all wickets taken in ipl and all kinds of wickets
wickets_ipl<-filter(data_project_1, is_wicket == 1)
wickets_ipl
total_wickets_ipl<-sum(data_project_1$is_wicket==1)
dismissal_bowled<-filter(data_project_1,dismissal_kind == "bowled")
da<-nrow(dismissal_bowled)
dismissal_caught<-filter(data_project_1,dismissal_kind == "caught")
db<-nrow(dismissal_caught)
dismissal_caughtnbowled<-filter(data_project_1,dismissal_kind == "caught and bowled")
dc<-nrow(dismissal_caughtnbowled)
dismissal_hit_wicket<-filter(data_project_1,dismissal_kind == "hit wicket")
dd<-nrow(dismissal_hit_wicket)
dismissal_lbw<-filter(data_project_1,dismissal_kind == "lbw")
de<-nrow(dismissal_lbw)
dismissal_obstructing_the_field<-filter(data_project_1,dismissal_kind == "obstructing the field")
df<-nrow(dismissal_obstructing_the_field)
dismissal_retired_hurt<-filter(data_project_1,dismissal_kind == "retired hurt")
dg<-nrow(dismissal_retired_hurt)
dismissal_run_out<-filter(data_project_1,dismissal_kind == "run out")
dh<-nrow(dismissal_run_out)
dismissal_stumped<-filter(data_project_1,dismissal_kind == "stumped")
di<-nrow(dismissal_stumped)

ggplot(data_project_1, aes(x=as.factor(total_runs), y=over)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("total_runs")

ggplot(data_project_1, aes(x = dismissal_kind, y = is_wicket)) + geom_boxplot(fill = "bisque") 

ggplot(data_project_1, aes(x=as.factor(dismissal_kind), y=is_wicket)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("dismissal kind")

total_catches_ipl<-sum(data_project_1$dismissal_kind == caught)


#----------------------------------------------------------
#***multiple bar graphs on Toss decision taken by teams after winning the toss in the respective cities
toss_lol <- data_project_2%>%filter(city=="Mumbai"|city=="Bangalore"|city=="Delhi"|city=="Chennai"|city=="Kolkata"|city=="Jaipur"|city=="Hyderabad"|city=="Chandigarh"|city=="Pune")%>%ggplot() +
  geom_bar(aes(x=toss_decision,fill=toss_winner))+facet_wrap(~city)
toss_lol




#--------------------------------------------------------------
#How many times a team won the toss won the match?
#does winning toss really increase the winning percentage?
toss_win_match <- data_project_2%>%filter(toss_winner==winner)
match_winner_after_winning_toss=nrow(toss_win_match)

toss_lose_match <- data_project_2%>%filter(toss_winner!=winner)
loosing_match_after_wining_toss=nrow(toss_lose_match)

data.frame(match_winner_after_winning_toss ,loosing_match_after_wining_toss)

(match_winner_after_winning_toss/(match_winner_after_winning_toss+loosing_match_after_wining_toss))*100
#as it is hyped winning toss does'nt effect the match much which increases their chance only by 1.5%

#--------------------------------------------------------------------
 


team_margin<-ggplot(data_project_2, aes(x=date , y = result_margin , col = winner)) + geom_point()
team_margin



ggplot(data_project_2, aes(x = date , y = result_margin , col = winner)) +
  geom_point() +
  stat_smooth()


#------------------------------------------------------------------------------------------------
batsmen <- data_project_1 %>%group_by(batsman) %>% summarise(batsman_runs = sum(batsman_runs)) %>% arrange(desc(batsman_runs))  %>% ggplot(aes(x= reorder(batsman,batsman_runs),y = batsman_runs )) + geom_bar(stat = "identity",fill = "orange" )+coord_flip() + scale_x_discrete("players") + scale_y_continuous("Total runs")
batsmen


batsmen_table <- data_project_1 %>% group_by(batsman) %>% summarise(batsman_runs = sum(batsman_runs),batsmen_fours = sum(total_runs == 4),batsmen_sixes = sum(total_runs == 6)) %>% arrange(desc(batsman_runs))
batsmen_table

ggplot(batsmen_table, aes(x = batsman_runs , y = batsmen_fours )) +
  geom_point() +
  stat_smooth()

#linear regression
rmodel4 <- lm(batsman_runs~batsmen_fours,data=batsmen_table)
par(mfrow = c(2,2))
print(rmodel4)

summary(rmodel4)
plot(rmodel4)

#prediction
pred4 <- predict(rmodel4,batsmen_table)
pred4
#plotting
#plot(batsmen_table$batsmen_fours.type= "l")
plot(pred4,type = "l",lty = 4,col="blue" )

#---------------------------------------------------------------------
ggplot(batsmen_table, aes(x = batsman_runs , y = batsmen_sixes )) +
  geom_point() +
  stat_smooth()
#linear regression
rmodel6 <- lm(batsman_runs~batsmen_sixes,data=batsmen_table)
par(mfrow = c(2,2))
print(rmodel6)

summary(rmodel6)

plot(rmodel6)

#prediction
pred6 <- predict(rmodel6,batsmen_table)
pred6
#plotting
#plot(batsmen_table$batsmen_sixes.type= "l")
plot(pred6,type = "l",lty = 1.8,col="blue" )

covr<-(cov(batsmen_table$batsman_runs,batsmen_table$batsmen_sixes))/100000

covr

#-----------------------------------------------------------------------
only_fours <- filter(data_project_1,total_runs == 4)
only_sixes <- filter(data_project_1,total_runs == 6)
  
batsmen4 <- only_fours %>%group_by(batsman) %>% summarise(batsman_runs = sum(batsman_runs)) %>% arrange(desc(batsman_runs))  %>% ggplot(aes(x= reorder(batsman,batsman_runs),y = batsman_runs )) + geom_bar(stat = "identity",fill = "orange" )+coord_flip() + scale_x_discrete("players") + scale_y_continuous("Total runs")
batsmen4

batsmen6 <- only_sixes %>%group_by(batsman) %>% summarise(batsman_runs = sum(batsman_runs)) %>% arrange(desc(batsman_runs))  %>% ggplot(aes(x= reorder(batsman,batsman_runs),y = batsman_runs )) + geom_bar(stat = "identity",fill = "orange" )+coord_flip() + scale_x_discrete("players") + scale_y_continuous("Total runs")
batsmen6
