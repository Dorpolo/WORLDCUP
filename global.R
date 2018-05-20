# =========================================================================
# Load libraries and scripts
# =========================================================================


library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(shinythemes)
library(DT)
library(scales)


my_theme <- function() {
  
  # Colors
  color.background = "white"
  color.text = "#22211d"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "none") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
# =========================================================================

fixtures <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX",
                                "-1vQ4jRITA24Oj_h-i4cVxEGstFTS7-qKH0bv_pp61h-Jj4G",
                                "0t-fLh6TUiZU-Qor1WA2pt50TJkENnCkh/pub?gid=0&single=true&output=csv")),
                     stringsAsFactors = FALSE)

resultes <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vTQKuVDYTvMW9n",
                                "B24QGAQ0M5l8xgeUGYbCjGTX3dxeQ1jaklYy-989osaccZgcwVelbzpxq4nZEtP",
                                "xw/pub?gid=0&single=true&output=csv")),
                     stringsAsFactors = FALSE)

resultes_knokout <- as.data.frame(read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vSOkvRyeKlUP",
                                                      "vNwNScSsL-_FfXYOtVGRZqI2NUSq7NkxHS8T4If7Q0GJUhCWkiqgpFGHYkF3AgVXyPS/",
                                                      "pub?gid=0&single=true&output=csv"))),stringsAsFactors = FALSE)

User_ID <- as.data.frame(read.csv(url(paste0('https://docs.google.com/spreadsheets/d/e/2PACX-1vSG0P3o9q0tK',
                                             'Hek7MJ7oFHOgmDy-1tBaG1eXVlg2Fbh64iDQafupr9JFlWZznyPjfg-Lf59WmTA3aV0/',
                                             'pub?gid=0&single=true&output=csv'))),
                         stringsAsFactors = FALSE)


resultes <- resultes %>% left_join(User_ID %>% select(Full.Name,User_Nick),by = c('User.Name'='Full.Name'))

# Need to be added in production #
{
  ### resultes <- resultes[1:nrow(User_ID),]
  ### resultes_knokout <- resultes_knokout[1:nrow(User_ID),]
}


#### Adjustments between results_knockout col names to game names ###

knokout_col_adj <- data_frame(GameID = fixtures$GameID[49:64],
                              original_fixture_name = fixtures$NameID[49:64],
                              resultes_knokout_name = (names(resultes_knokout)[-c(1,2)])[seq(1,32,by=2)])


#list which containes all application data frames

all_df_list <- list(userID =   list(df = User_ID),
                    resultes = list(df = resultes),
                    fixtures = list(df = fixtures))

# Special Functions
## Points per game per user
{
  
  worldcup_points <- function(x)
  {
    if(x$started %in% c("TRUE"))
    {
      user <- c(x$user_Home_Goals,x$user_Away_Goals,x$user_Direction)
      true <- c(x$true_Home_Goals,x$true_Away_Goals,x$true_Direction)
      if(x$Stage == "Group Stage")
      {
        return(ifelse(sum(user == true) == 3,3,ifelse(user[3] == true[3],1,0)))
      }else{
        return(ifelse(sum(user == true) == 3 ,
                      4,
                      ifelse(user[3] == true[3] | (user[1:2] == true[1:2] & user[3] != true[3]),
                             1,
                             0)))
      }
    }else{return(0)}
  }
}


# Union between true & user resultes , create one version of anlysis ready table by each relevant parameter
names(all_df_list$resultes$df) <- gsub("..."," - ",names(all_df_list$resultes$df),fixed = TRUE)
names(all_df_list$resultes$df) <- gsub("."," ",names(all_df_list$resultes$df),fixed = TRUE)
names(all_df_list$userID$df) <- gsub("."," ",names(all_df_list$userID$df),fixed = TRUE)

####### All Results from group stage  ##########



resultes_edited <- all_df_list$resultes$df %>%
  melt(id = c("User Name","Submission ID")) %>%
  separate(variable,c("user_Home","user_Away")," - ") %>%
  separate(value,c("user_Home_Goals","user_Away_Goals"),"-") %>%
  mutate(user_Direction = ifelse(`user_Home_Goals`>`user_Away_Goals`,"Home",
                                 ifelse(`user_Home_Goals`==`user_Away_Goals`,"Draw",
                                        "Away")),
         NameID = paste0(user_Home," - ",user_Away)) %>% 
  inner_join(all_df_list$fixtures$df,
             by = c("NameID")) %>% left_join(User_ID %>% select(User = `Full.Name`,User_Nick),by = c('User Name'='User'))

####### All Results from Knockout stage  ##########

resultes_knokout_edited = resultes_knokout %>% 
  gather(variable, value, -c(User.Name,Submission.ID)) %>% 
  select(`User Name` = User.Name,
         `Submission ID` = Submission.ID,variable,value) %>% 
  filter(!(value %in% c('NA',"") | is.na(value) ) ) %>% 
  mutate(Game = substr(variable,1,regexpr("_",variable)+1) ) %>% 
  group_by(`User Name`,`Submission ID`,Game) %>% 
  summarise(variable = paste0(value,collapse = ";")) %>% 
  separate(variable,c("variable","user_Direction_pre"),";") %>% 
  inner_join(knokout_col_adj,by = c('Game'='resultes_knokout_name')) %>%
  separate(original_fixture_name,c("user_Home","user_Away")," - ") %>% 
  separate(variable,c("user_Home_Goals","user_Away_Goals"),"-") %>% 
  mutate(NameID = paste0(user_Home," - ",user_Away)) %>% 
  mutate(user_dir_validation = ifelse(`user_Home_Goals`>`user_Away_Goals`,"Home",
                                      ifelse(`user_Home_Goals`==`user_Away_Goals`,"Draw",
                                             "Away")),
         user_Direction = ifelse(user_dir_validation == 'Draw',user_Direction_pre,user_dir_validation)) %>%
  select(`User Name`,`Submission ID`,user_Home,user_Away,user_Home_Goals,user_Away_Goals,user_Direction,NameID) %>%
  inner_join(all_df_list$fixtures$df,by = c("NameID")) %>% 
  left_join(User_ID %>% select(User = `Full.Name`,User_Nick),by = c('User Name'='User'))


resultes_edited <- bind_rows(resultes_edited,resultes_knokout_edited)

# Points vector, for each game,user. will be mutate to the "resultes_edited" table



user_game_points <- rep(NA,nrow(resultes_edited))

for(i in 1:nrow(resultes_edited)){
  user_game_points[i] <- worldcup_points(resultes_edited[i,])}



user_results_validation <- resultes_edited %>% mutate(user_game_points,
                                                      boom = ifelse(user_game_points>1,TRUE,FALSE),
                                                      winning_goals = ifelse(boom == TRUE,as.numeric(user_Home_Goals)+
                                                                               as.numeric(true_Away_Goals),0))


# User Table
N_users = length(unique(resultes$User.Name))

# Number off Game Overs

N_games_complited = sum(fixtures$started == TRUE & fixtures$active == FALSE)
N_games = sum(fixtures$started == TRUE)

league_standings <- user_results_validation %>% filter(Active_Included == "Complited Games") %>% 
  group_by(`User_Nick`) %>% 
  summarise(Points = sum(user_game_points),
            Boom = sum(boom),
            Winning_Goals = sum(winning_goals),
            Games = sum(started == TRUE)) %>% arrange(desc(Points),
                                                      desc(Boom),
                                                      desc(Winning_Goals)) %>% mutate(Rank = 1:N_users)

only_rank_for_vlookup <- league_standings %>% select(User_Nick,Rank)

date.id <- fixtures %>% select(Date,GameID) %>% arrange(GameID) %>% 
  distinct(Date,.keep_all = TRUE) %>% mutate(Day = rank(GameID)) %>% select(-GameID)

user_results_validation$user_Home_Goals <- as.numeric(user_results_validation$user_Home_Goals)
user_results_validation$user_Away_Goals <- as.numeric(user_results_validation$user_Away_Goals)
user_results_validation$true_Home_Goals <- as.numeric(user_results_validation$true_Home_Goals)
user_results_validation$true_Away_Goals <- as.numeric(user_results_validation$true_Away_Goals)

team_table_by_users_pre <- user_results_validation %>% filter(Stage == "Group Stage") %>% 
  mutate(Home_Real_Points = ifelse(user_Home_Goals>user_Away_Goals,3,
                                   ifelse(user_Home_Goals<user_Away_Goals,0,1)),
         Away_Real_Points = ifelse(Home_Real_Points == 3,0,
                                   ifelse(Home_Real_Points == 0,3,1))) %>% 
  mutate(GD_Home = user_Home_Goals-user_Away_Goals,
         GD_Away = -GD_Home) %>% 
  select(`User_Nick`,Group,Home=true_Home,Away=true_Away,Home_Real_Points,Away_Real_Points,GD_Home,GD_Away)

home = team_table_by_users_pre %>% select(User = `User_Nick`,Group,Team = Home,Points = Home_Real_Points,GD = GD_Home)
away = team_table_by_users_pre %>% select(User = `User_Nick`,Group,Team = Away,Points = Away_Real_Points,GD = GD_Away)


team_table_by_users <- bind_rows(home,away)


### All User Guesses ### 
temp_resultes_knokout <- (resultes_knokout %>% 
                            select(`Submission ID` = Submission.ID,
                                   `User Name`=User.Name,everything()))[,
                                                                        c(1,2,seq(3,16*2+2,2))]

names(temp_resultes_knokout)[-c(1,2)] <- fixtures$NameID[49:64]


user_guesses <- all_df_list$resultes$df %>% 
  select(-`Submission ID`) %>% 
  melt(id = 'User_Nick') %>% select(User = `User_Nick`,Match = variable, Resulte = value) %>%
  inner_join(all_df_list$fixtures$df %>% select(Match = NameID,Stage,Group,Date,Hour,GameID),
             by = c("Match")) %>% select(User,GameID,Stage,Group,Date,Hour,Match,Resulte) %>% 
  arrange(GameID)

choices <- list(
  Round = unique(user_results_validation$Round),
  Group =unique(user_results_validation$Group),
  nameID = unique(user_results_validation$NameID),
  userID = unique(sort(user_results_validation$`User_Nick`)),
  Active =  unique(sort(user_results_validation$Active_Included))
)

worldcup_palette <- list(backround =   list(main = "#4e5d6c"),
                         lines =       list(darkblue = "#2b3e50",
                                            red_cup =  "#a8251f",
                                            light_blue_cup = "#006da8"),
                         data_labels = list(darkblue = "#00384A",
                                            light_green = "#B0D494",
                                            light_brune = "#B6A78D"))

################################################################################

### Very Importent - If there is an active game will indicate on him, else - will indicate on the next comming game ### 

current_game <- ifelse(all(resultes_edited$active == FALSE),
                       max(resultes_edited$GameID[which(resultes_edited$started==TRUE)])+1,
                       resultes_edited$GameID[which(resultes_edited$active == TRUE)[1]])

current_Game_Name <- (resultes_edited %>% filter(GameID == current_game) %>% select(NameID))[1,1]

################################################################################

game_date_day_id <- fixtures %>% select(GameID,Date) %>% left_join(date.id,by = c('Date'))

# current competiton date

current_competition_day <- game_date_day_id$Day[which(game_date_day_id$GameID == current_game)][1]

# Help Data Frame for the rank table

user_results_validation_b <- user_results_validation %>% left_join(date.id,by = c('Date'))

# Empty array for user ranks by competition day

rank <- array(data = NA ,dim = c(N_users,current_competition_day),
              dimnames = list(resultes$User_Nick,1:current_competition_day))


for(i in 1:current_competition_day)
{
  league_standings <- user_results_validation_b %>% filter(Active_Included == "Complited Games" & Day.y <= i) %>% 
    group_by(User_Nick) %>% 
    summarise(Points = sum(user_game_points),
              Boom = sum(boom),
              Winning_Goals = sum(winning_goals),
              Games = sum(started == TRUE)) %>% arrange(desc(Points),
                                                        desc(Boom),
                                                        desc(Winning_Goals)) %>% mutate(Rank = 1:N_users)
  for(j in 1:N_users)
  {
    
    rank[j,i] <- league_standings$Rank[which(league_standings$User_Nick == names(provideDimnames(rank)[,1])[j])]
  }
}

rank <- as.data.frame(cbind(row.names(rank),rank))
rownames(rank) <- c()
names(rank)[1] <- 'user'

user_rank_by_day <- rank %>% melt(id = c('user')) %>% select(User=user,Day=variable,Rank=value) %>%
  mutate(alpha = 1,
         x   = ifelse(current_competition_day<7,0.975,ifelse(current_competition_day<15,0.85,0.75)) ,
         x.2 = ifelse(current_competition_day<7,current_competition_day+0.025,ifelse(current_competition_day<15,
                                                                                     current_competition_day+0.15,
                                                                                     current_competition_day+0.25) ))

user_rank_by_day$Rank <- as.numeric(user_rank_by_day$Rank)

#########################  CUP POINTS #################################


current_cup_stage <- fixtures$Cup_ID[which(fixtures$GameID == current_game)]  


cup <- user_results_validation %>% 
  select(User = `User_Nick`,GameID,Cup_ID,Points = user_game_points) %>% 
  mutate(CUPID = substr(Cup_ID,1,1)) %>% 
  filter(CUPID != '0') %>% 
  group_by(User,CUPID) %>% summarise(Points = sum(Points)) %>% arrange(CUPID,desc(Points))


cup_points <- user_results_validation %>% 
  select(User = `User_Nick`,
         GameID,
         CupID=Cup_ID,
         Points = user_game_points,
         Boom=boom,
         `Winning Goals` = winning_goals) %>% 
  mutate(CUPID = substr(CupID,1,1)) %>% 
  filter(CUPID != '0') %>%  mutate(User_Cup_ID = paste0(User,"_",CUPID)) %>% 
  left_join(resultes_edited %>% select(User=`User_Nick`,GameID,NameID,true_Home_Goals,true_Away_Goals,
                                       Home = user_Home_Goals,
                                       Away = user_Away_Goals),by=c('User','GameID')) %>% 
  mutate(Bet = paste0(Home,"-",Away),
         Result = paste0(true_Home_Goals,"-",true_Away_Goals)) %>% select(-c(true_Home_Goals,true_Away_Goals,Home,Away))


data_cup <- cup_points %>% left_join(User_ID %>% 
                                       select(User = User_Nick,Draw_32)) %>% arrange(Draw_32) 
  

vec <- rep(c(rep(TRUE,4),rep(FALSE,4)),nrow(data_cup)/8)

data_cup_a <- data_cup[which(vec==TRUE),] %>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_32,Game=NameID,Result)
data_cup_b <- data_cup[which(vec==FALSE),]%>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_32,Game=NameID,Result)

the_data <- data_cup_a %>% inner_join(data_cup_b,by=c('GameID','CupID','Game','Result','Draw_32')) %>% select(-c(GameID,CupID,Draw_32))

