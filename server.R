
#Data Preperation
{
#Required Packages
{
  library(shiny)
  library(ggplot2)
  library(reshape2)
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(shinythemes)
  library(DT)
  library(scales)
  library(visNetwork)
  library(lubridate)
  library(XML)
  library(RCurl)
  library(httr)
}

#Special 'home made' functions
{
  # ggplot2 graph template
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
  
  ## Points counter per game per user

  
  # Special colurs
  worldcup_palette <- list(backround =   list(main = "#4e5d6c"),
                           lines =       list(darkblue = "#2b3e50",
                                              red_cup =  "#a8251f",
                                              light_blue_cup = "#006da8"),
                           data_labels = list(darkblue = "#00384A",
                                              light_green = "#B0D494",
                                              light_brune = "#B6A78D"))
  
  cup_palate <- list(winner = '#82E0AA',
                     loser  = '#CD6155',
                     draw = '#EB984E',
                     game_over ='#FEF5E7',
                     future_game = '#F7F9F9')
  
  
  # Dor's Rank
  {
    bet_score <- c("5-4", "5-3", "5-2", "5-1", "5-0",
                   "4-3", "4-2", "4-1", "4-0",
                   "3-2", "3-1", "3-0", 
                   "2-1", "2-0",
                   "1-0",
                   "5-5", "3-3", "1-1", "0-0", "2-2", "4-4",
                   "0-1",
                   "0-2", "1-2",
                   "0-3", "1-3", "2-3",
                   "0-4", "1-4", "2-4", "3-4",
                   "0-5", "1-5", "2-5", "3-5", "4-5")
  }
  
}

### Loading Data Sets from Google Sheets
{
  
  # All Paricipants
  User_ID <- as.data.frame(read.csv(url(paste0('https://docs.google.com/spreadsheets/d/e/2PACX-1vSG0P3o9q0tK',
                                               'Hek7MJ7oFHOgmDy-1tBaG1eXVlg2Fbh64iDQafupr9JFlWZznyPjfg-Lf59WmTA3aV0/',
                                               'pub?gid=0&single=true&output=csv'))),
                           stringsAsFactors = FALSE)
  
  # All Games Fixtures + Resultes
  fixtures <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX",
                                  "-1vQ4jRITA24Oj_h-i4cVxEGstFTS7-qKH0bv_pp61h-Jj4G",
                                  "0t-fLh6TUiZU-Qor1WA2pt50TJkENnCkh/pub?gid=0&single=true&output=csv")),
                       stringsAsFactors = FALSE)
  
  initial_fixtures <- fixtures
  

  
  resultes <-   read.csv(url(paste0('https://docs.google.com/spreadsheets/d/e/2',
  'PACX-1vTQKuVDYTvMW9nB24QGAQ0M5l8xgeUGYbCjGTX3dxeQ1j',
  'aklYy-989osaccZgcwVelbzpxq4nZEtPxw/pub?gid=0&single=true&output=csv')),
  stringsAsFactors = FALSE)


  
  # Adjestment
  
  resultes  <- resultes  %>% select(-c(Winner,Top_Scorer))
  resultes <- resultes %>% left_join(User_ID %>% select(Full.Name,User_Nick),by = c('User.Name'='Full.Name'))
  
  # Users' score predictions - Knockout Stage
  resultes_knokout <- as.data.frame(read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vSkadUbrVQBA",
                                                        "-QGTzBipY49WPaGziS5uOox5hHx-Ib8vL5sCDNqiWfR1LYbcSZV6AgjlI8CiZxbbI5s/",
                                                        "pub?gid=0&single=true&output=csv"))),stringsAsFactors = FALSE)
  
  # Help table - Adjestment for the resultes_knokout
  
  knokout_col_adj <- fixtures %>% filter(Stage != "Group Stage") %>% 
    select(GameID,
           original_fixture_name = NameID)  %>% 
    mutate(resultes_knokout_name = (names(resultes_knokout)[-c(1,2)])[seq(1,32,by=2)])
  
  
  # List which holds all DFs
  
  all_df_list <- list(userID =   list(df = User_ID),
                      resultes = list(df = resultes),
                      fixtures = list(df = fixtures))
  
  # Final adjustments due to the read.csv which makes some problems...
  names(all_df_list$resultes$df) <- gsub("..."," - ",names(all_df_list$resultes$df),fixed = TRUE)
  names(all_df_list$resultes$df) <- gsub("."," ",names(all_df_list$resultes$df),fixed = TRUE)
  names(all_df_list$userID$df) <- gsub("."," ",names(all_df_list$userID$df),fixed = TRUE)
  


  
  # and we are all set for data manipulation
}

# all UI filter selection options
{
  choices <- list(
    nameID = unique(fixtures$NameID),
    userID = c(levels(sort(User_ID$`User_Nick`))),
    Active =  unique(sort(fixtures$Active_Included)),
    Cup = unique((fixtures %>% filter(Cup_Stage != "None"))$Cup_Stage)
  )
}

# resultes_edited - crucial data join between user's predictions & real results
{
  # Results edited join between the fixtures and the user results (Group Stage), for each user, in each game 
  
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
  
  # Results knokout edited join between the fixtures and the user results (knokout), for each user, in each game 
  
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
  
  # Creating one version of truth - comparing between user predictions to real results and connect the Group Stage & Knokout bulks
  
  resultes_edited <- bind_rows(resultes_edited,resultes_knokout_edited)
  
  # adding Points vector with the user's points for each user in each match
  
  # Empty vector
  user_game_points <- rep(NA,nrow(resultes_edited))
  
  # apply worldcup points function for each row in the results edited
  for(i in 1:nrow(resultes_edited)){
    user_game_points[i] <- worldcup_points(resultes_edited[i,])}
  
  user_results_validation <- resultes_edited %>% mutate(user_game_points,
                                                        boom = ifelse(user_game_points>1,TRUE,FALSE),
                                                        winning_goals = ifelse(boom == TRUE,as.numeric(user_Home_Goals)+
                                                                                 as.numeric(true_Away_Goals),0))
  
}

# Extracting competition parameters 
{
  N_users <- nrow(User_ID)
  N_games_complited = sum(fixtures$started == TRUE & fixtures$active == FALSE)
  N_games = sum(fixtures$started == TRUE)
}

# User's league table #asItStands 
{
  league_standings <- user_results_validation  %>% 
    group_by(`User_Nick`) %>% 
    summarise(Points = sum(user_game_points),
              Boom = sum(boom),
              Winning_Goals = sum(winning_goals),
              Games = sum(started == TRUE)) %>% arrange(desc(Points),
                                                        desc(Boom),
                                                        desc(Winning_Goals)) %>% mutate(Rank = 1:N_users)
}

# Will serve us in the main user league output
only_rank_for_vlookup <- league_standings %>% select(User_Nick,Rank)

# Date Ranking for user rank graph
date.id <- fixtures %>% select(Date,GameID) %>% arrange(GameID) %>% 
  distinct(Date,.keep_all = TRUE) %>% mutate(Day = rank(GameID)) %>% select(-GameID)

# column adjusments (user_results_validation)
{
  user_results_validation$user_Home_Goals <- as.numeric(user_results_validation$user_Home_Goals)
  user_results_validation$user_Away_Goals <- as.numeric(user_results_validation$user_Away_Goals)
  user_results_validation$true_Home_Goals <- as.numeric(user_results_validation$true_Home_Goals)
  user_results_validation$true_Away_Goals <- as.numeric(user_results_validation$true_Away_Goals)
  }

# World Cup Teams Points Calculations (For the tab called "Users Prediction")
{
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
}

### All User Guesses ###
{
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
}

# 'Current' Parameters - current game, current game name, current cup rank, etc
{
  ### Very Importent - If there is an active game will indicate on him, else - will indicate on the next comming game ### 
  
  current_game <- ifelse(all(resultes_edited$active == FALSE),
                         max(resultes_edited$GameID[which(resultes_edited$started==TRUE)])+1,
                         resultes_edited$GameID[which(resultes_edited$active == TRUE)[1]])
  
  current_Game_Name <- (resultes_edited %>% filter(GameID == current_game) %>% select(NameID))[1,1]
  
  current_cup_rank <-   fixtures$Cup_Rank[which(fixtures$GameID == current_game)]
}

### Data preperation for the "Rank users by competiton day" Graph
{
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
    league_standings <- user_results_validation_b %>% filter((Active_Included == "Complited Games" | Active_Included == 'Active Games') & Day.y <= i) %>% 
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
}
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  autoInvalidate <- reactiveTimer(200000)
  
  # Genrate Offical User's League
  output$league_table <- DT::renderDataTable({
    
    autoInvalidate()
    
    # All Games Fixtures + Resultes
    fixtures <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX",
                                    "-1vQ4jRITA24Oj_h-i4cVxEGstFTS7-qKH0bv_pp61h-Jj4G",
                                    "0t-fLh6TUiZU-Qor1WA2pt50TJkENnCkh/pub?gid=0&single=true&output=csv")),
                         stringsAsFactors = FALSE)
    
    all_df_list$fixtures$df <- fixtures
    
    
    ### Fixtures & Score API Joined 
      {
      fixtures <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vTNgD6oZivKRepzwPWDc",
                                      "YMg4tOQQq8B3sJLFdtHHE7p8lYs4lv_C4Wk_B3lkAPx-nZA",
                                      "4O6DETuEAhxw/pub?gid=0&single=true&output=csv")),
                           stringsAsFactors = FALSE)
      
      partial_fixtures <- fixtures %>% select(-c(started,active,Active_Included,true_Home_Goals,true_Away_Goals,true_Direction))
      
      partial_fixtures$new_id <- as.character(partial_fixtures$new_id)
      
      url <- 'https://perodriguezl-russia-2018-worldcup-tournament-v1.p.mashape.com/public/tournament/1'
      r <-  GET(url,add_headers(`X-Mashape-Key` = "GfEVgWsBovmshScrSarHgMWOihvjp17eUsvjsnwtfD1M1esYX9"))
      fixt <-  as.data.frame(t(as.data.frame(unlist(content(r)$data[[1]]))),
                             row.names = NULL)
      
      for( i in 2:64)
      {
        b <- as.data.frame(t(as.data.frame(unlist(content(r)$data[[i]]))),
                           row.names = NULL)
        fixt <- bind_rows(fixt,b)
      }
      
      fixt$countdown <- as.numeric(fixt$countdown)
      fixtures$new_id <- as.character(fixtures$new_id)
      fixt_to_be_added <- fixt %>% select(GameID = id,
                                          true_Home_Goals = results.home ,
                                          true_Away_Goals = results.visitor ,
                                          finished,
                                          countdown) %>% mutate(started = ifelse(countdown == 0,TRUE,FALSE),
                                                                active  = ifelse(finished  == FALSE & countdown == 0,TRUE,FALSE)) %>% 
        left_join(fixtures %>% select(new_id,NameID),by = c('GameID'='new_id'))   %>% mutate(Active_Included = ifelse(started == TRUE & active == FALSE,'Complited Games',
                                                                                                                      ifelse(started == TRUE & active == TRUE,'Active Games',
                                                                                                                             'Future Games'))) %>% 
        mutate(true_Direction  = ifelse(true_Home_Goals>true_Away_Goals,"Home",
                                        ifelse(true_Home_Goals<true_Away_Goals,"Away",
                                               "Draw"))) %>%
        select(GameID,NameID,countdown,finished,started,active,Active_Included,true_Home_Goals,true_Away_Goals,true_Direction)
      
      partial_fixtures$new_id <- as.character(partial_fixtures$new_id)
      
      final <- partial_fixtures %>% inner_join(fixt_to_be_added,by = c('new_id'='GameID'))
      
      fixtures <- final
      all_df_list$fixtures$df <- final
    }

    # resultes_edited - data join between user's predictions & real results
    {
      # Results edited join between the fixtures and the user results (Group Stage), for each user, in each game 
      
      resultes_edited <- all_df_list$resultes$df %>%
        melt(id = c("User Name","Submission ID")) %>%
        separate(variable,c("user_Home","user_Away")," - ") %>%
        separate(value,c("user_Home_Goals","user_Away_Goals"),"-") %>%
        mutate(user_Direction = ifelse(`user_Home_Goals`>`user_Away_Goals`,"Home",
                                       ifelse(`user_Home_Goals`==`user_Away_Goals`,"Draw",
                                              "Away")),
               NameID = paste0(user_Home," - ",user_Away)) %>% 
        inner_join(all_df_list$fixtures$df,
                   by = c("NameID"="NameID.y")) %>% left_join(User_ID %>% select(User = `Full.Name`,User_Nick),by = c('User Name'='User'))
      
      # Results knokout edited join between the fixtures and the user results (knokout), for each user, in each game 
      
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
        inner_join(all_df_list$fixtures$df,by = c("NameID"="NameID.y")) %>% 
        left_join(User_ID %>% select(User = `Full.Name`,User_Nick),by = c('User Name'='User'))
      
      # Creating one version of truth - comparing between user predictions to real results and connect the Group Stage & Knokout bulks
      
      resultes_edited <- bind_rows(resultes_edited,resultes_knokout_edited)
      
      resultes_edited$user_Away_Goals <-   as.numeric(resultes_edited$user_Away_Goals)
      resultes_edited$user_Home_Goals <-   as.numeric(resultes_edited$user_Home_Goals)
      resultes_edited$true_Direction <- as.character(resultes_edited$true_Direction)
      
      # adding Points vector with the user's points for each user in each match
      
      # Empty vector
      user_game_points <- rep(NA,nrow(resultes_edited))
      

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
      
      
      # apply worldcup points function for each row in the results edited
      for(i in 1:nrow(resultes_edited)){
        user_game_points[i] <- worldcup_points(resultes_edited[i,])}
      
      
      
      user_results_validation <- resultes_edited %>% mutate(user_game_points,
                                                            boom = ifelse(user_game_points>1,TRUE,FALSE),
                                                            winning_goals = ifelse(boom == TRUE,as.numeric(user_Home_Goals)+
                                                                                     as.numeric(true_Away_Goals),0))
      
      
      
    }
    
    # THE OFFICAL LEAGUE TABLE - INCLUDING REAL TIME UPDATES #
    
    # User's league table #asItStands 
    {
      league_standings <- user_results_validation %>%  
        group_by(`User_Nick`) %>% 
        summarise(Points = sum(user_game_points),
                  Boom = sum(boom),
                  Winning_Goals = sum(winning_goals),
                  Games = sum(started == TRUE)) %>% arrange(desc(Points),
                                                            desc(Boom),
                                                            desc(Winning_Goals)) %>% mutate(Rank = 1:N_users)
    }
    
    # Will serve us in the main user league output
    only_rank_for_vlookup <- league_standings %>% select(User_Nick,Rank)
    
    
    
    DT::datatable(data = user_results_validation %>% 
                    group_by(`User_Nick`) %>% 
                    summarise(Points = sum(user_game_points),
                              Boom = sum(boom),
                              WG = sum(winning_goals),
                              Pct = ifelse(as.numeric(Sys.time()) < 1528920303 + 54000,
                                           percent(0),
                                percent(sum(user_game_points)/(4*sum((Active_Included=="Complited Games" | Active_Included=="Active Games"))-sum((Active_Included=="Complited Games" | Active_Included=="Active Games") & Stage=="Group Stage")))),
                              Games = sum(started == TRUE)) %>% 
                    arrange(desc(Points),
                            desc(Boom),
                            desc(WG)) %>% 
                    left_join(only_rank_for_vlookup,by = c("User_Nick")) %>% 
                    left_join(User_ID %>% select(User_Nick,Img),by = c("User_Nick")) %>% 
                    select(Rank,User=Img,Name = `User_Nick`,everything()),
                  options = list(pageLength = N_users,
                                 lengthChange=FALSE,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#F70443', 'color': '#FFFF00'});",
                                   "}"),
                                 searching = FALSE,
                                 columnDefs = list(list(width = 200, targets =  "_all" ),
                                                   list(className = 'dt-center', targets = "_all")),
                                 scrollX=TRUE,
                                 scrollCollapse=TRUE,
                                 lengthMenu = c(10,20,N_users)), 
                  rownames = FALSE,
                  escape = FALSE,
                  class = 'cell-border stripe') %>% 
      formatStyle('Rank',
                  target = 'row',
                  color  = styleEqual(c(1,2:5,6:N_users),c('#641E16',rep('#00384A',4),rep('#00384A',(N_users-6+1))))) %>%
      formatStyle('Rank',
                  target = 'row',
                  fontWeight = styleEqual(c(1:5),rep("bold",5)))
    
    
    
    
  })
  
  # Reactive Data frame for the User predictions and the User Rank plot
  polo_3 <- reactive({user_rank_by_day %>% filter(`User` %in% input$userID)})
  
  output$lineplot <- renderPlot({
    
    ggplot(data = polo_3(), aes(x = Day, y = Rank, group = User)) +
      geom_line(aes(color =  User,alpha = alpha), size = 2) +
      geom_point(aes(color = User,alpha = alpha), size = 4) +
      geom_point(color = "#FFFFFF", size = 1) +
      scale_y_reverse(breaks=1:N_users) + 
      scale_x_discrete(breaks = 1:current_competition_day, expand = c(.05, .05)) + 
      geom_text(data =  polo_3() %>% filter(Day == "1"),
                aes(label = User, x = x) , hjust = .85, color = "#00384A", size = 3) +
      geom_text(data = polo_3() %>% filter(Day == current_competition_day),
                aes(label = User, x = x.2) , hjust = 0.15, color = "#00384A", size = 3) +
      coord_cartesian(ylim = c(1,N_users)) + 
      theme(legend.position = "none") +
      labs(x = "Competition Days with games",
           y = "Rank",
           title = "World Cup as a Service",
           subtitle = "Users ranked by overall points after each competition Day") + my_theme()
    
  })
  
  # Reactive Data Frame for the User Predictions Tab
  polo_4 <- reactive({ team_table_by_users  %>%
      filter(User %in% input$userID) 
  })
  
  # User Predictions Output I
  
  output$table.a <- renderDataTable({
    datatable(data =polo_4() %>% group_by(Group,Team) %>%
                summarise(Points = sum(Points),
                          GD = sum(GD)) %>% arrange(Group,desc(Points),desc(GD)) %>% 
                mutate(Rank=row_number(),`Won Stage` = ifelse(Rank %in% c(1,2),"Yes","No")) %>%
                select(Rank,everything()),
              options = list(pageLength = 32,
                             searching = FALSE,
                             columnDefs = list(list(width = '5%', targets = "_all"),
                                               list(className = 'dt-center', targets = "_all")),
                             list(visible=FALSE, targets=c(1,6)),
                             scrollX=TRUE,
                             scrollCollapse=TRUE), 
              rownames = FALSE,
              class = 'cell-border stripe') %>% 
      formatStyle("Won Stage",
                  target = 'row',
                  color = styleEqual(c('Yes','No'),c('green','red'))) 
  })
  
  user_predictions <- user_guesses %>% left_join(fixtures %>% select(GameID,Active_Included),by = c('GameID'))
  
  # User Guesses Tab II 
  
  polo_5 <- reactive({ user_predictions %>%
      filter(User %in% input$userID) %>% select(-c(GameID,Hour,Stage))
  })
  
  # User Guesses Tab Output II 
  
  output$user_guess <- renderDataTable({
    
    datatable(data = polo_5() %>% mutate(finished = ifelse(Active_Included=="Complited Games",'Yes','No')) %>% select(-Active_Included),
              options = list(pageLength = 48,
                             searching = FALSE,
                             columnDefs = list(list(width = '10px', targets = "_all"),
                                               list(className = 'dt-center', targets = "_all"))), 
              rownames = FALSE,
              class = 'cell-border stripe') %>% 
      formatStyle("finished",
                  target = 'row',
                  backgroundColor  = styleEqual(c('Yes','No'),c("#E3D7E5","white")),
                  color = "#00384A") 
    
  })
  
  ################ Dorki ###########################
  {
  next_game_tbl <- reactive({
    resultes_edited %>%
      filter(NameID %in% input$nameID) %>%
      mutate(team_direction = ifelse(user_Direction == "Away",
                                     user_Away,
                                     ifelse(user_Direction == "Home",
                                            user_Home, user_Direction
                                     )
      )) %>%
      group_by(team_direction) %>%
      summarise(count = n()) %>%
      mutate(
        sum = sum(count),
        proportion = round(count / sum(count), 3) * 100
      )
  })
  
  
  #### Viz output I
  
  output$pre_game_I <- renderPlot({
    ggplot(next_game_tbl(), aes(y = proportion, x = "", fill = team_direction)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      geom_label(aes(label = paste(team_direction, "\n", round((count / sum * 100), 0), "%")),
                 position = position_stack(vjust = 0.5),
                 size = 4,
                 color = "white",
                 fontface = "bold"
      ) +
      scale_fill_manual(values = c("#A11B1F", "#DEBF83", "#0191D1")) +
      theme(
        plot.margin = unit(c(0,0,0,0), "mm"),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "#272B30", colour = "#272B30"),
        plot.title = element_text(hjust = 0.5, size = 50),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#272B30", colour = "#272B30"),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_blank()
      )
  })
  
  
  ### Score distribution 
  
  #### Data prep
  
  resultes_edited$bet_score <- with(resultes_edited, paste(user_Home_Goals,
                                                           "-",
                                                           user_Away_Goals,
                                                           sep = ""
  ))
  
  position_bar <- seq(1:length(bet_score))
  sort_by_helper <- as.data.frame(cbind(bet_score, position_bar))
  sort_by_helper$c <- as.character(sort_by_helper$bet_score)
  sort_by_helper$position_bar <- as.numeric(as.character(sort_by_helper$position_bar))
  
  #### Table prep
  
  next_game_unique_res <- reactive({
    resultes_edited %>%
      filter(NameID == input$nameID) %>%
      select(bet_score) %>%
      distinct(bet_score)
  })
  
  next_game_scores <- reactive({
    resultes_edited %>%
      filter(NameID == input$nameID) %>%
      group_by(bet_score) %>%
      summarise(count = n()) %>%
      mutate(
        sum = sum(count),
        proportion = round(count / sum(count), 3) * 100
      ) %>%
      left_join(sort_by_helper, by = "bet_score") %>%
      arrange(desc(position_bar)) %>%
      mutate(bet_score = factor(bet_score,
                                levels = bet_score[1:nrow(next_game_unique_res())]
      ))
  })
  
  
  ############## Viz output II
  
  output$pre_game_II <- renderPlot({
    par(bg = "#2b3e50")
    ggplot(next_game_scores(), aes(y = proportion, x = bet_score, fill = count)) +
      geom_col(position = "dodge") +
      scale_fill_continuous(low = "#CDAD72", high = "#A72A28") +
      geom_text(aes(label = count),
                position = position_dodge(width = 1), hjust = -0.5,
                size = 5, color = "white"
      ) +
      theme(
        plot.margin = unit(c(0,0,0,0), "mm"),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "#272B30", colour = "#272B30"),
        plot.title = element_text(hjust = 0.5, size = 20, color = "white"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#272B30", colour = "#272B30"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "white"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none"
      ) +
      coord_flip()
  })
  
  ############# Pre Game Name ###################
  
  output$game_name <- renderUI({
    
    game = as.character(input$nameID)
    HTML(paste0('<div style = "background-color: #F70443; width: 100%; height: 25px; border-radius: 4px;">
                <center> <font size="4"> <font color="#F8F9F9">',game,
                '</div>'))
    
    
    
  })
  
  }
  
  
  ############# Title for Live Game #############
  
  output$title <- renderUI({
    
    autoInvalidate()
    
    
    ##
    all_df_list$fixtures$df <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX",
                                                   "-1vQ4jRITA24Oj_h-i4cVxEGstFTS7-qKH0bv_pp61h-Jj4G",
                                                   "0t-fLh6TUiZU-Qor1WA2pt50TJkENnCkh/pub?gid=0&single=true&output=csv")),
                                        stringsAsFactors = FALSE)
    
    fixtures <-  all_df_list$fixtures$df 
    
    
    ### Fixtures & Score API Joined 
    {
      fixtures <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vTNgD6oZivKRepzwPWDc",
                                      "YMg4tOQQq8B3sJLFdtHHE7p8lYs4lv_C4Wk_B3lkAPx-nZA",
                                      "4O6DETuEAhxw/pub?gid=0&single=true&output=csv")),
                           stringsAsFactors = FALSE)
      
      partial_fixtures <- fixtures %>% select(-c(started,active,Active_Included,true_Home_Goals,true_Away_Goals,true_Direction))
      
      partial_fixtures$new_id <- as.character(partial_fixtures$new_id)
      
      url <- 'https://perodriguezl-russia-2018-worldcup-tournament-v1.p.mashape.com/public/tournament/1'
      r <-  GET(url,add_headers(`X-Mashape-Key` = "GfEVgWsBovmshScrSarHgMWOihvjp17eUsvjsnwtfD1M1esYX9"))
      fixt <-  as.data.frame(t(as.data.frame(unlist(content(r)$data[[1]]))),
                             row.names = NULL)
      
      for( i in 2:64)
      {
        b <- as.data.frame(t(as.data.frame(unlist(content(r)$data[[i]]))),
                           row.names = NULL)
        fixt <- bind_rows(fixt,b)
      }
      
      fixt$countdown <- as.numeric(fixt$countdown)
      fixtures$new_id <- as.character(fixtures$new_id)
      fixt_to_be_added <- fixt %>% select(GameID = id,
                                          true_Home_Goals = results.home ,
                                          true_Away_Goals = results.visitor ,
                                          finished,
                                          countdown) %>% mutate(started = ifelse(countdown == 0,TRUE,FALSE),
                                                                active  = ifelse(finished  == FALSE & countdown == 0,TRUE,FALSE)) %>% 
        left_join(fixtures %>% select(new_id,NameID),by = c('GameID'='new_id'))   %>% mutate(Active_Included = ifelse(started == TRUE & active == FALSE,'Complited Games',
                                                                                                                      ifelse(started == TRUE & active == TRUE,'Active Games',
                                                                                                                             'Future Games'))) %>% 
        mutate(true_Direction  = ifelse(true_Home_Goals>true_Away_Goals,"Home",
                                        ifelse(true_Home_Goals<true_Away_Goals,"Away",
                                               "Draw"))) %>%
        select(GameID,NameID,countdown,finished,started,active,Active_Included,true_Home_Goals,true_Away_Goals,true_Direction)
      
      partial_fixtures$new_id <- as.character(partial_fixtures$new_id)
      
      final <- partial_fixtures %>% inner_join(fixt_to_be_added,by = c('new_id'='GameID'))
      
      fixtures <- final
      all_df_list$fixtures$df <- final
    }
    
    # resultes_edited - data join between user's predictions & real results
    {
      # Results edited join between the fixtures and the user results (Group Stage), for each user, in each game 
      
      resultes_edited <- all_df_list$resultes$df %>%
        melt(id = c("User Name","Submission ID")) %>%
        separate(variable,c("user_Home","user_Away")," - ") %>%
        separate(value,c("user_Home_Goals","user_Away_Goals"),"-") %>%
        mutate(user_Direction = ifelse(`user_Home_Goals`>`user_Away_Goals`,"Home",
                                       ifelse(`user_Home_Goals`==`user_Away_Goals`,"Draw",
                                              "Away")),
               NameID = paste0(user_Home," - ",user_Away)) %>% 
        inner_join(all_df_list$fixtures$df,
                   by = c("NameID"="NameID.y")) %>% left_join(User_ID %>% select(User = `Full.Name`,User_Nick),by = c('User Name'='User'))
      
      # Results knokout edited join between the fixtures and the user results (knokout), for each user, in each game 
      
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
        inner_join(all_df_list$fixtures$df,by = c("NameID"="NameID.y")) %>% 
        left_join(User_ID %>% select(User = `Full.Name`,User_Nick),by = c('User Name'='User'))
      
      # Creating one version of truth - comparing between user predictions to real results and connect the Group Stage & Knokout bulks
      
      resultes_edited <- bind_rows(resultes_edited,resultes_knokout_edited)
      
      resultes_edited$user_Away_Goals <-   as.numeric(resultes_edited$user_Away_Goals)
      resultes_edited$user_Home_Goals <-   as.numeric(resultes_edited$user_Home_Goals)
      resultes_edited$true_Direction <- as.character(resultes_edited$true_Direction)
      
      # adding Points vector with the user's points for each user in each match
      
      # Empty vector
      user_game_points <- rep(NA,nrow(resultes_edited))
      
      
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
      
      
      # apply worldcup points function for each row in the results edited
      for(i in 1:nrow(resultes_edited)){
        user_game_points[i] <- worldcup_points(resultes_edited[i,])}
      
      
      
      user_results_validation <- resultes_edited %>% mutate(user_game_points,
                                                            boom = ifelse(user_game_points>1,TRUE,FALSE),
                                                            winning_goals = ifelse(boom == TRUE,as.numeric(user_Home_Goals)+
                                                                                     as.numeric(true_Away_Goals),0))
      
      
      
    }
    
    ##
    
    # Extracting competition parameters 
    {
      N_users <- nrow(User_ID)
      N_games_complited = sum(fixtures$started == TRUE & fixtures$active == FALSE)
      N_games = sum(fixtures$started == TRUE)
    }
    
    # 'Current' Parameters - current game, current game name, current cup rank, etc
    {
      ### Very Importent - If there is an active game will indicate on him, else - will indicate on the next comming game ### 
      
      current_game <- ifelse(all(resultes_edited$active == FALSE),
                             max(resultes_edited$GameID[which(resultes_edited$started==TRUE)])+1,
                             resultes_edited$GameID[which(resultes_edited$active == TRUE)[1]])
      
      current_Game_Name <- (resultes_edited %>% filter(GameID == current_game) %>% select(NameID))[1,1]
      
      current_cup_rank <-   fixtures$Cup_Rank[which(fixtures$GameID == current_game)]
    }
    
    text <- paste0(fixtures$true_Home[current_game],
                   " ",
                   fixtures$true_Home_Goals[current_game],
                   " - ",
                   fixtures$true_Away_Goals[current_game],
                   " ",
                   fixtures$true_Away[current_game])
    
    hour <- hour(Sys.time()) + 3
    minute <- ifelse(nchar(minute(Sys.time())) == 1,paste0(0,minute(Sys.time())),minute(Sys.time()))
    
    countdown_to <- paste0('Countdown - ',round(fixtures$countdown[current_game]/(60),0)," minutes ")
    
    if(fixtures$countdown[current_game] == 0)
    {
      html_text <- paste0('<div style = "background-color: #F70443; width: 100%; height: 75px; border-radius: 4px;">
                <center> <font size="4"> <font color="#F8F9F9"> Live Score : <br>',text,'<br>',hour,":", minute,
                     '</div>')
    }else{
      html_text <- paste0('<div style = "background-color: #F70443; width: 100%; height: 75px; border-radius: 4px;">
                <center> <font size="4"> <font color="#F8F9F9"> Next Game : <br>',text,'<br>',countdown_to,
                     '</div>')
    }
   
                     
                
    HTML(html_text)


    

    
   
  })
  
  ############# Live Game Indicator #############
  
  output$result_table <- DT::renderDataTable({
    
    autoInvalidate()
    
    
    fixtures <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX",
                                    "-1vQ4jRITA24Oj_h-i4cVxEGstFTS7-qKH0bv_pp61h-Jj4G",
                                    "0t-fLh6TUiZU-Qor1WA2pt50TJkENnCkh/pub?gid=0&single=true&output=csv")),
                         stringsAsFactors = FALSE)
    
    all_df_list$fixtures$df  <- fixtures
    
    ### Fixtures & Score API Joined 
    {
      fixtures <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vTNgD6oZivKRepzwPWDc",
                                      "YMg4tOQQq8B3sJLFdtHHE7p8lYs4lv_C4Wk_B3lkAPx-nZA",
                                      "4O6DETuEAhxw/pub?gid=0&single=true&output=csv")),
                           stringsAsFactors = FALSE)
      
      partial_fixtures <- fixtures %>% select(-c(started,active,Active_Included,true_Home_Goals,true_Away_Goals,true_Direction))
      
      partial_fixtures$new_id <- as.character(partial_fixtures$new_id)
      
      url <- 'https://perodriguezl-russia-2018-worldcup-tournament-v1.p.mashape.com/public/tournament/1'
      r <-  GET(url,add_headers(`X-Mashape-Key` = "GfEVgWsBovmshScrSarHgMWOihvjp17eUsvjsnwtfD1M1esYX9"))
      fixt <-  as.data.frame(t(as.data.frame(unlist(content(r)$data[[1]]))),
                             row.names = NULL)
      
      for( i in 2:64)
      {
        b <- as.data.frame(t(as.data.frame(unlist(content(r)$data[[i]]))),
                           row.names = NULL)
        fixt <- bind_rows(fixt,b)
      }
      
      fixt$countdown <- as.numeric(fixt$countdown)
      fixtures$new_id <- as.character(fixtures$new_id)
      fixt_to_be_added <- fixt %>% select(GameID = id,
                                          true_Home_Goals = results.home ,
                                          true_Away_Goals = results.visitor ,
                                          finished,
                                          countdown) %>% mutate(started = ifelse(countdown == 0,TRUE,FALSE),
                                                                active  = ifelse(finished  == FALSE & countdown == 0,TRUE,FALSE)) %>% 
        left_join(fixtures %>% select(new_id,NameID),by = c('GameID'='new_id'))   %>% mutate(Active_Included = ifelse(started == TRUE & active == FALSE,'Complited Games',
                                                                                                                      ifelse(started == TRUE & active == TRUE,'Active Games',
                                                                                                                             'Future Games'))) %>% 
        mutate(true_Direction  = ifelse(true_Home_Goals>true_Away_Goals,"Home",
                                        ifelse(true_Home_Goals<true_Away_Goals,"Away",
                                               "Draw"))) %>%
        select(GameID,NameID,countdown,finished,started,active,Active_Included,true_Home_Goals,true_Away_Goals,true_Direction)
      
      partial_fixtures$new_id <- as.character(partial_fixtures$new_id)
      
      final <- partial_fixtures %>% inner_join(fixt_to_be_added,by = c('new_id'='GameID'))
      
      fixtures <- final
      all_df_list$fixtures$df <- final
    }
    
    
    # resultes_edited - data join between user's predictions & real results
    {
      # Results edited join between the fixtures and the user results (Group Stage), for each user, in each game 
      
      resultes_edited <- all_df_list$resultes$df %>%
        melt(id = c("User Name","Submission ID")) %>%
        separate(variable,c("user_Home","user_Away")," - ") %>%
        separate(value,c("user_Home_Goals","user_Away_Goals"),"-") %>%
        mutate(user_Direction = ifelse(`user_Home_Goals`>`user_Away_Goals`,"Home",
                                       ifelse(`user_Home_Goals`==`user_Away_Goals`,"Draw",
                                              "Away")),
               NameID = paste0(user_Home," - ",user_Away)) %>% 
        inner_join(all_df_list$fixtures$df,
                   by = c("NameID"="NameID.y")) %>% left_join(User_ID %>% select(User = `Full.Name`,User_Nick),by = c('User Name'='User'))
      
      # Results knokout edited join between the fixtures and the user results (knokout), for each user, in each game 
      
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
        inner_join(all_df_list$fixtures$df,by = c("NameID"="NameID.y")) %>% 
        left_join(User_ID %>% select(User = `Full.Name`,User_Nick),by = c('User Name'='User'))
      
      # Creating one version of truth - comparing between user predictions to real results and connect the Group Stage & Knokout bulks
      
      resultes_edited <- bind_rows(resultes_edited,resultes_knokout_edited)
      
      resultes_edited$user_Away_Goals <-   as.numeric(resultes_edited$user_Away_Goals)
      resultes_edited$user_Home_Goals <-   as.numeric(resultes_edited$user_Home_Goals)
      resultes_edited$true_Direction <- as.character(resultes_edited$true_Direction)
      
      # adding Points vector with the user's points for each user in each match
      
      # Empty vector
      user_game_points <- rep(NA,nrow(resultes_edited))
      
      
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
      
      
      # apply worldcup points function for each row in the results edited
      for(i in 1:nrow(resultes_edited)){
        user_game_points[i] <- worldcup_points(resultes_edited[i,])}
      
      
      
      user_results_validation <- resultes_edited %>% mutate(user_game_points,
                                                            boom = ifelse(user_game_points>1,TRUE,FALSE),
                                                            winning_goals = ifelse(boom == TRUE,as.numeric(user_Home_Goals)+
                                                                                     as.numeric(true_Away_Goals),0))
      
      
      
    }
    
    ##
    
    r_16 <- resultes_knokout[1:N_users,2:18]
    r_8  <- resultes_knokout[(N_users+1):(2*N_users),c(2,19:26)]
    r_4  <- resultes_knokout[(2*N_users+1):(3*N_users),c(2,27:30)]
    r_2  <- resultes_knokout[(3*N_users+1):(5*N_users),c(2,31:34)]
    
    knokout <- r_16 %>% inner_join(r_16) %>%
      inner_join(r_8) %>%
      inner_join(r_4) %>%
      inner_join(r_2) %>% left_join(User_ID %>% select(Full.Name,User_Nick),by=c('User.Name'='Full.Name')) %>%
      select(-User.Name) %>% select(User = User_Nick,everything())
    
    names_knok <- initial_fixtures$NameID[49:nrow(initial_fixtures)]
    dup_names_knok <- paste0(names_knok," - winner")
    names(knokout) <- c('User_Nick',sort(c(names_knok,dup_names_knok)))
    
    
  
    # Extracting competition parameters 
    {
      N_users <- nrow(User_ID)
      N_games_complited = sum(fixtures$started == TRUE & fixtures$active == FALSE)
      N_games = sum(fixtures$started == TRUE)
    }
    
    # 'Current' Parameters - current game, current game name, current cup rank, etc
    {
      ### Very Importent - If there is an active game will indicate on him, else - will indicate on the next comming game ### 
      
      current_game <- ifelse(all(fixtures$active == FALSE),
                             max(resultes_edited$GameID[which(resultes_edited$started==TRUE)])+1,
                             resultes_edited$GameID[which(resultes_edited$active == TRUE)[1]])
      
      current_Game_Name <- (resultes_edited %>% filter(GameID == current_game) %>% select(NameID))[1,1]
      
      current_cup_rank <-   fixtures$Cup_Rank[which(fixtures$GameID == current_game)]
    }
    
    
    all_games <- all_df_list$resultes$df %>% inner_join(knokout,
                                                        by = c('User_Nick'))
    
    
    
    test_game_pre_result <- all_games %>% select(User = 'User_Nick',current_Game_Name) 
    ##
    
    test_game <- test_game_pre_result %>% 
      separate(current_Game_Name,into = c('U1.Home','U1.Away'),remove = FALSE) %>%
      mutate(T.Home = as.numeric(fixtures$true_Home_Goals[which(initial_fixtures$NameID == current_Game_Name)]),
             T.Away = as.numeric(fixtures$true_Away_Goals[which(initial_fixtures$NameID == current_Game_Name)]),
             U.Home = as.numeric(U1.Home),
             U.Away = as.numeric(U1.Away),
             D.Home = T.Home-U.Home,
             D.Away = T.Away-U.Away) %>%
      filter(D.Home<= 0 & D.Away<=0) %>% select(User,current_Game_Name)
    
    true_result <- fixtures[which(initial_fixtures$NameID == names(test_game)[2]),
                            c(which(names(fixtures) == 'true_Home_Goals'),
                              which(names(fixtures)== 'true_Away_Goals'))]
    
    res_dist <- test_game %>% mutate(Home = true_result$true_Home_Goals,
                                     Away = true_result$true_Away_Goals) %>%
      separate(names(test_game)[2],into=c('H.Res','A.Res'),sep="-") %>% 
      mutate(Result = paste0(H.Res,"-",A.Res),
             dist = abs(as.numeric(Home)-as.numeric(H.Res)) + 
               abs(as.numeric(Away)-as.numeric(A.Res))) %>% select(User,Result,dist) %>%
      distinct(Result,dist)
    
    test_game_II <- test_game
    names(test_game_II) <- c('User','Result')
    
    user_predictions <-  test_game_II %>% left_join(res_dist) %>% arrange(dist) %>% left_join(User_ID %>% select(User = User_Nick,Img)) %>% select(User = Img,Name = User , Prediction = Result)
    
    
    
    DT::  datatable(data = user_predictions,
                    rownames = FALSE,
                    escape = FALSE,
                    class = 'cell-border stripe',
                    options = list( searching = FALSE,
                                    lengthChange=FALSE,
                                    autoWidth = TRUE,
                                    columnDefs = list(list(width = "10px", targets =  "_all" ),
                                                      list(className = 'dt-center', targets = "_all"))))     %>% 
      formatStyle("User",
                  target = 'row',
                  color =    "#00384A" )     %>% 
      formatStyle("User",
                  target = 'row',
                  backgroundColor =    "#FEF9E7" )     %>%
      formatStyle('User',
                  target = 'row',
                  fontWeight ="bold")
    
  })
  
  #############  Terms of Use Pictures   ############# 
  output$terms1 <- renderUI({tags$img(src= 'http://farm2.staticflickr.com/1755/40553342420_2b7812f781_b.jpg',height = "600px",width = "380px")})
  output$terms2 <- renderUI({tags$img(src= 'http://farm2.staticflickr.com/1733/27490778887_06c5899c54_b.jpg',height = "600px",width = "380px")})
  output$terms3 <- renderUI({tags$img(src= 'http://farm2.staticflickr.com/1727/40553341900_72491ce6f0_b.jpg',height = "600px",width = "380px")})
  output$terms4 <- renderUI({tags$img(src= 'http://farm2.staticflickr.com/1749/41637765894_1f19062eea_b.jpg',height = "500px",width = "380px")})
  output$terms5 <- renderUI({tags$img(src= 'http://farm2.staticflickr.com/1745/41637765524_0e0869bd46_b.jpg',height = "500px",width = "380px")})
  
  ############# CUP   ############# 
  output$cup_table <- renderDataTable({
    
    autoInvalidate()
    
    
    all_df_list$fixtures$df <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX",
                                                   "-1vQ4jRITA24Oj_h-i4cVxEGstFTS7-qKH0bv_pp61h-Jj4G",
                                                   "0t-fLh6TUiZU-Qor1WA2pt50TJkENnCkh/pub?gid=0&single=true&output=csv")),
                                        stringsAsFactors = FALSE)
    
    
    fixtures <- all_df_list$fixtures$df
    
    ### Function that generate cup ready data frame in a given dataset (which I created above)
    cup_gen <- function(x,n=60,k=4) # x is the relevant dataframe, n = N_users*number_of_games_in_stage/2 k = Number of games in cup Stage
    {
      the_data_img <- x %>% 
        left_join(User_ID %>% select(User_Nick,Img),by=c('User.x'='User_Nick')) %>% 
        left_join(User_ID %>% select(User_Nick,Img),by=c('User.y'='User_Nick')) %>% 
        select(User.x = Img.x,Bet.x,Pts.x=Points.x,Game,Result,User.y=Img.y,Bet.y,Pts.y=Points.y)
      
      names_x <- as.character(unique(the_data_img$User.x))
      names_y <- as.character(unique(the_data_img$User.y))
      n_user_div_2 <- length(names_y)
      
      the_data_img_con <- the_data_img %>%
        left_join(fixtures %>% select(NameID,Cup_Stage),by = c('Game'='NameID')) %>%
        group_by(User.x,Cup_Stage) %>%
        mutate(Agg.Pts.x = cumsum(Pts.x)) %>%  
        group_by(User.y,Cup_Stage) %>% 
        mutate(Agg.Pts.y = cumsum(Pts.y))  %>%  
        select(User.x,Bet.x,Pts.x ,Agg.Pts.x,Game,Result,Stage=Cup_Stage,User.y,Bet.y,Pts.y,Agg.Pts.y) %>%
        ungroup() %>% mutate(Rank = rep_len(c(rep(1,k),rep(0,k)),n))
      
      game_status <- fixtures %>% 
        select(Game = NameID,
               Active_Included,
               GameID,
               Stage = Cup_Stage) %>% 
        filter(GameID>48) 
      
      cup_col_by_user <- the_data_img_con %>% left_join(User_ID %>% 
                                                          select(User = User_Nick,Img),by = c('User.x'='Img')) %>%
        left_join(User_ID %>% 
                    select(User = User_Nick,Img),by = c('User.y'='Img')) %>% 
        select(User_x = User.x.x, Agg.Pts.x, Game,User_y = User.y.y,Agg.Pts.y) %>% 
        inner_join(user_results_validation %>% select(User=User_Nick,Game=NameID,boom,winning_goals),by=c('User_x'='User','Game'='Game')) %>%
        inner_join(user_results_validation %>% select(User=User_Nick,Game=NameID,boom,winning_goals),by=c('User_y'='User','Game'='Game')) %>%
        inner_join(game_status,by=c('Game')) %>% group_by(User_x,User_y) %>% 
        mutate(num = 1:k) %>% 
        group_by(User_x,Stage) %>% mutate(Agg.Boom.x = cumsum(boom.x),
                                          Agg.WG.x = cumsum(winning_goals.x)) %>% 
        group_by(User_y,Stage) %>% mutate(Agg.Boom.y = cumsum(boom.x),
                                          Agg.WG.y = cumsum(winning_goals.x))
      
      
      
      ###################
      
      ## TEST ##
      {
        polo_col <-  cup_col_by_user %>% mutate(
          col_user_x = ifelse(GameID < current_game, 
                              ifelse(num == k,
                                     ### If Stage games have been finished & we are in the last row of the knokout - we would like to see G/R/Y
                                     ifelse(Agg.Pts.x > Agg.Pts.y,cup_palate$winner,
                                            ifelse(Agg.Pts.x < Agg.Pts.y,cup_palate$loser,
                                                   ifelse(Agg.Boom.x > Agg.Boom.y ,cup_palate$winner,
                                                          ifelse(Agg.Boom.x < Agg.Boom.y,cup_palate$loser,
                                                                 ifelse(Agg.WG.x > Agg.WG.y , cup_palate$winner,
                                                                        ifelse(Agg.WG.x < Agg.WG.y,cup_palate$loser,cup_palate$draw)))))),
                                     cup_palate$game_over),
                              ifelse(GameID == current_game,
                                     ifelse(Agg.Pts.x > Agg.Pts.y,cup_palate$winner,
                                            ifelse(Agg.Pts.x < Agg.Pts.y,cup_palate$loser,
                                                   ifelse(Agg.Boom.x > Agg.Boom.y ,cup_palate$winner,
                                                          ifelse(Agg.Boom.x < Agg.Boom.y,cup_palate$loser,
                                                                 ifelse(Agg.WG.x > Agg.WG.y , cup_palate$winner,
                                                                        ifelse(Agg.WG.x < Agg.WG.y,cup_palate$loser,cup_palate$draw)))))),
                                     cup_palate$future_game))) %>%
          
          select(User_x,User_y,Stage,col_user_x,Game) %>% mutate(col_user_y = ifelse(col_user_x == cup_palate$loser,
                                                                                     cup_palate$winner,
                                                                                     ifelse(col_user_x == cup_palate$winner,cup_palate$loser,col_user_x)))
      }
      ##
      
      
      
      
      ###################
      
      the_data_img_con_Final <- the_data_img_con %>% left_join(User_ID %>% 
                                                                 select(User = User_Nick,Img),by = c('User.x'='Img')) %>%
        left_join(User_ID %>% 
                    select(User = User_Nick,Img),by = c('User.y'='Img'))
      
      cup_col <- the_data_img_con_Final %>% left_join(polo_col %>% select(User_x,Game,col_user_x),
                                                      by = c('Game','User.x.x'='User_x')) %>% left_join(polo_col %>% select(User_y,Game,col_user_y),
                                                                                                        by = c('Game','User.y.y'='User_y')) 
      
      
      
      final_cup_table <- cup_col %>% select(-c(Stage,Stage.y,User_y,User.y.y,User.x.x)) %>% 
        select(`User I` = User.x,
               `Bet I` = Bet.x,
               `Pts I`=  Pts.x,
               `Agg.Pts I` = Agg.Pts.x,
               Game,
               Result,
               Stage = Stage.x,
               `User II` = User.y,
               `Bet II` = Bet.y,
               `Pts II`=  Pts.y,
               `Agg.Pts II` = Agg.Pts.y,
               col_user_x,col_user_y,Rank)
      
      return(final_cup_table)
      
    }
    
    
    # resultes_edited - data join between user's predictions & real results
    {
      # Results edited join between the fixtures and the user results (Group Stage), for each user, in each game 
      
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
      
      # Results knokout edited join between the fixtures and the user results (knokout), for each user, in each game 
      
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
      
      # Creating one version of truth - comparing between user predictions to real results and connect the Group Stage & Knokout bulks
      
      resultes_edited <- bind_rows(resultes_edited,resultes_knokout_edited)
      
      resultes_edited$user_Away_Goals <-   as.numeric(resultes_edited$user_Away_Goals)
      resultes_edited$user_Home_Goals <-   as.numeric(resultes_edited$user_Home_Goals)
      resultes_edited$true_Direction <- as.character(resultes_edited$user_Direction)
      # adding Points vector with the user's points for each user in each match
      
      # Empty vector
      user_game_points <- rep(NA,nrow(resultes_edited))
      
      # apply worldcup points function for each row in the results edited
      for(i in 1:nrow(resultes_edited)){
        user_game_points[i] <- worldcup_points(resultes_edited[i,])}
      
      
      
      user_results_validation <- resultes_edited %>% mutate(user_game_points,
                                                            boom = ifelse(user_game_points>1,TRUE,FALSE),
                                                            winning_goals = ifelse(boom == TRUE,as.numeric(user_Home_Goals)+
                                                                                     as.numeric(true_Away_Goals),0))
      
    }
    
    # 'Current' Parameters - current game, current game name, current cup rank, etc
    {
      ### Very Importent - If there is an active game will indicate on him, else - will indicate on the next comming game ### 
      
      current_game <- ifelse(all(resultes_edited$active == FALSE),
                             max(resultes_edited$GameID[which(resultes_edited$started==TRUE)])+1,
                             resultes_edited$GameID[which(resultes_edited$active == TRUE)[1]])
      
      current_Game_Name <- (resultes_edited %>% filter(GameID == current_game) %>% select(NameID))[1,1]
      
      current_cup_rank <-   fixtures$Cup_Rank[which(fixtures$GameID == current_game)]
    }
    
    ##### Cup Data Preperation ####
    {
      current_cup_stage <- fixtures$Cup_ID[which(fixtures$GameID == current_game)]  
      
      # Aggrgate points for each user in each cup stage
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
      
      
      ## Cup Data preperation - Round of 32
      {
        data_cup <- cup_points  %>% left_join(User_ID %>%  select(User = User_Nick,Draw_32)) %>% 
          arrange(Draw_32,User) %>% filter(GameID < 53) 
        
        
        vec <- c(rep(c(rep(TRUE,4),rep(FALSE,4)),4*32/8))
        
        data_cup_a <- data_cup[which(vec==TRUE),] %>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_32,Game=NameID,Result)
        data_cup_b <- data_cup[which(vec==FALSE),]%>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_32,Game=NameID,Result)
        
        
        the_data <- data_cup_a %>% 
          inner_join(data_cup_b,by=c('GameID','CupID','Game','Result','Draw_32')) %>% arrange(User.x,GameID) %>% 
          select(-c(GameID,CupID,Draw_32)) 
        
               }
      ## Cup Data preperation - Round of 16
      {
        user_phase_16 <- User_ID %>% filter(Draw_16 != 0) %>% select(User_Nick)
        
        data_cup_16 <- cup_points %>% filter(User %in% user_phase_16$User_Nick) %>% left_join(User_ID %>% select(User = User_Nick,Draw_16)) %>% 
          arrange(Draw_16,User) %>% filter(GameID >= 53 & GameID < 57)
        
        vec_16 <- c(rep(c(rep(TRUE,4),rep(FALSE,4)),4*16/8))
        
        data_cup_a_16 <- data_cup_16[which(vec_16==TRUE),] %>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_16,Game=NameID,Result)
        data_cup_b_16 <- data_cup_16[which(vec_16==FALSE),]%>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_16,Game=NameID,Result)
        
        
        the_data_16 <- data_cup_a_16 %>% 
          inner_join(data_cup_b_16,by=c('GameID','CupID','Game','Result','Draw_16')) %>% arrange(User.x,GameID) %>% 
          select(-c(GameID,CupID,Draw_16))
      }
      ## Cup Data preperation - Quarter Finals
      {
        user_phase_8 <- User_ID %>% filter(Draw_8 != 0) %>% select(User_Nick)
        
        data_cup_8 <- cup_points %>% filter(User %in% user_phase_8$User_Nick) %>% left_join(User_ID %>% select(User = User_Nick,Draw_8)) %>% 
          arrange(Draw_8,User) %>% filter(GameID >= 57 & GameID < 61)
        
        vec_8 <- c(rep(c(rep(TRUE,4),rep(FALSE,4)),4*8/8))
        
        data_cup_a_8 <- data_cup_8[which(vec_8==TRUE),] %>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_8,Game=NameID,Result)
        data_cup_b_8 <- data_cup_8[which(vec_8==FALSE),]%>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_8,Game=NameID,Result)
        
        
        the_data_8 <- data_cup_a_8 %>% 
          inner_join(data_cup_b_8,by=c('GameID','CupID','Game','Result','Draw_8')) %>% arrange(User.x,GameID) %>% 
          select(-c(GameID,CupID,Draw_8))
        
        
      }
      ## Cup Data preperation - Semi Finals
      {
        user_phase_4 <- User_ID %>% filter(Draw_4 != 0) %>% select(User_Nick)
        
        data_cup_4 <- cup_points %>% filter(User %in% user_phase_4$User_Nick) %>% left_join(User_ID %>% select(User = User_Nick,Draw_4)) %>% 
          arrange(Draw_4,User) %>% filter(GameID >= 61 & GameID < 63)
        
        vec_4 <- c(rep(c(rep(TRUE,2),rep(FALSE,2)),2*4/4))
        
        data_cup_a_4 <- data_cup_4[which(vec_4==TRUE),] %>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_4,Game=NameID,Result)
        data_cup_b_4 <- data_cup_4[which(vec_4==FALSE),]%>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_4,Game=NameID,Result)
        
        
        the_data_4 <- data_cup_a_4 %>% 
          inner_join(data_cup_b_4,by=c('GameID','CupID','Game','Result','Draw_4')) %>% arrange(User.x,GameID) %>%  
          select(-c(GameID,CupID,Draw_4))
        
      }
      ## Cup Data preperation - Finals
      {
        user_phase_2 <- User_ID %>% filter(Draw_2 != 0) %>% select(User_Nick)
        
        data_cup_2 <- cup_points %>% filter(User %in% user_phase_2$User_Nick) %>% left_join(User_ID %>% select(User = User_Nick,Draw_2)) %>% 
          arrange(Draw_2,User) %>% filter(GameID >= 63 & GameID <= 64)
        
        vec_2 <- c(rep(TRUE,2),rep(FALSE,2))
        
        data_cup_a_2 <- data_cup_2[which(vec_2==TRUE),] %>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_2,Game=NameID,Result)
        data_cup_b_2 <- data_cup_2[which(vec_2==FALSE),]%>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_2,Game=NameID,Result)
        
        
        the_data_2 <- data_cup_a_2 %>% 
          inner_join(data_cup_b_2,by=c('GameID','CupID','Game','Result','Draw_2')) %>% arrange(User.x,GameID) %>%
          select(-c(GameID,CupID,Draw_2))
      }
      
      
      current_cup_rank <- fixtures$Cup_Rank[which(fixtures$GameID == current_game)]
      
      
      
      
      ready_32 <- cup_gen(the_data,64,4)
      ready_16 <- cup_gen(the_data_16,32,4)
      ready_8 <- cup_gen(the_data_8,16,4)
      ready_4 <- cup_gen(the_data_4,4,2)
      ready_2 <- cup_gen(the_data_2,2,2)
      ready_data_cup <- bind_rows(ready_32,ready_16,ready_8,ready_4,ready_2) 
      
      relevant_cup_stages <- as.character(unique(fixtures[1:current_game,]$Cup_Stage))
      
      ready_data_cup <- ready_data_cup %>% filter(Stage %in% relevant_cup_stages) %>% 
        select(`User I`,`Bet I`,`Pts I`,`Agg.Pts I`,Game,Result,`Agg.Pts II`,`Pts II`,`Bet II`,`User II`,`Stage`,col_user_x,col_user_y,Rank )  
      
      
      
    }
    
    
    DT::datatable(ready_data_cup,
                  options =  list(pageLength = nrow(ready_data_cup),
                                  columnDefs = list(list(width = 200, targets =  "_all" ),
                                                    list(visible=FALSE, targets=c(11,12,13)),
                                                    list(className = 'dt-center', targets = "_all")),
                                  scrollX=TRUE,
                                  scrollCollapse=TRUE,
                                  lengthMenu = c(32,64,128)), 
                  rownames = FALSE,
                  escape = FALSE)  %>% 
      formatStyle(c('User I','Agg.Pts I','Pts I','Bet I'),'col_user_x',
                  backgroundColor = styleEqual(as.vector(unlist(cup_palate)),
                                               as.vector(unlist(cup_palate)))) %>%
      formatStyle(c('User II','Agg.Pts II','Pts II','Bet II'),'col_user_y',
                  backgroundColor = styleEqual(as.vector(unlist(cup_palate)),
                                               c("#82E0AA","#CD6155","#EB984E","#FEF5E7","#F7F9F9"))) %>%
      formatStyle('Rank',
                  target = 'row',
                  color=styleEqual(c(0,1),c("#BA4A00","#F1C40F")),
                  backgroundColor ="#2B3E50")
    
    
  }) # End of utput$cup_table
  
  ############# Networe   ############# 
  output$network <- renderVisNetwork({
    
    autoInvalidate()
    
    fixtures <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX",
                                    "-1vQ4jRITA24Oj_h-i4cVxEGstFTS7-qKH0bv_pp61h-Jj4G",
                                    "0t-fLh6TUiZU-Qor1WA2pt50TJkENnCkh/pub?gid=0&single=true&output=csv")),
                         stringsAsFactors = FALSE)
    
    all_df_list$fixtures$df  <- fixtures
    
    ### Fixtures & Score API Joined 
    {
      fixtures <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vTNgD6oZivKRepzwPWDc",
                                      "YMg4tOQQq8B3sJLFdtHHE7p8lYs4lv_C4Wk_B3lkAPx-nZA",
                                      "4O6DETuEAhxw/pub?gid=0&single=true&output=csv")),
                           stringsAsFactors = FALSE)
      
      partial_fixtures <- fixtures %>% select(-c(started,active,Active_Included,true_Home_Goals,true_Away_Goals,true_Direction))
      
      partial_fixtures$new_id <- as.character(partial_fixtures$new_id)
      
      url <- 'https://perodriguezl-russia-2018-worldcup-tournament-v1.p.mashape.com/public/tournament/1'
      r <-  GET(url,add_headers(`X-Mashape-Key` = "GfEVgWsBovmshScrSarHgMWOihvjp17eUsvjsnwtfD1M1esYX9"))
      fixt <-  as.data.frame(t(as.data.frame(unlist(content(r)$data[[1]]))),
                             row.names = NULL)
      
      for( i in 2:64)
      {
        b <- as.data.frame(t(as.data.frame(unlist(content(r)$data[[i]]))),
                           row.names = NULL)
        fixt <- bind_rows(fixt,b)
      }
      
      fixt$countdown <- as.numeric(fixt$countdown)
      fixtures$new_id <- as.character(fixtures$new_id)
      fixt_to_be_added <- fixt %>% select(GameID = id,
                                          true_Home_Goals = results.home ,
                                          true_Away_Goals = results.visitor ,
                                          finished,
                                          countdown) %>% mutate(started = ifelse(countdown == 0,TRUE,FALSE),
                                                                active  = ifelse(finished  == FALSE & countdown == 0,TRUE,FALSE)) %>% 
        left_join(fixtures %>% select(new_id,NameID),by = c('GameID'='new_id'))   %>% mutate(Active_Included = ifelse(started == TRUE & active == FALSE,'Complited Games',
                                                                                                                      ifelse(started == TRUE & active == TRUE,'Active Games',
                                                                                                                             'Future Games'))) %>% 
        mutate(true_Direction  = ifelse(true_Home_Goals>true_Away_Goals,"Home",
                                        ifelse(true_Home_Goals<true_Away_Goals,"Away",
                                               "Draw"))) %>%
        select(GameID,NameID,countdown,finished,started,active,Active_Included,true_Home_Goals,true_Away_Goals,true_Direction)
      
      partial_fixtures$new_id <- as.character(partial_fixtures$new_id)
      
      final <- partial_fixtures %>% inner_join(fixt_to_be_added,by = c('new_id'='GameID'))
      
      fixtures <- final
      all_df_list$fixtures$df <- final
    }
    
    
    # resultes_edited - data join between user's predictions & real results
    {
      # Results edited join between the fixtures and the user results (Group Stage), for each user, in each game 
      
      resultes_edited <- all_df_list$resultes$df %>%
        melt(id = c("User Name","Submission ID")) %>%
        separate(variable,c("user_Home","user_Away")," - ") %>%
        separate(value,c("user_Home_Goals","user_Away_Goals"),"-") %>%
        mutate(user_Direction = ifelse(`user_Home_Goals`>`user_Away_Goals`,"Home",
                                       ifelse(`user_Home_Goals`==`user_Away_Goals`,"Draw",
                                              "Away")),
               NameID = paste0(user_Home," - ",user_Away)) %>% 
        inner_join(all_df_list$fixtures$df,
                   by = c("NameID"="NameID.y")) %>% left_join(User_ID %>% select(User = `Full.Name`,User_Nick),by = c('User Name'='User'))
      
      # Results knokout edited join between the fixtures and the user results (knokout), for each user, in each game 
      
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
        inner_join(all_df_list$fixtures$df,by = c("NameID"="NameID.y")) %>% 
        left_join(User_ID %>% select(User = `Full.Name`,User_Nick),by = c('User Name'='User'))
      
      # Creating one version of truth - comparing between user predictions to real results and connect the Group Stage & Knokout bulks
      
      resultes_edited <- bind_rows(resultes_edited,resultes_knokout_edited)
      
      resultes_edited$user_Away_Goals <-   as.numeric(resultes_edited$user_Away_Goals)
      resultes_edited$user_Home_Goals <-   as.numeric(resultes_edited$user_Home_Goals)
      resultes_edited$true_Direction <- as.character(resultes_edited$true_Direction)
      
      # adding Points vector with the user's points for each user in each match
      
      # Empty vector
      user_game_points <- rep(NA,nrow(resultes_edited))
      
      
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
      
      
      # apply worldcup points function for each row in the results edited
      for(i in 1:nrow(resultes_edited)){
        user_game_points[i] <- worldcup_points(resultes_edited[i,])}
      
      
      
      user_results_validation <- resultes_edited %>% mutate(user_game_points,
                                                            boom = ifelse(user_game_points>1,TRUE,FALSE),
                                                            winning_goals = ifelse(boom == TRUE,as.numeric(user_Home_Goals)+
                                                                                     as.numeric(true_Away_Goals),0))
      
      
      
    }
    
    ##
    
    r_16 <- resultes_knokout[1:N_users,2:18]
    r_8  <- resultes_knokout[(N_users+1):(2*N_users),c(2,19:26)]
    r_4  <- resultes_knokout[(2*N_users+1):(3*N_users),c(2,27:30)]
    r_2  <- resultes_knokout[(3*N_users+1):(5*N_users),c(2,31:34)]
    
    knokout <- r_16 %>% inner_join(r_16) %>%
      inner_join(r_8) %>%
      inner_join(r_4) %>%
      inner_join(r_2) %>% left_join(User_ID %>% select(Full.Name,User_Nick),by=c('User.Name'='Full.Name')) %>%
      select(-User.Name) %>% select(User = User_Nick,everything())
    
    names_knok <- initial_fixtures$NameID[49:nrow(initial_fixtures)]
    dup_names_knok <- paste0(names_knok," - winner")
    names(knokout) <- c('User_Nick',sort(c(names_knok,dup_names_knok)))
    
    
    # Extracting competition parameters 
    {
      N_users <- nrow(User_ID)
      N_games_complited = sum(fixtures$started == TRUE & fixtures$active == FALSE)
      N_games = sum(fixtures$started == TRUE)
    }
    
    # 'Current' Parameters - current game, current game name, current cup rank, etc
    {
      ### Very Importent - If there is an active game will indicate on him, else - will indicate on the next comming game ### 
      
      current_game <- ifelse(all(fixtures$active == FALSE),
                             max(resultes_edited$GameID[which(resultes_edited$started==TRUE)])+1,
                             resultes_edited$GameID[which(resultes_edited$active == TRUE)[1]])
      
      current_Game_Name <- (resultes_edited %>% filter(GameID == current_game) %>% select(NameID))[1,1]
      
      current_cup_rank <-   fixtures$Cup_Rank[which(fixtures$GameID == current_game)]
    }
    
    
    all_games <- all_df_list$resultes$df %>% inner_join(knokout,by = c('User_Nick'))
    
    
    test_game_pre_result <- all_games %>% select(User = 'User_Nick',current_Game_Name) 
    ##
    
    
    
    test_game <- test_game_pre_result %>% 
      separate(current_Game_Name,into = c('U1.Home','U1.Away'),remove = FALSE) %>%
      mutate(T.Home = as.numeric(fixtures$true_Home_Goals[which(initial_fixtures$NameID == current_Game_Name)]),
             T.Away = as.numeric(fixtures$true_Away_Goals[which(initial_fixtures$NameID == current_Game_Name)]),
             U.Home = as.numeric(U1.Home),
             U.Away = as.numeric(U1.Away),
             D.Home = T.Home-U.Home,
             D.Away = T.Away-U.Away) %>%
      filter(D.Home<= 0 & D.Away<=0) %>% select(User,current_Game_Name)
    
    true_result <- fixtures[which(initial_fixtures$NameID == names(test_game)[2]),
                            c(which(names(fixtures) == 'true_Home_Goals'),
                              which(names(fixtures)== 'true_Away_Goals'))]
    
    #### Special treatment to 0 or 1 remaining results
    
    N_test_game <- nrow(test_game)
    
    
    
    if(N_test_game == 1)
    {
      res_dist <- test_game %>% mutate(Home = true_result$true_Home_Goals,
                                       Away = true_result$true_Away_Goals) %>%
        separate(names(test_game)[2],into=c('H.Res','A.Res'),sep="-") %>% 
        mutate(Result = paste0(H.Res,"-",A.Res),
               dist = abs(as.numeric(Home)-as.numeric(H.Res)) + 
                 abs(as.numeric(Away)-as.numeric(A.Res))) %>% select(User,Result,dist) %>%
        distinct(Result,dist)
      
      
      user_dist <- data.frame(Result = test_game$User,
                              dist   = 1)
      
      dist_w = bind_rows(user_dist,res_dist) %>% 
        mutate(w = ifelse(nchar(Result)==3,ifelse(dist == 0,2,dist^(-1)),3),
               w2 = ifelse(nchar(Result)==3,10*w,w))
      
      graph_array <- array(data = NA,dim = c(nrow(test_game),length(unique(test_game[,2]))))
      
      dimnames(graph_array)[[1]] <- list(as.character(test_game$User))
      dimnames(graph_array)[[2]] <- unique(test_game[,2])
    }else{
      if(N_test_game == 0)
      {
        test_game <- test_game_pre_result
        
        res_dist <- test_game %>% mutate(Home = true_result$true_Home_Goals,
                                         Away = true_result$true_Away_Goals) %>%
          separate(names(test_game)[2],into=c('H.Res','A.Res'),sep="-") %>% 
          mutate(Result = paste0(H.Res,"-",A.Res),
                 dist = abs(as.numeric(Home)-as.numeric(H.Res)) + 
                   abs(as.numeric(Away)-as.numeric(A.Res))) %>% select(User,Result,dist) %>%
          distinct(Result,dist)
        
        
        user_dist <- data.frame(Result = test_game$User,
                                dist   = 1)
        
        dist_w = bind_rows(user_dist,res_dist) %>% 
          mutate(w = ifelse(nchar(Result)==3,ifelse(dist == 0,2,dist^(-1)),3),
                 w2 = ifelse(nchar(Result)==3,10*w,w))
        
        graph_array <- array(data = NA,dim = c(nrow(test_game),length(unique(test_game[,2]))))
        
        dimnames(graph_array)[[1]] <- as.character(test_game$User)
        dimnames(graph_array)[[2]] <- unique(test_game[,2])
      }else
      {
        res_dist <- test_game %>% mutate(Home = true_result$true_Home_Goals,
                                         Away = true_result$true_Away_Goals) %>%
          separate(names(test_game)[2],into=c('H.Res','A.Res'),sep="-") %>% 
          mutate(Result = paste0(H.Res,"-",A.Res),
                 dist = abs(as.numeric(Home)-as.numeric(H.Res)) + 
                   abs(as.numeric(Away)-as.numeric(A.Res))) %>% select(User,Result,dist) %>%
          distinct(Result,dist)
        
        
        user_dist <- data.frame(Result = test_game$User,
                                dist   = 1)
        
        dist_w = bind_rows(user_dist,res_dist) %>% 
          mutate(w = ifelse(nchar(Result)==3,ifelse(dist == 0,2,dist^(-1)),3),
                 w2 = ifelse(nchar(Result)==3,10*w,w))
        
        graph_array <- array(data = NA,dim = c(nrow(test_game),length(unique(test_game[,2]))))
        
        dimnames(graph_array)[[1]] <- as.character(test_game$User)
        dimnames(graph_array)[[2]] <- unique(test_game[,2])
        
      }
    }
    
    
    for( i in 1:dim(graph_array)[1])
    {
      for(j in 1:dim(graph_array)[2])
      {
        graph_array[i,j] <- ifelse(test_game[i,2]==dimnames(graph_array)[[2]][j],1,0)
      }
      
    }
    
    names_1 <- data.frame(nms=dimnames(graph_array)[[1]])
    names_2 <- data.frame(nms=dimnames(graph_array)[[2]])
    
    N = nrow(names_1) + nrow(names_2)
    nodes <- bind_rows(names_1,names_2) %>% mutate(id = 1:N) %>% select(id,label = nms) %>% 
      left_join(dist_w %>% select(-c(w,dist)),by = c('label'='Result')) %>% 
      select(id,label,value = w2) %>% mutate(group = ifelse(nchar(label) == 3,'Result','User'),
                                             shpae = ifelse(nchar(label) == 3,'circle','star'),
                                             color = ifelse(nchar(label) == 3,'#B0D494','#9FB9E6'), 
                                             shadow = ifelse(nchar(label) == 3,TRUE,FALSE),
                                             font.color =ifelse(nchar(label) == 3,'#C0392B','#ECF0F1'),
                                             font.size = ifelse(nchar(label) == 3,value*5,value*10))
    
    
    edges <- graph_array %>% melt()  %>% filter(value!=0) %>%
      select(label_from = Var1,label_to = Var2) %>% 
      left_join(nodes %>% select(id,label),by = c('label_from'='label')) %>% 
      left_join(nodes %>% select(id,label),by = c('label_to'='label')) %>% select(from= id.x,to=id.y)
    
    
    visNetwork(nodes, edges,width = "100%",height  =600) %>% 
      visInteraction(dragNodes = TRUE, dragView = FALSE)
    
    
  })
  
})





