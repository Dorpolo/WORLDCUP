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
                                              "0t-fLh6TUiZU-Qor1WA2pt50TJkENnCkh/pub?output=csv")),
                     stringsAsFactors = FALSE)

resultes <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vTQKuVDYTvMW9n",
                                              "B24QGAQ0M5l8xgeUGYbCjGTX3dxeQ1jaklYy-989osaccZgcwVelbzpxq4nZEtP",
                                              "xw/pub?gid=0&single=true&output=csv")),
                     stringsAsFactors = FALSE)
                          
User_ID  <- as.data.frame(read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ6Wd4NT4ZG6yBf",
                                              "4_ctJnV3-6M8CdQq3K8MTgcB6M_JG1la_2kAIkW2mCa0U6KunkqIq5Wm9US2aDN",
                                              "t/pub?gid=0&single=true&output=csv"))),
                        stringsAsFactors = FALSE)



#list which containes all application data frames

all_df_list <- list(userID =   list(df = User_ID),
                    resultes = list(df = resultes),
                    fixtures = list(df = fixtures))

# Special Functions
## Points per game per user
{
  
  worldcup_points <- function(x)
  {
    if(x$started == "TRUE")
    {
      if(x$Stage == "Group Stage")
      {
        f <- x$true_Direction 
        user <- c(x$user_Home_Goals,x$user_Away_Goals,x$user_Direction)
        true <- c(x$true_Home_Goals,x$true_Away_Goals,x$true_Direction)
        return(ifelse(sum(user == true) == 3,3,ifelse(user[3] == true[3],1,0)))
      }else{return(0)}
    }else{return(0)}
  }
}

# Union between true & user resultes , create one version of anlysis ready table by each relevant parameter
names(all_df_list$resultes$df) <- gsub("..."," - ",names(all_df_list$resultes$df),fixed = TRUE)
names(all_df_list$resultes$df) <- gsub("."," ",names(all_df_list$resultes$df),fixed = TRUE)
names(all_df_list$userID$df) <- gsub("."," ",names(all_df_list$userID$df),fixed = TRUE)


resultes_edited <- all_df_list$resultes$df %>%
  melt(id = c("User Name","Submission ID")) %>%
  separate(variable,c("user_Home","user_Away")," - ") %>%
  separate(value,c("user_Home_Goals","user_Away_Goals"),"-") %>%
  mutate(user_Direction = ifelse(`user_Home_Goals`>`user_Away_Goals`,"Home",
                                 ifelse(`user_Home_Goals`==`user_Away_Goals`,"Draw",
                                        "Away")),
         NameID = paste0(user_Home," - ",user_Away)) %>% 
  inner_join(all_df_list$fixtures$df,
             by = c("NameID"))


# Points vector, for each game,user. will be mutate to the "resultes_edited" table

user_game_points <- rep(NA,nrow(resultes_edited))

for(i in 1:nrow(resultes_edited)){
  user_game_points[i] <- worldcup_points(resultes_edited[i,])}

user_results_validation <- resultes_edited %>% mutate(user_game_points,
                                                      boom = ifelse(user_game_points>1,TRUE,FALSE),
                                                      winning_goals = ifelse(boom == TRUE,as.numeric(user_Home_Goals)+
                                                                               as.numeric(true_Away_Goals),0)
)

# User Table
N_users = length(unique(resultes$User.Name))

# Number off Game Overs

N_games_complited = sum(fixtures$started == TRUE & fixtures$active == FALSE)
N_games = sum(fixtures$started == TRUE)

league_standings <- user_results_validation %>% filter(Active_Included == "Complited Games") %>% 
  group_by(`User Name`) %>% 
  summarise(Points = sum(user_game_points),
            Boom = sum(boom),
            Winning_Goals = sum(winning_goals),
            Games = sum(started == TRUE)) %>% arrange(desc(Points),
                                                      desc(Boom),
                                                      desc(Winning_Goals)) %>% mutate(Rank = 1:N_users)

only_rank_for_vlookup <- league_standings %>% select(`User Name`,Rank)

date.id <- fixtures %>% select(Date,GameID) %>% arrange(GameID) %>% 
            distinct(Date,.keep_all = TRUE) %>% mutate(Day = rank(GameID)) %>% select(-GameID)





# =========================================================================
# ui.R variables
# =========================================================================


choices <- list(
  Round = unique(user_results_validation$Round),
  Group =unique(user_results_validation$Group),
  nameID = unique(user_results_validation$NameID),
  userID = unique(sort(user_results_validation$`User Name`)),
  Active =  unique(sort(user_results_validation$Active_Included))
)
