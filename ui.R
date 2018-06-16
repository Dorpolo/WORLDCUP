
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



##### WorldCup Friends Zone - Shiny App #####

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
  
  # Users' score predictions - Group Stage
  
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

# Extracting competition parameters 
{
  N_users <- nrow(User_ID)
  N_games_complited = sum(fixtures$started == TRUE & fixtures$active == FALSE)
  N_games = sum(fixtures$started == TRUE)
  
  current_game <- ifelse(all(fixtures$active == FALSE),
                         max(fixtures$GameID[which(fixtures$started==TRUE)])+1,
                         fixtures$GameID[which(fixtures$active == TRUE)[1]])
  
  current_Game_Name <- (fixtures %>% filter(GameID == current_game) %>% select(NameID))[1,1]
  
  current_cup_rank <-   fixtures$Cup_Rank[which(fixtures$GameID == current_game)]
  
}



ui <- fluidPage(theme = shinytheme("slate"),
                
                # App title ----
                titlePanel("World Cup Friends Zone"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    HTML('<img src="https://fsprdcdnpublic.azureedge.net/global-pictures/tournaments-sq-4/254645_w" height="95" width="130" align="middle">'),
                    HTML('<img src="http://farm2.staticflickr.com/1745/41637992304_5a17b268a9_b.jpg" height="95" width="170" align="middle">'),
                    
                    HTML("<br> <br>"),
                    
                    selectInput(inputId = "userID",
                                label =  "User:",
                                choices  = choices$userID,
                                selected = choices$userID[sample(31,1)],
                                multiple = TRUE),     
                    selectInput(inputId = "nameID",
                                label =  "Game :",
                                choices  = choices$nameID,
                                selected = current_Game_Name,
                                multiple = FALSE)
                    
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    
                    tabsetPanel(type = "tabs",
                                tabPanel(title = "League Table",
                                         h4("League Table"),
                                         DT::dataTableOutput(outputId = "league_table"),
                                         br(),
                                         plotOutput(outputId = "lineplot",
                                                    click = "plot_click")),
                                tabPanel(title = "Pre Game",
                                         h4("Pre Game Statistics"),
                                         htmlOutput("game_name"),
                                         plotOutput(outputId = "pre_game_I", width = "250px", height = "250px"),
                                         plotOutput(outputId = "pre_game_II", width = "350px", height = "600px")),
                                tabPanel(title =  'Live Game',
                                         htmlOutput("title"),
                                         DT::dataTableOutput(outputId = "result_table"),
                                         visNetworkOutput(outputId = "network",height = "500px")),
                                tabPanel(title =  'CUP',
                                         h3("The FZ Cup"),
                                         DT::dataTableOutput(outputId = "cup_table")       
                                ),
                                
                                tabPanel(title = "User Predictions",
                                         h4("All User Guesses"),
                                         DT::dataTableOutput(outputId = "user_guess"),
                                         br(),
                                         DT::dataTableOutput(outputId = "table.a")),
                                tabPanel(title = "Terms",
                                         uiOutput(outputId = "terms1"),
                                         uiOutput(outputId = "terms2"),
                                         uiOutput(outputId = "terms3"),
                                         uiOutput(outputId = "terms4"),
                                         uiOutput(outputId = "terms5"))
                                
                                
                    ) 
                  )
                )
)




