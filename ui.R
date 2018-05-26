######################## UI Component #############################
library('visNetwork')

choices <- list(
  Round = unique(user_results_validation$Round),
  Group =unique(user_results_validation$Group),
  nameID = unique(user_results_validation$NameID),
  userID = c('Select All',levels(unique(sort(user_results_validation$`User_Nick`)))),
  Active =  unique(sort(user_results_validation$Active_Included)),
  Cup = unique((fixtures %>% filter(Cup_Stage != "None"))$Cup_Stage)
)


ui <- fluidPage(theme = shinytheme("superhero"),
                
                # App title ----
                titlePanel("World Cup Friends Zone"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    
                    HTML('<img src="https://fsprdcdnpublic.azureedge.net/global-pictures/tournaments-sq-4/254645_w" height="95" width="130" align="middle">'),
                    HTML('<img src="http://farm2.staticflickr.com/1745/41637992304_5a17b268a9_b.jpg" height="95" width="170" align="middle">'),


                    
                    
                    
                    titlePanel(paste("League Filters : ")),
                    
                    sliderInput(inputId ="game_number",
                                label ="Game Number:",
                                min = 1, max = 64,
                                value = c(1,N_games)),
                    
                    selectInput(inputId = "nameID",
                                label =  "Game :",
                                choices  = choices$nameID,
                                selected = current_Game_Name,
                                multiple = FALSE),
                    selectInput(inputId = "userID",
                                label =  "User:",
                                choices  = choices$userID,
                                selected = choices$userID[sample(31,1)],
                                multiple = TRUE),
                    
                    # Select variables for the PIE CHART
                    
                    
                    
                    div(id = 'large',
                        checkboxInput(inputId = "somevalue",
                                      label = h4("Active Games Included"),
                                      value = FALSE,
                                      width = validateCssUnit(200))),
                    selectInput(inputId = "cup_round",
                                label =  "Cup Round:",
                                choices  = choices$Cup,
                                selected = choices$Cup[1],
                                multiple = TRUE)
                    
                    
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                tabPanel(title = "League Table",
                                         h4("Users League"),
                                         DT::dataTableOutput(outputId = "league_table"),
                                         br(),
                                         plotOutput(outputId = "lineplot",
                                                    click = "plot_click")),
                                tabPanel(title = "Pre Game Statistics",
                                         h4("Pre Game Stats"),
                                         plotOutput(outputId = "piechart")),
                                tabPanel(title =  'Live Game',
                                         h4("Live Game Fan Zone"),
                                         visNetworkOutput(outputId = "network",height = "500px")),
                                tabPanel(title =  'CUP',
                                         h2("CUP Zone"),
                                          DT::dataTableOutput(outputId = "cup_table")       
                                ),
                                tabPanel(title = "User Guesses",
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



