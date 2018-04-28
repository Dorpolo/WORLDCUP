######################## UI Component #############################

ui <- fluidPage(theme = shinytheme("superhero"),
                
                # App title ----
                titlePanel("World Cup Friends Zone"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
           
               
                    HTML('<img src="http://i65.tinypic.com/11ht1et.png" height="70" width="140">'),
                      titlePanel(paste("League Filters : ")),
                      sliderInput(inputId ="game_number",
                                  label ="Game Number:",
                                  min = 1, max = 64,
                                  value = c(1,N_games)),
                      # Select variables for the PIE CHART
                      selectInput(inputId = "userID",
                                  label =  "User Name :",
                                  choices  = choices$userID,
                                  selected = choices$userID,
                                  multiple = TRUE),
             
                      selectInput(inputId = "nameID",
                                  label =  "Game :",
                                  choices  = choices$nameID,
                                  selected = current_Game_Name,
                                  multiple = FALSE),
                      
                      checkboxInput(inputId = "somevalue",
                                    label = h1("Active Games Included"),
                                    value = FALSE,
                                    width = validateCssUnit(800)),
                    
                    h5("Russia 2018", img(src='https://fsprdcdnpublic.azureedge.net/global-pictures/tournaments-sq-4/254645_w',height = "60px" ),
                       "Friends Zone",
                       img(src=paste0(as.character(User_ID$Picture[10])),
                           height = "60px"))
                    
                    ,width = 6),
                  
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
                                         h4("Live Game Fan Zone")),
                                tabPanel(title =  'CUP',
                                         
                                        
                                         h4("Welcome to the CUP Zone"),
                                         fluidRow(column(4,
                                         uiOutput(outputId = "image1"),
                                         uiOutput(outputId = "image2"),
                                         uiOutput(outputId = "image3"),
                                         uiOutput(outputId = "image4"),
                                         uiOutput(outputId = "image5")),
                                         column(4,
                                         uiOutput(outputId = "image6"),
                                         uiOutput(outputId = "image7"),
                                         uiOutput(outputId = "image8"),
                                         uiOutput(outputId = "image9"),
                                         uiOutput(outputId = "image10")),
                                         column(4,
                                                uiOutput(outputId = "imagecup")))
                                         
                                         ),
                                tabPanel(title = "User Guesses",
                                         h4("All User Guesses"),
                                         DT::dataTableOutput(outputId = "user_guess"),
                                         br(),
                                         DT::dataTableOutput(outputId = "table.a"))
                                
                                
                                
                                # Single line break for a little bit of visual separation
                                # Horizontal line for visual separation
                                
                    )      
                  ,width = 6)
                )
)



