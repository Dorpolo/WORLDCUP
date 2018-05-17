######################## UI Component #############################



ui <- fluidPage(theme = shinytheme("superhero"),

                # App title ----
                titlePanel("World Cup Friends Zone"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    

                     HTML('<img src="https://fsprdcdnpublic.azureedge.net/global-pictures/tournaments-sq-4/254645_w" height="95" width="130" align="middle">'),
                    HTML('<img src="http://i65.tinypic.com/11ht1et.png" height="95" width="170" align="middle">'),
            


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
                                selected = choices$userID,
                                multiple = TRUE),
                  
                      # Select variables for the PIE CHART
                    
             
              
                      div(id = 'large',
                      checkboxInput(inputId = "somevalue",
                                    label = h4("Active Games Included"),
                                    value = FALSE,
                                    width = validateCssUnit(200)))
                
                    
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
                                         h4("Live Game Fan Zone")),
                                tabPanel(title =  'CUP',
                                         h4("Welcome to the CUP Zone"),
                                         fluidRow(column(8,
                                        DT::dataTableOutput(outputId = "cup_table")), 
                                         column(4,
                                                uiOutput(outputId = "imagecup")))
                                         
                                         ),
                                tabPanel(title = "User Guesses",
                                         h4("All User Guesses"),
                                         DT::dataTableOutput(outputId = "user_guess"),
                                         br(),
                                         DT::dataTableOutput(outputId = "table.a")),
                                tabPanel(title = "Terms",
                                         uiOutput(outputId = "terms1"),
                                         uiOutput(outputId = "terms2"),
                                         uiOutput(outputId = "terms3"))
                                
                                
                                
                                # Single line break for a little bit of visual separation
                                # Horizontal line for visual separation
                                
                    )      
                  )
                )
)



