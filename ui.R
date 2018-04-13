
######################## UI Component #############################

 ui <- fluidPage(theme = shinytheme("superhero"),
  
  # App title ----
  titlePanel("World Cup Friends Zone"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
              wellPanel(
                        titlePanel(paste("League Table Filters : ")),
                        sliderInput(inputId ="game_number",
                                    label ="Game Number:",
                                    min = 1, max = 64,
                                    value = c(1,N_games)),
                        selectInput(inputId ="inActive", 
                                      label = "Game Status :",
                                    choices = choices$Active,
                                    selected ="Complited Games",
                                    multiple = TRUE),
                        # Select variables for the League Table
                        selectInput(inputId = "Round",
                                    label =  "Round:",
                                    choices = choices$Round,
                                    selected = unique(user_results_validation$Round),
                                    multiple = TRUE),
                        selectInput(inputId = "Group",
                                    label =  "Group:",
                                    choices = choices$Group,
                                    selected = unique(user_results_validation$Group),
                                    multiple = TRUE),
                        br(),br())      , # Horizontal line for visual separation
                      wellPanel(
                        titlePanel(paste("User Rank Plot Filters : ")),
                        # Select variables for the PIE CHART
                        selectInput(inputId = "nameID",
                                    label =  "Game :",
                                    choices  = choices$nameID,
                                    selected = FALSE,
                                    multiple = FALSE),
                        selectInput(inputId = "userID",
                                    label =  "User Name :",
                                    choices  = choices$userID,
                                    selected = choices$userID,
                                    multiple = TRUE)),
 br(),
                      h5("World Cup 2018",
                         img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/6/67/2018_FIFA_World_Cup.svg/1200px-2018_FIFA_World_Cup.svg.png",
                             height = "60px"),"The Official Friends Zone")
                      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
                    tabsetPanel(type = "tabs",
                                  tabPanel(title = "League Table",
                                           h4("Users League"),
                                           DT::dataTableOutput(outputId = "league_table"),
                                           br(),
                                           plotOutput(outputId = "lineplot")),
                                  tabPanel(title = "Pre Game Statistics",
                                           h4("Pre Game Stats"),
                                           plotOutput(outputId = "piechart"))
                                  
                                  # Single line break for a little bit of visual separation
                                  # Horizontal line for visual separation
                                  
                      )      
    )
  )
)
