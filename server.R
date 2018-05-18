

server <- function(input, output, session) {
  
  
  polo <- reactive({ 
    if(input$somevalue){user_results_validation %>% filter( Active_Included %in% c("Active Games",'Complited Games') &
                                                               GameID >= input$game_number[1] & 
                                                               GameID <= input$game_number[2] ) 
    }else {user_results_validation %>% filter(  Active_Included %in% c('Complited Games') &
                                                  GameID >= input$game_number[1] & 
                                                  GameID <= input$game_number[2] )} 
    
  })
  
  
  
  
  output$league_table <- DT::renderDataTable({
    DT::datatable(data = polo() %>% 
                    group_by(`User_Nick`) %>% 
                    summarise(Points = sum(user_game_points),
                              Boom = sum(boom),
                              WG = sum(winning_goals),
                              Pct = percent(sum(user_game_points)/(4*sum(Active_Included=="Complited Games")-sum(Active_Included=="Complited Games"&Stage=="Group Stage"))),
                              Games = sum(started == TRUE)) %>% 
                    arrange(desc(Points),
                            desc(Boom),
                            desc(WG)) %>% 
                    left_join(only_rank_for_vlookup,by = c("User_Nick")) %>% select(Rank,Name = `User_Nick`,everything()),
                  options = list(pageLength = 10,
                                 columnDefs = list(list(width = 200, targets =  "_all" )),
                                 scrollX=TRUE,
                                 scrollCollapse=TRUE,
                                 pageLength = 30, lengthMenu = c(10,20,30)), 
                  rownames = FALSE,
                  class = 'cell-border stripe') %>% 
      formatStyle('Rank',
                  target = 'row',
                  color  = styleEqual(c(1,2:5,6:N_users),c('#000066',rep('#660000',4),rep('#00384A',(N_users-6+1))))) %>%
      formatStyle('Rank',
                  target = 'row',
                  backgroundColor   = styleEqual(c(1),c('#ffff66')))
  })
  
  
  polo_2 <- reactive({
    user_results_validation %>% 
      filter(NameID %in% input$nameID) %>%
      summarise(Home = sum(user_Direction == "Home"),
                Draw = sum(user_Direction == "Draw"),
                Away = sum(user_Direction == "Away")) %>%
      mutate(graph = "User Game Winner Dist") %>% melt(id = "graph") 
  })
  
  output$piechart <- renderPlot({
    ggplot(data = polo_2() , aes(x="", y=nrow(user_results_validation), fill=value))+
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + theme_void() +
      theme(legend.position="top",
            legend.direction = "horizontal",
            legend.title = element_blank())})
  
  
  ##################
  
  polo_3 <- reactive({user_rank_by_day %>% filter(`User` %in% input$userID)})
  
  observe({
    if ("Select All" %in% input$userID) {
      # choose all the choices _except_ "Select All"
      selected_choices <- setdiff(choices$userID, "Select All")
      updateSelectInput(session, 'userID', selected = selected_choices)
    }
  })
  
  
  ###################
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
      coord_cartesian(ylim = c(1,30)) + 
      theme(legend.position = "none") +
      labs(x = "Competition Days with games",
           y = "Rank",
           title = "World Cup as a Service",
           subtitle = "Users ranked by overall points after each competition Day") + my_theme()
  })
  
  
  polo_4 <- reactive({ team_table_by_users  %>%
      filter(User %in% input$userID) 
  })
  
  
  
  output$table.a <- renderDataTable({
    datatable(data =polo_4() %>% group_by(Group,Team) %>%
                summarise(Points = sum(Points),
                          GD = sum(GD)) %>% arrange(Group,desc(Points),desc(GD)) %>% 
                mutate(Rank=row_number(),`Won Stage` = ifelse(Rank %in% c(1,2),"Yes","No")) ,
              options = list(pageLength = 32,autoWidth = TRUE,
                             columnDefs = list(list(width = '5%', targets = "_all"))), 
              rownames = FALSE,
              class = 'cell-border stripe') %>% 
      formatStyle("Won Stage",
                  target = 'row',
                  color = styleEqual(c('Yes','No'),c('green','red'))) 
    
  })
  
  polo_5 <- reactive({ user_guesses %>%
      filter(User %in% input$userID) %>% select(-c(GameID,Hour,Stage))
  })
  
  output$user_guess <- renderDataTable({
    datatable(data = polo_5(),
              options = list(pageLength = 48,
                             columnDefs = list(list(width = '10px', targets = "_all"))), 
              rownames = FALSE,
              class = 'cell-border stripe') %>% 
      formatStyle("Date",
                  target = 'row',
                  backgroundColor  = 'white',
                  color = "#00384A") 
    
  })
  
  output$cup_table <- renderDataTable({
    datatable(data =the_data ,
              options = list(pageLength = 32,
                             columnDefs = list(list(width = '5%', targets = "_all"))), 
              rownames = FALSE,
              class = 'cell-border stripe')     
  })
  
  
  output$imagecup <- renderUI({tags$img(src='https://fsprdcdnpublic.azureedge.net/global-pictures/tournaments-sq-4/254645_w',height = "300px")})
  
  output$terms1 <- renderUI({tags$img(src= 'http://i67.tinypic.com/2d26umw.png',height = "600px")})
  output$terms2 <- renderUI({tags$img(src= 'http://i64.tinypic.com/2ed8bjl.png',height = "600px")})
  output$terms3 <- renderUI({tags$img(src= 'http://i67.tinypic.com/o07780.png',height = "600px")})
}






