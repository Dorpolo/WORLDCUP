server <- function(input, output) {
  
  polo <- reactive({
    user_results_validation %>% filter(Round %in% input$Round & 
                                         Active_Included %in% input$inActive &
                                         Group %in% input$Group & 
                                         GameID >= input$game_number[1] & 
                                         GameID <= input$game_number[2] ) })
  
  output$league_table <- DT::renderDataTable({
    DT::datatable(data = polo() %>% 
                    group_by(`User Name`) %>% 
                    summarise(Points = sum(user_game_points),
                              Boom = sum(boom),
                              `Winning Goals` = sum(winning_goals),
                              Pct = percent(sum(user_game_points)/(4*sum(Active_Included=="Complited Games")-sum(Active_Included=="Complited Games"&Stage=="Group Stage"))),
                              Games = sum(started == TRUE)) %>% 
                    arrange(desc(Points),
                            desc(Boom),
                            desc(`Winning Goals`)) %>% 
                    left_join(only_rank_for_vlookup,by = c("User Name")) %>% select(Rank,everything()),
                  options = list(pageLength = 10), 
                  rownames = FALSE,
                  class = 'cell-border stripe') %>%  
      formatStyle(c('Rank','User Name','Points','Boom','Winning Goals','Pct','Games'),
                  color = '#586261')
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
  
  polo_3 <- reactive({ user_results_validation %>%
      filter(`User Name` %in% input$userID) %>%
      select(Date,User = `User Name`,Game = GameID ,user_game_points) %>%
      group_by(Date,User) %>% 
      summarise(Points = sum(user_game_points)) %>% 
      group_by(User) %>% 
      mutate(Cum = cumsum(Points)) %>% 
      arrange(desc(Cum)) %>%
      group_by(Date) %>% 
      mutate(ranking = row_number())%>% 
      arrange(Date) %>%
      left_join(date.id,by = c("Date")) %>% mutate(alpha = 1, x = 0.5 ,x.2 = 16)
  })
  
  output$lineplot <- renderPlot({
    
    ggplot(data = polo_3(), aes(x = Day, y = ranking, group = User)) +
      geom_line(aes(color =  User,alpha = alpha), size = 2) +
      geom_point(aes(color = User,alpha = alpha), size = 4) +
      geom_point(color = "#FFFFFF", size = 1) +
      scale_y_reverse(breaks = 1:nrow(polo_3())) +
      scale_x_continuous(breaks = 1:16, expand = c(.05, .05)) +
      geom_text(data =  polo_3() %>% filter(Day == "1"),
                aes(label = User, x = x) , hjust = .85, fontface = "bold", color = "#00384A", size = 4) +
      geom_text(data = polo_3() %>% filter(Day == "15"),
                aes(label = User, x = x.2) , hjust = 0.15, fontface = "bold", color = "#00384A", size = 4) +
      coord_cartesian(ylim = c(1,length(unique(polo_3()$User)))) + 
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
              options = list(pageLength = 32), 
              rownames = FALSE,
              class = 'cell-border stripe') %>% 
      formatStyle("Won Stage",
                 target = 'row',
                  color = styleEqual(c('Yes','No'),c('green','red'))) 
                                                 
  })
  
  polo_5 <- reactive({ user_guesses %>%
      filter(User %in% input$userID) 
  })
  
  output$user_guess <- renderDataTable({
    datatable(data = polo_5(),
              options = list(pageLength = 48), 
              rownames = FALSE,
              class = 'cell-border stripe') %>% 
      formatStyle("GameID",
                  target = 'row',
                  backgroundColor  = 'white',
                  color = "#00384A") 
    
  })
  
}

