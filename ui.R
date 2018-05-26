User_ID <- as.data.frame(read.csv(url(paste0('https://docs.google.com/spreadsheets/d/e/2PACX-1vSG0P3o9q0tK',
                                             'Hek7MJ7oFHOgmDy-1tBaG1eXVlg2Fbh64iDQafupr9JFlWZznyPjfg-Lf59WmTA3aV0/',
                                             'pub?gid=0&single=true&output=csv'))),
                         stringsAsFactors = FALSE)

fixtures <- read.csv(url(paste0("https://docs.google.com/spreadsheets/d/e/2PACX",
                                "-1vQ4jRITA24Oj_h-i4cVxEGstFTS7-qKH0bv_pp61h-Jj4G",
                                "0t-fLh6TUiZU-Qor1WA2pt50TJkENnCkh/pub?gid=0&single=true&output=csv")),
                     stringsAsFactors = FALSE)


data_cup <- cup_points %>% filter(!(User %in% auto_32_winner)) %>% left_join(User_ID %>% 
                                       select(User = User_Nick,Draw_32)) %>% 
  arrange(Draw_32,User) %>% filter(GameID < 53)

vec <- c(rep(c(rep(TRUE,4),rep(FALSE,4)),4*30/8))

data_cup_a <- data_cup[which(vec==TRUE),] %>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_32,Game=NameID,Result)
data_cup_b <- data_cup[which(vec==FALSE),]%>% select(-c(CupID,User_Cup_ID)) %>% select(User,Bet,Points,Boom,WG=`Winning Goals`,GameID,CupID = CUPID,Draw_32,Game=NameID,Result)


the_data <- data_cup_a %>% 
  inner_join(data_cup_b,by=c('GameID','CupID','Game','Result','Draw_32')) %>% 
  select(-c(GameID,CupID,Draw_32))

the_data_img <- the_data %>% 
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
  ungroup() %>% mutate(Rank = rep_len(c(rep(1,4),rep(0,4)),60))



game_status <- fixtures %>% 
  select(Game = NameID,Active_Included,GameID,Stage = Cup_Stage) %>% 
  filter(GameID>48) %>% select(-GameID)



cup_col_by_user <- the_data_img_con %>% left_join(User_ID %>% 
                                                    select(User = User_Nick,Img),by = c('User.x'='Img')) %>%
  left_join(User_ID %>% 
              select(User = User_Nick,Img),by = c('User.y'='Img')) %>% 
  select(User_x = User.x.x, Agg.Pts.x, Game,User_y = User.y.y,Agg.Pts.y) %>% 
  inner_join(user_results_validation %>% select(User=User_Nick,Game=NameID,boom,winning_goals),by=c('User_x'='User','Game'='Game')) %>%
  inner_join(user_results_validation %>% select(User=User_Nick,Game=NameID,boom,winning_goals),by=c('User_y'='User','Game'='Game')) %>%
  inner_join(game_status,by=c('Game')) %>% group_by(User_x,User_y) %>% 
  mutate(num = 1:4) %>% 
  group_by(User_x,Stage) %>% mutate(Agg.Boom.x = cumsum(boom.x),
                                    Agg.WG.x = cumsum(winning_goals.x)) %>% 
  group_by(User_y,Stage) %>% mutate(Agg.Boom.y = cumsum(boom.x),
                                    Agg.WG.y = cumsum(winning_goals.x))

cup_palate <- list(winner = '#82E0AA',
                   loser  = '#CD6155',
                   draw = '#ED99E8',
                   game_over = '#566573',
                   future_game = '#F9E79F')

current_cup_rank <- fixtures$Cup_Rank[which(fixtures$GameID == current_game)]


###################

  polo_col <-  cup_col_by_user %>% mutate(
    col_user_x = ifelse(num == 4 & current_cup_rank == 4,
                        ifelse(Agg.Pts.x > Agg.Pts.y,cup_palate$winner,
                               ifelse(Agg.Pts.x < Agg.Pts.y,cup_palate$loser,
                                      ifelse(Agg.Boom.x > Agg.Boom.y ,cup_palate$winner,
                                             ifelse(Agg.Boom.x < Agg.Boom.y,cup_palate$loser,
                                                    ifelse(Agg.WG.x > Agg.WG.y , cup_palate$winner,
                                                           ifelse(Agg.WG.x < Agg.WG.y,cup_palate$loser,cup_palate$draw)))))),
                        ifelse(num < current_cup_rank ,cup_palate$game_over,
                               ifelse(num == current_cup_rank,
                                      ifelse(Agg.Pts.x > Agg.Pts.y,cup_palate$winner,
                                             ifelse(Agg.Pts.x < Agg.Pts.y,cup_palate$loser,
                                                    ifelse(Agg.Boom.x > Agg.Boom.y ,cup_palate$winner,
                                                           ifelse(Agg.Boom.x < Agg.Boom.y,cup_palate$loser,
                                                                  ifelse(Agg.WG.x > Agg.WG.y , cup_palate$winner,
                                                                         ifelse(Agg.WG.x < Agg.WG.y,cup_palate$loser,cup_palate$draw)))))),cup_palate$future_game)
                        ))
  ) %>% select(User_x,User_y,Stage,col_user_x,Game) %>% mutate(col_user_y = ifelse(col_user_x == cup_palate$loser,
                                                                                   cup_palate$winner,
                                                                                   ifelse(col_user_x == cup_palate$winner,cup_palate$loser,col_user_x)))
  
  
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
           `User II` = User.y,
           `Bet II` = Bet.y,
           `Pts II`=  Pts.y,
           `Agg.Pts II` = Agg.Pts.y,
           Stage = Stage.x,
           col_user_x,col_user_y,Rank)
  



server <- function(input, output, session) {
  
  polo <- reactive({ 
    if(input$somevalue){user_results_validation  %>% 
        filter( Active_Included %in% c("Active Games",'Complited Games') &
                                                               GameID >= input$game_number[1] & 
                                                               GameID <= input$game_number[2] ) 
    }else {user_results_validation  %>% 
        filter(  Active_Included %in% c('Complited Games') &
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
                    left_join(only_rank_for_vlookup,by = c("User_Nick")) %>% 
                    left_join(User_ID %>% select(User_Nick,Img),by = c("User_Nick")) %>% 
                   select(Rank,User=Img,Name = `User_Nick`,everything()),
                  options = list(pageLength = 10,
                                 columnDefs = list(list(width = 200, targets =  "_all" )),
                                 scrollX=TRUE,
                                 scrollCollapse=TRUE,
                                 pageLength = 30, lengthMenu = c(10,20,30)), 
                  rownames = FALSE,
                   escape = FALSE,
                  class = 'cell-border stripe') %>% 
      formatStyle('Rank',
                  target = 'row',
                  color  = styleEqual(c(1,2:5,6:N_users),c('#000066',rep('#660000',4),rep('#00384A',(N_users-6+1))))) %>%
      formatStyle('Rank',
                  target = 'row',
                  backgroundColor   = styleEqual(c(1),c('khaki')))
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
  
  names_x <- as.character(unique(the_data_img$User.x))
  names_y <- as.character(unique(the_data_img$User.y))
  n_user_div_2 <- length(names_y)
 
  
  chava_the_data_img <- reactive({final_cup_table %>% filter(Stage %in% input$cup_round) })
  
  
  output$cup_table <- renderDataTable({
    DT::datatable(chava_the_data_img() ,
                  options =  list(pageLength = nrow(chava_the_data_img()),
                                  columnDefs = list(list(width = 200, targets =  "_all" ),
                                                    list(visible=FALSE, targets=c(11,12,13))),
                                  scrollX=TRUE,
                                  scrollCollapse=TRUE,
                                  lengthMenu = c(30,40,200)), 
                  rownames = FALSE,
                  escape = FALSE)  %>% 
      formatStyle(c('Pts I','Bet I','User I','Agg.Pts I'),'col_user_x',
                  color="#00384A",
                  backgroundColor = styleEqual(as.vector(unlist(cup_palate)),
                                               as.vector(unlist(cup_palate)))) %>%
      formatStyle(c('Pts II','Bet II','User II','Agg.Pts II'),'col_user_y',
                  color="#00384A",
                  backgroundColor = styleEqual(as.vector(unlist(cup_palate)),
                                               as.vector(unlist(cup_palate)))) %>%
      formatStyle(c('Game','Result','Stage'),'Rank',
                  color=styleEqual(c(0,1),c("white","yellow")),
                  backgroundColor = '#00384A')
  })    # Additonal coulmn with image HTML url's

  output$terms1 <- renderUI({tags$img(src= 'http://farm2.staticflickr.com/1755/40553342420_2b7812f781_b.jpg',height = "600px",width = "380px")})
  output$terms2 <- renderUI({tags$img(src= 'http://farm2.staticflickr.com/1733/27490778887_06c5899c54_b.jpg',height = "600px",width = "380px")})
  output$terms3 <- renderUI({tags$img(src= 'http://farm2.staticflickr.com/1727/40553341900_72491ce6f0_b.jpg',height = "600px",width = "380px")})

  output$terms4 <- renderUI({tags$img(src= 'http://farm2.staticflickr.com/1749/41637765894_1f19062eea_b.jpg',height = "500px",width = "380px")})


  output$terms5 <- renderUI({tags$img(src= 'http://farm2.staticflickr.com/1745/41637765524_0e0869bd46_b.jpg',height = "500px",width = "380px")})

  r_16 <- resultes_knokout[1:N_users,2:18]
  r_8  <- resultes_knokout[(N_users+1):(2*N_users),c(2,19:26)]
  r_4  <- resultes_knokout[(2*N_users+1):(3*N_users),c(2,27:30)]
  r_2  <- resultes_knokout[(3*N_users+1):(5*N_users),c(2,31:34)]
  
  knokout <- r_16 %>% inner_join(r_16) %>%
    inner_join(r_8) %>%
    inner_join(r_4) %>%
    inner_join(r_2) %>% left_join(User_ID %>% select(Full.Name,User_Nick),by=c('User.Name'='Full.Name')) %>%
    select(-User.Name) %>% select(User = User_Nick,everything())
  
  names_knok <- fixtures$NameID[49:nrow(fixtures)]
  dup_names_knok <- paste0(names_knok," - winner")
  names(knokout) <- c('User_Nick',sort(c(names_knok,dup_names_knok)))
  
    

  all_games <- all_df_list$resultes$df %>% inner_join(knokout,by = c('User_Nick'))
  
  test_game_pre_result <- all_games %>% select(User = 'User_Nick',current_Game_Name) 
  
   
  test_game <- test_game_pre_result %>% 
    separate(current_Game_Name,into = c('U1.Home','U1.Away'),remove = FALSE) %>%
    mutate(T.Home = as.numeric(fixtures$true_Home_Goals[which(fixtures$NameID == current_Game_Name)]),
           T.Away = as.numeric(fixtures$true_Away_Goals[which(fixtures$NameID == current_Game_Name)]),
           U.Home = as.numeric(U1.Home),
           U.Away = as.numeric(U1.Away),
           D.Home = T.Home-U.Home,
           D.Away = T.Away-U.Away) %>%
     filter(D.Home<= 0 & D.Away<=0) %>% select(User,current_Game_Name)

  true_result <- fixtures[which(fixtures$NameID == names(test_game)[2]),
                          c(which(names(fixtures) == 'true_Home_Goals'),
                            which(names(fixtures)== 'true_Away_Goals'))]
  
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
  
  



  output$network <- renderVisNetwork({
    visNetwork(nodes, edges,width = "100%",height  =600 ,
               main = list(text =  paste0(current_Game_Name),
                          style = "font-family:Comic Sans MS;color:#F7DC6F;font-size:20px;text-align:center;"),
                          submain  =  list(text =paste0("Live Score:  " ,paste0(true_result,collapse = " - ") ) ,
                              style = "font-family:Comic Sans MS;color:#F7DC6F;font-size:20px;text-align:center;" ) ) %>% 
  visInteraction(dragNodes = TRUE, dragView = FALSE)
  })  


}






