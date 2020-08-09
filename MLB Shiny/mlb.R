# pacotes

require(Lahman) #dados
require(dplyr) #manipulacao dados
require(lubridate) #data
require(shiny)
require(DT) #tabela interativa
require(shinydashboard) #costumizacao shiny
require(shinyWidgets) #inputs shiny
require(fmsb) #radar chart

#### Ajeitando bases ####

# base rebatedores

people_b = People %>% 
  filter(year(finalGame) == 2019) %>%
  transmute(playerID, name = paste(nameFirst, nameLast), bats, age = as.numeric(floor((as.Date("2019-09-29") - birthDate)/365)))

batting = Batting %>%
  filter(yearID == 2019) %>%
  group_by(playerID) %>%
  summarize(AB=sum(AB), R=sum(R), H=sum(H), HR=sum(HR), RBI=sum(RBI), SB=sum(SB), SO=sum(SO), BB=sum(BB), HBP=sum(HBP), SF = sum(SF), AB=sum(AB), X2B=sum(X2B), X3B=sum(X3B)) %>%
  filter(AB > 100) %>%
  transmute(playerID, AB, R, HR, RBI, SB, SOP = SO/(AB+BB+HBP+SF), AVG = H/AB, OBP = (H+BB+HBP)/(AB+BB+HBP+SF), SLG = (((H-X2B-X3B-HR)+X2B*2+X3B*3+HR*4)/AB))

fielding = Fielding %>%
  filter(yearID == 2019, POS != 'P') %>%
  group_by(playerID, POS) %>%
  summarize(G = sum(G)) %>%
  group_by(playerID) %>%
  filter(G == max(G)) %>%
  select(-G)

temp1 = left_join(batting, people_b, by="playerID")
temp2 = left_join(temp1, fielding, by="playerID")
batters = filter(temp2, !(playerID == 'adriaeh01' & POS == '3B'), !(playerID == 'beatyma01' & POS == 'OF')) %>%
  transmute(Name=name, POS = ifelse(is.na(POS),"DH",POS), AVG=round(AVG,3), OBP=round(OBP,3), SLG=round(SLG,3), RBI, HR, SB, '%SO'=round(SOP,3), Bats=bats, Age=age) %>%
  arrange(desc(RBI))

# base pitchers

people_p = People %>% 
  filter(year(finalGame) == 2019) %>%
  transmute(playerID, name = paste(nameFirst, nameLast), throws, age = as.numeric(floor((as.Date("2019-09-29") - birthDate)/365)))

pitching = Pitching %>% 
  filter(yearID == 2019) %>%
  group_by(playerID) %>%
  summarize(W=sum(W), GS=sum(GS), SV=sum(SV), IPouts=sum(IPouts), SO=sum(SO), ER=sum(ER), BB=sum(BB), H=sum(H), BFP=sum(BFP), HBP=sum(HBP), SH=sum(SH), SF=sum(SF)) %>%
  filter(IPouts >= 9) %>%
  transmute(playerID, W, GS, SV, Inn = floor(IPouts/3), SOP = 9*SO/(IPouts/3), BBP = 9*BB/(IPouts/3), AVG = (H/(BFP-BB-HBP-SH-SF)), ERA = 9*ER/(IPouts/3),
            POS = case_when((IPouts/3)>100 | GS>=4 ~ "SP",
                            GS<=1 ~ "RP",
                            TRUE ~ "Undefined"))

pitchers = left_join(pitching, people_p, by='playerID') %>%
  transmute(Name=name, ERA = round(ERA,2), AVG = round(AVG,3), GS, W, SV, Inn, 'SO/9' = round(SOP,2), 'BB/9' = round(BBP,2), Throws=throws, Age=age, POS) %>%
  arrange(desc(Inn))

# radar chart

batters_radar = data.frame(matrix(rep(0,8*2),2,8))
names(batters_radar) = c('AVG', 'OBP', 'SLG', 'RBI', 'HR', 'SB', '%SO', 'Age')
batters_radar[1,] = c(0.345, 0.438, 0.671, 127, 53, 46, 0.039, 21)
batters_radar[2,] = c(0.125, 0.196, 0.167, 0, 0, 0, 0.458, 39)

pitchers_radar = data.frame(matrix(rep(0,9*2),2,9))
names(pitchers_radar) = c('ERA', 'AVG', 'GS', 'W', 'SV', 'Inn', 'SO/9', 'BB/9', 'Age')
pitchers_radar[1,] = c(0, 0.094, 34, 21, 41, 223, 17.65, 0, 20)
pitchers_radar[2,] = c(27, 0.526, 0, 0, 0, 3, 0, 18, 42)

colors_border = c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9))
colors_in = c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4))

#### Shiny ####

ui <- dashboardPage(
  
  skin = "red",
  
  dashboardHeader(title = "2019 MLB Players Stats", titleWidth = 350),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(startExpanded = F,
      menuItem("Compare the Batters", tabName = "batterscomp", icon = icon("bar-chart-o"), badgeLabel = "new", badgeColor = "green"),
      menuItem("Compare the Pitchers", tabName = "pitcherscomp", icon = icon("bar-chart-o"), badgeLabel = "new", badgeColor = "green"),
      menuItem("Batters", tabName = "batterstab", icon = icon("th"), selected = T),
      menuItem("Pitchers", tabName = "pitcherstab", icon = icon("th")),
      menuItem("Batters Filters", startExpanded = TRUE,
               sliderInput("avg", "AVG", min = 0.125, max = 0.345, value = c(0.125,0.345), ticks = F, width=350),
               sliderInput("obp", "OBP", min = 0.196, max = 0.438, value = c(0.196,0.438), ticks = F, width=350),
               sliderInput("slg", "SLG", min = 0.167, max = 0.671, value = c(0.167,0.671), ticks = F, width=350),
               sliderInput("rbi", "RBI", min = 0, max = 127, value = c(0,127), ticks = F, width=350),
               sliderInput("hr", "HR", min = 0, max = 53, value = c(0,53), ticks = F, width=350),
               sliderInput("sb", "SB", min = 0, max = 46, value = c(0,46), ticks = F, width=350),
               sliderInput("so", "%SO", min = 0.039, max = 0.458, value = c(0.039,0.458), ticks = F, width=350),
               sliderInput("age", "Age", min = 21, max = 39, value = c(21,39), ticks = F, width=350),
               checkboxGroupInput("pos", "POS", choices = c("C", "1B", "2B", "SS", "3B", "OF", "DH"), selected = c("C", "1B", "2B", "SS", "3B", "OF", "DH"), inline=T, width=350),
               checkboxGroupInput("bats", "Bats", choices = c("R", "L", "B"), selected = c("R", "L", "B"), inline=T, width=350),
               hr()),
      menuItem("Pitcher Filters",
               sliderInput("era", "ERA", min = 0.0, max = 27.5, value = c(0.0,27.5), ticks = F, width=350),
               sliderInput("avga", "AVG", min = 0.094, max = 0.526, value = c(0.094,0.526), ticks = F, width=350),
               sliderInput("gs", "GS", min = 0, max = 34, value = c(0,34), ticks = F, width=350),
               sliderInput("w", "W", min = 0, max = 21, value = c(0,21), ticks = F, width=350),
               sliderInput("sv", "SV", min = 0, max = 41, value = c(0,41), ticks = F, width=350),
               sliderInput("inn", "Inn", min = 3, max = 223, value = c(0,223), ticks = F, width=350),
               sliderInput("sop", "SO/9", min = 0.00, max = 17.65, value = c(0.00,17.65), ticks = F, width=350),
               sliderInput("bbp", "BB/9", min = 0.00, max = 18.01, value = c(0.00,18.01), ticks = F, width=350),
               sliderInput("agep", "Age", min = 20, max = 42, value = c(20,42), ticks = F, width=350),
               checkboxGroupInput("posp", "POS", choices = c("SP","RP","Undefined"), selected = c("SP","RP","Undefined"), inline=T, width=350),
               checkboxGroupInput("throws", "Throws", choices = c("R", "L", "S"), selected = c("R", "L", "S"), inline=T, width=350),
               hr()),
      menuItem("About", tabName = "about", icon = icon('question-circle'))
    )
  ),
  
  dashboardBody(tabItems(
    
    tabItem(tabName = "batterscomp",
            fluidRow(box(title = "Select the Players", width = 4,
                         selectInput("batter1", "Genoa", choices = batters$Name, selected = batters$Name[1]),
                         selectInput("batter2", "Medium Red Violet", choices = batters$Name, selected = batters$Name[2]),
                         selectInput("batter3", "Buddha Gold", choices = batters$Name, selected = batters$Name[3])),
                     box(title = "Radar Chart", width = 8,
                         plotOutput("plotbatters")))
    ),
    
    tabItem(tabName = "pitcherscomp",
            fluidRow(box(title = "Select the Players", width = 4,
                         selectInput("pitcher1", "Genoa", choices = pitchers$Name, selected = pitchers$Name[1]),
                         selectInput("pitcher2", "Medium Red Violet", choices = pitchers$Name, selected = pitchers$Name[2]),
                         selectInput("pitcher3", "Buddha Gold", choices = pitchers$Name, selected = pitchers$Name[3])),
                     box(title = "Radar Chart", width = 8,
                         plotOutput("plotpitchers")))
    ),
    
    tabItem(tabName = "batterstab",
            DTOutput("batters"),
            downloadButton("downloadData_b", "Download Data")
    ),
      
    tabItem(tabName = "pitcherstab",
            DTOutput("pitchers"),
            downloadButton("downloadData_p", "Download Data")
    ),
    
    tabItem(tabName = "about",
            HTML(
              paste(
                h3("This is my app!"),'<br/>',
                h4("You may select and view the players using the filters or compare up to three of them rigth away."),'<br/>',
                h4("More info and code:"),
                h4("https://github.com/pedropires97/MyRepository")
                   )
                ))
    
   )
  )
  
)

server <- function(input, output) {
  
  batters_filtered <- reactive({
    batters %>% filter(AVG >= input$avg[1], AVG <= input$avg[2],
                       OBP >= input$obp[1], OBP <= input$obp[2],
                       SLG >= input$slg[1], SLG <= input$slg[2],
                       RBI >= input$rbi[1], RBI <= input$rbi[2],
                       HR >= input$hr[1], HR <= input$hr[2],
                       SB >= input$sb[1], SB <= input$sb[2],
                       `%SO` >= input$so[1], `%SO` <= input$so[2],
                       Age >= input$age[1], Age <= input$age[2],
                       POS %in% input$pos,
                       Bats %in% input$bats)
  })
  
  pitchers_filtered <- reactive({
    pitchers %>% filter(ERA >= input$era[1], ERA <= input$era[2],
                        AVG >= input$avga[1], AVG <= input$avga[2],
                        GS >= input$gs[1], GS <= input$gs[2],
                        W >= input$w[1], W <= input$w[2],
                        SV >= input$sv[1], SV <= input$sv[2],
                        Inn >= input$inn[1], Inn <= input$inn[2],
                        `SO/9` >= input$sop[1], `SO/9` <= input$sop[2],
                        `BB/9` >= input$bbp[1], `BB/9` <= input$bbp[2],
                        Age >= input$agep[1], Age <= input$agep[2],
                        POS %in% input$posp,
                        Throws %in% input$throws)
  })
  
  output$batters <- renderDT({
    datatable(batters_filtered(), rownames = FALSE, options = list(columnDefs = list(list(className = 'dt-center', targets = 1:10)), pageLength = 35, lengthMenu = c(10, 35, 50)))
  })
  
  output$pitchers <- renderDT({
    datatable(pitchers_filtered()[,-c(12)], rownames = FALSE, options = list(columnDefs = list(list(className = 'dt-center', targets = 1:10)), pageLength = 35, lengthMenu = c(10, 35, 50)))
  })
  
  batters_selected <- reactive({
    batters_radar[3,] <- batters %>%
      filter(Name == input$batter1) %>%
      select(AVG, OBP, SLG, RBI, HR, SB, `%SO`, Age)
    batters_radar[4,] <- batters %>%
      filter(Name == input$batter2) %>%
      select(AVG, OBP, SLG, RBI, HR, SB, `%SO`, Age)
    batters_radar[5,] <- batters %>%
      filter(Name == input$batter3) %>%
      select(AVG, OBP, SLG, RBI, HR, SB, `%SO`, Age)
    batters_radar
  })
  
  batters_legend <- reactive({
      b_legend = c(input$batter1, input$batter2, input$batter3)
  })
  
  output$plotbatters <- renderPlot({
    radarchart(batters_selected(),
               pcol=colors_border, plwd=2, plty=1,
               cglcol="grey", cglty=1)
    legend(x=1.25, y=1.25, legend = batters_legend(), bty = "n", pch="-", col=colors_border, text.col = "black", cex=1, pt.cex=4)
  })
  
  pitchers_selected <- reactive({
    pitchers_radar[3,] <- pitchers %>%
      filter(Name == input$pitcher1) %>%
      select(ERA, AVG, GS, W, SV, Inn, `SO/9`, `BB/9`, Age)
    pitchers_radar[4,] <- pitchers %>%
      filter(Name == input$pitcher2) %>%
      select(ERA, AVG, GS, W, SV, Inn, `SO/9`, `BB/9`, Age)
    pitchers_radar[5,] <- pitchers %>%
      filter(Name == input$pitcher3) %>%
      select(ERA, AVG, GS, W, SV, Inn, `SO/9`, `BB/9`, Age)
    pitchers_radar
  })
  
  pitchers_legend <- reactive({
    p_legend = c(input$pitcher1, input$pitcher2, input$pitcher3)
  })
  
  output$plotpitchers <- renderPlot({
    radarchart(pitchers_selected(),
               pcol=colors_border, plwd=2, plty=1,
               cglcol="grey", cglty=1)
    legend(x=1.25, y=1.25, legend = pitchers_legend(), bty = "n", pch="-", col=colors_border, text.col = "black", cex=1, pt.cex=4)
  })
  
  output$downloadData_b <- downloadHandler(
    filename = "batters_data.csv",
    content = function(file) {
      write.csv(batters_filtered(), file, row.names = F)
    }
  )
  
  output$downloadData_p <- downloadHandler(
    filename = "pitchers_data.csv",
    content = function(file) {
      write.csv(pitchers_filtered(), file, row.names = F)
    }
  )
  
}

shinyApp(ui = ui, server = server)
