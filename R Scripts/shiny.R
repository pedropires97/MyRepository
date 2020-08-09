require(shiny)

# parte do user interface
# aonde coleta os inputs e mostra os outputs
# refere com 'objeto'

ui <- fluidPage(
  'Hello, world!!!'
)

# parte do server
# onde faz as contas
# refere como obj$eto

server <- function(input, output, session){ 
  
}

shinyApp(ui = ui, server = server)

##############

ui <- fluidPage(
  textInput("name", "Name:") #coleta um input de texto
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)

#############

ui <- fluidPage(
  textInput("name", "What is your name?"),
  textOutput("greeting") #mostra o output que foi calculado no server
)

server <- function(input, output) {
  output$greeting <- renderText({ #calcula um output baseado em um input do ui
    paste("Hello", input$name)
  })
  
}

shinyApp(ui = ui, server = server)

##############

require(ggplot2)
require(babynames)

ui <- fluidPage(
  titlePanel("Baby Name Explorer"), #titulo do painel
  sidebarLayout( #layout
    sidebarPanel( #layout
      textInput('name', 'Enter Name', 'David')
    ),
    mainPanel( #layout
      plotOutput('trend')
      )
  )
)

server <- function(input, output, session) {
  output$trend <- renderPlot({ #output de plot
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x = year, y = prop, color = sex))
  })
}
shinyApp(ui = ui, server = server)

##############

# tipos de input
textInput
selectInput
numericInput
sliderInput
checkBoxInput
radioInput
dateInput
dateRangeInput
verbatimTextOutput
passwordInput
textAreaInput
# botoes, lista, check box, link...

selectInput('sex', 'sex:', selected='F', choices=c('M', 'F'))

# outputs
# passo 1: criar output
# passo 2: renderizar com a funcao render___({}) apropriada
# passo 3: assign para uma variavel output$x
# passo 4: jogar no UI com a funcao ___Output apropriada

# tabela
tableOutput('x') #UI
renderTable({output$x}) #server

# tabela interativa
DT::datatable() #funcao pra ficar interativa
DT::renderDT() #server
DT::DTOutput() #UI

dataTableOutput()
renderDataTable()

# plotly
plotly::plotlyOutput

# wordcloud: D3

##############

# essa funcao roda um shiny com varias opcoes de input para escolher
require(shinyWidgets)
require(shinydashboard)
shinyWidgetsGallery()

##############

require(babynames)
require(plotly)

ui <- fluidPage(
  selectInput('name', 'Select Name', choices = c('Mary', 'Anna', 'Emma')),

  plotly::plotlyOutput('plot_trendy_names')
)

server <- function(input, output, session){

  output$plot_trendy_names = renderPlotly({
    babyfilter = filter(babynames, babynames$name == input$name) #criei objetos aqui dentro
    p = ggplot(babyfilter, aes(x = year, y = n)) + geom_col() 
    p
    })
}
shinyApp(ui = ui, server = server)

##############

# layout
sidebarLayout() #layout com sidebar
sidebarPanel() #painel na sidebar (esta dentro do sidebarLayout)
mainPanel() #painel central (esta dentro do sidebarLayout)
tabsetPanel() #criar abas dentro do mainPanel
tabPanel() #o que vai entrar em qual aba

# o mais comum eh esse layout com sidebar
# mas tem outros, ex:
fluidPage()...fluidRow()...column()
column(4,...) #tamanho da coluna, max 12 

titlePanel() #titulo do painel

shinythemes::themeSelector() #add caixa para selecionar tema
shinythemes::shinytheme("cerulean") #seleciona tema pre definido

# texto sem ser titulo
mainPanel(
  h1("Header 1"),
  h2("Header 2"),
  p("Some text"))

hr() #pula um linha de um objeto para outro

##############

ui <- fluidPage(
  
  titlePanel("aaaa"),
  
  shinythemes::themeSelector(), #tema
  
  sidebarLayout(
    sidebarPanel(
      selectInput('name', 'Select Name', choices = c('Mary', 'Anna', 'Emma'))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot', plotly::plotlyOutput('plot_trendy_names')),
        tabPanel('Table', DT::DTOutput('table_trendy_names'))
      )
    )
  )
)
server <- function(input, output, session){
  
  plot_trends <- function(){
    babynames %>% 
      filter(name == input$name) %>% 
      ggplot(aes(x = year, y = n)) +
      geom_col()
  }
  output$plot_trendy_names <- plotly::renderPlotly({
    plot_trends()
  })
  
  output$table_trendy_names <- DT::renderDT({
    babynames %>% 
      filter(name == input$name)
  })
}
shinyApp(ui = ui, server = server)

# dica: usar unique() ou level() no choices

##############

# reactive!
# cria um objeto, baseado no input, que vai ser usado para construir o output
# ex: um filtro
# eh bom que so roda uma vez
# importante: se vc rodar duas coisas aleatorias, mesmo que com parametros iguais,
# sem reactive, elas vao vir diferentes!

ui <- fluidPage(
  titlePanel('BMI Calculator'),
  sidebarLayout(
    sidebarPanel(
      numericInput('height', 'Enter your height in meters', 1.5, 1, 2),
      numericInput('weight', 'Enter your weight in Kilograms', 60, 45, 120)
    ),
    mainPanel(
      textOutput("bmi"),
      textOutput("bmi_range")
    )
  )
)

server <- function(input, output, session) {
  
  rval_bmi <- reactive({ #reactive
    input$weight/(input$height^2)
  })
  
  output$bmi <- renderText({
    
    bmi <- rval_bmi() #tem que usar esse ()
    paste("Your BMI is", round(bmi, 1)) #ou rval_bmi() direto
  })
  output$bmi_range <- renderText({
    
    bmi <- rval_bmi()
    bmi_status <- cut(bmi, 
                      breaks = c(0, 18.5, 24.9, 29.9, 40),
                      labels = c('underweight', 'healthy', 'overweight', 'obese')
    )
    paste("You are", bmi_status)
  })
}

shinyApp(ui = ui, server = server)

##############

# pode usar um reactive dentro do outro

rval_bmi_status <- reactive({
  cut(rval_bmi(), 
      breaks = c(0, 18.5, 24.9, 29.9, 40),
      labels = c('underweight', 'healthy', 'overweight', 'obese'))
})

##############

# observer
# faz alguma acao mas nao guarda nenhum objeto

ui <- fluidPage(
  textInput('name', 'Enter your name')
)

server <- function(input, output, session) {
  
  observe({
    showNotification( #pop up
      paste("You entered the name", input$name)
    )
  })
  
}

shinyApp(ui = ui, server = server)

##############

# isolate
# output so atualiza quando outras variaveis mudam
# quando a variavel dentro do isolate muda, nada acontece

ui <- fluidPage(
  titlePanel('BMI Calculator'),
  sidebarLayout(
    sidebarPanel(
      textInput('name', 'Enter your name'),
      numericInput('height', 'Enter your height (in m)', 1.5, 1, 2, step = 0.1),
      numericInput('weight', 'Enter your weight (in Kg)', 60, 45, 120)
    ),
    mainPanel(
      textOutput("bmi")
    )
  )
)

server <- function(input, output, session) {
  rval_bmi <- reactive({
    input$weight/(input$height^2)
  })
  output$bmi <- renderText({
    bmi <- rval_bmi()
    paste("Hi", isolate({input$name}), ". Your BMI is", round(bmi, 1)) #aqui
  })
}

shinyApp(ui = ui, server = server)

##############

# delay
# so calcula output quando ocorre uma acao
# ex: apertar um botao

ui <- fluidPage(
  titlePanel('BMI Calculator'),
  sidebarLayout(
    sidebarPanel(
      textInput('name', 'Enter your name'),
      numericInput('height', 'Enter height (in m)', 1.5, 1, 2, step = 0.1),
      numericInput('weight', 'Enter weight (in Kg)', 60, 45, 120),
      actionButton("show_bmi", "Show BMI") #add o botao aqui
    ),
    mainPanel(
      textOutput("bmi")
    )
  )
)

server <- function(input, output, session) {
  
  rval_bmi <- eventReactive(input$show_bmi, { #funcao, botao e calculo
    input$weight/(input$height^2)
  })
  output$bmi <- renderText({
    bmi <- rval_bmi()
    paste("Hi", input$name, ". Your BMI is", round(bmi, 1))
  })
}

shinyApp(ui = ui, server = server)

##############

# trigger
# parecido com o delay
# realiza uma acao quando acontece algo, sem calcular output
# ex: mostra um pop up quando aperta um botao

ui <- fluidPage(
  titlePanel('BMI Calculator'),
  sidebarLayout(
    sidebarPanel(
      textInput('name', 'Enter your name'),
      numericInput('height', 'Enter your height in meters', 1.5, 1, 2),
      numericInput('weight', 'Enter your weight in Kilograms', 60, 45, 120),
      actionButton("show_bmi", "Show BMI"),
      actionButton('show_help', "Help") #botao
    ),
    mainPanel(
      textOutput("bmi")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$show_help, {showModal(modalDialog("aaaa"))}) #funcao, botao, acao
  
  rv_bmi <- eventReactive(input$show_bmi, {
    input$weight/(input$height^2)
  })
  output$bmi <- renderText({
    bmi <- rv_bmi()
    paste("Hi", input$name, ". Your BMI is", round(bmi, 1))
  })
}

shinyApp(ui = ui, server = server)

##############

# mensagem incial de erro

server <- function(input, output, session) {
  output$age <- renderTable({
    
    validate(
      need(input$age != "", "Be sure to select an age.")
    )
    
    mental_health_survey %>%
      summarize(avg_age = mean(Age))
  })
}

##############

ui <- bootstrapPage( #layout
  theme = shinythemes::shinytheme('simplex'),
  leaflet::leafletOutput('map', width = '100%', height = '100%'),
  absolutePanel(top = 10, right = 10, id = 'controls',
                sliderInput('nb_fatalities', 'Minimum Fatalities', 1, 40, 10),
                dateRangeInput('date_range', 'Select Date', "2010-01-01", "2019-12-01"),
                actionButton("show_about", "show_about")
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}   
    #controls{background-color:white;padding:20px;}")
)
server <- function(input, output, session) {

  observeEvent(input$show_about,{showModal(modalDialog(text_about, title = 'About'))})
  
  output$map <- leaflet::renderLeaflet({
    mass_shootings %>% 
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        fatalities >= input$nb_fatalities
      ) %>% 
      leaflet() %>% 
      setView( -98.58, 39.82, zoom = 5) %>% 
      addTiles() %>% 
      addCircleMarkers(
        popup = ~ summary, radius = ~ sqrt(fatalities)*3,
        fillColor = 'red', color = 'red', weight = 1
      )
  })
}

shinyApp(ui, server)

##############

# shiny atualiza sozinho (ex: simulacao)

server <- function(input, output, session) {
  
  timer <- reactiveTimer(500) #aqui
  
  x1 <- reactive({
    timer() #aqui
    rpois(input$n, input$lambda1)
  })
  
  output$hist <- renderPlot({hist(x1())}, res = 96)
}
shinyApp(ui = ui, server = server)

##############

# pode usar condicional
# ex: dentro do render, no server
# outra dica, usar res = 96 dentro do renderPlot

output$age_sex <- renderPlot({
  if (input$y == "count") {
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  } else {
    summary() %>%
      ggplot(aes(age, rate, colour = sex)) +
      geom_line(na.rm = TRUE) +
      labs(y = "Injuries per 10,000 people")
  }
}, res = 96)

##############

# outras possibilidades

# registrar cliques e interagir no ggplot
# tamanho dinamico do grafico
# renderCachedPlot() para plots mais pesados
# renderImage() para imagem

#############

# so roda se tiver preenchido os dois inputs

server <- function(input, output, session) {
  greetings <- c(
    English = "Hello", 
    Maori = "Ki ora"
  )
  output$greeting <- renderText({
    req(input$language, input$name) #aqui
    paste0(greetings[[input$language]], " ", input$name, "!")
  })
}

# mensagem de erro

require(shinyFeedback)

server <- function(input, output, session) {
  half <- reactive({
    even <- input$n %% 2 == 0
    shinyFeedback::feedbackWarning("n", !even, "Please select an even number")
    req(even)
    input$n / 2    
  })
  
  output$half <- renderText(half())
}

#############

require(shinydashboard)

# transformar shiny em uma dashboard mais customizavel
# sistema eh o mesmo
# da pra customizar bastante coisa

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

#############

# dados podem ficar dentro do server ou fora de tudo
# objetos que necessitam do input vao dentro do output
# demais podem ser fora (ex: dados, funcoes...)

# opcoes de run app quando abre um arquivo shiny
# pop up, browser e viewer

# para dploy
installed.packages("rsconnect")
rsconnect::setAccountInfo(name='pedropires',
                          token='BC6CF7A5CA07F9CBBF995FAA68B85CD1',
                          secret='bzlgn2qHcmk3ICB5oHgpfS/qFVK1lyOv87KAdS/U')
# publish dentro do rstudio

