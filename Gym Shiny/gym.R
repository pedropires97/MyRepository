# Inicialization

require(shiny)
require(lubridate)
require(shinydashboard)
require(shinyWidgets)

ui <- dashboardPage(
  
  dashboardHeader(title = "Home Workout"),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
  fluidRow(
    box(title = "Control Panel", width = 4, status = "info", solidHeader = TRUE,
        sliderInput("minutes", "Time:", min = 1, max = 60, value = 30),
        hr(),
        actionBttn(inputId = "start", label = "Start", style = "simple", color = "primary", size='md'),
        actionBttn(inputId = "stop", label = "Stop", style = "simple", color = "primary", size='md'),
        actionBttn(inputId = "reset", label = "Reset", style = "simple", color = "primary", size='md'),
        hr(),
        actionBttn(inputId = "sample", label = "Generate", style = "simple", color = "primary", block=T)
     ),
     
    box(title = "Exercices", width = 4, status = "success", solidHeader = TRUE,
        tableOutput("exercices"),
        tags$style("#exercices {font-size:20px;
                    color:black;
                    display:block; }")
     ),
    
    box(title = "Timer", width = 4, status = "warning", solidHeader = TRUE,
        textOutput('timeleft'),
        tags$style("#timeleft {font-size:100px;
                    color:black;
                    text-align:center;
                    display:block; }")
    )
  )
  )
)

server <- function(input, output, session) {

# exercices
  
exercices = c('Jumping Jacks 45'
             ,'Agachamento 20 + peso'
             ,'Corrida 60 / High Knee 45'
             ,'Lunge 26 + peso'
             ,'Burpee 12'
             ,'Jumping Calf Raise 45'
             ,'Push Ups 10'
             ,'Lateral drill 45'
             ,'Jumping Lunge 30')

gera_exe <- eventReactive(input$sample, {
  sample = vector()
  sample[1] = exercices[1]
  sample[2:length(exercices)] = sample(exercices[-1], length(exercices)-1, replace = F)
  as.data.frame(sample)
})

output$exercices <- renderTable({
   gera_exe()
}, colnames = F)

# timer

timer <- reactiveVal(30*60)
active <- reactiveVal(FALSE)

output$timeleft <- renderText({
  paste(seconds_to_period(timer()))
})

observe({
  invalidateLater(1000, session)
  isolate({
    if(active())
    {
      timer(timer()-1)
      if(timer()<1)
      {
        active(FALSE)
        showModal(modalDialog(
          title = "",
          "Countdown completed!"
        ))
      }
    }
  })
})

min_to_sec <- reactive({
  input$minutes*60
})

observeEvent(input$start, {active(TRUE)})
observeEvent(input$stop, {active(FALSE)})
observeEvent(input$reset, {timer(min_to_sec())})

}

shinyApp(ui = ui, server = server)
