library(shiny)
library(rhandsontable)
library(learnPopGen)

M<-matrix(c("0.6","1.5","0.5","1.0"),2,2,byrow=TRUE,dimnames=list(c("hawk","dove"),
  c("hawk","dove")))

ui <- fluidPage(
  h3("Hawk-dove game",align="center"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="p",label="p (initial frequency of hawks):",value=0.01,
        min=0,max=1,step=0.01),
      sliderInput(inputId="time",label="number of generations:",value=100,min=0,
        max=1000),
      p(strong("Pay-off matrix:")),
      rHandsontableOutput("M"),
      p("(pay-off to row when interacting with column)"),
      checkboxInput(inputId="react",
        label="reactive (plot changes instantly with new parameter values)",
        value=TRUE),
      actionButton("newplot","new plot"),
      h4("Instructions:\n"),
      p("This application numerically analyzes a simple game-theoretic hawk-dove
        model in which strategy is heritable across generations, and competitive 
        outcome determines fitness.\n\n"),
      h4("Details:\n"),
      p("Web interface for hawk.dove of the learnPopGen R package (Revell, 2019).")
    ),
    mainPanel(
      plotOutput("plot",width="600px",height="800px")
    )
  )
)

server <- function(input, output) {
  values=reactiveValues(M=M)
  output$plot<-renderPlot({
    par(mar=c(5.1,4.1,4.1,1.1))
    if(!input$react){
      input$newplot
      isolate({
        if(input$time>0){
          M<-matrix(as.numeric(values[["M"]]),2,2,dimnames=list(c("hawk","dove"),c("hawk","dove")))
          hawk.dove(c(input$p,1-input$p),M=M,time=input$time)
        }
       })
    } else {
      input$newplot
      if(input$time>0){
        M<-matrix(as.numeric(values[["M"]]),2,2,dimnames=list(c("hawk","dove"),c("hawk","dove")))
        hawk.dove(c(input$p,1-input$p),M=M,time=input$time)
      }
    }
  })
  output$M = renderRHandsontable({
    if (!is.null(input$M)) {
      DF = hot_to_r(input$M)
      values[["M"]] = DF
      rhandsontable(DF,colHeaders=c("hawk","dove"),rowHeaders=c("hawk","dove"))
    } else if (!is.null(values[["M"]])) {
      DF = values[["M"]]
      rhandsontable(DF)
    }
  })
}

shinyApp(ui = ui, server = server)
