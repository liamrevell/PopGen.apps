library(shiny)
library(PopGen)

ui<-fluidPage(
  h3("Proving the Central Limit Theorem",align="center"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="nvar",label="number of variables",
        min=0,max=100,value=1),
      sliderInput(inputId="nobs",label="number of observations",
        min=0,max=10000,value=4000),
      selectInput(inputId="df",label="distribution function",
        choices=c("exponential","normal","uniform")),
      numericInput(inputId="breaks",label="number of breaks in histogram",
        min=1,max=40,value=20),
      checkboxInput(inputId="react",
        label=
        "reactive (plot changes instantly with new parameter values)",
        value=TRUE),
      textInput(inputId="seed",label="set seed (optional)",
        value=""),
      actionButton("newplot","new plot"),
      h4("Instructions:\n"),
      p("Change the number of variables or underlying 
        distribution functions to prove CLT to yourself.\n\n"),
      h4("Details:\n"),
      p("Web application for clt of the PopGen R package 
        (Revell, 2018).")
    ),
    mainPanel(
      plotOutput("plot",width="600px",height="600px")
    )
  )
)

server <- function(input, output) {
  output$plot<-renderPlot({
    if(input$seed!="") set.seed(as.numeric(input$seed))
    if(!input$react){
      input$newplot
      isolate(
        if(input$nvar>0&&input$nobs>0){
          clt(input$nvar,input$nobs,input$df,breaks=input$breaks)
          if(input$nvar<=1) mtext("add more!")
          else if(input$nvar<=10) mtext(paste(input$nvar,"may not be enough!"))
          else if(input$nvar<=20) mtext("looks pretty normal, right?")
          else if(input$nvar<=50) mtext("definitely (pretty much) normal by now!")
          else mtext("GbN: gotta be normal!")
        }
      )
    } else {
      input$newplot
      if(input$nvar>0&&input$nobs>0){
        clt(input$nvar,input$nobs,input$df,breaks=input$breaks)
        if(input$nvar<=1) mtext("add more!")
        else if(input$nvar<=10) mtext(paste(input$nvar,"may not be enough!"))
        else if(input$nvar<=20) mtext("looks pretty normal, right?")
        else if(input$nvar<=50) mtext("definitely (pretty much) normal by now!")
        else mtext("GbN: gotta be normal!")      }
    }
  })
}

shinyApp(ui = ui, server = server)
