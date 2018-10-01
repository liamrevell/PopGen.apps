library(shiny)
library(PopGen)

ui <- fluidPage(
  sliderInput(inputId="ngen",
              label="Number of generations",
              value=10,min=0,max=100),
  actionButton("newplot","new plot"),
  ngen<-input$ngen
  for(i in 0:ngen){
    input$ngen<-i
    plotOutput(outputId="plot")
  }
)

server <- function(input, output) {
  output$plot<-renderPlot({
    phenotype.selection(nloci=8,p=rep(0.1,8),ngen=100,sleep=0)
  })
}

shinyApp(ui = ui, server = server)
