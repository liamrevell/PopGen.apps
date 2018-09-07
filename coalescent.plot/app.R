library(shiny)
library(PopGen)

ui <- fluidPage(
  sliderInput(inputId="n",
              label="number of individuals",
              value=10,min=1,max=100),
  sliderInput(inputId="ngen",
              label="number of generations",
              value=20,min=2,max=100)
  plotOutput(outputId="coal")
)

server <- function(input, output) {
  output$coal<-renderPlot({ 
    coalescent.plot()
  })
}

shinyApp(ui = ui, server = server)
