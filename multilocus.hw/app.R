library(shiny)
library(PopGen)

ui <- fluidPage(
  sliderInput(inputId="nloci",
              label="Number of biallelic loci",
              value=1,min=1,max=5),
  plotOutput(outputId="hist")
)

server <- function(input, output) {
  output$hist<-renderPlot({ 
      title<-"Multi-locus Hardy-Weinberg frequencies (all p=0.5)"
      barplot(multilocus.hw(input$nloci),las=2,
              cex.names=3.5/input$nloci^1.3)
  })
}

shinyApp(ui = ui, server = server)
