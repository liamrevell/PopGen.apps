library(shiny)
library(PopGen)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
     sliderInput(inputId="n",
                  label="number of individuals",
                  value=10,min=1,max=100),
      sliderInput(inputId="ngen",
                  label="number of generations",
                  value=20,min=2,max=100),
      actionButton("newplot", "New plot")
    ),
    mainPanel(
      plotOutput("plot",height="800px")
    )
  )
)

server <- function(input, output) {
  output$plot<-renderPlot({
    input$newplot
    coalescent.plot(input$n,input$ngen,sleep=0)
  })
}

shinyApp(ui = ui, server = server)
