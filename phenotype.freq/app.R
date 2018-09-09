library(shiny)
library(PopGen)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="nloci",
                  label="number of loci",
                  value=6,min=0,max=12),
      textInput(inputId="p",
        label="p(A) per locus, separated by commas (optional)",
        value=""),
      p("or:\n"),
      checkboxInput(inputId="random",
        label="random allele frequencies"),
      actionButton("newplot","new plot")
    ),
    mainPanel(
      plotOutput("plot",width="600px",
                 height="600px")
    )
  )
)

server <- function(input, output) {  
  output$plot<-renderPlot({
    input$newplot
    isolate(
      if(input$nloci>0){
        if(input$p!=""){
            p<-as.numeric(strsplit(input$p,",")[[1]])
        } else p<-NULL
        if(input$random) p<-runif(input$nloci)
        phenotype.freq(input$nloci,p=p)
      }
    )
  })
}

shinyApp(ui = ui, server = server)