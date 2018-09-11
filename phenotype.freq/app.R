library(shiny)
library(PopGen)

ui <- fluidPage(
  h3("Phenotypic trait distribution for a polygenic trait",align="center"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="nloci",
                  label="number of loci",
                  value=6,min=0,max=12),
      textInput(inputId="p",
        label="p(A) per locus, separated by commas (turn off reactive)",
        value=""),
      checkboxInput(inputId="random",
        label="use random allele frequencies"),
      checkboxInput(inputId="react",
        label=
        "reactive (plot changes instantly with new parameter values)",
        value=TRUE),
      actionButton("newplot","new plot"),
      h4("Instructions:\n"),
      p("This application computes the expected distribution of
        a polygenic phenotypic trait when each gene contributes 
        equally & additively to the trait value. Compare to one locus
        in which the trait distribution matches HW proportions.\n\n"),
      h4("Details:\n"),
      p("Web interface for phenotype.freq of the PopGen R package 
        (Revell, 2018).")
    ),
    mainPanel(
      plotOutput("plot",width="600px",
        height="500px")
    )
  )
)

server <- function(input, output) {  
  output$plot<-renderPlot({
    par(mar=c(5.1,4.1,0.5,1.1))
    if(!input$react){
      input$newplot
      isolate(
        if(input$nloci>0){
          if(input$p!=""){
              p<-as.numeric(strsplit(input$p,",")[[1]])
          } else p<-NULL
          if(input$random) p<-runif(input$nloci)
          phenotype.freq(input$nloci,p=p)
        })
    } else {
      input$newplot
      if(input$nloci>0){
        if(input$p!=""){
          p<-as.numeric(strsplit(input$p,",")[[1]])
        } else p<-NULL
        if(input$random) p<-runif(input$nloci)
        phenotype.freq(input$nloci,p=p)
      }
    }
  })
}

shinyApp(ui = ui, server = server)