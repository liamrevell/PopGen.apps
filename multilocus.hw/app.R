library(shiny)
library(learnPopGen)

ui <- fluidPage(
  h3("Multilocus Hardy-Weinberg frequencies",align="center"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="nloci",label="number of loci",value=2,min=1,max=6),
      textInput(inputId="p",
        label="p (allele frequency) per locus, separated by commas (turn off reactive)",
        value=""),
      checkboxInput(inputId="random",
        label="use random allele frequencies"),
      checkboxInput(inputId="react",
        label="reactive (plot changes instantly with new parameter values)",value=TRUE),
      actionButton("newplot","new plot"),
      h4("Instructions:\n"),
      p("This multilocus Hardy-Weinberg frequencies for a set of biallelic loci.\n\n"),
      h4("Details:\n"),
      p("Web interface for multilocus.hw of the learnPopGen R package (Revell, 2018).")
    ),
    mainPanel(
      plotOutput("plot",width="600px",height="500px")
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
          hw<-multilocus.hw(input$nloci,p=p)
          barplot(hw,las=2,cex.names=min(24/length(hw),1.5),ylim=c(0,min(2*max(hw),1)),
            col=phytools::make.transparent("blue",0.5),
            border=phytools::make.transparent("grey",0.5))
        })
    } else {
      input$newplot
      if(input$nloci>0){
        if(input$p!=""){
          p<-as.numeric(strsplit(input$p,",")[[1]])
        } else p<-NULL
        if(input$random) p<-runif(input$nloci)
        hw<-multilocus.hw(input$nloci,p=p)
        barplot(hw,las=2,cex.names=min(24/length(hw),1.5),ylim=c(0,min(2*max(hw),1)),
          col=phytools::make.transparent("blue",0.5),
          border=phytools::make.transparent("grey",0.5))
      }
    }
  })
}

shinyApp(ui = ui, server = server)
