library(shiny)
library(rhandsontable)
library(PopGen)

w<-matrix(c("1.00","1.00","1.00"),1,3,dimnames=list(NULL,c("W(AA)","W(Aa)","W(aa)")))

ui <- fluidPage(
  h3("Genetic drift & selection at a biallelic locus",align="center"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="p0",label="p0 (initial value of p):",value=0.5,min=0,max=1,
        step=0.001,round=3),
      sliderInput(inputId="Ne",label="Ne (effective population size):",value=100,min=0,
        max=1000),
      sliderInput(inputId="ngen",label="number of generations:",value=400,min=0,
        max=1000),
      textInput(inputId="nrep",label="number of simulations:",value=5,width="80px"),
      p(strong("Fitnesses:")),
      rHandsontableOutput("w"),
      checkboxInput(inputId="react",
        label="reactive (plot changes instantly with new parameter values)",
        value=FALSE),
      checkboxInput(inputId="theoretic",
        label="superimpose theoretical expectation",value=FALSE),
      textInput(inputId="seed",
        label="set seed (optional)",value=""),
      actionButton("newplot","new plot"),
      h4("Instructions:\n"),
      p("This application simulates drift & natural selection on a biallelic locus.\n\n"),
      h4("Details:\n"),
      p("Web interface for drift.selection of the PopGen R package (Revell, 2018).")
    ),
    mainPanel(
      plotOutput("plot",width="100%",height="500px")
    )
  )
)

server <- function(input, output) {
  values=reactiveValues(w=w)
  output$plot<-renderPlot({
    if(input$ngen>0 && input$Ne>0){
      if(input$seed!="") set.seed(as.numeric(input$seed))
      par(mar=c(5.1,4.1,0.5,1.1))
      if(!input$react){
        input$newplot
        isolate({
          drift.selection(input$p0,input$Ne,w=as.numeric(values[["w"]]),
            ngen=input$ngen,nrep=as.numeric(input$nrep))
          if(input$theoretic)
            selection(input$p0,w=as.numeric(values[["w"]]),time=input$ngen,
              color=phytools::make.transparent("lightgrey",0.5),lwd=6,add=TRUE)
        })
      } else {
        input$newplot
        drift.selection(input$p0,input$Ne,w=as.numeric(values[["w"]]),
          ngen=input$ngen,nrep=as.numeric(input$nrep))
        if(input$theoretic)
        selection(input$p0,w=as.numeric(values[["w"]]),time=input$ngen,
          color=phytools::make.transparent("lightgrey",0.5),lwd=6,add=TRUE)
      }
    }
  })
  output$w = renderRHandsontable({
    if (!is.null(input$w)) {
      DF = hot_to_r(input$w)
      values[["w"]] = DF
      rhandsontable(DF)
    } else if (!is.null(values[["w"]])) {
      DF = values[["w"]]
      rhandsontable(DF)
    }
  })
}

shinyApp(ui = ui, server = server)
