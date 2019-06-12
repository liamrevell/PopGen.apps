library(shiny)
library(rhandsontable)
library(learnPopGen)

w<-matrix(c("1.00","0.90","0.80"),1,3,dimnames=list(NULL,c("W(AA)","W(Aa)","W(aa)")))

ui <- fluidPage(
  h3("Selection at a biallelic locus",align="center"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="p0",label="p0 (initial value of p):",value=0.01,min=0,max=1,
        step=0.01),
      sliderInput(inputId="ngen",label="number of generations:",value=100,min=0,
        max=1000),
      selectInput(inputId="show",label="Show plot of:",
        choices=c("p","q","fitness","surface","deltap","cobweb")),
      p(strong("Fitnesses:")),
      rHandsontableOutput("w"),
      checkboxInput(inputId="equil",
        label="show equilibrium (if applicable)",value=FALSE),
      checkboxInput(inputId="react",
        label="reactive (plot changes instantly with new parameter values)",
        value=TRUE),
      actionButton("newplot","new plot"),
      h4("Instructions:\n"),
      p("This application numerically analyzes a simple natural selection model for 
        a biallelic locus.\n\n"),
      h4("Details:\n"),
      p("Web interface for selection of the learnPopGen R package (Revell, 2019).")
    ),
    mainPanel(
      plotOutput("plot",width="100%",height="500px")
    )
  )
)

server <- function(input, output) {
  values=reactiveValues(w=w)
  output$plot<-renderPlot({
    if(input$ngen>0){
      par(mar=c(5.1,4.1,4.1,1.1))
      if(!input$react){
        input$newplot
        isolate(
          selection(input$p0,w=as.numeric(values[["w"]]),time=input$ngen,
            show=input$show,equil=input$equil,
            color=phytools::make.transparent("blue",0.5))
         )
      } else {
        input$newplot
        selection(input$p0,w=as.numeric(values[["w"]]),time=input$ngen,
          show=input$show,equil=input$equil,
          color=phytools::make.transparent("blue",0.5))
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
