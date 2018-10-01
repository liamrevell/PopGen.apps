library(shiny)
library(rhandsontable)
library(learnPopGen)

p0<-matrix(c("0.5","0.5"),1,2,dimnames=list(NULL,c("p[1]","p[2]")))
Ne<-matrix(c("400","400"),1,2,dimnames=list(NULL,c("Ne[1]","Ne[2]")))
w1<-matrix(c("1.00","1.00","1.00"),1,3,dimnames=list(NULL,c("W(AA)","W(Aa)","W(aa)")))
w2<-matrix(c("1.00","1.00","1.00"),1,3,dimnames=list(NULL,c("W(AA)","W(Aa)","W(aa)")))

ui <- fluidPage(
  h3("Migration, selection, and drift",align="center"),
  sidebarLayout(
    sidebarPanel(
      p(strong("Initial gene frequencies:")),
      rHandsontableOutput("p0"),
      br(),
      p(strong("Effective population sizes:")),
      rHandsontableOutput("Ne"),
      br(),
      sliderInput(inputId="m1",label="m[1] (rate of emigration pop'n 1):",
        value=0.01,min=0,max=0.5,step=0.01),
      sliderInput(inputId="m2",label="m[2] (rate of emigration pop'n 2):",
        value=0.01,min=0,max=0.5,step=0.01),
      sliderInput(inputId="ngen",label="number of generations:",value=400,min=0,
        max=1000),
      p(strong("Fitnesses population 1:")),
      rHandsontableOutput("w1"),
      br(),
      p(strong("Fitnesses population 2:")),
      rHandsontableOutput("w2"),
      checkboxInput(inputId="react",
        label="reactive (plot changes instantly with new parameter values)",
        value=FALSE),
      actionButton("newplot","new plot"),
      h4("Instructions:\n"),
      p("This application simulates migration, selection, & drift for a biallelic
        locus evolving in two populations potentially experiencing differential
        natural selection.\n\n"),
      h4("Details:\n"),
      p("Web interface for msd of the learnPopGen R package (Revell, 2018).")
    ),
    mainPanel(
      plotOutput("plot",width="100%",height="500px")
    )
  )
)

server <- function(input, output) {
  values=reactiveValues(p0=p0,Ne=Ne,w1=w1,w2=w2)
  output$plot<-renderPlot({
    par(mar=c(5.1,4.1,4.1,1.1))
    if(!input$react){
      input$newplot
      isolate(
        if(input$ngen>0){
          msd(p0=as.numeric(values[["p0"]]),Ne=as.integer(values[["Ne"]]),
              w=list(as.numeric(values[["w1"]]),as.numeric(values[["w2"]])),
              m=c(input$m1,input$m2),ngen=input$ngen)
        }
       )
    } else {
      input$newplot
      if(input$ngen>0){
        msd(p0=as.numeric(values[["p0"]]),Ne=as.integer(values[["Ne"]]),
            w=list(as.numeric(values[["w1"]]),as.numeric(values[["w2"]])),
            m=c(input$m1,input$m2),ngen=input$ngen)
      }
    }
  })
  output$p0 = renderRHandsontable({
    if (!is.null(input$p0)) {
      DF = hot_to_r(input$p0)
      values[["p0"]] = DF
      rhandsontable(DF)
    } else if (!is.null(values[["p0"]])) {
      DF = values[["p0"]]
      rhandsontable(DF)
    }
  })
  output$Ne = renderRHandsontable({
    if (!is.null(input$Ne)) {
      DF = hot_to_r(input$Ne)
      values[["Ne"]] = DF
      rhandsontable(DF)
    } else if (!is.null(values[["Ne"]])) {
      DF = values[["Ne"]]
      rhandsontable(DF)
    }
  })
  output$w1 = renderRHandsontable({
    if (!is.null(input$w1)) {
      DF = hot_to_r(input$w1)
      values[["w1"]] = DF
      rhandsontable(DF)
    } else if (!is.null(values[["w1"]])) {
      DF = values[["w1"]]
      rhandsontable(DF)
    }
  })
  output$w2 = renderRHandsontable({
    if (!is.null(input$w2)) {
      DF = hot_to_r(input$w2)
      values[["w2"]] = DF
      rhandsontable(DF)
    } else if (!is.null(values[["w2"]])) {
      DF = values[["w2"]]
      rhandsontable(DF)
    }
  })
}

shinyApp(ui = ui, server = server)
